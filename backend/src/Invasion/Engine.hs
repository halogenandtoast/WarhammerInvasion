{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Engine (module Invasion.Engine, module Invasion.Message) where

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Traversable
import Invasion.Capital
import Invasion.Card
import Invasion.CardDef
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Control.Concurrent.STM
import Queue
import System.Random.Shuffle

-- | A single incoming item on a game's mailbox. Either a fresh
-- 'Message' from a client (engine processes it like any queued msg)
-- or a 'PromptResult' answering an outstanding 'askPrompt'.
data EngineMail
  = EngineMsg Message
  | EnginePromptAnswer PromptResult
  deriving stock Show

-- | Per-game runtime context used when the engine runs as a long-lived
-- worker thread. Carries the mailbox the worker drains, the published
-- state TVar clients observe, and a broadcast hook the engine calls
-- after every state publish so the WebSocket layer can push updates.
data EngineCtx = EngineCtx
  { mailbox :: TQueue EngineMail
  , publishedState :: TVar Game
  , broadcastUpdate :: STM ()
  }

data Env = Env
  { queue :: Queue Message
  , game :: Game
  , ctx :: Maybe EngineCtx
    -- ^ 'Nothing' for one-shot 'applyMessage' calls (tests, debug).
    -- 'Just' once a 'GameWorker' is attached; receive bodies that
    -- call 'askPrompt' will then actually publish + block.
  }

newEnv :: Game -> IO Env
newEnv g = do
  q <- newQueue
  pure $ Env q g Nothing

newEnvWithCtx :: Game -> EngineCtx -> IO Env
newEnvWithCtx g c = do
  q <- newQueue
  pure $ Env q g (Just c)

newtype GameT a = GameT (StateT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadState Env)

instance HasQueue Message GameT where
  getQueue = gets (.queue)

instance HasGame GameT where
  getGame = gets (.game)

-- | Effect-typeclass for receive bodies that need to suspend until a
-- player answers. Tests stub it with an auto-resolver; the worker
-- thread runs the real blocking version.
class Monad m => HasPromptIO m where
  askPrompt :: Prompt -> m PromptResult

instance HasPromptIO m => HasPromptIO (StateT s m) where
  askPrompt = lift . askPrompt

instance HasPromptIO GameT where
  askPrompt p = do
    env <- get
    case env.ctx of
      Nothing ->
        -- Test / debug path: no worker is wired, so just decline.
        -- Receive bodies see PickNone / PickBool False (depending on
        -- prompt kind) and proceed as if the player skipped.
        pure (autoResolve p)
      Just c -> do
        -- Stash the prompt on the in-memory state, sync to the
        -- published TVar (so clients see it), then STM-retry-block
        -- waiting for an answer in the mailbox.
        modify \g -> g {game = g.game {pendingPrompt = Just p}}
        publishCurrent c
        answer <- liftIO (waitForPromptAnswer c.mailbox)
        modify \g -> g {game = g.game {pendingPrompt = Nothing}}
        publishCurrent c
        pure answer

-- | Default answer when no mailbox is attached. Most prompts in the
-- core set treat skip / decline as a no-op for the source card, which
-- keeps tests deterministic.
autoResolve :: Prompt -> PromptResult
autoResolve p = case p.kind of
  ChooseYesNo {} -> PickBool False
  _ -> PickNone

-- | Mirror the working state into the published TVar, then fire the
-- broadcast hook so clients re-render. Called whenever the engine
-- suspends (pending prompt) so the wire sees the latest snapshot.
publishCurrent :: EngineCtx -> GameT ()
publishCurrent c = do
  g <- gets (.game)
  liftIO $ atomically do
    writeTVar c.publishedState g
    c.broadcastUpdate

-- | STM-retry until a 'PromptResult' arrives in the mailbox. Any
-- 'EngineMsg's drained while waiting get put back at the front so
-- they process in arrival order after the prompt resolves.
waitForPromptAnswer :: TQueue EngineMail -> IO PromptResult
waitForPromptAnswer mb = atomically loop
  where
    loop = do
      drained <- drainAll
      case partitionAnswer [] drained of
        Just (r, rest) -> do
          traverse_ (writeTQueue mb) rest
          pure r
        Nothing -> do
          traverse_ (writeTQueue mb) drained
          retry
    drainAll = do
      ma <- tryReadTQueue mb
      case ma of
        Nothing -> pure []
        Just a -> (a :) <$> drainAll
    partitionAnswer _ [] = Nothing
    partitionAnswer acc (EnginePromptAnswer r : xs) =
      Just (r, reverse acc <> xs)
    partitionAnswer acc (x : xs) = partitionAnswer (x : acc) xs

data Deck = Deck
  { cards :: [CardCode]
  , race :: Race
  }

dwarfStarterDeck :: Deck
dwarfStarterDeck =
  let cards =
        replicate 3 "core-001"
          <> replicate 1 "core-002"
          <> replicate 3 "core-003"
          <> replicate 3 "core-004"
          <> replicate 3 "core-005"
          <> replicate 1 "core-006"
          <> replicate 1 "core-007"
          <> replicate 1 "core-008"
          <> replicate 1 "core-009"
          <> replicate 2 "core-010"
          <> replicate 2 "core-011"
          <> replicate 1 "core-012"
          <> replicate 1 "core-013"
          <> replicate 3 "core-014"
          <> replicate 1 "core-015"
          <> replicate 1 "core-016"
          <> replicate 2 "core-017"
          <> replicate 2 "core-018"
          <> replicate 1 "core-019"
          <> replicate 1 "core-020"
          <> replicate 2 "core-021"
          <> replicate 1 "core-022"
          <> replicate 1 "core-023"
          <> replicate 1 "core-024"
          <> replicate 1 "core-025"
      race = Dwarf
  in Deck {..}

type DeckLoadError = String

loadDeck :: Deck -> Either DeckLoadError (Race, [SomeCardDef])
loadDeck Deck {race, cards} = (race,) <$> for cards \c ->
  case Map.lookup c allCards of
    Nothing -> Left $ "Card not found: " <> show c
    Just cardDef -> Right cardDef

runGame :: GameT a -> Env -> IO a
runGame (GameT inner) = evalStateT inner

execGameT :: GameT a -> Env -> IO Env
execGameT (GameT inner) = execStateT inner

-- | Run a long-lived engine pump for one game. Owns the engine
-- state; processes incoming 'EngineMail' items one at a time. Each
-- 'EngineMsg' is fed into the queue and 'gameMain' drains it; any
-- 'askPrompt' inside a receive body publishes intermediate state to
-- 'publishedState' and blocks until the matching
-- 'EnginePromptAnswer' arrives. Loops forever; kill the thread to
-- stop. Stray 'EnginePromptAnswer's (no prompt outstanding) are
-- silently dropped.
runEngineWorker :: Game -> EngineCtx -> IO ()
runEngineWorker initial ctx = do
  env0 <- newEnvWithCtx initial ctx
  atomically do
    writeTVar ctx.publishedState initial
    ctx.broadcastUpdate
  let loop env = do
        item <- atomically (readTQueue ctx.mailbox)
        case item of
          EngineMsg msg -> do
            env' <- execGameT (send msg >> () <$ gameMain) env
            atomically do
              writeTVar ctx.publishedState env'.game
              ctx.broadcastUpdate
            loop env'
          EnginePromptAnswer _ ->
            -- Out-of-context prompt answer. Ignore and keep
            -- listening — askPrompt has its own loop that uses STM
            -- retry, so it won't miss a real answer.
            loop env
  loop env0

overGame :: (Game -> GameT Game) -> GameT ()
overGame f = do
  game <- f =<< getGame
  modify \e -> e {game}

-- | Pump the queue until either empty or the game has ended. Returning
-- here is also how the engine exposes "we're waiting for player input"
-- — an open action window stops emitting messages and the queue drains,
-- and a pending prompt halts pumping entirely until the client posts a
-- 'ResolvePrompt'.
gameMain :: GameT Game
gameMain = do
  game <- getGame
  case game.lifecycle of
    GameFinished _ -> pure game
    _ -> case game.pendingPrompt of
      Just _ -> pure game
      Nothing -> do
        mmsg <- pop
        case mmsg of
          Just msg -> do
            overGame $ distribute msg
            gameMain
          Nothing -> pure game

class Run a where
  distribute :: Message -> a -> GameT a
  distribute msg = execStateT (receive msg)
  receive :: Message -> StateT a GameT ()

send :: HasQueue Message m => Message -> m ()
send = push

class Keyed a where
  type KeyOf a
  toKey :: a -> KeyOf a

onKey :: (Keyed a, Eq (KeyOf a), Monad m) => KeyOf a -> StateT a m () -> StateT a m ()
onKey k f = do
  a <- get
  when (toKey a == k) f

instance Keyed Player where
  type KeyOf Player = PlayerKey
  toKey = (.key)

instance Run Player where
  receive = \case
    Setup -> do
      k <- gets (.key)
      send $ ShuffleDeck k
      send $ Draw $ Drawing StartingHand k
    ShuffleDeck k -> onKey k do
      deck <- shuffleM =<< gets (.deck)
      modify \p -> p {deck}
    Draw drawing -> onKey drawing.player do
      case drawing.kind of
        StartingHand -> do
          (hand, deck) <- splitAt 7 <$> gets (.deck)
          modify \p -> p {hand, deck}
        StandardDraw -> do
          deck <- gets (.deck)
          case deck of
            [] -> pure ()
            (c : rest) -> do
              hand <- gets (.hand)
              modify \p -> p {hand = hand <> [c], deck = rest}
          -- "Drawing from an empty deck does not auto-fail mid-phase
          -- — but the standing 'running out of cards' rule eliminates
          -- a player who has zero cards in deck." So check AFTER each
          -- draw, including the one that emptied the deck.
          deck' <- gets (.deck)
          elim <- gets (.eliminated)
          when (null deck' && not elim) $
            send $ Eliminate drawing.player DeckedOut
    ReturnResources k -> onKey k $
      modify \p -> p {resources = Resources 0}
    -- CollectResources and QuestDraw set the right values from
    -- Game.receive once it can see all in-play unit/support power
    -- icons across both sides. Player.receive deliberately doesn't
    -- touch resources / draws for these messages — the engine-level
    -- handler owns them.
    CollectResources _k -> pure ()
    QuestDraw _k -> pure ()
    Eliminate k reason -> onKey k $
      modify \p -> p {state = Eliminated reason}
    _ -> pure ()

instance Run Game where
  distribute msg g = do
    player1 <- distribute msg g.player1
    player2 <- distribute msg g.player2
    let preUnits = g.units
        preSupports = g.supports
        preQuests = g.quests
        preLegends = g.legends
    g' <- execStateT (receive msg) (g {player1, player2})
    -- Recompute cached effective stats on every unit before card
    -- receives run. This way attachments/experiences/burning all show
    -- through immediately and damage destruction uses the right HP.
    let g'' = recomputeUnitStats g'
    dispatchToInPlayUnits msg preUnits g''
    dispatchToInPlaySupports msg preSupports g''
    dispatchToInPlayQuests msg preQuests g''
    dispatchToInPlayLegends msg preLegends g''
    pure g''
  receive = \case
    Setup -> do
      fp <- sample2 Player1 Player2
      modify \g -> g {firstPlayer = fp, currentPlayer = fp}
      logIt LogSystem "log.setup.begins" [("player", playerParam fp)]
    BeginGame -> do
      fp <- gets (.firstPlayer)
      modify \g -> g {lifecycle = GamePlaying}
      logIt LogSystem "log.game.begins" []
      send $ BeginTurn fp
    BeginTurn k -> do
      modify \g ->
        g
          { currentPlayer = k
          , turn = g.turn + Turn 1
          , history = Map.insert ThisTurn mempty g.history
          }
      t <- gets (.turn)
      logIt LogTurn
        "log.turn.begins"
        [("turn", turnText t), ("player", playerParam k)]
      -- Phase 0 (FAQ 2.2): all "at the beginning of the turn" triggered
      -- effects fire on BeginTurn itself via card receive hooks, then
      -- this action window lets either player respond before the
      -- Kingdom phase begins.
      send $ OpenActionWindow BeginningOfTurnActionWindow
      send $ BeginPhase KingdomPhase
    EndTurn k -> do
      -- Phase 5 (FAQ 2.2): action window first, then "at end of turn"
      -- triggered effects fire, then EndOfTurn modifiers expire.
      send $ OpenActionWindow EndOfTurnActionWindow
      -- Fire scheduled end-of-turn effects after the window closes.
      pending <- gets (.pendingEndOfTurn)
      modify \g -> g {pendingEndOfTurn = []}
      traverse_ firePendingEffect pending
      -- Drop EndOfTurn modifiers.
      send $ ClearScopedModifiers EndOfTurn
      modify \g -> g {phase = Nothing}
      logIt LogTurn "log.turn.ends" [("player", playerParam k)]
      send $ BeginTurn k.next
    BeginPhase phase -> do
      g <- get
      modify \gx ->
        gx
          { phase = Just phase
          , history = Map.insert ThisPhase mempty gx.history
          }
      if shouldSkipFirstTurnPhase phase g
        then do
          logIt LogPhase "log.phase.skipped" [("phase", phaseParam phase)]
          send $ EndPhase phase
        else do
          logIt LogPhase "log.phase.begins" [("phase", phaseParam phase)]
          let active = g.currentPlayer
          case phase of
            KingdomPhase -> do
              send $ ReturnResources active
              send $ RestoreOneCorruptCard active
              send $ CollectResources active
              send $ OpenActionWindow KingdomActionWindow
            QuestPhase -> do
              send $ QuestDraw active
              send $ OpenActionWindow QuestActionWindow
            CapitalPhase ->
              send $ OpenActionWindow CapitalActionWindow
            BattlefieldPhase ->
              -- With no units yet, the active player has no attack to
              -- declare; the single window suffices. Combat will later
              -- emit the 5-step sub-sequence here.
              send $ OpenActionWindow BattlefieldActionWindow
    EndPhase phase -> do
      -- Fire any scheduled end-of-phase effects for this phase before
      -- handing off.
      (mine, rest) <- gets (partition ((== phase) . fst) . (.pendingEndOfPhase))
      modify \g -> g {pendingEndOfPhase = rest}
      traverse_ (firePendingEffect . snd) mine
      modify \g -> g {phase = Nothing}
      logIt LogPhase "log.phase.ends" [("phase", phaseParam phase)]
      case nextPhase phase of
        Just np -> send $ BeginPhase np
        Nothing -> do
          current <- gets (.currentPlayer)
          send $ EndTurn current
    OpenActionWindow trigger -> do
      current <- gets (.currentPlayer)
      let aw = ActionWindow {trigger, awaiting = NoPasses current}
      modify \g ->
        let stack' = aw : g.actionWindowStack
         in g
              { actionWindowStack = stack'
              , actionWindow = Just aw
              }
      logIt LogSystem
        "log.window.open"
        [("trigger", triggerParam trigger), ("player", playerParam current)]
      maybeAutoPassPriority trigger current
    PassPriority k -> do
      g <- get
      case g.actionWindowStack of
        (aw : rest) | priorityHolder aw.awaiting == k -> do
          logIt LogPlayerAction "log.priority.pass" [("player", playerParam k)]
          case aw.awaiting of
            NoPasses _ -> do
              let aw' = aw {awaiting = OnePass k.next}
                  stack' = aw' : rest
              modify \gx ->
                gx {actionWindowStack = stack', actionWindow = Just aw'}
              maybeAutoPassPriority aw.trigger k.next
            OnePass _ ->
              -- Both players have now passed consecutively.
              send CloseActionWindow
        -- Invalid pass (no window open, or not the priority holder):
        -- silently ignore. The server should reject these at the
        -- protocol boundary; this is belt-and-braces.
        _ -> pure ()
    CloseActionWindow -> do
      g <- get
      let (closed, rest) = case g.actionWindowStack of
            (w : ws) -> (Just w, ws)
            [] -> (Nothing, [])
          trigger = (.trigger) <$> closed
      modify \gx ->
        gx
          { actionWindowStack = rest
          , actionWindow = case rest of
              (w : _) -> Just w
              [] -> Nothing
          }
      logIt LogSystem "log.window.close" []
      -- Combat sub-step windows advance to the next sub-step; the
      -- top-of-stack phase window ends its phase.
      case trigger of
        Just AfterDeclareCombatTarget -> send AdvanceCombatToAttackers
        Just AfterDeclareAttackers -> send AdvanceCombatToDefenders
        Just AfterDeclareDefenders -> send AdvanceCombatToAssign
        Just AfterAssignCombatDamage -> send AdvanceCombatToApply
        Just AfterApplyCombatDamage -> send EndCombat
        _ -> case g.phase of
          Just p -> send $ EndPhase p
          Nothing -> pure ()
    Eliminate k reason -> do
      -- A player whose elimination is being processed loses immediately;
      -- the other player wins. If both somehow become eliminated, the
      -- first to be processed determines the winner.
      g <- get
      case g.lifecycle of
        GameFinished _ -> pure ()
        _ -> do
          let result = GameResult
                { winner = k.next
                , reason = case reason of
                    DeckedOut -> OpponentDeckedOut
                    CapitalBurned -> OpponentCapitalBurned
                }
          modify \gx -> gx {lifecycle = GameFinished result}
          logIt LogResult
            "log.player.eliminated"
            [("player", playerParam k), ("reason", elimReasonParam reason)]
          logIt LogResult
            "log.game.over"
            [ ("winner", playerParam result.winner)
            , ("reason", winReasonParam result.reason)
            ]
    -- Player-upkeep messages: the Player 'Run' instance has already
    -- carried out the actual mutation by the time we get here (Player
    -- runs before Game in 'distribute'). We only narrate them.
    ShuffleDeck k ->
      logIt LogSystem "log.deck.shuffled" [("player", playerParam k)]
    Draw drawing -> case drawing.kind of
      StartingHand ->
        logIt LogSystem
          "log.draw.opening"
          [("player", playerParam drawing.player)]
      StandardDraw ->
        logIt LogSystem
          "log.draw.card"
          [("player", playerParam drawing.player)]
    ReturnResources k ->
      logIt LogSystem "log.resources.returned" [("player", playerParam k)]
    CollectResources k -> do
      g <- get
      let n = zonePower g k KingdomZone
          p = lookupPlayer k g
          p' = p {resources = Resources n}
      modify (setPlayer k p')
      logIt LogSystem
        "log.resources.collected"
        [("player", playerParam k), ("count", tshow n)]
    QuestDraw k -> do
      g <- get
      let n = zonePower g k QuestZone
      logIt LogSystem "log.quest.draw" [("player", playerParam k)]
      replicateM_ n (send (Draw (Drawing StandardDraw k)))
    PlayUnit pk cardKey zone ->
      -- Reuse the card's existing key as its in-play UnitKey. This is
      -- what lets the frontend's view-transition map a hand card to its
      -- zone landing spot.
      withPaidPlay pk (takeUnitFromHand cardKey) (\g cd -> effectiveTotalCost g pk cd)
        \cardDef paidPlayer n -> do
          let unit = freshUnit cardKey pk zone cardDef
          modify \gx -> (setPlayer pk paidPlayer gx) {units = unit : gx.units}
          logIt LogPlayerAction "log.unit.played"
            [ ("player", playerParam pk)
            , ("card", T.pack cardDef.title)
            , ("cost", tshow n)
            ]
          send $ UnitEnteredPlay pk cardKey
    PlayUnitOnQuest pk cardKey questKey -> do
      g <- get
      let player = lookupPlayer pk g
      case (takeUnitFromHand cardKey player, findQuest questKey g) of
        (Just (cardDef, playerWithoutCard), Just q)
          | q.controller == pk
          , q.questingUnit == Nothing
          , BattlefieldOnly `notElem` cardDef.keywords
          , canPlayCard pk cardDef g ->
              case cardDef.cost of
                Variable -> pure ()
                Fixed printed -> do
                  let n = effectiveUnitCost g pk cardDef printed
                  when (player.resources >= Resources n) $ do
                    markPlayedLimited cardDef
                    let paidPlayer =
                          playerWithoutCard
                            {resources = player.resources - Resources n}
                        unitDetails =
                          UnitDetails
                            { key = cardKey
                            , controller = pk
                            , zone = QuestZone
                            , cardDef
                            , damage = Damage 0
                            , corrupted = False
                            , attachments = []
                            , experiences = []
                            , effectivePower = cardDef.power
                            , effectiveMaxHP = unitPrintedHPFromDef cardDef
                            , attacking = False
                            , defending = False
                            }
                        q' = (q {questingUnit = Just cardKey}) :: QuestDetails
                    modify \gx ->
                      (setPlayer pk paidPlayer gx)
                        { units = unitDetails : gx.units
                        , quests = replaceQuest q' gx.quests
                        }
                    logIt LogPlayerAction
                      "log.unit.played_on_quest"
                      [ ("player", playerParam pk)
                      , ("card", T.pack cardDef.title)
                      , ("quest", T.pack q.cardDef.title)
                      , ("cost", tshow n)
                      ]
                    send $ UnitEnteredPlay pk cardKey
        _ -> pure ()
    UnitEnteredPlay pk _key ->
      -- The card's own 'receive' fires via 'dispatchToInPlayUnits'.
      -- Game just narrates.
      logIt LogSystem "log.unit.entered_play" [("player", playerParam pk)]
    AssignUnitToQuest pk uKey qKey -> do
      g <- get
      case (findUnit uKey g, findQuest qKey g) of
        (Just u, Just q)
          | u.controller == pk
          , u.zone == QuestZone
          , q.controller == pk
          , q.questingUnit == Nothing -> do
              let q' = (q {questingUnit = Just uKey}) :: QuestDetails
              modify \gx -> gx {quests = replaceQuest q' gx.quests}
              logIt LogSystem
                "log.unit.assigned_to_quest"
                [ ("player", playerParam pk)
                , ("card", T.pack u.cardDef.title)
                , ("quest", T.pack q.cardDef.title)
                ]
        _ -> pure ()
    DealDamageToUnit ukey amount -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
          -- Passive multiplier (Bloodletter doubles all damage) applies
          -- to the raw assignment; Toughness then cancels off the top,
          -- then per-turn caps (Daemonettes of Slaanesh) clip the
          -- remainder.
          let inflated = applyDamageMultipliers g (max 0 amount)
              toughness = totalToughness g u
              afterToughness = max 0 (inflated - toughness)
              already =
                Map.findWithDefault 0 ukey
                  (historyOfScope ThisTurn g).damageTaken
              capped = applyPerTurnCap u already afterToughness
              landing = capped
              cancelled = inflated - landing
          when (cancelled > 0) $
            logIt LogSystem
              "log.damage.cancelled"
              [ ("card", T.pack u.cardDef.title)
              , ("amount", tshow cancelled)
              ]
          when (landing > 0) $ do
            let Damage existing = u.damage
                newDmg = Damage (existing + landing)
                u' = u {damage = newDmg} :: UnitDetails
            modify \gx -> gx {units = replaceUnit u' gx.units}
            recordEvent \h -> h
              { damageTaken = Map.insertWith (+) ukey landing h.damageTaken
              , damagedUnits =
                  if ukey `elem` h.damagedUnits
                    then h.damagedUnits
                    else ukey : h.damagedUnits
              }
            logIt LogSystem
              "log.unit.damaged"
              [ ("card", T.pack u.cardDef.title)
              , ("amount", tshow landing)
              ]
            let Damage total = newDmg
            when (total >= u.effectiveMaxHP) $
              send $ DestroyUnit ukey
    DealDamageToUnitUncancellable ukey amount -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
          -- Uncancellable damage still respects per-turn caps
          -- (Daemonettes) — the cap is independent of cancellation.
          let inflated = applyDamageMultipliers g (max 0 amount)
              already =
                Map.findWithDefault 0 ukey
                  (historyOfScope ThisTurn g).damageTaken
              landing = applyPerTurnCap u already inflated
          when (landing > 0) $ do
            let Damage existing = u.damage
                newDmg = Damage (existing + landing)
                u' = u {damage = newDmg} :: UnitDetails
            modify \gx -> gx {units = replaceUnit u' gx.units}
            recordEvent \h -> h
              { damageTaken = Map.insertWith (+) ukey landing h.damageTaken
              , damagedUnits =
                  if ukey `elem` h.damagedUnits
                    then h.damagedUnits
                    else ukey : h.damagedUnits
              }
            logIt LogSystem
              "log.unit.damaged"
              [ ("card", T.pack u.cardDef.title)
              , ("amount", tshow landing)
              ]
            let Damage total = newDmg
            when (total >= u.effectiveMaxHP) $
              send $ DestroyUnit ukey
    HealUnit ukey amount -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
          let Damage existing = u.damage
              healed = max 0 (existing - max 0 amount)
              u' = (u {damage = Damage healed}) :: UnitDetails
          modify \gx -> gx {units = replaceUnit u' gx.units}
          logIt LogSystem
            "log.unit.healed"
            [ ("card", T.pack u.cardDef.title)
            , ("amount", tshow amount)
            ]
    DestroyUnit ukey -> do
      munit <- gets (findUnit ukey)
      whenJust munit \u -> do
          -- Remove the unit and all its attachments. Each card lands in
          -- its OWN controller's discard pile carrying the same key it
          -- had in play, so the frontend's view-transition continues to
          -- track the same card visually from board to pile.
          --
          -- Attachments may be controlled by either side — Branded by
          -- Khorne is the canonical hostile attachment.
          discardToController u.controller $ mkCard u.key (UnitCardDef u.cardDef)
          for_ u.attachments \a ->
            discardToController a.controller $ mkCard a.key (SupportCardDef a.cardDef)
          modify \gx -> gx {units = removeById ukey gx.units}
          recordEvent \h -> h {unitsDiscarded = h.unitsDiscarded + 1}
          logIt LogSystem
            "log.unit.destroyed"
            [ ("player", playerParam u.controller)
            , ("card", T.pack u.cardDef.title)
            ]
          send $ UnitLeftPlay DepartedUnit
            { key = ukey
            , controller = u.controller
            , zone = u.zone
            , cardDef = u.cardDef
            }
    UnitLeftPlay du -> do
      let ukey = du.key
      -- If the departed unit was questing on a quest, clear the slot
      -- and dump accumulated resource tokens.
      g <- get
      let touched =
            [ (q {questingUnit = Nothing, tokens = 0}) :: QuestDetails
            | q <- g.quests
            , q.questingUnit == Just ukey
            ]
      unless (null touched) $ do
        let updated = foldr replaceQuest g.quests touched
        modify \gx -> gx {quests = updated}
        traverse_
          ( \q ->
              logIt LogSystem
                "log.quest.unit_left"
                [("quest", T.pack q.cardDef.title)]
          )
          touched
      -- Pure hook point beyond the questing-slot bookkeeping:
      -- 'dispatchToInPlayUnits' runs cards' bespoke reactions; Game
      -- itself has nothing more to do.
      pure ()
    CorruptUnit ukey -> setCorrupted True "log.unit.corrupted" ukey
    CleanseUnit ukey -> setCorrupted False "log.unit.cleansed" ukey
    RestoreOneCorruptCard pk -> do
      g <- get
      whenJust (find (\u -> u.controller == pk && u.corrupted) g.units) \u ->
        send $ CleanseUnit u.key
    PlayAttachment pk cardKey targetKey -> do
      mhost <- gets (findUnit targetKey)
      whenJust mhost \host ->
        withPaidPlay pk (takeSupportFromHand cardKey)
          (\g cd -> effectiveTotalCost g pk cd
                  + extraTargetTax pk (TargetUnit targetKey) g)
          \cardDef paidPlayer n -> do
            let attachment = freshSupport cardKey pk host.zone (Just targetKey) cardDef
                host' = (host {attachments = attachment : host.attachments}) :: UnitDetails
            modify \gx -> (setPlayer pk paidPlayer gx) {units = replaceUnit host' gx.units}
            logIt LogPlayerAction "log.attachment.played"
              [ ("player", playerParam pk)
              , ("card", T.pack cardDef.title)
              , ("target", T.pack host.cardDef.title)
              , ("cost", tshow n)
              ]
            send $ SupportEnteredPlay pk cardKey
    SupportEnteredPlay _pk _key ->
      -- 'dispatchToInPlayUnits' walks attachments via their host; any
      -- bespoke reaction lives in the support card's 'receive'.
      pure ()
    PlaySupport pk cardKey zone ->
      withPaidPlay pk (takeSupportFromHand cardKey) (\g cd -> effectiveTotalCost g pk cd)
        \cardDef paidPlayer n -> do
          let support = freshSupport cardKey pk zone Nothing cardDef
          modify \gx -> (setPlayer pk paidPlayer gx) {supports = support : gx.supports}
          logIt LogPlayerAction "log.support.played"
            [ ("player", playerParam pk)
            , ("card", T.pack cardDef.title)
            , ("cost", tshow n)
            ]
          send $ SupportEnteredPlay pk cardKey
    PlaySupportFromDeck pk cardKey zone -> do
      player <- getPlayerS pk
      whenJust (takeSupportFromDeck cardKey player) \(cardDef, playerWithoutCard) -> do
        let support = freshSupport cardKey pk zone Nothing cardDef
        modify \gx -> (setPlayer pk playerWithoutCard gx) {supports = support : gx.supports}
        logIt LogSystem "log.support.played_from_deck"
          [("player", playerParam pk), ("card", T.pack cardDef.title)]
        send $ SupportEnteredPlay pk cardKey
    PlayQuest pk cardKey ->
      withPaidPlay pk (takeQuestFromHand cardKey) (\g cd -> effectiveTotalCost g pk cd)
        \cardDef paidPlayer n -> do
          let hostPlayer
                | PlayInOpponentArea `elem` cardDef.keywords = pk.next
                | otherwise = pk
              quest = QuestDetails
                { key = cardKey
                , controller = pk
                , zoneOwner = hostPlayer
                , cardDef
                , tokens = 0
                , questingUnit = Nothing
                }
          modify \gx -> (setPlayer pk paidPlayer gx) {quests = quest : gx.quests}
          logIt LogPlayerAction "log.quest.played"
            [ ("player", playerParam pk)
            , ("card", T.pack cardDef.title)
            , ("cost", tshow n)
            ]
          send $ QuestEnteredPlay pk cardKey
    QuestEnteredPlay _pk _key ->
      -- Per-card reactions fire via dispatch (see
      -- 'dispatchToInPlayUnits' which now also walks 'Game.supports'
      -- and 'Game.quests').
      pure ()
    AdjustSupportTokens skey delta -> do
      g <- get
      whenJust (findSupport skey g) \s -> do
          let n = max 0 (s.tokens + delta)
              s' = (s {tokens = n}) :: SupportDetails
          modify \gx -> gx {supports = replaceSupport s' gx.supports}
          logIt LogSystem
            "log.support.tokens"
            [ ("card", T.pack s.cardDef.title)
            , ("count", tshow n)
            ]
    AdjustQuestTokens qkey delta -> do
      g <- get
      whenJust (findQuest qkey g) \q -> do
          let n = max 0 (q.tokens + delta)
              q' = (q {tokens = n}) :: QuestDetails
          modify \gx -> gx {quests = replaceQuest q' gx.quests}
          logIt LogSystem
            "log.quest.tokens"
            [ ("card", T.pack q.cardDef.title)
            , ("count", tshow n)
            ]
    DestroySupport skey -> do
      msupport <- gets (findSupport skey)
      whenJust msupport \s -> do
        discardToController s.controller $ mkCard s.key (SupportCardDef s.cardDef)
        modify \gx -> gx {supports = removeById skey gx.supports}
        logIt LogSystem "log.support.destroyed"
          [("player", playerParam s.controller), ("card", T.pack s.cardDef.title)]
        send $ SupportLeftPlay s.controller skey s.cardDef.code
    SupportLeftPlay _pk _skey _code -> pure ()
    DestroyQuest qkey -> do
      mquest <- gets (findQuest qkey)
      whenJust mquest \q -> do
        discardToController q.controller $ mkCard q.key (QuestCardDef q.cardDef)
        modify \gx -> gx {quests = removeById qkey gx.quests}
        logIt LogSystem "log.quest.destroyed"
          [("player", playerParam q.controller), ("card", T.pack q.cardDef.title)]
        send $ QuestLeftPlay q.controller qkey q.cardDef.code
    QuestLeftPlay _pk _qkey _code -> pure ()
    AttachExperience hostKey expCode -> do
      g <- get
      whenJust (findUnit hostKey g) \u -> do
          let u' = (u {experiences = expCode : u.experiences}) :: UnitDetails
          modify \gx -> gx {units = replaceUnit u' gx.units}
          logIt LogSystem
            "log.unit.experience_attached"
            [ ("card", T.pack u.cardDef.title)
            , ("count", tshow (length u'.experiences))
            ]
    PlayTactic pk cardKey target -> do
      g <- get
      let player = lookupPlayer pk g
      case takeTacticFromHand cardKey player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard)
          | not (canPlayCard pk cardDef g) ->
              -- Limited already played this turn (tactics never trip
              -- the uniqueness check because they don't persist).
              pure ()
          | not (validateTarget pk (tacticTargetSchema cardDef) target g) ->
              pure ()
          | otherwise -> case cardDef.cost of
              Variable -> pure ()
              Fixed _ -> do
                let baseCost = effectiveTotalCost g pk cardDef
                    targetTax = extraTargetTax pk target g
                    n = baseCost + targetTax
                when (player.resources >= Resources n) $ do
                  markPlayedLimited cardDef
                  let paidPlayer =
                        playerWithoutCard
                          { resources = player.resources - Resources n
                          , discard =
                              mkCard cardKey (TacticCardDef cardDef)
                                : playerWithoutCard.discard
                          }
                  modify (setPlayer pk paidPlayer)
                  logIt LogPlayerAction
                    "log.tactic.played"
                    [ ("player", playerParam pk)
                    , ("card", T.pack cardDef.title)
                    , ("cost", tshow n)
                    ]
                  send $ TacticResolved pk cardDef.code target
    TacticResolved pk code target -> do
      g <- get
      case Map.lookup code allCards of
        Just (TacticCardDef cardDef) -> do
          let ctx = TacticContext {controller = pk, cardDef}
              owner = lookupPlayer pk g
          case cardDef.receive of
            Receive f -> f (TacticResolved pk code target) owner ctx
        _ -> pure ()
    RequestPrompt p -> do
      modify \g -> g {pendingPrompt = Just p}
      logIt LogSystem
        "log.prompt.opened"
        [("player", playerParam p.player)]
    ResolvePrompt result -> do
      g <- get
      case g.pendingPrompt of
        Nothing -> pure ()
        Just p -> do
          modify \gx -> gx {pendingPrompt = Nothing}
          logIt LogSystem
            "log.prompt.resolved"
            [("player", playerParam p.player)]
          dispatchPromptCallback p.callback result
    TriggerCardAction pk srcKey idx target -> do
      g <- get
      case findActionSource srcKey g of
        Nothing -> pure ()
        Just src -> case actionAt src idx of
          Nothing -> pure ()
          Just (name, baseCost, schema) -> do
            let player = lookupPlayer pk g
                tax = extraTargetTax pk target g
                totalCost = baseCost + tax
                extras = actionExtraCostsAt src idx
            when (validateActionSource pk src) $
              when (actionAvailableHere src idx) $
                when (player.resources >= Resources totalCost) $
                  when (validateTarget pk schema target g) $
                    when (canPayExtras pk extras g) $ do
                      let paid = player {resources = player.resources - Resources totalCost}
                      modify (setPlayer pk paid)
                      logIt LogPlayerAction
                        "log.action.triggered"
                        [ ("player", playerParam pk)
                        , ("card", T.pack (actionSourceTitle src))
                        , ("action", name)
                        , ("cost", tshow totalCost)
                        ]
                      mpayments <- payExtras pk extras
                      whenJust mpayments \payments ->
                        fireAction src idx pk target payments
    DeferDamageToUnitUntilEoT ukey n -> do
      modify \g ->
        g {pendingEndOfTurn = PEDealDamageToUnit ukey n : g.pendingEndOfTurn}
      logIt LogSystem
        "log.effect.deferred_damage"
        [ ("amount", tshow n)
        , ("trigger", "EndTurn")
        ]
    DealDamageToZone targetPlayer zoneKind raw -> do
      let amount = max 0 raw
      when (amount > 0) $ do
        g <- get
        let target = lookupPlayer targetPlayer g
            zoneL = getZone zoneKind target
            -- Add damage; burn if it now meets or exceeds HP, and if
            -- that's the second burn on this capital eliminate the
            -- player.
            Damage existing = zoneL.damage
            HitPoints zoneHp = zoneL.hitPoints
            total = existing + amount
            (newDmg, justBurned) =
              if total >= zoneHp && not zoneL.burned
                then (Damage 0, True)
                else (Damage total, False)
            zoneL' =
              zoneL
                { damage = newDmg
                , burned = zoneL.burned || justBurned
                }
            target' = setZone zoneKind zoneL' target
        modify (setPlayer targetPlayer target')
        logIt LogSystem
          "log.zone.damaged"
          [ ("player", playerParam targetPlayer)
          , ("zone", zoneParam zoneKind)
          , ("amount", tshow amount)
          ]
        when justBurned $ do
          logIt LogResult
            "log.zone.burned"
            [ ("player", playerParam targetPlayer)
            , ("zone", zoneParam zoneKind)
            ]
          -- Check for elimination (two burned zones = lose).
          let burnedNow = burnedZoneCount target'.capital
          when (burnedNow >= 2) $
            send $ Eliminate targetPlayer CapitalBurned
    HealCapital pk raw -> do
      -- Heal up to N total damage tokens off the capital, spending the
      -- budget greedily on the most-damaged unburned zone, then the
      -- next, and so on. Burned zones are not healed.
      let budget = max 0 raw
      when (budget > 0) $ do
        g <- get
        let player0 = lookupPlayer pk g
            step p b
              | b <= 0 = p
              | otherwise =
                  let candidates =
                        [ (d, z)
                        | z <- [KingdomZone, QuestZone, BattlefieldZone]
                        , let zL = getZone z p
                        , not zL.burned
                        , let Damage d = zL.damage
                        , d > 0
                        ]
                   in case candidates of
                        [] -> p
                        _ ->
                          let (dMost, zMost) = maximum candidates
                              taken = min b dMost
                              zL = getZone zMost p
                              zL' = (zL {damage = Damage (dMost - taken)}) :: Zone
                           in step (setZone zMost zL' p) (b - taken)
            player' = step player0 budget
        modify (setPlayer pk player')
        logIt LogSystem
          "log.capital.healed"
          [("player", playerParam pk), ("amount", tshow budget)]
    HealZone pk zone raw -> do
      let amount = max 0 raw
      when (amount > 0) $ do
        g <- get
        let player = lookupPlayer pk g
            zoneL = getZone zone player
            Damage d = zoneL.damage
            taken = min amount d
        when (taken > 0) $ do
          let zoneL' = (zoneL {damage = Damage (d - taken)}) :: Zone
              player' = setZone zone zoneL' player
          modify (setPlayer pk player')
          logIt LogSystem
            "log.zone.healed"
            [ ("player", playerParam pk)
            , ("zone", zoneParam zone)
            , ("amount", tshow taken)
            ]
    AddDevelopment pk zone -> do
      g <- get
      let player = lookupPlayer pk g
      case player.deck of
        [] -> pure ()
        (topCard : restDeck) -> do
          let zoneL = getZone zone player
              Developments d = zoneL.developments
              zoneL' = (zoneL {developments = Developments (d + 1)}) :: Zone
              existing = Map.findWithDefault [] zone player.developmentCards
              player' =
                (setZone zone zoneL' player)
                  { deck = restDeck
                  , developmentCards =
                      Map.insert zone (topCard : existing) player.developmentCards
                  }
          modify (setPlayer pk player')
          logIt LogSystem
            "log.zone.development_added"
            [("player", playerParam pk), ("zone", zoneParam zone)]
          -- Decking-out check: AddDevelopment can empty the deck too.
          when (null restDeck) $
            send $ Eliminate pk DeckedOut
    DealDamageToEachEnemyUnitInZone pk zone raw -> do
      let amount = max 0 raw
      when (amount > 0) $ do
        g <- get
        let targets =
              [ u.key
              | u <- g.units
              , u.controller /= pk
              , u.zone == zone
              ]
        traverse_ (\k -> send (DealDamageToUnit k amount)) targets
    DealDamageToEachUnitInCombat amount -> do
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just cs ->
          traverse_ (\k -> send (DealDamageToUnit k (max 0 amount))) (cs.attackers <> cs.defenders)
    CancelAllBattlefieldDamageThisTurn -> do
      -- The crude version: drop the in-flight combat (cancels any
      -- pending damage that would have been dealt) and clear the
      -- attacker list. Subsequent attacks this phase are still allowed
      -- per the card text — the engine doesn't yet expose a "this
      -- turn" suppression flag, so this is a best-effort approximation.
      modify \gx -> gx {combat = Nothing}
      logIt LogSystem "log.combat.cancelled" []
    CancelAssignedDamageOnUnit ukey raw -> do
      let cap = max 0 raw
      modify \gx -> case gx.combat of
        Nothing -> gx
        Just cs ->
          let pa' =
                map
                  ( \pd -> case pd.target of
                      PDUnit k
                        | k == ukey ->
                            pd {cancellable = max 0 (pd.cancellable - cap)}
                      _ -> pd
                  )
                  cs.pendingAssignments
              cs' = (cs {pendingAssignments = pa'}) :: CombatState
           in gx {combat = Just cs'}
      logIt LogSystem
        "log.damage.cancelled_pending"
        [("amount", tshow cap)]
    CancelAllAssignedDamage -> do
      modify \gx -> case gx.combat of
        Nothing -> gx
        Just cs ->
          let cs' = (cs {pendingAssignments = []}) :: CombatState
           in gx {combat = Just cs'}
      logIt LogSystem "log.damage.cancelled_all" []
    DiscardRandomFromHand pk -> do
      g <- get
      let player = lookupPlayer pk g
      case player.hand of
        [] -> pure ()
        cards -> do
          idx <- getRandomR (0, length cards - 1)
          let (before, after) = splitAt idx cards
          case after of
            (picked : rest) -> do
              let player' =
                    player
                      { hand = before <> rest
                      , discard = picked : player.discard
                      }
              modify (setPlayer pk player')
              logIt LogSystem
                "log.hand.discarded"
                [("player", playerParam pk)]
            [] -> pure ()
    GainResources pk raw -> do
      let amount = max 0 raw
      when (amount > 0) $ do
        g <- get
        let player = lookupPlayer pk g
            Resources r = player.resources
            player' = player {resources = Resources (r + amount)}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.resources.gained"
          [("player", playerParam pk), ("amount", tshow amount)]
    SpendResources pk raw -> do
      let amount = max 0 raw
      when (amount > 0) $ do
        g <- get
        let player = lookupPlayer pk g
            Resources r = player.resources
            spent = min amount r
            player' = player {resources = Resources (r - spent)}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.resources.spent"
          [("player", playerParam pk), ("amount", tshow spent)]
    BeginCombat attacker zone attackerKeys -> do
      g <- get
      let defender = attacker.next
          -- Filter attackers by per-card eligibility (Sworn of Khorne,
          -- corruption gating, etc.) before committing the combat.
          eligible = filter (eligibleAttacker g defender zone) attackerKeys
      if null eligible
        then do
          logIt LogSystem
            "log.combat.aborted"
            [("attacker", playerParam attacker)]
        else do
          -- Auto-pick defenders: every enemy unit currently in the
          -- corresponding zone. Real combat would prompt the defender.
          let autoDefenders =
                [ u.key
                | u <- g.units
                , u.controller == defender
                , u.zone == zone
                , not u.corrupted
                ]
              -- Rune-of-Fortitude family: each support whose
              -- 'runeOfFortitudeTax' slice is set, sitting in the
              -- defender's same zone, imposes the 1-per-attacker tax.
              -- All-or-nothing approximation: if the attacker can
              -- afford the full tax, pay it and the penalty stays at
              -- 0; otherwise leave resources intact and impose -1
              -- per attacker for this combat.
              runeHere =
                any
                  ( \s ->
                      s.controller == defender
                        && s.zone == zone
                        && s.cardDef.extras.runeOfFortitudeTax
                  )
                  g.supports
              attackerPlayer = lookupPlayer attacker g
              Resources attackerRes = attackerPlayer.resources
              runeCost = length eligible
              (paidRune, penalty) =
                if runeHere
                  then if attackerRes >= runeCost
                    then (True, 0)
                    else (False, 1)
                  else (False, 0)
              attackerAfterRune =
                if paidRune
                  then attackerPlayer {resources = Resources (attackerRes - runeCost)}
                  else attackerPlayer
              combatState =
                CombatState
                  { attackingPlayer = attacker
                  , defendingPlayer = defender
                  , targetZone = zone
                  , attackers = eligible
                  , defenders = autoDefenders
                  , attackerPowerPenalty = penalty
                  , pendingAssignments = []
                  }
          modify \gx ->
            (setPlayer attacker attackerAfterRune gx)
              { combat = Just combatState
              , history = Map.insert ThisCombat mempty gx.history
              }
          recordEvent \h -> h {attackersDeclared = eligible <> h.attackersDeclared}
          logIt LogPlayerAction
            "log.combat.begins"
            [ ("attacker", playerParam attacker)
            , ("zone", zoneParam zone)
            ]
          when paidRune $
            logIt LogSystem
              "log.combat.rune_paid"
              [("amount", tshow runeCost)]
          when (penalty > 0) $
            logIt LogSystem
              "log.combat.rune_penalty"
              [("amount", tshow penalty)]
          -- Step 1 done (target + attackers committed). Open the
          -- AfterDeclareCombatTarget window on top of the current
          -- BattlefieldActionWindow; auto-pass for now since the
          -- protocol doesn't surface combat decision points yet.
          openAutoCombatWindow AfterDeclareCombatTarget
    AdvanceCombatToAttackers ->
      openAutoCombatWindow AfterDeclareAttackers
    AdvanceCombatToDefenders -> do
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just cs -> send $ DeclareDefenders cs.defenders
    DeclareDefenders defs -> do
      modify \gx -> case gx.combat of
        Just cs -> gx {combat = Just (cs {defenders = defs} :: CombatState)}
        Nothing -> gx
      -- Fire Counterstrike: each defending unit with Counterstrike N
      -- immediately deals N uncancellable damage to one attacker of
      -- the defender's choice (auto-pick the first attacker still in
      -- play). Triggers in step 3, before regular damage assigns.
      g <- get
      case g.combat of
        Just cs -> do
          let defenderUnits =
                [ u
                | k <- defs
                , Just u <- [findUnit k g]
                ]
              attackerKeys = cs.attackers
          traverse_
            ( \def ->
                let cs_total = sum [n | Counterstrike n <- def.cardDef.keywords]
                 in when (cs_total > 0) $
                      case attackerKeys of
                        (aKey : _) ->
                          send $ DealDamageToUnitUncancellable aKey cs_total
                        [] -> pure ()
            )
            defenderUnits
        Nothing -> pure ()
      openAutoCombatWindow AfterDeclareDefenders
    AdvanceCombatToAssign -> do
      -- Step 4: assignment. Compute per-defender allocations and
      -- queue DealDamageToUnit messages. The damage-cancel window
      -- (Defenders of the Faith, Master Rune of Valaya) opens
      -- afterwards.
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just cs -> assignCombatDamage g cs
      openAutoCombatWindow AfterAssignCombatDamage
    AdvanceCombatToApply -> do
      -- Step 5: commit the staged damage and open the post-apply
      -- response window. Cancellation effects had their chance to
      -- mutate the pending list during the AfterAssign window.
      commitPendingCombatDamage
      openAutoCombatWindow AfterApplyCombatDamage
    ResolveCombat -> do
      -- Legacy entry-point: the staged 5-step flow does this work
      -- via AdvanceCombatToAssign + CloseActionWindow advances.
      -- Calling ResolveCombat directly runs assign + commit + ends
      -- without opening the response windows.
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just cs -> do
          assignCombatDamage g cs
          commitPendingCombatDamage
          send EndCombat
    EndCombat -> do
      g <- get
      -- Fire post-damage "when this unit damages an enemy" effects
      -- now that every queued combat-damage message has flushed.
      case g.combat of
        Just cs -> firePerSourceCombatEffects g cs
        Nothing -> pure ()
      modify \gx -> gx {combat = Nothing}
      logIt LogSystem "log.combat.ends" []
    PutUnitIntoPlay pk cardKey zone -> do
      -- Skip cost; same wiring as 'PlayUnit' but no resource debit and
      -- no Variable-cost gate.
      player <- getPlayerS pk
      whenJust (takeUnitFromHand cardKey player) \(cardDef, playerWithoutCard) -> do
        let unit = freshUnit cardKey pk zone cardDef
        modify \gx -> (setPlayer pk playerWithoutCard gx) {units = unit : gx.units}
        logIt LogSystem "log.unit.summoned_free"
          [("player", playerParam pk), ("card", T.pack cardDef.title)]
        send $ UnitEnteredPlay pk cardKey
    PutUnitIntoPlayFromDiscard pk cardKey zone -> do
      player <- getPlayerS pk
      whenJust (takeUnitFromDiscard cardKey player) \(cardDef, playerWithoutCard) -> do
        let unit = freshUnit cardKey pk zone cardDef
            -- If a combat is in progress with this player as attacker,
            -- also add the fresh unit to its attackers list (Reckless
            -- Attack relies on this).
            joinAsAttacker cs = cs {attackers = cardKey : cs.attackers} :: CombatState
        modify \gx -> (setPlayer pk playerWithoutCard gx)
          { units = unit : gx.units
          , combat = case gx.combat of
              Just cs | cs.attackingPlayer == pk -> Just $ joinAsAttacker cs
              other -> other
          }
        g' <- get
        whenJust g'.combat \cs ->
          when (cs.attackingPlayer == pk) $
            recordEvent \h -> h {attackersDeclared = cardKey : h.attackersDeclared}
        logIt LogSystem "log.unit.summoned_from_discard"
          [("player", playerParam pk), ("card", T.pack cardDef.title)]
        send $ UnitEnteredPlay pk cardKey
    InstallModifier target modifier -> do
      modify \g ->
        g
          { modifiers =
              Map.insertWith (++) target [modifier] g.modifiers
          }
      logIt LogSystem "log.modifier.installed" []
    ClearScopedModifiers scope -> do
      modify \g ->
        g
          { modifiers =
              Map.map (filter (\m -> m.scope /= scope)) g.modifiers
          }
      logIt LogSystem "log.modifier.cleared" []
    ScheduleAttackerSacrifice -> do
      modify \g ->
        g
          { pendingEndOfPhase =
              (BattlefieldPhase, PESacrificeAttackersThisPhase)
                : g.pendingEndOfPhase
          }
      logIt LogSystem
        "log.effect.scheduled"
        [("trigger", "EndOfBattlefieldPhase"), ("what", "sacrifice attackers")]
    MoveAllDamage fromKey toKey -> do
      g <- get
      case (findUnit fromKey g, findUnit toKey g) of
        (Just src, Just dst) -> do
          let Damage srcDmg = src.damage
          when (srcDmg > 0) $ do
            let src' = (src {damage = Damage 0}) :: UnitDetails
                Damage dstDmg = dst.damage
                dst' = (dst {damage = Damage (dstDmg + srcDmg)}) :: UnitDetails
            modify \gx ->
              gx {units = replaceUnit src' (replaceUnit dst' gx.units)}
            logIt LogSystem
              "log.unit.damage_moved"
              [ ("source", T.pack src.cardDef.title)
              , ("target", T.pack dst.cardDef.title)
              , ("amount", tshow srcDmg)
              ]
            -- Destination might now exceed its HP.
            when (dstDmg + srcDmg >= dst.effectiveMaxHP) $
              send $ DestroyUnit toKey
        _ -> pure ()
    PlayLegend pk cardKey -> do
      -- One legend per player at a time. If one is already in play for
      -- this player, silently refuse.
      hasLegend <- gets (isJust . legendOf pk)
      unless hasLegend $
        withPaidPlay pk (takeLegendFromHand cardKey) (\g cd -> effectiveTotalCost g pk cd)
          \cardDef paidPlayer n -> do
            let legendDetails = LegendDetails
                  { key = cardKey
                  , controller = pk
                  , zone = BattlefieldZone
                  , cardDef
                  , damage = Damage 0
                  }
            modify \gx -> (setPlayer pk paidPlayer gx) {legends = legendDetails : gx.legends}
            logIt LogPlayerAction
              "log.legend.played"
              [ ("player", playerParam pk)
              , ("card", T.pack cardDef.title)
              , ("cost", tshow n)
              ]
            send $ LegendEnteredPlay pk cardKey
    LegendEnteredPlay pk _key ->
      logIt LogSystem "log.legend.entered_play" [("player", playerParam pk)]
    DealDamageToLegend lkey amount -> do
      g <- get
      whenJust (findLegend lkey g) \l -> do
          let inflated = max 0 amount
              Damage existing = l.damage
              newDmg = Damage (existing + inflated)
              l' = l {damage = newDmg} :: LegendDetails
          modify \gx -> gx {legends = replaceLegend l' gx.legends}
          logIt LogSystem
            "log.legend.damaged"
            [ ("card", T.pack l.cardDef.title)
            , ("amount", tshow inflated)
            ]
          let Damage total = newDmg
              hp = legendPrintedHPFromDef l.cardDef
          when (total >= hp) $
            send $ DestroyLegend lkey
    DestroyLegend lkey -> do
      mlegend <- gets (findLegend lkey)
      whenJust mlegend \l -> do
        discardToController l.controller $ mkCard l.key (LegendCardDef l.cardDef)
        modify \gx -> gx {legends = removeById lkey gx.legends}
        logIt LogSystem "log.legend.destroyed"
          [("player", playerParam l.controller), ("card", T.pack l.cardDef.title)]
        send $ LegendLeftPlay l.controller lkey l.cardDef.code
    LegendLeftPlay _pk _lkey _code -> pure ()

-- | First-turn penalty: the starting player skips Quest and Battlefield
-- on the very first turn of the game.
shouldSkipFirstTurnPhase :: Phase -> Game -> Bool
shouldSkipFirstTurnPhase phase g =
  g.turn == Turn 1
    && g.currentPlayer == g.firstPlayer
    && (phase == QuestPhase || phase == BattlefieldPhase)

-- | Wrap a list of bare card definitions into 'Card's with sequential
-- keys minted starting from the given counter. Returns the next-free
-- key plus the wrapped cards (in input order). Used at game-init to
-- stamp every starting-deck card with a stable identity that survives
-- through hand, play, and discard.
mintCards :: UnitKey -> [SomeCardDef] -> (UnitKey, [Card])
mintCards (UnitKey n0) = go n0 []
  where
    go n acc [] = (UnitKey n, reverse acc)
    go n acc (d : rest) = go (n + 1) (mkCard (UnitKey n) d : acc) rest

newPlayer :: PlayerKey -> Race -> [Card] -> Player
newPlayer k race cards =
  Player
    { key = k
    , state = IdlePlayer
    , capital = newCapital
    , resources = Resources 0
    , hand = []
    , deck = cards
    , discard = []
    , developmentCards = emptyDevelopmentCards
    , race
    }

newGame :: Deck -> Deck -> GameOptions -> Either DeckLoadError Game
newGame deck1 deck2 opts = do
  (race1, defs1) <- loadDeck deck1
  (race2, defs2) <- loadDeck deck2
  let (afterP1, cards1) = mintCards (UnitKey 0) defs1
      (nextKey, cards2) = mintCards afterP1 defs2
      player1 = newPlayer Player1 race1 cards1
      player2 = newPlayer Player2 race2 cards2
  pure $
    Game
      { player1
      , player2
      , firstPlayer = Player1
      , currentPlayer = Player1
      , turn = Turn 0
      , phase = Nothing
      , actionWindow = Nothing
      , actionWindowStack = []
      , pendingPrompt = Nothing
      , modifiers = mempty
      , lifecycle = GameSetup
      , log = []
      , units = []
      , supports = []
      , quests = []
      , legends = []
      , nextUnitKey = nextKey
      , pendingEndOfTurn = []
      , combat = Nothing
      , pendingEndOfPhase = []
      , history = emptyHistory
      , autoSkipActionWindows = opts.autoSkipActionWindows
      }

-- | Host-chosen options that shape engine behavior without altering
-- rules. Currently a single toggle; new options join this record so
-- existing callers stay source-compatible via 'defaultGameOptions'.
data GameOptions = GameOptions
  { autoSkipActionWindows :: Bool
  }

defaultGameOptions :: GameOptions
defaultGameOptions = GameOptions {autoSkipActionWindows = False}

-- | Look up a player record by key.
lookupPlayer :: PlayerKey -> Game -> Player
lookupPlayer Player1 g = g.player1
lookupPlayer Player2 g = g.player2

-- | Replace a player record by key.
setPlayer :: PlayerKey -> Player -> Game -> Game
setPlayer Player1 p g = g {player1 = p}
setPlayer Player2 p g = g {player2 = p}

-- | 'lookupPlayer' lifted into the engine's 'StateT Game' carrier.
getPlayerS :: Monad m => PlayerKey -> StateT Game m Player
getPlayerS pk = gets (lookupPlayer pk)

-- | Apply @f@ to the named player and write the result back. The new
-- player value is observed via 'lookupPlayer' on the current state, so
-- this composes correctly with other in-flight mutations to the same
-- 'Game'.
modifyPlayer :: Monad m => PlayerKey -> (Player -> Player) -> StateT Game m ()
modifyPlayer pk f = modify \g -> setPlayer pk (f (lookupPlayer pk g)) g

-- | Drop the element whose 'key' matches. The dual of 'replaceById'.
-- Works on any keyed in-play record (units, supports, quests, legends).
removeById :: HasField "key" a UnitKey => UnitKey -> [a] -> [a]
removeById k = filter ((/= k) . (.key))

-- | Construct the @InPlay Unit@ wrapper for a freshly-entering unit.
-- Used by the various play paths ('PlayUnit', 'PutUnitIntoPlay', …)
-- so they don't each have to spell out the same 11-field record.
freshUnit :: UnitKey -> PlayerKey -> ZoneKind -> CardDef Unit -> UnitDetails
freshUnit key controller zone cardDef = UnitDetails
  { key
  , controller
  , zone
  , cardDef
  , damage = Damage 0
  , corrupted = False
  , attachments = []
  , experiences = []
  , effectivePower = cardDef.power
  , effectiveMaxHP = unitPrintedHPFromDef cardDef
  , attacking = False
  , defending = False
  }

-- | Construct the @InPlay Support@ wrapper for a fresh support. Pass
-- @Just hostKey@ for an attachment, @Nothing@ for a free-standing
-- support.
freshSupport
  :: UnitKey
  -> PlayerKey
  -> ZoneKind
  -> Maybe UnitKey
  -> CardDef Support
  -> SupportDetails
freshSupport key controller zone attachedTo cardDef = SupportDetails
  { key
  , controller
  , zone
  , cardDef
  , attachedTo
  , tokens = 0
  }

-- | Push a card onto the named player's discard pile.
discardToController
  :: Monad m => PlayerKey -> Card -> StateT Game m ()
discardToController pk c = modifyPlayer pk \p -> p {discard = c : p.discard}

-- | Flip a unit's 'corrupted' flag and emit the matching log entry.
-- No-op when the unit is missing or already in the requested state.
setCorrupted :: Bool -> Text -> UnitKey -> StateT Game GameT ()
setCorrupted newVal logKey ukey = do
  munit <- gets (findUnit ukey)
  whenJust munit \u ->
    when (u.corrupted /= newVal) do
      let u' = (u {corrupted = newVal}) :: UnitDetails
      modify \gx -> gx {units = replaceUnit u' gx.units}
      logIt LogSystem logKey [("card", T.pack u.cardDef.title)]

-- | The "I can pay for it" preamble shared by the vanilla play handlers
-- (PlayUnit, PlaySupport, PlayQuest, PlayLegend, and PlayTactic / …
-- with their own pre-flight checks). Runs the @install@ body with the
-- card def, the player record after the cost has been debited, and the
-- final cost. Silently no-ops on missing card, failed 'canPlayCard',
-- 'Variable' cost, or insufficient resources.
withPaidPlay
  :: PlayerKey
  -> (Player -> Maybe (CardDef k, Player))
  -> (Game -> CardDef k -> Int)
  -> (CardDef k -> Player -> Int -> StateT Game GameT ())
  -> StateT Game GameT ()
withPaidPlay pk extract costFn install = do
  g <- get
  let player = lookupPlayer pk g
  whenJust (extract player) \(cardDef, playerWithoutCard) ->
    when (canPlayCard pk cardDef g) case cardDef.cost of
      Variable -> pure ()
      Fixed _ -> do
        let n = costFn g cardDef
        when (player.resources >= Resources n) do
          markPlayedLimited cardDef
          let paidPlayer = playerWithoutCard
                { resources = player.resources - Resources n
                }
          install cardDef paidPlayer n

-- | A 'Pile' is a getter/setter pair for one of a 'Player'\'s card
-- collections. Lets 'takeFromPile' work uniformly across hand, deck
-- and discard.
data Pile = Pile
  { read :: Player -> [Card]
  , write :: [Card] -> Player -> Player
  }

handPile, deckPile, discardPile :: Pile
handPile = Pile (.hand) \cs p -> p {hand = cs}
deckPile = Pile (.deck) \cs p -> p {deck = cs}
discardPile = Pile (.discard) \cs p -> p {discard = cs}

-- | Pull a card matching the given key from a 'Pile', if its 'def' is
-- of the requested kind. Returns the unwrapped definition and the
-- player with the card removed (pile order preserved otherwise).
takeFromPile
  :: Pile
  -> (SomeCardDef -> Maybe (CardDef k))
  -> UnitKey
  -> Player
  -> Maybe (CardDef k, Player)
takeFromPile pile asKind key p = go [] (pile.read p)
  where
    go _ [] = Nothing
    go acc (c : rest)
      | c.key == key, Just cd <- asKind c.def =
          Just (cd, pile.write (reverse acc ++ rest) p)
      | otherwise = go (c : acc) rest

takeUnitFromHand :: UnitKey -> Player -> Maybe (CardDef Unit, Player)
takeUnitFromHand = takeFromPile handPile asUnit

takeSupportFromHand :: UnitKey -> Player -> Maybe (CardDef Support, Player)
takeSupportFromHand = takeFromPile handPile asSupport

takeSupportFromDeck :: UnitKey -> Player -> Maybe (CardDef Support, Player)
takeSupportFromDeck = takeFromPile deckPile asSupport

takeQuestFromHand :: UnitKey -> Player -> Maybe (CardDef Quest, Player)
takeQuestFromHand = takeFromPile handPile asQuest

-- | Fire one scheduled effect by translating it into messages.
firePendingEffect :: PendingEffect -> StateT Game GameT ()
firePendingEffect = \case
  PEDealDamageToUnit ukey n -> send $ DealDamageToUnit ukey n
  PESacrificeAttackersThisPhase -> do
    g <- get
    traverse_ (send . DestroyUnit) (historyOfScope ThisPhase g).attackersDeclared

-- | The damage a single unit contributes in combat. Adds card-specific
-- bonuses (Lord of Khorne self-burning, Rift of Battle, …) on top of
-- the cached effective power. Per-card slices live on
-- 'UnitExtras.combatPowerBonus' (self) and 'SupportExtras.supportCombatBonus'
-- (every in-play support — free-standing or attached — gets to
-- contribute).
combatDamageOf :: Game -> PlayerKey -> UnitDetails -> Int
combatDamageOf g side u =
  max 0
    ( u.effectivePower
        + u.cardDef.extras.combatPowerBonus g u
        + sum
            [ s.cardDef.extras.supportCombatBonus g s u
            | s <- allInPlaySupports g
            ]
        - runeOfFortitudePenalty
    )
  where
    isAttacker = case g.combat of
      Just cs -> side == cs.attackingPlayer && u.key `elem` cs.attackers
      Nothing -> False

    -- Rune of Fortitude (core-013): if BeginCombat couldn't charge
    -- the per-attacker tax, every attacker eats -1 power for this
    -- combat. The penalty lives on the in-flight CombatState.
    runeOfFortitudePenalty
      | isAttacker = case g.combat of
          Just cs -> cs.attackerPowerPenalty
          Nothing -> 0
      | otherwise = 0

-- | Card-aware attacker eligibility check at 'BeginCombat'. Returns
-- 'True' if the unit can attack the named defender zone right now.
-- Driven by the per-card 'canAttackZone' slice on 'UnitExtras'
-- (Sworn of Khorne today; default lets every unit attack).
eligibleAttacker :: Game -> PlayerKey -> ZoneKind -> UnitKey -> Bool
eligibleAttacker g defender zone ukey =
  maybe False (\u -> u.cardDef.extras.canAttackZone g defender zone u) (findUnit ukey g)

-- | Split the combat damage contributed by a list of units into a
-- (cancellable, uncancellable) pair. Damage from units with the
-- 'DamageCannotBeCancelled' keyword — or whose attached supports
-- grant that property (e.g. Hammer of Sigmar) — goes into the
-- uncancellable bucket.
splitDamage :: Game -> PlayerKey -> [UnitDetails] -> (Int, Int)
splitDamage g side units =
  foldr step (0, 0) units
  where
    step u (c, n) =
      let d = combatDamageOf g side u
       in if hasUncancellableDamage u
            then (c, n + d)
            else (c + d, n)

-- | True if this unit's damage is uncancellable, accounting for both
-- its printed keywords and any attached supports that grant that
-- property (via 'SupportExtras.grantsUncancellableDamage').
hasUncancellableDamage :: UnitDetails -> Bool
hasUncancellableDamage u =
  DamageCannotBeCancelled `elem` u.cardDef.keywords
    || any (.cardDef.extras.grantsUncancellableDamage) u.attachments

-- | Apply a cancellable + uncancellable damage budget to a list of
-- units, in order. Cancellable damage is offered first so Toughness
-- absorbs as much of it as possible before any lands; uncancellable
-- damage then fills in the remaining slack.
--
-- Returns the budget left over (cancellable, uncancellable) after
-- every defender has been processed — that's what the caller sends
-- on to the zone as spillover.
applyDamageToUnitsSplit
  :: Game
  -> Int
  -- ^ cancellable budget
  -> Int
  -- ^ uncancellable budget
  -> [UnitDetails]
  -> StateT Game GameT (Int, Int)
applyDamageToUnitsSplit g = go
  where
    go cAvail uAvail [] = pure (cAvail, uAvail)
    go 0 0 _ = pure (0, 0)
    go cAvail uAvail (u : rest) = do
      let Damage existing = u.damage
          slack = max 0 (u.effectiveMaxHP - existing)
          tough = totalToughness g u
          -- Cancellable budget absorbed: enough to fill (slack +
          -- toughness) tokens of assignment, capped by what's
          -- available. The first 'tough' of that is cancelled; the
          -- rest lands.
          cancellableUsed = min cAvail (slack + tough)
          landingFromCancellable = max 0 (cancellableUsed - tough)
          slackAfterCancellable = slack - landingFromCancellable
          uncancellableUsed = min uAvail slackAfterCancellable
      when (cancellableUsed > 0) $
        send $ DealDamageToUnit u.key cancellableUsed
      when (uncancellableUsed > 0) $
        send $ DealDamageToUnitUncancellable u.key uncancellableUsed
      go (cAvail - cancellableUsed) (uAvail - uncancellableUsed) rest

-- | Read a zone from a 'Capital' by 'ZoneKind'.
getZone :: ZoneKind -> Player -> Zone
getZone kind p = case kind of
  KingdomZone -> p.capital.kingdom
  QuestZone -> p.capital.quest
  BattlefieldZone -> p.capital.battlefield

-- | Replace a zone within a player's capital, preserving the other two.
setZone :: ZoneKind -> Zone -> Player -> Player
setZone kind z p =
  let c = p.capital
      c' = case kind of
        KingdomZone -> c {kingdom = z}
        QuestZone -> c {quest = z}
        BattlefieldZone -> c {battlefield = z}
   in p {capital = c'}

-- | Wire-side enum encoding for 'ZoneKind' (mirrors 'playerParam').
zoneParam :: ZoneKind -> Text
zoneParam = tshow

-- | Apply any in-play passive damage multipliers to a raw damage
-- amount. Driven by the per-card 'damageMultiplierWhileInPlay' slice
-- on 'UnitExtras'; the strongest in-play multiplier wins (Bloodletter
-- gives 2; default is 1; duplicate copies don't stack).
applyDamageMultipliers :: Game -> Int -> Int
applyDamageMultipliers g amount =
  amount
    * maximum (1 : map (.cardDef.extras.damageMultiplierWhileInPlay) g.units)

-- | A wrapper over the four card-kinds that can host an action
-- ability. Used to dispatch a 'TriggerCardAction' message uniformly
-- regardless of the source kind.
data ActionSource
  = UnitSource UnitDetails
  | SupportSource SupportDetails
  | QuestSource QuestDetails
  | LegendSource LegendDetails

-- | Look up an in-play card by 'UnitKey' across units, supports,
-- quests and legends. First hit wins; the card kinds use disjoint
-- key spaces in practice so the order only matters for malformed input.
findActionSource :: UnitKey -> Game -> Maybe ActionSource
findActionSource k g =
      (UnitSource <$> findUnit k g)
  <|> (SupportSource <$> findSupport k g)
  <|> (QuestSource <$> findQuest k g)
  <|> (LegendSource <$> findLegend k g)

-- | Run a polymorphic action over the wrapped in-play record, no matter
-- which kind it is. Lets us write @actionAt@, @actionSourceTitle@, etc.
-- once instead of casing on every 'ActionSource' constructor.
withActionSource
  :: ActionSource
  -> ( forall a k
       . ( HasField "controller" a PlayerKey
         , HasField "cardDef" a (CardDef k)
         , InPlay k ~ a
         )
      => a -> r
     )
  -> r
withActionSource src f = case src of
  UnitSource u -> f u
  SupportSource s -> f s
  QuestSource q -> f q
  LegendSource l -> f l

-- | Metadata for the action at the given index on a source. Looks up
-- the static action list printed on the card; runtime-evaluated
-- availability ('availableInZone') is enforced by the caller before
-- firing.
actionAt :: ActionSource -> Int -> Maybe (Text, Int, TargetSchema)
actionAt src i = withActionSource src \a ->
  meta <$> safeIndex a.cardDef.actions i
  where
    meta a = (a.actionName, a.actionCost, a.actionTarget)

-- | Resolve the 'availableInZone' gate on the action at the given
-- index. Returns 'True' iff the action either has no zone gate or its
-- host card is currently in the gated zone. Non-Unit sources have no
-- zone (yet) and pass through.
actionAvailableHere :: ActionSource -> Int -> Bool
actionAvailableHere src i = withActionSource src \a ->
  case safeIndex a.cardDef.actions i of
    Nothing -> False
    Just def -> case def.availableInZone of
      Nothing -> True
      Just z -> case src of
        UnitSource u -> u.zone == z
        _ -> True

-- | Non-resource costs the action at the given index imposes. Empty
-- list when the action has no extra costs or the index is invalid.
actionExtraCostsAt :: ActionSource -> Int -> [ExtraCost]
actionExtraCostsAt src i = withActionSource src \a ->
  maybe [] (.actionExtraCosts) (safeIndex a.cardDef.actions i)

-- | Validate that every non-resource cost can be paid by 'pk' given
-- the current game state. For 'SacrificeUnit' this means the player
-- controls at least one unit.
canPayExtras :: PlayerKey -> [ExtraCost] -> Game -> Bool
canPayExtras pk extras g = all (canPayExtra pk g) extras

canPayExtra :: PlayerKey -> Game -> ExtraCost -> Bool
canPayExtra pk g SacrificeUnit =
  any (\u -> u.controller == pk) g.units

-- | Pay every non-resource cost, prompting the player for choices
-- where needed. Returns the list of 'Payment' receipts on success,
-- or 'Nothing' if any payment fails (e.g. the player declined a
-- sacrifice prompt). The caller skips firing the effect on failure.
payExtras
  :: PlayerKey -> [ExtraCost] -> StateT Game GameT (Maybe [Payment])
payExtras pk = go []
  where
    go acc [] = pure (Just (reverse acc))
    go acc (SacrificeUnit : rest) = do
      answer <- askPrompt Prompt
        { player = pk
        , kind = ChooseUnits
            { filterSpec = AnyOwnUnit
            , minPick = 1
            , maxPick = 1
            , description = "Sacrifice a unit (cost)."
            }
        , callback = CallbackInlinePrompt
        }
      case answer of
        PickUnits (chosen : _) -> do
          g <- get
          case findUnit chosen g of
            Just u | u.controller == pk -> do
              send (DestroyUnit u.key)
              go (SacrificedUnit u.key : acc) rest
            _ -> pure Nothing
        _ -> pure Nothing

-- | The card title for log lines.
actionSourceTitle :: ActionSource -> String
actionSourceTitle src = withActionSource src (.cardDef.title)

-- | Each card's actions are "controlled by" its controller — only that
-- player can trigger them.
validateActionSource :: PlayerKey -> ActionSource -> Bool
validateActionSource pk src = withActionSource src \a -> a.controller == pk

-- | Fire the chosen action's effect closure. No-op if the index is out
-- of bounds. The zone-availability check ('availableInZone') is the
-- caller's responsibility — 'TriggerCardAction' enforces it via
-- 'actionAvailableHere'.
fireAction
  :: ActionSource
  -> Int
  -> PlayerKey
  -> ActionTarget
  -> [Payment]
  -> StateT Game GameT ()
fireAction src i pk tgt payments = withActionSource src \self ->
  whenJust (safeIndex self.cardDef.actions i) \a ->
    case a.actionEffect of
      ActionEffect f ->
        lift $ f ActionUsage {user = pk, self, target = tgt, payments}

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 = Nothing
  | otherwise = case drop i xs of
      (x : _) -> Just x
      [] -> Nothing

-- | Type-indexed accessors for the per-kind 'cardDef.actions' field on
-- the metadata side. Used so the schema for tactic targets lives in
-- the same place as actions on in-play cards.
tacticTargetSchema :: CardDef Tactic -> TargetSchema
tacticTargetSchema cd = case cd.actions of
  (a : _) -> a.actionTarget
  [] -> NoTargetSchema

-- | Check that an 'ActionTarget' satisfies a 'TargetSchema' against
-- the current game state, from the perspective of the player firing.
validateTarget :: PlayerKey -> TargetSchema -> ActionTarget -> Game -> Bool
validateTarget pk schema tgt g = case (schema, tgt) of
  (NoTargetSchema, NoTarget) -> True
  (AnyUnitTargetSchema, TargetUnit k) -> isJust $ findUnit k g
  (EnemyUnitTargetSchema, TargetUnit k) -> maybe False ((/= pk) . (.controller)) $ findUnit k g
  (FriendlyUnitTargetSchema, TargetUnit k) -> maybe False ((== pk) . (.controller)) $ findUnit k g
  (AnyZoneTargetSchema, TargetZone _ _) -> True
  (EnemyZoneTargetSchema, TargetZone owner _) -> owner /= pk
  (SupportTargetSchema, TargetSupport k) -> isJust $ findSupport k g
  _ -> False

-- | Total power available to the named player in the named zone:
-- base power printed on the capital board, plus the power icons on
-- every unit/support/legend currently in the zone, plus any
-- zone-targeting aura bonuses (e.g. Lighthouse of Lothern).
zonePower :: Game -> PlayerKey -> ZoneKind -> Int
zonePower g pk zone =
  let Power base = basePower zone
      mine
        :: ( HasField "controller" a PlayerKey
           , HasField "zone" a ZoneKind
           )
        => [a] -> [a]
      mine = filter \x -> x.controller == pk && x.zone == zone
      unitPow = sum $ map (.effectivePower) $ filter (not . (.corrupted)) $ mine g.units
      supportPow = sum $ map (.cardDef.power) $ mine g.supports
      legendPow = sum $ map (.cardDef.power) $ mine g.legends
   in base + unitPow + supportPow + legendPow + zoneAuraBonus g pk zone

-- | Extra power a player's zone gets from in-play cards that grant a
-- zone-wide bonus. Driven by the per-support 'zonePowerBonus' slice
-- on 'SupportExtras' (Lighthouse of Lothern, Rift of Chaos, …); each
-- support reports its contribution for the queried (controller, zone)
-- pair.
zoneAuraBonus :: Game -> PlayerKey -> ZoneKind -> Int
zoneAuraBonus g pk zone =
  sum
    [ s.cardDef.extras.zonePowerBonus g s zone
    | s <- g.supports
    , s.controller == pk
    ]

-- | Compute and STAGE damage assignments for the in-flight combat
-- without yet committing them. The pending list lives on
-- 'CombatState.pendingAssignments'; cancellation effects (Defenders
-- of the Faith, Master Rune of Valaya) can mutate it during the
-- AfterAssignCombatDamage window. 'commitPendingCombatDamage' is the
-- counterpart that turns each entry into a real DealDamage message.
assignCombatDamage :: Game -> CombatState -> StateT Game GameT ()
assignCombatDamage g cs = do
  let attackerUnits = mapMaybe (`findUnit` g) cs.attackers
      defenderUnits = mapMaybe (`findUnit` g) cs.defenders
      (attackerCanc, attackerUncanc) =
        splitDamage g cs.attackingPlayer attackerUnits
      (defenderCanc, defenderUncanc) =
        splitDamage g cs.defendingPlayer defenderUnits
      defenderAssignments =
        allocateDamage g attackerCanc attackerUncanc defenderUnits
      attackerAssignments =
        allocateDamage g defenderCanc defenderUncanc attackerUnits
      (defenderUnitAssignments, defenderSpillover) = defenderAssignments
      (attackerUnitAssignments, _attackerSpillover) = attackerAssignments
      zoneEntry =
        let leftover = defenderSpillover
         in if leftover > 0
              then
                [ PendingDamage
                    { target = PDZone cs.defendingPlayer cs.targetZone
                    , cancellable = leftover
                    , uncancellable = 0
                    }
                ]
              else []
      pendings =
        zoneEntry
          <> map toPending defenderUnitAssignments
          <> map toPending attackerUnitAssignments
      toPending (ukey, canc, uncanc) =
        PendingDamage {target = PDUnit ukey, cancellable = canc, uncancellable = uncanc}
      cs' = (cs {pendingAssignments = pendings}) :: CombatState
  modify \gx -> gx {combat = Just cs'}
  logIt LogSystem
    "log.combat.assigned"
    [ ("attacker_damage", tshow (attackerCanc + attackerUncanc))
    , ("defender_damage", tshow (defenderCanc + defenderUncanc))
    ]

-- | Convert the in-flight 'pendingAssignments' into actual damage
-- messages. Clears the pending list. Also queues Scout post-combat
-- discards.
commitPendingCombatDamage :: StateT Game GameT ()
commitPendingCombatDamage = do
  g <- get
  case g.combat of
    Nothing -> pure ()
    Just cs -> do
      traverse_ commitOne cs.pendingAssignments
      modify \gx -> case gx.combat of
        Just c -> gx {combat = Just (c {pendingAssignments = []} :: CombatState)}
        Nothing -> gx
      -- Scout: queue post-damage discards now (they fire after the
      -- damage messages flush via the FIFO queue).
      let scoutOf u = Scout `elem` u.cardDef.keywords
          attackerScouts = filter scoutOf $ mapMaybe (`findUnit` g) cs.attackers
          defenderScouts = filter scoutOf $ mapMaybe (`findUnit` g) cs.defenders
      replicateM_ (length attackerScouts) $ send $ DiscardRandomFromHand cs.defendingPlayer
      replicateM_ (length defenderScouts) $ send $ DiscardRandomFromHand cs.attackingPlayer
  where
    commitOne pd = case pd.target of
      PDUnit k -> do
        when (pd.cancellable > 0) $ send $ DealDamageToUnit k pd.cancellable
        when (pd.uncancellable > 0) $ send $ DealDamageToUnitUncancellable k pd.uncancellable
      PDZone owner z ->
        send $ DealDamageToZone owner z (pd.cancellable + pd.uncancellable)

-- | Allocate a (cancellable, uncancellable) damage budget across a
-- list of recipients (in order), respecting Toughness on each one.
-- Returns the per-recipient (cancellable, uncancellable) tuples plus
-- the leftover cancellable+uncancellable that would have spilled
-- past the last recipient.
allocateDamage
  :: Game
  -> Int
  -- ^ cancellable budget
  -> Int
  -- ^ uncancellable budget
  -> [UnitDetails]
  -> ([(UnitKey, Int, Int)], Int)
allocateDamage g = go []
  where
    go acc 0 0 _ = (reverse acc, 0)
    go acc cAvail uAvail [] = (reverse acc, cAvail + uAvail)
    go acc cAvail uAvail (u : rest) =
      let Damage existing = u.damage
          slack = max 0 (u.effectiveMaxHP - existing)
          tough = totalToughness g u
          cancellableUsed = min cAvail (slack + tough)
          landingFromCancellable = max 0 (cancellableUsed - tough)
          slackAfterCancellable = slack - landingFromCancellable
          uncancellableUsed = min uAvail slackAfterCancellable
          entry = (u.key, cancellableUsed, uncancellableUsed)
       in go
            (entry : acc)
            (cAvail - cancellableUsed)
            (uAvail - uncancellableUsed)
            rest

-- | Open a combat sub-step window and immediately enqueue the two
-- passes that close it. Real client interaction (Defenders of the
-- Faith etc.) will eventually replace the auto-pass with a real
-- prompt window; for now we maintain rules-correct structure
-- without blocking the engine.
openAutoCombatWindow :: ActionWindowTrigger -> StateT Game GameT ()
openAutoCombatWindow trigger = do
  send $ OpenActionWindow trigger
  active <- gets (.currentPlayer)
  send $ PassPriority active
  send $ PassPriority active.next

-- | The six phase-level action windows where 'autoSkipActionWindows'
-- applies. Combat sub-step windows (After*) are always auto-passed by
-- 'openAutoCombatWindow' and don't go through this path.
isAutoSkippableTrigger :: ActionWindowTrigger -> Bool
isAutoSkippableTrigger = \case
  BeginningOfTurnActionWindow -> True
  KingdomActionWindow -> True
  QuestActionWindow -> True
  CapitalActionWindow -> True
  BattlefieldActionWindow -> True
  AfterDeclareCombatTarget -> False
  AfterDeclareAttackers -> False
  AfterDeclareDefenders -> False
  AfterAssignCombatDamage -> False
  AfterApplyCombatDamage -> False
  EndOfTurnActionWindow -> True

-- | A player has "something to do" in an action window if they hold a
-- Tactic card in hand or control an in-play card that prints any
-- action ability. Cost and loyalty are intentionally not consulted —
-- erring toward the prompt keeps the spectator-information tell weak
-- and avoids skipping past a state the player may still want to react
-- to.
playerHasActionMove :: PlayerKey -> Game -> Bool
playerHasActionMove pk g =
  let p = lookupPlayer pk g
   in any handIsTactic p.hand
        || hasAction g.units
        || hasAction g.supports
        || hasAction g.quests
        || hasAction g.legends
  where
    handIsTactic c = case c.def of
      TacticCardDef _ -> True
      _ -> False
    hasAction
      :: ( HasField "controller" a PlayerKey
         , HasField "cardDef" a (CardDef k)
         )
      => [a] -> Bool
    hasAction = any \x -> x.controller == pk && not (null x.cardDef.actions)

-- | If the host enabled 'autoSkipActionWindows', the window is one of
-- the four phase action windows, and the priority holder has no
-- playable move, enqueue a 'PassPriority' for them. Combat sub-step
-- windows skip this path so they don't double-pass on top of the
-- explicit passes 'openAutoCombatWindow' already enqueues.
maybeAutoPassPriority
  :: ActionWindowTrigger -> PlayerKey -> StateT Game GameT ()
maybeAutoPassPriority trigger pk = do
  g <- get
  when
    ( g.autoSkipActionWindows
        && isAutoSkippableTrigger trigger
        && not (playerHasActionMove pk g)
    )
    (send (PassPriority pk))

-- | Translate a resolved prompt into engine messages. Today all
-- prompts are 'CallbackInlinePrompt' — the receive body that issued
-- the prompt resumes inline via 'askPrompt' returning the answer, so
-- there's nothing to dispatch here. Kept as the seam for future
-- callback-style flows (e.g. cross-card chained prompts).
dispatchPromptCallback
  :: PromptCallback
  -> PromptResult
  -> StateT Game GameT ()
dispatchPromptCallback _cb _result = pure ()

-- | Fire post-combat "when this unit damages an enemy" effects. Read
-- 'damagedInCurrentCombat' to know which units actually took damage,
-- then iterate Plaguebearer / Beasts-of-Nurgle participants on each
-- side and corrupt damaged enemies they were dealing damage to.
firePerSourceCombatEffects :: Game -> CombatState -> StateT Game GameT ()
firePerSourceCombatEffects g cs = do
  fireFor cs.attackingPlayer cs.attackers
  fireFor cs.defendingPlayer cs.defenders
  where
    damagedEnemiesOf side =
      [ k
      | k <- (historyOfScope ThisCombat g).damagedUnits
      , Just u <- [findUnit k g]
      , u.controller /= side
      , not u.corrupted
      ]
    hasCorruptOnDamage u = u.cardDef.extras.corruptsOnCombatDamage
    fireFor side keys = do
      let sources = mapMaybe (`findUnit` g) keys
      when (any hasCorruptOnDamage sources) $
        traverse_ (send . CorruptUnit) (damagedEnemiesOf side)

-- | Per-turn damage caps that some cards impose on themselves —
-- e.g. Daemonettes of Slaanesh "cannot be assigned more than 1
-- damage per turn". Given how much damage has already landed on the
-- unit this turn and how much is incoming, returns the amount that
-- actually lands.
applyPerTurnCap :: UnitDetails -> Int -> Int -> Int
applyPerTurnCap u already incoming = case perTurnCap u of
  Nothing -> incoming
  Just cap -> max 0 (min incoming (cap - already))

-- | The per-turn damage cap for a unit, if any. Driven by the per-card
-- 'damageCap' slice on 'UnitExtras'.
perTurnCap :: UnitDetails -> Maybe Int
perTurnCap u = u.cardDef.extras.damageCap

-- | True iff the card carries the 'Limited' keyword.
isLimitedCard :: CardDef k -> Bool
isLimitedCard cd = Limited `elem` cd.keywords

-- | True iff the named player already controls a copy of this card
-- code in play (units, supports, quests, or legends). Discard / hand
-- copies do not count.
controlsCopyInPlay :: PlayerKey -> CardCode -> Game -> Bool
controlsCopyInPlay pk code g =
  hit g.units || hit g.supports || hit g.quests || hit g.legends
  where
    hit
      :: ( HasField "controller" a PlayerKey
         , HasField "cardDef" a (CardDef k)
         )
      => [a] -> Bool
    hit = any \x -> x.controller == pk && x.cardDef.code == code

-- | Refuse if a unique card already has a copy under the player's
-- control, if a Limited card has already been played this turn, or if
-- the card's per-card 'canPlay' predicate refuses (e.g. Stubborn
-- Refusal requires a damaged unit and a peer in its zone).
canPlayCard :: PlayerKey -> CardDef k -> Game -> Bool
canPlayCard pk cd g =
  (not cd.unique || not (controlsCopyInPlay pk cd.code g))
    && (not (isLimitedCard cd) || (historyOfScope ThisTurn g).limitedPlayed == 0)
    && cd.canPlay g pk

-- | Bump the Limited-played counter for the current turn if appropriate.
markPlayedLimited :: CardDef k -> StateT Game GameT ()
markPlayedLimited cd =
  when (isLimitedCard cd) $
    recordEvent \h -> h {limitedPlayed = h.limitedPlayed + 1}

-- | Sum of all Toughness contributions on a unit. Fixed values come
-- straight from the keyword; 'Toughness Variable' (Ironbreakers of
-- Ankhor) scales with the number of developments in the unit's zone.
totalToughness :: Game -> UnitDetails -> Int
totalToughness g u = sum (map asInt u.cardDef.keywords)
  where
    asInt (Toughness (Fixed n)) = n
    asInt (Toughness Variable) = devsInZone g u
    asInt _ = 0

-- 'devsInZone' moved to 'Invasion.Card' so both engine and card defs
-- can read it.

-- | Count race symbols matching 'r' that the named player controls.
-- The player's capital board contributes 1 for its faction; every
-- in-play card (unit, support, quest, legend) bearing the race adds
-- 1 more per instance.
raceSymbolCount :: Game -> PlayerKey -> Race -> Int
raceSymbolCount g pk r =
  capitalSymbol + count g.units + count g.supports + count g.quests + count g.legends
  where
    capitalSymbol = if (lookupPlayer pk g).race == r then 1 else 0
    count
      :: ( HasField "controller" a PlayerKey
         , HasField "cardDef" a (CardDef k)
         )
      => [a] -> Int
    count xs = length [x | x <- xs, x.controller == pk, r `elem` x.cardDef.races]

-- | Loyalty surcharge: each loyalty icon costs 1 resource, reduced by
-- matching race symbols you control (floor at 0). For multi-race
-- cards we take the most generous (largest) symbol count across the
-- card's races.
loyaltySurcharge :: Game -> PlayerKey -> CardDef k -> Int
loyaltySurcharge g pk cardDef =
  let perRace = map (raceSymbolCount g pk) cardDef.races
      bestMatch = if null perRace then 0 else maximum perRace
   in max 0 (cardDef.loyalty - bestMatch)

-- | Card-specific adjustments to the printed (non-loyalty) part of a
-- play cost. Pulls per-card slices: a self adjustment lives directly
-- on 'CardDef.selfCostAdjustment' (Bloodcrusher); external
-- adjustments come from in-play supports via the per-support
-- 'globalCostAdjustment' slice (Imperial Crown, Master Rune of
-- Dismay). Result may be negative — the final cost is clamped in
-- 'effectiveTotalCost'.
printedCostAdjustment :: Game -> PlayerKey -> CardDef k -> Int
printedCostAdjustment g pk cardDef =
  cardDef.selfCostAdjustment g pk + supportAdjust
  where
    filt = cardCodeFilter cardDef
    supportAdjust =
      sum
        [ s.cardDef.extras.globalCostAdjustment g s pk filt
        | s <- g.supports
        ]

-- | Additional resource cost an effect must pay to target the unit
-- referenced in the supplied 'ActionTarget'. Driven by the per-card
-- 'extraTargetTax' slice on 'UnitExtras' (King Kazador today).
extraTargetTax :: PlayerKey -> ActionTarget -> Game -> Int
extraTargetTax caster target g = case target of
  TargetUnit k -> case findUnit k g of
    Just u -> u.cardDef.extras.extraTargetTax g caster u
    Nothing -> 0
  _ -> 0

-- | Total cost to play a card: max(0, printed + adjustments) + loyalty
-- surcharge. Works uniformly for every card kind because the inputs
-- (printed cost, loyalty icons, races) all live on 'CardDef'.
effectiveTotalCost :: Game -> PlayerKey -> CardDef k -> Int
effectiveTotalCost g pk cardDef =
  let printed = case cardDef.cost of
        Fixed n -> n
        Variable -> 0
      adjustedPrinted = max 0 (printed + printedCostAdjustment g pk cardDef)
   in adjustedPrinted + loyaltySurcharge g pk cardDef

-- | Legacy entry-point preserved for callers that haven't migrated
-- yet. Returns the same answer as 'effectiveTotalCost' for units.
effectiveUnitCost :: Game -> PlayerKey -> CardDef Unit -> Int -> Int
effectiveUnitCost g pk cardDef _printed = effectiveTotalCost g pk cardDef

-- | Printed HP for a 'CardDef Unit' (no in-play context). Variable HP
-- defaults to 1; we'll grow this once X-cost units are in scope.
unitPrintedHPFromDef :: CardDef Unit -> Int
unitPrintedHPFromDef cd = case cd.hitPoints of
  Just (Fixed n) -> n
  Just Variable -> 1
  Nothing -> 1

-- | Recompute cached effective stats for every in-play unit. Called
-- after each engine step so 'effectivePower', 'effectiveMaxHP', and
-- combat-role flags ('attacking' / 'defending') always reflect
-- current attachments, experiences, scoped modifiers, zone state,
-- and the in-flight combat.
recomputeUnitStats :: Game -> Game
recomputeUnitStats g = g {units = map update g.units}
  where
    combatAttackers = maybe [] (.attackers) g.combat
    combatDefenders = maybe [] (.defenders) g.combat
    update u =
      u
        { effectivePower = computePower u
        , effectiveMaxHP = computeMaxHP u
        , attacking = u.key `elem` combatAttackers
        , defending = u.key `elem` combatDefenders
        }
        :: UnitDetails
    computePower u =
      u.cardDef.power
        + sum (map (attachmentPowerBonus u) u.attachments)
        + modifierPowerBonus u
        + auraPowerBonus g u
        + selfScalingPowerBonus g u
        + runtimeEffectsPowerBonus g u
    computeMaxHP u =
      unitPrintedHPFromDef u.cardDef
        + sum (map (attachmentHPBonus u) u.attachments)
    modifierPowerBonus u =
      let mods = fromMaybe [] (Map.lookup (UnitRef u.key) g.modifiers)
       in sum [n | Modifier (GainPower n) _ <- mods]

-- | Per-attachment power contribution. Read from the support's
-- 'attachmentPowerBonus' slice on 'SupportExtras'.
attachmentPowerBonus :: UnitDetails -> SupportDetails -> Int
attachmentPowerBonus _host s = s.cardDef.extras.attachmentPowerBonus

-- | Per-attachment HP contribution. Read from the support's
-- 'attachmentHPBonus' slice on 'SupportExtras'.
attachmentHPBonus :: UnitDetails -> SupportDetails -> Int
attachmentHPBonus _host s = s.cardDef.extras.attachmentHPBonus

-- | Continuous aura contributions to a unit's effective power, summed
-- across every in-play unit's 'unitAuraPower' slice (Karl Franz,
-- Templar of Sigmar) and every in-play support's 'supportAuraPower'
-- slice (Iron Tower, Cauldron of Blood). Read by 'recomputeUnitStats'
-- so the bonus is visible in every zone-dependent calculation
-- (resources, quest draw, combat).
auraPowerBonus :: Game -> UnitDetails -> Int
auraPowerBonus g u =
  sum [v.cardDef.extras.unitAuraPower g v u | v <- g.units]
    + sum [s.cardDef.extras.supportAuraPower g s u | s <- allInPlaySupports g]

-- | Self-scaling power based on game state. Reads the per-card
-- 'selfPowerBonus' slice on 'UnitExtras' (which subsumes the old
-- 'experiencePowerBonus' for cards that scale with experience tokens).
selfScalingPowerBonus :: Game -> UnitDetails -> Int
selfScalingPowerBonus g u = u.cardDef.extras.selfPowerBonus g u

-- | Pure read of a 'Scope's 'History' bucket. Missing buckets fall
-- back to 'mempty' so card bodies can treat all scopes uniformly.
historyOfScope :: Scope -> Game -> History
historyOfScope s g = Map.findWithDefault mempty s g.history

-- | Apply a transformation to every 'Scope's 'History' bucket. Used
-- by the engine when an event happens — every scope advances in
-- lockstep, and individual scopes are then truncated by 'BeginTurn'
-- / 'BeginPhase' / 'BeginCombat' resets.
recordEvent :: (History -> History) -> StateT Game GameT ()
recordEvent f = modify \g -> g {history = Map.map f g.history}

-- | Power bonus produced by 'runtimeEffects' — the per-tick builder
-- output authored via the 'battlefield' / 'kingdom' / 'quest' high-level
-- DSL. Folded into 'effectivePower' alongside the legacy
-- 'selfScalingPowerBonus' slot.
runtimeEffectsPowerBonus :: Game -> UnitDetails -> Int
runtimeEffectsPowerBonus g u = activeBonusPower (u.cardDef.extras.runtimeEffects g u)

takeUnitFromDiscard :: UnitKey -> Player -> Maybe (CardDef Unit, Player)
takeUnitFromDiscard = takeFromPile discardPile asUnit

takeTacticFromHand :: UnitKey -> Player -> Maybe (CardDef Tactic, Player)
takeTacticFromHand = takeFromPile handPile asTactic

takeLegendFromHand :: UnitKey -> Player -> Maybe (CardDef Legend, Player)
takeLegendFromHand = takeFromPile handPile asLegend

-- | Printed HP for a legend card definition. Mirrors
-- 'unitPrintedHPFromDef'; 'Variable' falls back to 1 for now.
legendPrintedHPFromDef :: CardDef Legend -> Int
legendPrintedHPFromDef cd = case cd.hitPoints of
  Just (Fixed n) -> n
  Just Variable -> 1
  Nothing -> 1

-- | Replace the keyed element whose 'key' matches @x@\'s 'key'. No-op
-- if no such element exists (e.g. concurrent destroy). Works for any
-- record with a @key :: UnitKey@ field — units, supports, quests,
-- legends, all use the same identity.
replaceById :: HasField "key" a UnitKey => a -> [a] -> [a]
replaceById x = map \v -> if v.key == x.key then x else v

replaceUnit :: UnitDetails -> [UnitDetails] -> [UnitDetails]
replaceUnit = replaceById

replaceSupport :: SupportDetails -> [SupportDetails] -> [SupportDetails]
replaceSupport = replaceById

replaceQuest :: QuestDetails -> [QuestDetails] -> [QuestDetails]
replaceQuest = replaceById

replaceLegend :: LegendDetails -> [LegendDetails] -> [LegendDetails]
replaceLegend = replaceById

-- | Dispatch a message to a snapshot of in-play units. The snapshot is
-- usually taken BEFORE 'Run Game.receive' runs, so a unit being
-- destroyed by this very message still sees the destruction notice and
-- can fire leave-play hooks. The current 'Player' records are sourced
-- from the post-receive 'Game' so log lines and re-reads of game state
-- see the latest mutations.
-- | Fire one in-play card's 'receive' against a message. The card's
-- own record stands in for the @InPlay k@ argument; the @owner@ player
-- is looked up from its 'controller' field. Polymorphic over kind so a
-- single helper serves units / supports / quests / legends.
fireReceive
  :: ( HasField "controller" a PlayerKey
     , HasField "cardDef" a (CardDef k)
     , InPlay k ~ a
     )
  => Game -> Message -> a -> GameT ()
fireReceive g msg self = case self.cardDef.receive of
  Receive f -> f msg (lookupPlayer self.controller g) self

dispatchToInPlayUnits :: Message -> [UnitDetails] -> Game -> GameT ()
dispatchToInPlayUnits msg snapshot g = for_ snapshot \u -> do
  fireReceive g msg u
  -- Also deliver to each attached support so attachment receives
  -- (Daemonsword, Branded by Khorne, Mark of Chaos, …) fire.
  traverse_ (fireReceive g msg) u.attachments

dispatchToInPlaySupports :: Message -> [SupportDetails] -> Game -> GameT ()
dispatchToInPlaySupports msg snapshot g = traverse_ (fireReceive g msg) snapshot

dispatchToInPlayQuests :: Message -> [QuestDetails] -> Game -> GameT ()
dispatchToInPlayQuests msg snapshot g = traverse_ (fireReceive g msg) snapshot

dispatchToInPlayLegends :: Message -> [LegendDetails] -> Game -> GameT ()
dispatchToInPlayLegends msg snapshot g = traverse_ (fireReceive g msg) snapshot

-- | Append a single transcript line to the running 'Game.log'. Each
-- entry carries an i18n key and a map of interpolation params; the
-- frontend resolves them via 'frontend/src/locales/'. Capped at the
-- most recent 'logCap' entries so a long-running game can't balloon
-- the JSON snapshot pushed to clients on every update.
logIt
  :: LogCategory
  -> Text
  -- ^ i18n key, e.g. @"log.phase.begins"@.
  -> [(Text, Text)]
  -- ^ Interpolation params. Enum-shaped values (player keys, phases,
  -- triggers, reasons) are written raw — the frontend resolves them
  -- through further i18n lookups before substitution.
  -> StateT Game GameT ()
logIt cat key params = do
  now <- liftIO getCurrentTime
  modify \g ->
    let entry = LogEntry
          { at = now
          , category = cat
          , key
          , params = Map.fromList params
          }
        appended = g.log <> [entry]
        capped =
          if length appended > logCap
            then drop (length appended - logCap) appended
            else appended
     in g {log = capped}

logCap :: Int
logCap = 500

-- | Param-value encodings. These deliberately echo the wire-side
-- constructor names ('Player1', 'KingdomPhase', etc.) so the frontend
-- can key directly into the matching i18n bundle without an extra
-- mapping layer. Each is just 'tshow' on an enum whose derived 'Show'
-- yields the constructor name; the named aliases document intent at
-- call sites.
playerParam :: PlayerKey -> Text
playerParam = tshow

phaseParam :: Phase -> Text
phaseParam = tshow

triggerParam :: ActionWindowTrigger -> Text
triggerParam = tshow

elimReasonParam :: EliminationReason -> Text
elimReasonParam = tshow

winReasonParam :: WinReason -> Text
winReasonParam = tshow

turnText :: Turn -> Text
turnText (Turn n) = tshow n

-- | Process the given messages on top of a game value, pumping the
-- queue until it drains (or the game ends). Returns the resulting
-- game.
--
-- The engine state lives in a fresh 'Env' for each call: the queue
-- starts empty and is built from the provided messages, then drained.
-- This is the shape the WebSocket runner will use to apply each
-- incoming player frame to the current game value held in its TVar.
applyMessages :: Game -> [Message] -> IO Game
applyMessages g msgs = do
  env <- newEnv g
  runGame (traverse_ send msgs >> gameMain) env

-- | Convenience: apply a single message.
applyMessage :: Game -> Message -> IO Game
applyMessage g m = applyMessages g [m]

-- | Deal hands and pick a first player. The returned game is paused in
-- 'GameSetup' — to actually begin turn 1, follow with
-- @'applyMessage' g 'BeginGame'@.
runSetup :: IO (Either DeckLoadError Game)
runSetup = case newGame dwarfStarterDeck dwarfStarterDeck defaultGameOptions of
  Left err -> pure $ Left err
  Right game -> Right <$> applyMessage game Setup
