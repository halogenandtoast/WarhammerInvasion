{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Engine (module Invasion.Engine, module Invasion.Message) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Traversable
import Invasion.Capital
import Invasion.Card
import Invasion.CardDef
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..), unitPrintedHP)
import Invasion.Game
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Data.Maybe (fromMaybe, isJust)
import Queue
import System.Random.Shuffle

data Env = Env
  { queue :: Queue Message
  , game :: Game
  }

newEnv :: Game -> IO Env
newEnv game = do
  queue <- newQueue
  pure $ Env queue game

newtype GameT a = GameT (StateT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadState Env)

instance HasQueue Message GameT where
  getQueue = gets (.queue)

instance HasGame GameT where
  getGame = gets (.game)

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
            send (Eliminate drawing.player DeckedOut)
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
      send (BeginTurn fp)
    BeginTurn k -> do
      modify \g ->
        g
          { currentPlayer = k
          , turn = g.turn + Turn 1
          , limitedPlayedThisTurn = False
          , unitsDiscardedThisTurn = 0
          , damageTakenThisTurn = mempty
          , damagedInCurrentCombat = []
          }
      t <- gets (.turn)
      logIt LogTurn
        "log.turn.begins"
        [("turn", turnText t), ("player", playerParam k)]
      send (BeginPhase KingdomPhase)
    EndTurn k -> do
      -- Fire scheduled end-of-turn effects before handing off. Each
      -- effect becomes a real message in the queue.
      pending <- gets (.pendingEndOfTurn)
      modify \g -> g {pendingEndOfTurn = []}
      traverse_ firePendingEffect pending
      -- Drop UntilEndOfTurn modifiers.
      send (ClearScopedModifiers UntilEndOfTurn)
      modify \g -> g {phase = Nothing}
      logIt LogTurn "log.turn.ends" [("player", playerParam k)]
      send (BeginTurn k.next)
    BeginPhase phase -> do
      g <- get
      modify \gx ->
        gx
          { phase = Just phase
          , attackersThisPhase =
              if phase == BattlefieldPhase
                then []
                else gx.attackersThisPhase
          }
      if shouldSkipFirstTurnPhase phase g
        then do
          logIt LogPhase "log.phase.skipped" [("phase", phaseParam phase)]
          send (EndPhase phase)
        else do
          logIt LogPhase "log.phase.begins" [("phase", phaseParam phase)]
          let active = g.currentPlayer
          case phase of
            KingdomPhase -> do
              send (ReturnResources active)
              send (RestoreOneCorruptCard active)
              send (CollectResources active)
              send (OpenActionWindow KingdomActionWindow)
            QuestPhase -> do
              send (QuestDraw active)
              send (OpenActionWindow QuestActionWindow)
            CapitalPhase ->
              send (OpenActionWindow CapitalActionWindow)
            BattlefieldPhase ->
              -- With no units yet, the active player has no attack to
              -- declare; the single window suffices. Combat will later
              -- emit the 5-step sub-sequence here.
              send (OpenActionWindow BattlefieldActionWindow)
    EndPhase phase -> do
      -- Fire any scheduled end-of-phase effects for this phase before
      -- handing off.
      pending <- gets (.pendingEndOfPhase)
      let (mine, rest) = (filter ((== phase) . fst) pending, filter ((/= phase) . fst) pending)
      modify \g -> g {pendingEndOfPhase = rest}
      traverse_ (firePendingEffect . snd) mine
      modify \g -> g {phase = Nothing}
      logIt LogPhase "log.phase.ends" [("phase", phaseParam phase)]
      case nextPhase phase of
        Just np -> send (BeginPhase np)
        Nothing -> do
          current <- gets (.currentPlayer)
          send (EndTurn current)
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
          Just p -> send (EndPhase p)
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
        [("player", playerParam k), ("count", T.pack (show n))]
    QuestDraw k -> do
      g <- get
      let n = zonePower g k QuestZone
      logIt LogSystem "log.quest.draw" [("player", playerParam k)]
      replicateM_ n (send (Draw (Drawing StandardDraw k)))
    PlayUnit pk cardKey zone -> do
      g <- get
      let player = lookupPlayer pk g
      case takeUnitFromHand cardKey player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard)
          | not (canPlayCard pk cardDef g) -> pure ()
          | otherwise -> case cardDef.cost of
              Variable -> pure ()
              Fixed printed -> do
                let n = effectiveUnitCost g pk cardDef printed
                when (player.resources >= Resources n) $ do
                  markPlayedLimited cardDef
                  -- Reuse the card's existing key as its in-play
                  -- UnitKey. This is what lets the frontend's
                  -- view-transition map a hand card to its zone
                  -- landing spot.
                  let paidPlayer =
                        playerWithoutCard
                          {resources = player.resources - Resources n}
                      unitDetails =
                        UnitDetails
                          { key = cardKey
                          , controller = pk
                          , zone
                          , cardDef
                          , damage = Damage 0
                          , corrupted = False
                          , attachments = []
                          , experiences = []
                          , effectivePower = cardDef.power
                          , effectiveMaxHP = unitPrintedHPFromDef cardDef
                          }
                  modify \gx ->
                    (setPlayer pk paidPlayer gx)
                      { units = unitDetails : gx.units
                      }
                  logIt LogPlayerAction
                    "log.unit.played"
                    [ ("player", playerParam pk)
                    , ("card", T.pack cardDef.title)
                    , ("cost", T.pack (show n))
                    ]
                  send (UnitEnteredPlay pk cardKey)
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
                      , ("cost", T.pack (show n))
                      ]
                    send (UnitEnteredPlay pk cardKey)
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
      case findUnit ukey g of
        Nothing -> pure ()
        Just u -> do
          -- Passive multiplier (Bloodletter doubles all damage) applies
          -- to the raw assignment; Toughness then cancels off the top,
          -- then per-turn caps (Daemonettes of Slaanesh) clip the
          -- remainder.
          let inflated = applyDamageMultipliers g (max 0 amount)
              toughness = totalToughness g u
              afterToughness = max 0 (inflated - toughness)
              already = Map.findWithDefault 0 ukey g.damageTakenThisTurn
              capped = applyPerTurnCap u already afterToughness
              landing = capped
              cancelled = inflated - landing
          when (cancelled > 0) $
            logIt LogSystem
              "log.damage.cancelled"
              [ ("card", T.pack u.cardDef.title)
              , ("amount", T.pack (show cancelled))
              ]
          when (landing > 0) $ do
            let Damage existing = u.damage
                newDmg = Damage (existing + landing)
                u' = u {damage = newDmg} :: UnitDetails
            modify \gx ->
              gx
                { units = replaceUnit u' gx.units
                , damageTakenThisTurn =
                    Map.insertWith (+) ukey landing gx.damageTakenThisTurn
                , damagedInCurrentCombat =
                    if isJust gx.combat && ukey `notElem` gx.damagedInCurrentCombat
                      then ukey : gx.damagedInCurrentCombat
                      else gx.damagedInCurrentCombat
                }
            logIt LogSystem
              "log.unit.damaged"
              [ ("card", T.pack u.cardDef.title)
              , ("amount", T.pack (show landing))
              ]
            let Damage total = newDmg
            when (total >= u.effectiveMaxHP) $
              send (DestroyUnit ukey)
    DealDamageToUnitUncancellable ukey amount -> do
      g <- get
      case findUnit ukey g of
        Nothing -> pure ()
        Just u -> do
          -- Uncancellable damage still respects per-turn caps
          -- (Daemonettes) — the cap is independent of cancellation.
          let inflated = applyDamageMultipliers g (max 0 amount)
              already = Map.findWithDefault 0 ukey g.damageTakenThisTurn
              landing = applyPerTurnCap u already inflated
          when (landing > 0) $ do
            let Damage existing = u.damage
                newDmg = Damage (existing + landing)
                u' = u {damage = newDmg} :: UnitDetails
            modify \gx ->
              gx
                { units = replaceUnit u' gx.units
                , damageTakenThisTurn =
                    Map.insertWith (+) ukey landing gx.damageTakenThisTurn
                , damagedInCurrentCombat =
                    if isJust gx.combat && ukey `notElem` gx.damagedInCurrentCombat
                      then ukey : gx.damagedInCurrentCombat
                      else gx.damagedInCurrentCombat
                }
            logIt LogSystem
              "log.unit.damaged"
              [ ("card", T.pack u.cardDef.title)
              , ("amount", T.pack (show landing))
              ]
            let Damage total = newDmg
            when (total >= u.effectiveMaxHP) $
              send (DestroyUnit ukey)
    HealUnit ukey amount -> do
      g <- get
      case findUnit ukey g of
        Nothing -> pure ()
        Just u -> do
          let Damage existing = u.damage
              healed = max 0 (existing - max 0 amount)
              u' = (u {damage = Damage healed}) :: UnitDetails
          modify \gx -> gx {units = replaceUnit u' gx.units}
          logIt LogSystem
            "log.unit.healed"
            [ ("card", T.pack u.cardDef.title)
            , ("amount", T.pack (show amount))
            ]
    DestroyUnit ukey -> do
      g <- get
      case findUnit ukey g of
        Nothing -> pure ()
        Just u -> do
          -- Remove the unit and all its attachments. Each card lands in
          -- its OWN controller's discard pile carrying the same key it
          -- had in play, so the frontend's view-transition continues to
          -- track the same card visually from board to pile.
          --
          -- Attachments may be controlled by either side — Branded by
          -- Khorne is the canonical hostile attachment.
          let dropUnitCard pl =
                pl {discard = mkCard u.key (UnitCardDef u.cardDef) : pl.discard}
              dropSupport gx attach =
                let owner = lookupPlayer attach.controller gx
                    owner' =
                      owner
                        { discard =
                            mkCard attach.key (SupportCardDef attach.cardDef)
                              : owner.discard
                        }
                 in setPlayer attach.controller owner' gx
              gAfterUnit =
                let owner = lookupPlayer u.controller g
                    owner' = dropUnitCard owner
                 in (setPlayer u.controller owner' g)
                      {units = filter (\v -> v.key /= ukey) g.units}
              gFinal = foldl dropSupport gAfterUnit u.attachments
          put gFinal
          modify \gx ->
            gx {unitsDiscardedThisTurn = gx.unitsDiscardedThisTurn + 1}
          logIt LogSystem
            "log.unit.destroyed"
            [ ("player", playerParam u.controller)
            , ("card", T.pack u.cardDef.title)
            ]
          send (UnitLeftPlay u.controller ukey u.zone u.cardDef.code)
    UnitLeftPlay _pk ukey _zone _code -> do
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
    CorruptUnit ukey -> do
      g <- get
      case findUnit ukey g of
        Just u | not u.corrupted -> do
          let u' = (u {corrupted = True}) :: UnitDetails
          modify \gx -> gx {units = replaceUnit u' gx.units}
          logIt LogSystem
            "log.unit.corrupted"
            [("card", T.pack u.cardDef.title)]
        _ -> pure ()
    CleanseUnit ukey -> do
      g <- get
      case findUnit ukey g of
        Just u | u.corrupted -> do
          let u' = (u {corrupted = False}) :: UnitDetails
          modify \gx -> gx {units = replaceUnit u' gx.units}
          logIt LogSystem
            "log.unit.cleansed"
            [("card", T.pack u.cardDef.title)]
        _ -> pure ()
    RestoreOneCorruptCard pk -> do
      g <- get
      case
        [ u
        | u <- g.units
        , u.controller == pk
        , u.corrupted
        ]
        of
          (u : _) -> send (CleanseUnit u.key)
          [] -> pure ()
    PlayAttachment pk cardKey targetKey -> do
      g <- get
      let player = lookupPlayer pk g
      case (takeSupportFromHand cardKey player, findUnit targetKey g) of
        (Just (cardDef, playerWithoutCard), Just host)
          | not (canPlayCard pk cardDef g) -> pure ()
          | otherwise -> case cardDef.cost of
              Variable -> pure ()
              Fixed _ -> do
                let baseCost = effectiveTotalCost g pk cardDef
                    targetTax = extraTargetTax pk (TargetUnit targetKey) g
                    n = baseCost + targetTax
                when (player.resources >= Resources n) $ do
                  markPlayedLimited cardDef
                  let paidPlayer =
                        playerWithoutCard
                          {resources = player.resources - Resources n}
                      attachment =
                        SupportDetails
                          { key = cardKey
                          , controller = pk
                          , zone = host.zone
                          , cardDef
                          , attachedTo = Just targetKey
                          , tokens = 0
                          }
                      host' =
                        (host {attachments = attachment : host.attachments})
                          :: UnitDetails
                  modify \gx ->
                    (setPlayer pk paidPlayer gx)
                      { units = replaceUnit host' gx.units
                      }
                  logIt LogPlayerAction
                    "log.attachment.played"
                    [ ("player", playerParam pk)
                    , ("card", T.pack cardDef.title)
                    , ("target", T.pack host.cardDef.title)
                    , ("cost", T.pack (show n))
                    ]
                  send (SupportEnteredPlay pk cardKey)
        _ -> pure ()
    SupportEnteredPlay _pk _key ->
      -- 'dispatchToInPlayUnits' walks attachments via their host; any
      -- bespoke reaction lives in the support card's 'receive'.
      pure ()
    PlaySupport pk cardKey zone -> do
      g <- get
      let player = lookupPlayer pk g
      case takeSupportFromHand cardKey player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard)
          | not (canPlayCard pk cardDef g) -> pure ()
          | otherwise -> case cardDef.cost of
              Variable -> pure ()
              Fixed _ -> do
                let n = effectiveTotalCost g pk cardDef
                when (player.resources >= Resources n) $ do
                  markPlayedLimited cardDef
                  let paidPlayer =
                        playerWithoutCard
                          {resources = player.resources - Resources n}
                      support =
                        SupportDetails
                          { key = cardKey
                          , controller = pk
                          , zone
                          , cardDef
                          , attachedTo = Nothing
                          , tokens = 0
                          }
                  modify \gx ->
                    (setPlayer pk paidPlayer gx)
                      { supports = support : gx.supports
                      }
                  logIt LogPlayerAction
                    "log.support.played"
                    [ ("player", playerParam pk)
                    , ("card", T.pack cardDef.title)
                    , ("cost", T.pack (show n))
                    ]
                  send (SupportEnteredPlay pk cardKey)
    PlaySupportFromDeck pk cardKey zone -> do
      g <- get
      let player = lookupPlayer pk g
      case takeSupportFromDeck cardKey player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard) -> do
          let support =
                SupportDetails
                  { key = cardKey
                  , controller = pk
                  , zone
                  , cardDef
                  , attachedTo = Nothing
                  , tokens = 0
                  }
          modify \gx ->
            (setPlayer pk playerWithoutCard gx)
              { supports = support : gx.supports
              }
          logIt LogSystem
            "log.support.played_from_deck"
            [ ("player", playerParam pk)
            , ("card", T.pack cardDef.title)
            ]
          send (SupportEnteredPlay pk cardKey)
    PlayQuest pk cardKey -> do
      g <- get
      let player = lookupPlayer pk g
      case takeQuestFromHand cardKey player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard)
          | not (canPlayCard pk cardDef g) -> pure ()
          | otherwise -> case cardDef.cost of
              Variable -> pure ()
              Fixed _ -> do
                let n = effectiveTotalCost g pk cardDef
                when (player.resources >= Resources n) $ do
                  markPlayedLimited cardDef
                  let paidPlayer =
                        playerWithoutCard
                          {resources = player.resources - Resources n}
                      quest =
                        QuestDetails
                          { key = cardKey
                          , controller = pk
                          , cardDef
                          , tokens = 0
                          , questingUnit = Nothing
                          }
                  modify \gx ->
                    (setPlayer pk paidPlayer gx)
                      { quests = quest : gx.quests
                      }
                  logIt LogPlayerAction
                    "log.quest.played"
                    [ ("player", playerParam pk)
                    , ("card", T.pack cardDef.title)
                    , ("cost", T.pack (show n))
                    ]
                  send (QuestEnteredPlay pk cardKey)
    QuestEnteredPlay _pk _key ->
      -- Per-card reactions fire via dispatch (see
      -- 'dispatchToInPlayUnits' which now also walks 'Game.supports'
      -- and 'Game.quests').
      pure ()
    AdjustSupportTokens skey delta -> do
      g <- get
      case findSupport skey g of
        Nothing -> pure ()
        Just s -> do
          let n = max 0 (s.tokens + delta)
              s' = (s {tokens = n}) :: SupportDetails
          modify \gx -> gx {supports = replaceSupport s' gx.supports}
          logIt LogSystem
            "log.support.tokens"
            [ ("card", T.pack s.cardDef.title)
            , ("count", T.pack (show n))
            ]
    AdjustQuestTokens qkey delta -> do
      g <- get
      case findQuest qkey g of
        Nothing -> pure ()
        Just q -> do
          let n = max 0 (q.tokens + delta)
              q' = (q {tokens = n}) :: QuestDetails
          modify \gx -> gx {quests = replaceQuest q' gx.quests}
          logIt LogSystem
            "log.quest.tokens"
            [ ("card", T.pack q.cardDef.title)
            , ("count", T.pack (show n))
            ]
    DestroySupport skey -> do
      g <- get
      case findSupport skey g of
        Nothing -> pure ()
        Just s -> do
          let owner = lookupPlayer s.controller g
              owner' =
                owner
                  { discard =
                      mkCard s.key (SupportCardDef s.cardDef) : owner.discard
                  }
          modify \gx ->
            (setPlayer s.controller owner' gx)
              {supports = filter (\v -> v.key /= skey) gx.supports}
          logIt LogSystem
            "log.support.destroyed"
            [ ("player", playerParam s.controller)
            , ("card", T.pack s.cardDef.title)
            ]
          send (SupportLeftPlay s.controller skey s.cardDef.code)
    SupportLeftPlay _pk _skey _code -> pure ()
    DestroyQuest qkey -> do
      g <- get
      case findQuest qkey g of
        Nothing -> pure ()
        Just q -> do
          let owner = lookupPlayer q.controller g
              owner' =
                owner
                  { discard =
                      mkCard q.key (QuestCardDef q.cardDef) : owner.discard
                  }
          modify \gx ->
            (setPlayer q.controller owner' gx)
              {quests = filter (\v -> v.key /= qkey) gx.quests}
          logIt LogSystem
            "log.quest.destroyed"
            [ ("player", playerParam q.controller)
            , ("card", T.pack q.cardDef.title)
            ]
          send (QuestLeftPlay q.controller qkey q.cardDef.code)
    QuestLeftPlay _pk _qkey _code -> pure ()
    AttachExperience hostKey expCode -> do
      g <- get
      case findUnit hostKey g of
        Nothing -> pure ()
        Just u -> do
          let u' = (u {experiences = expCode : u.experiences}) :: UnitDetails
          modify \gx -> gx {units = replaceUnit u' gx.units}
          logIt LogSystem
            "log.unit.experience_attached"
            [ ("card", T.pack u.cardDef.title)
            , ("count", T.pack (show (length u'.experiences)))
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
                    , ("cost", T.pack (show n))
                    ]
                  send (TacticResolved pk cardDef.code target)
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
            when (validateActionSource pk src) $
              when (player.resources >= Resources totalCost) $
                when (validateTarget pk schema target g) $ do
                  let paid = player {resources = player.resources - Resources totalCost}
                  modify (setPlayer pk paid)
                  logIt LogPlayerAction
                    "log.action.triggered"
                    [ ("player", playerParam pk)
                    , ("card", T.pack (actionSourceTitle src))
                    , ("action", name)
                    , ("cost", T.pack (show totalCost))
                    ]
                  fireAction src idx pk target
    DeferDamageToUnitUntilEoT ukey n -> do
      modify \g ->
        g {pendingEndOfTurn = PEDealDamageToUnit ukey n : g.pendingEndOfTurn}
      logIt LogSystem
        "log.effect.deferred_damage"
        [ ("amount", T.pack (show n))
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
          , ("amount", T.pack (show amount))
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
            send (Eliminate targetPlayer CapitalBurned)
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
          [("player", playerParam pk), ("amount", T.pack (show budget))]
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
            , ("amount", T.pack (show taken))
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
            send (Eliminate pk DeckedOut)
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
          [("player", playerParam pk), ("amount", T.pack (show amount))]
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
              -- Rune of Fortitude (core-013): each unit attacking
              -- this zone loses 1 power unless its controller pays 1
              -- per attacker. All-or-nothing approximation here: if
              -- the attacker can afford the full tax, pay it and the
              -- penalty stays at 0; otherwise leave the resources
              -- intact and impose -1 per attacker for this combat.
              runeHere =
                any
                  ( \s ->
                      s.controller == defender
                        && s.zone == zone
                        && s.cardDef.code == CardCode "core-013"
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
                  }
          modify \gx ->
            (setPlayer attacker attackerAfterRune gx)
              { combat = Just combatState
              , attackersThisPhase = eligible <> gx.attackersThisPhase
              , damagedInCurrentCombat = []
              }
          logIt LogPlayerAction
            "log.combat.begins"
            [ ("attacker", playerParam attacker)
            , ("zone", zoneParam zone)
            ]
          when paidRune $
            logIt LogSystem
              "log.combat.rune_paid"
              [("amount", T.pack (show runeCost))]
          when (penalty > 0) $
            logIt LogSystem
              "log.combat.rune_penalty"
              [("amount", T.pack (show penalty))]
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
        Just cs -> send (DeclareDefenders cs.defenders)
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
                          send (DealDamageToUnitUncancellable aKey cs_total)
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
    AdvanceCombatToApply ->
      -- Step 5: damage actually lands. With the current pipeline
      -- DealDamageToUnit is the apply, and we queued it in step 4,
      -- so this window is just the response window. EndCombat
      -- triggers from CloseActionWindow(AfterApplyCombatDamage).
      openAutoCombatWindow AfterApplyCombatDamage
    ResolveCombat -> do
      -- Legacy entry-point: the staged 5-step flow does this work
      -- via AdvanceCombatToAssign (which queues damage) + window
      -- closes (which advance to apply + end). Calling ResolveCombat
      -- directly still works — it just runs assign + skips straight
      -- to EndCombat without opening the response window.
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just cs -> do
          assignCombatDamage g cs
          send EndCombat
    EndCombat -> do
      g <- get
      -- Fire post-damage "when this unit damages an enemy" effects
      -- now that every queued combat-damage message has flushed.
      case g.combat of
        Just cs -> firePerSourceCombatEffects g cs
        Nothing -> pure ()
      modify \gx ->
        gx
          { combat = Nothing
          , damagedInCurrentCombat = []
          }
      logIt LogSystem "log.combat.ends" []
    PutUnitIntoPlay pk cardKey zone -> do
      g <- get
      let player = lookupPlayer pk g
      case takeUnitFromHand cardKey player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard) -> do
          -- Skip cost; same wiring as 'PlayUnit' but no resource debit
          -- and no Variable-cost gate.
          let unitDetails =
                UnitDetails
                  { key = cardKey
                  , controller = pk
                  , zone
                  , cardDef
                  , damage = Damage 0
                  , corrupted = False
                  , attachments = []
                  , experiences = []
                  , effectivePower = cardDef.power
                  , effectiveMaxHP = unitPrintedHPFromDef cardDef
                  }
          modify \gx ->
            (setPlayer pk playerWithoutCard gx)
              { units = unitDetails : gx.units
              }
          logIt LogSystem
            "log.unit.summoned_free"
            [ ("player", playerParam pk)
            , ("card", T.pack cardDef.title)
            ]
          send (UnitEnteredPlay pk cardKey)
    PutUnitIntoPlayFromDiscard pk cardKey zone -> do
      g <- get
      let player = lookupPlayer pk g
      case takeUnitFromDiscard cardKey player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard) -> do
          let unitDetails =
                UnitDetails
                  { key = cardKey
                  , controller = pk
                  , zone
                  , cardDef
                  , damage = Damage 0
                  , corrupted = False
                  , attachments = []
                  , experiences = []
                  , effectivePower = cardDef.power
                  , effectiveMaxHP = unitPrintedHPFromDef cardDef
                  }
          modify \gx ->
            (setPlayer pk playerWithoutCard gx)
              { units = unitDetails : gx.units
              -- If a combat is in progress with this player as
              -- attacker, also add the fresh unit to its attackers
              -- list (Reckless Attack relies on this).
              , combat = case gx.combat of
                  Just cs
                    | cs.attackingPlayer == pk ->
                        Just
                          ( cs {attackers = cardKey : cs.attackers}
                            :: CombatState
                          )
                  other -> other
              , attackersThisPhase =
                  case gx.combat of
                    Just cs
                      | cs.attackingPlayer == pk -> cardKey : gx.attackersThisPhase
                    _ -> gx.attackersThisPhase
              }
          logIt LogSystem
            "log.unit.summoned_from_discard"
            [ ("player", playerParam pk)
            , ("card", T.pack cardDef.title)
            ]
          send (UnitEnteredPlay pk cardKey)
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
              , ("amount", T.pack (show srcDmg))
              ]
            -- Destination might now exceed its HP.
            when (dstDmg + srcDmg >= dst.effectiveMaxHP) $
              send (DestroyUnit toKey)
        _ -> pure ()
    PlayLegend pk cardKey -> do
      g <- get
      -- One legend per player at a time. If one is already in play for
      -- this player, silently refuse.
      case legendOf pk g of
        Just _ -> pure ()
        Nothing -> do
          let player = lookupPlayer pk g
          case takeLegendFromHand cardKey player of
            Nothing -> pure ()
            Just (cardDef, playerWithoutCard)
              | not (canPlayCard pk cardDef g) -> pure ()
              | otherwise -> case cardDef.cost of
                  Variable -> pure ()
                  Fixed _ -> do
                    let n = effectiveTotalCost g pk cardDef
                    when (player.resources >= Resources n) $ do
                      markPlayedLimited cardDef
                      let paidPlayer =
                            playerWithoutCard
                              {resources = player.resources - Resources n}
                          legendDetails =
                            LegendDetails
                              { key = cardKey
                              , controller = pk
                              , zone = BattlefieldZone
                              , cardDef
                              , damage = Damage 0
                              }
                      modify \gx ->
                        (setPlayer pk paidPlayer gx)
                          { legends = legendDetails : gx.legends
                          }
                      logIt LogPlayerAction
                        "log.legend.played"
                        [ ("player", playerParam pk)
                        , ("card", T.pack cardDef.title)
                        , ("cost", T.pack (show n))
                        ]
                      send (LegendEnteredPlay pk cardKey)
    LegendEnteredPlay pk _key ->
      logIt LogSystem "log.legend.entered_play" [("player", playerParam pk)]
    DealDamageToLegend lkey amount -> do
      g <- get
      case findLegend lkey g of
        Nothing -> pure ()
        Just l -> do
          let inflated = max 0 amount
              Damage existing = l.damage
              newDmg = Damage (existing + inflated)
              l' = l {damage = newDmg} :: LegendDetails
          modify \gx -> gx {legends = replaceLegend l' gx.legends}
          logIt LogSystem
            "log.legend.damaged"
            [ ("card", T.pack l.cardDef.title)
            , ("amount", T.pack (show inflated))
            ]
          let Damage total = newDmg
              hp = legendPrintedHPFromDef l.cardDef
          when (total >= hp) $
            send (DestroyLegend lkey)
    DestroyLegend lkey -> do
      g <- get
      case findLegend lkey g of
        Nothing -> pure ()
        Just l -> do
          let owner = lookupPlayer l.controller g
              owner' =
                owner
                  { discard =
                      mkCard l.key (LegendCardDef l.cardDef) : owner.discard
                  }
          modify \gx ->
            (setPlayer l.controller owner' gx)
              {legends = filter (\v -> v.key /= lkey) gx.legends}
          logIt LogSystem
            "log.legend.destroyed"
            [ ("player", playerParam l.controller)
            , ("card", T.pack l.cardDef.title)
            ]
          send (LegendLeftPlay l.controller lkey l.cardDef.code)
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

newGame :: Deck -> Deck -> Either DeckLoadError Game
newGame deck1 deck2 = do
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
      , attackersThisPhase = []
      , pendingEndOfPhase = []
      , limitedPlayedThisTurn = False
      , unitsDiscardedThisTurn = 0
      , damageTakenThisTurn = mempty
      , damagedInCurrentCombat = []
      }

-- | Look up a player record by key.
lookupPlayer :: PlayerKey -> Game -> Player
lookupPlayer Player1 g = g.player1
lookupPlayer Player2 g = g.player2

-- | Replace a player record by key.
setPlayer :: PlayerKey -> Player -> Game -> Game
setPlayer Player1 p g = g {player1 = p}
setPlayer Player2 p g = g {player2 = p}

-- | Pull a unit card matching the given 'CardCode' out of a player's
-- hand. Returns 'Nothing' if no such card exists in hand (or if the
-- matching card isn't a unit). Preserves hand order for the remaining
-- cards.
takeUnitFromHand :: UnitKey -> Player -> Maybe (CardDef Unit, Player)
takeUnitFromHand k p = go p.hand []
  where
    go :: [Card] -> [Card] -> Maybe (CardDef Unit, Player)
    go [] _ = Nothing
    go (c : rest) acc = case (c.key == k, c.def) of
      (True, UnitCardDef cd) ->
        Just (cd, p {hand = reverse acc ++ rest})
      _ -> go rest (c : acc)

-- 'findUnit' is exported by 'Invasion.Card' for use in card receive
-- bodies; we reuse it here.

-- | Pull a Support card matching the given 'UnitKey' out of a player's
-- hand. Mirror of 'takeUnitFromHand'.
-- | Pull a Support card matching the given 'UnitKey' out of a
-- player's deck (used by deck-search effects such as Dwarf Cannon
-- Crew).
takeSupportFromDeck :: UnitKey -> Player -> Maybe (CardDef Support, Player)
takeSupportFromDeck k p = go p.deck []
  where
    go :: [Card] -> [Card] -> Maybe (CardDef Support, Player)
    go [] _ = Nothing
    go (c : rest) acc = case (c.key == k, c.def) of
      (True, SupportCardDef cd) ->
        Just (cd, p {deck = reverse acc ++ rest})
      _ -> go rest (c : acc)

takeSupportFromHand :: UnitKey -> Player -> Maybe (CardDef Support, Player)
takeSupportFromHand k p = go p.hand []
  where
    go :: [Card] -> [Card] -> Maybe (CardDef Support, Player)
    go [] _ = Nothing
    go (c : rest) acc = case (c.key == k, c.def) of
      (True, SupportCardDef cd) ->
        Just (cd, p {hand = reverse acc ++ rest})
      _ -> go rest (c : acc)

-- | Pull a Quest card matching the given 'UnitKey' out of a player's
-- hand.
takeQuestFromHand :: UnitKey -> Player -> Maybe (CardDef Quest, Player)
takeQuestFromHand k p = go p.hand []
  where
    go :: [Card] -> [Card] -> Maybe (CardDef Quest, Player)
    go [] _ = Nothing
    go (c : rest) acc = case (c.key == k, c.def) of
      (True, QuestCardDef cd) ->
        Just (cd, p {hand = reverse acc ++ rest})
      _ -> go rest (c : acc)

-- | Fire one scheduled effect by translating it into messages.
firePendingEffect :: PendingEffect -> StateT Game GameT ()
firePendingEffect = \case
  PEDealDamageToUnit ukey n -> send (DealDamageToUnit ukey n)
  PESacrificeAttackersThisPhase -> do
    g <- get
    traverse_ (send . DestroyUnit) g.attackersThisPhase

-- | The damage a single unit contributes in combat. Adds card-specific
-- bonuses (Lord of Khorne, Rift of Battle, …) on top of the cached
-- effective power. The 'PlayerKey' is the unit's side, which lets
-- some bonuses scope to attackers vs defenders if they grow such a
-- distinction later.
combatDamageOf :: Game -> PlayerKey -> UnitDetails -> Int
combatDamageOf g side u =
  max 0
    ( u.effectivePower
        + lordOfKhorne
        + riftOfBattle
        + organGunDefendingBonus
        + daBadMoonAttackingAura
        + bigBossesBannerAura
        + gorbadIronclawSelf
        - runeOfFortitudePenalty
    )
  where
    isAttacker = case g.combat of
      Just cs -> side == cs.attackingPlayer && u.key `elem` cs.attackers
      Nothing -> False
    isDefender = case g.combat of
      Just cs -> side == cs.defendingPlayer && u.key `elem` cs.defenders
      Nothing -> False

    -- Rune of Fortitude (core-013): if BeginCombat couldn't charge
    -- the per-attacker tax, every attacker eats -1 power for this
    -- combat. The penalty lives on the in-flight CombatState.
    runeOfFortitudePenalty
      | isAttacker = case g.combat of
          Just cs -> cs.attackerPowerPenalty
          Nothing -> 0
      | otherwise = 0

    -- Lord of Khorne (cataclysm-033) deals +1 damage in combat per
    -- burning zone (across both capitals).
    lordOfKhorne
      | u.cardDef.code == CardCode "cataclysm-033" =
          burningZoneCount g
      | otherwise = 0

    -- Rift of Battle (the-accursed-dead-052) buffs every unit's
    -- combat damage by +1 while it's in play.
    riftOfBattle
      | any (\s -> s.cardDef.code == CardCode "the-accursed-dead-052") g.supports =
          1
      | otherwise = 0

    -- Organ Gun (core-015): attached unit gains +2 power while
    -- defending. Implemented per-unit because the bonus is conditional.
    organGunDefendingBonus
      | isDefender
      , any (\a -> a.cardDef.code == CardCode "core-015") u.attachments = 2
      | otherwise = 0

    -- Da Bad Moon (core-121): "Battlefield." prefix — Bad Moon must
    -- be in your battlefield. Your other Orc units gain {power} while
    -- attacking.
    daBadMoonAttackingAura
      | isAttacker
      , Orc `elem` u.cardDef.races
      , any
          ( \s ->
              s.controller == u.controller
                && s.cardDef.code == CardCode "core-121"
                && s.zone == BattlefieldZone
          )
          g.supports = 1
      | otherwise = 0

    -- Big Boss's Banner (core-123): the attached unit's other Orc
    -- attackers gain {power}. Granted to U if there's an Orc attacker
    -- carrying the banner on this side that isn't U itself.
    bigBossesBannerAura
      | isAttacker
      , Orc `elem` u.cardDef.races
      , let bannerHosts =
              [ v
              | v <- g.units
              , v.controller == u.controller
              , v.key /= u.key
              , any (\a -> a.cardDef.code == CardCode "core-123") v.attachments
              ]
      , not (null bannerHosts)
      , let isHostAttacker v = case g.combat of
              Just cs -> v.key `elem` cs.attackers
              Nothing -> False
      , any isHostAttacker bannerHosts = 1
      | otherwise = 0

    -- Gorbad Ironclaw (core-108): +1 damage in combat when this unit
    -- damages a zone — approximated as a flat +1 to its own combat
    -- power (spillover to the zone scales naturally with that).
    gorbadIronclawSelf
      | u.cardDef.code == CardCode "core-108"
      , isAttacker = 1
      | otherwise = 0

-- | Card-aware attacker eligibility check at 'BeginCombat'. Returns
-- 'True' if the unit can attack the named defender zone right now.
-- Sworn of Khorne is the current special case; others should plug in
-- here as they land.
eligibleAttacker :: Game -> PlayerKey -> ZoneKind -> UnitKey -> Bool
eligibleAttacker g defender zone ukey = case findUnit ukey g of
  Nothing -> False
  Just u
    -- Sworn of Khorne: only if the defender zone contains a corrupted
    -- defending unit.
    | u.cardDef.code == CardCode "fragments-of-power-031" ->
        any
          ( \v ->
              v.controller == defender
                && v.zone == zone
                && v.corrupted
          )
          g.units
    | otherwise -> True

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
-- property.
hasUncancellableDamage :: UnitDetails -> Bool
hasUncancellableDamage u =
  DamageCannotBeCancelled `elem` u.cardDef.keywords
    || any
      ( \a ->
          a.cardDef.code == CardCode "core-042" -- Hammer of Sigmar
      )
      u.attachments

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
        send (DealDamageToUnit u.key cancellableUsed)
      when (uncancellableUsed > 0) $
        send (DealDamageToUnitUncancellable u.key uncancellableUsed)
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
zoneParam = \case
  KingdomZone -> "KingdomZone"
  QuestZone -> "QuestZone"
  BattlefieldZone -> "BattlefieldZone"

-- | Apply any in-play passive damage multipliers to a raw damage
-- amount. Currently only Bloodletter (legends-031) doubles, but
-- additional multipliers (Slaaneshi mirror effects, etc.) plug in here.
applyDamageMultipliers :: Game -> Int -> Int
applyDamageMultipliers g amount =
  let hasBloodletter =
        any (\u -> u.cardDef.code == CardCode "legends-031") g.units
   in if hasBloodletter then amount * 2 else amount

-- | A wrapper over the four card-kinds that can host an action
-- ability. Used to dispatch a 'TriggerCardAction' message uniformly
-- regardless of the source kind.
data ActionSource
  = UnitSource UnitDetails
  | SupportSource SupportDetails
  | QuestSource QuestDetails
  | LegendSource LegendDetails

-- | Look up an in-play card by 'UnitKey' across units, supports,
-- quests and legends.
findActionSource :: UnitKey -> Game -> Maybe ActionSource
findActionSource k g =
  case findUnit k g of
    Just u -> Just (UnitSource u)
    Nothing -> case findSupport k g of
      Just s -> Just (SupportSource s)
      Nothing -> case findQuest k g of
        Just q -> Just (QuestSource q)
        Nothing -> case findLegend k g of
          Just l -> Just (LegendSource l)
          Nothing -> Nothing

-- | Static metadata for the action at the given index of a source's
-- declared 'actions' list.
actionAt :: ActionSource -> Int -> Maybe (Text, Int, TargetSchema)
actionAt src i = case src of
  UnitSource u -> meta <$> safeIndex u.cardDef.actions i
  SupportSource s -> meta <$> safeIndex s.cardDef.actions i
  QuestSource q -> meta <$> safeIndex q.cardDef.actions i
  LegendSource l -> meta <$> safeIndex l.cardDef.actions i
  where
    meta a = (a.actionName, a.actionCost, a.actionTarget)

-- | The card title for log lines.
actionSourceTitle :: ActionSource -> String
actionSourceTitle = \case
  UnitSource u -> u.cardDef.title
  SupportSource s -> s.cardDef.title
  QuestSource q -> q.cardDef.title
  LegendSource l -> l.cardDef.title

-- | Each card's actions are "controlled by" its controller — only that
-- player can trigger them.
validateActionSource :: PlayerKey -> ActionSource -> Bool
validateActionSource pk = \case
  UnitSource u -> u.controller == pk
  SupportSource s -> s.controller == pk
  QuestSource q -> q.controller == pk
  LegendSource l -> l.controller == pk

-- | Fire the chosen action's effect closure. No-op if the index is out
-- of bounds (should never happen since 'actionAt' already validated).
fireAction
  :: ActionSource
  -> Int
  -> PlayerKey
  -> ActionTarget
  -> StateT Game GameT ()
fireAction src i pk tgt = case src of
  UnitSource u -> case safeIndex u.cardDef.actions i of
    Just a -> case a.actionEffect of
      ActionEffect f -> lift (f pk u tgt)
    Nothing -> pure ()
  SupportSource s -> case safeIndex s.cardDef.actions i of
    Just a -> case a.actionEffect of
      ActionEffect f -> lift (f pk s tgt)
    Nothing -> pure ()
  QuestSource q -> case safeIndex q.cardDef.actions i of
    Just a -> case a.actionEffect of
      ActionEffect f -> lift (f pk q tgt)
    Nothing -> pure ()
  LegendSource l -> case safeIndex l.cardDef.actions i of
    Just a -> case a.actionEffect of
      ActionEffect f -> lift (f pk l tgt)
    Nothing -> pure ()

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
  (AnyUnitTargetSchema, TargetUnit k) -> case findUnit k g of
    Just _ -> True
    Nothing -> False
  (EnemyUnitTargetSchema, TargetUnit k) -> case findUnit k g of
    Just u -> u.controller /= pk
    Nothing -> False
  (FriendlyUnitTargetSchema, TargetUnit k) -> case findUnit k g of
    Just u -> u.controller == pk
    Nothing -> False
  (AnyZoneTargetSchema, TargetZone _ _) -> True
  (EnemyZoneTargetSchema, TargetZone owner _) -> owner /= pk
  (SupportTargetSchema, TargetSupport k) -> case findSupport k g of
    Just _ -> True
    Nothing -> False
  _ -> False

-- | Total power available to the named player in the named zone:
-- base power printed on the capital board, plus the power icons on
-- every unit/support/legend currently in the zone, plus any
-- zone-targeting aura bonuses (e.g. Lighthouse of Lothern).
zonePower :: Game -> PlayerKey -> ZoneKind -> Int
zonePower g pk zone =
  let Power base = basePower zone
      unitsHere =
        [ u
        | u <- g.units
        , u.controller == pk
        , u.zone == zone
        , not u.corrupted
        ]
      supportsHere =
        [ s
        | s <- g.supports
        , s.controller == pk
        , s.zone == zone
        ]
      legendsHere =
        [ l
        | l <- g.legends
        , l.controller == pk
        , l.zone == zone
        ]
      unitPow = sum (map (.effectivePower) unitsHere)
      supportPow = sum (map (.cardDef.power) supportsHere)
      legendPow = sum (map (.cardDef.power) legendsHere)
      auraBonus = zoneAuraBonus g pk zone
   in base + unitPow + supportPow + legendPow + auraBonus

-- | Extra power a player's zone gets from in-play cards that grant a
-- zone-wide bonus (Lighthouse of Lothern, Rift of Chaos, …).
zoneAuraBonus :: Game -> PlayerKey -> ZoneKind -> Int
zoneAuraBonus g pk zone =
  lighthouseOfLothernBonus + riftOfChaosBonus
  where
    -- Lighthouse of Lothern (core-066): your quest zone gains +1
    -- power while the Lighthouse itself is in the quest zone (the
    -- "Quest." prefix scopes the effect to that zone).
    lighthouseOfLothernBonus
      | zone == QuestZone
      , any
          ( \s ->
              s.controller == pk
                && s.cardDef.code == CardCode "core-066"
                && s.zone == QuestZone
          )
          g.supports = 1
      | otherwise = 0
    -- Rift of Chaos (cataclysm-037): +1 to its own zone per burning
    -- zone on either capital. Applies wherever the rift sits.
    riftOfChaosBonus =
      sum
        [ burningZoneCount g
        | s <- g.supports
        , s.controller == pk
        , s.cardDef.code == CardCode "cataclysm-037"
        , s.zone == zone
        ]

-- | Compute and queue per-defender / per-attacker damage assignments
-- for the in-flight combat. Spillover to the targeted zone goes out
-- via 'DealDamageToZone'. Scout post-combat discards queue here too,
-- but they read damage state AFTER the queued damage applies (the
-- queue is FIFO).
assignCombatDamage :: Game -> CombatState -> StateT Game GameT ()
assignCombatDamage g cs = do
  let attackerUnits = [u | k <- cs.attackers, Just u <- [findUnit k g]]
      defenderUnits = [u | k <- cs.defenders, Just u <- [findUnit k g]]
      (attackerCanc, attackerUncanc) =
        splitDamage g cs.attackingPlayer attackerUnits
      (defenderCanc, defenderUncanc) =
        splitDamage g cs.defendingPlayer defenderUnits
  (cRem, uRem) <-
    applyDamageToUnitsSplit g attackerCanc attackerUncanc defenderUnits
  let toZone = cRem + uRem
  when (toZone > 0) $
    send (DealDamageToZone cs.defendingPlayer cs.targetZone toZone)
  _ <- applyDamageToUnitsSplit g defenderCanc defenderUncanc attackerUnits
  logIt LogSystem
    "log.combat.resolved"
    [ ("attacker_damage", T.pack (show (attackerCanc + attackerUncanc)))
    , ("defender_damage", T.pack (show (defenderCanc + defenderUncanc)))
    ]
  -- Scout: every surviving Scout participant on each side forces the
  -- opposite controller to discard one card at random. We queue
  -- DiscardRandomFromHand for each scout; the discards process AFTER
  -- the queued damage messages, so they see the post-damage state.
  let scoutOf u = Scout `elem` u.cardDef.keywords
      attackerScouts = filter scoutOf attackerUnits
      defenderScouts = filter scoutOf defenderUnits
  traverse_
    (\_ -> send (DiscardRandomFromHand cs.defendingPlayer))
    attackerScouts
  traverse_
    (\_ -> send (DiscardRandomFromHand cs.attackingPlayer))
    defenderScouts

-- | Open a combat sub-step window and immediately enqueue the two
-- passes that close it. Real client interaction (Defenders of the
-- Faith etc.) will eventually replace the auto-pass with a real
-- prompt window; for now we maintain rules-correct structure
-- without blocking the engine.
openAutoCombatWindow :: ActionWindowTrigger -> StateT Game GameT ()
openAutoCombatWindow trigger = do
  send (OpenActionWindow trigger)
  active <- gets (.currentPlayer)
  send (PassPriority active)
  send (PassPriority active.next)

-- | Translate a resolved prompt into engine messages. Each callback
-- knows the structure of its expected result (units to summon, unit
-- to sacrifice, etc.) and is responsible for validating that the
-- chosen targets pass the prompt's filter / count gates before
-- firing the follow-up effect.
dispatchPromptCallback
  :: PromptCallback
  -> PromptResult
  -> StateT Game GameT ()
dispatchPromptCallback cb result = case (cb, result) of
  (CallbackIronThroneroomPayoff srcKey, PickUnits chosen) -> do
    g <- get
    case findSupport srcKey g of
      Nothing -> pure ()
      Just self -> do
        let owner = lookupPlayer self.controller g
            handKeySet = [c.key | c <- owner.hand]
            discardKeySet = [c.key | c <- owner.discard]
            picks = take 3 chosen
            chaosUnitInList list k =
              case [c | c <- list, c.key == k] of
                (c : _) -> case c.def of
                  UnitCardDef cd -> Chaos `elem` cd.races
                  _ -> False
                [] -> False
            fromHand =
              [ k
              | k <- picks
              , k `elem` handKeySet
              , chaosUnitInList owner.hand k
              ]
            fromDiscard =
              [ k
              | k <- picks
              , k `elem` discardKeySet
              , chaosUnitInList owner.discard k
              ]
        traverse_
          (\k -> send (PutUnitIntoPlay self.controller k KingdomZone))
          fromHand
        traverse_
          (\k -> send (PutUnitIntoPlayFromDiscard self.controller k KingdomZone))
          fromDiscard
  (CallbackBloodthirsterSacrifice pk _, PickUnits (chosen : _)) -> do
    g <- get
    case findUnit chosen g of
      Just u | u.controller == pk ->
        send (DestroyUnit u.key)
      _ -> pure ()
  (CallbackBloodthirsterSacrifice _ _, PickNone) -> pure ()
  _ -> pure ()

-- | Fire post-combat "when this unit damages an enemy" effects. Read
-- 'damagedInCurrentCombat' to know which units actually took damage,
-- then iterate Plaguebearer / Beasts-of-Nurgle participants on each
-- side and corrupt damaged enemies they were dealing damage to.
firePerSourceCombatEffects :: Game -> CombatState -> StateT Game GameT ()
firePerSourceCombatEffects g cs = do
  let damaged = g.damagedInCurrentCombat
      damagedEnemiesOf side =
        [ k
        | k <- damaged
        , Just u <- [findUnit k g]
        , u.controller /= side
        , not u.corrupted
        ]
      hasCorruptOnDamage u =
        u.cardDef.code == CardCode "core-085" -- Plaguebearers of Nurgle
          || u.cardDef.code == CardCode "core-096" -- Beasts of Nurgle
      attackerSources =
        [ u
        | k <- cs.attackers
        , Just u <- [findUnit k g]
        , hasCorruptOnDamage u
        ]
      defenderSources =
        [ u
        | k <- cs.defenders
        , Just u <- [findUnit k g]
        , hasCorruptOnDamage u
        ]
  when (not (null attackerSources)) $
    traverse_ (send . CorruptUnit) (damagedEnemiesOf cs.attackingPlayer)
  when (not (null defenderSources)) $
    traverse_ (send . CorruptUnit) (damagedEnemiesOf cs.defendingPlayer)

-- | Per-turn damage caps that some cards impose on themselves —
-- e.g. Daemonettes of Slaanesh "cannot be assigned more than 1
-- damage per turn". Given how much damage has already landed on the
-- unit this turn and how much is incoming, returns the amount that
-- actually lands.
applyPerTurnCap :: UnitDetails -> Int -> Int -> Int
applyPerTurnCap u already incoming = case perTurnCap u of
  Nothing -> incoming
  Just cap -> max 0 (min incoming (cap - already))

-- | The per-turn damage cap for a unit, if any.
perTurnCap :: UnitDetails -> Maybe Int
perTurnCap u = case u.cardDef.code of
  CardCode "core-095" -> Just 1 -- Daemonettes of Slaanesh
  _ -> Nothing

-- | True iff the card carries the 'Limited' keyword.
isLimitedCard :: CardDef k -> Bool
isLimitedCard cd = Limited `elem` cd.keywords

-- | True iff the named player already controls a copy of this card
-- code in play (units, supports, quests, or legends). Discard / hand
-- copies do not count.
controlsCopyInPlay :: PlayerKey -> CardCode -> Game -> Bool
controlsCopyInPlay pk code g =
  any (\u -> u.controller == pk && u.cardDef.code == code) g.units
    || any (\s -> s.controller == pk && s.cardDef.code == code) g.supports
    || any (\q -> q.controller == pk && q.cardDef.code == code) g.quests
    || any (\l -> l.controller == pk && l.cardDef.code == code) g.legends

-- | Refuse if a unique card already has a copy under the player's
-- control, or if a Limited card has already been played this turn.
canPlayCard :: PlayerKey -> CardDef k -> Game -> Bool
canPlayCard pk cd g =
  (not cd.unique || not (controlsCopyInPlay pk cd.code g))
    && (not (isLimitedCard cd) || not g.limitedPlayedThisTurn)

-- | Set the limited-played-this-turn flag if appropriate.
markPlayedLimited :: CardDef k -> StateT Game GameT ()
markPlayedLimited cd =
  when (isLimitedCard cd) $
    modify \gx -> gx {limitedPlayedThisTurn = True}

-- | Sum of all Toughness contributions on a unit. Fixed values come
-- straight from the keyword; 'Toughness Variable' (Ironbreakers of
-- Ankhor) scales with the number of developments in the unit's zone.
totalToughness :: Game -> UnitDetails -> Int
totalToughness g u = sum (map asInt u.cardDef.keywords)
  where
    asInt (Toughness (Fixed n)) = n
    asInt (Toughness Variable) = devsInZone g u
    asInt _ = 0

-- | Number of facedown developments currently sitting in the named
-- unit's zone. Read for Toughness X and a couple of dev-scaling
-- cards (Troll Slayers, Ironbreakers of Ankhor).
devsInZone :: Game -> UnitDetails -> Int
devsInZone g u =
  let player = case u.controller of
        Player1 -> g.player1
        Player2 -> g.player2
      Developments d = case u.zone of
        KingdomZone -> player.capital.kingdom.developments
        QuestZone -> player.capital.quest.developments
        BattlefieldZone -> player.capital.battlefield.developments
   in d

-- | Count race symbols matching 'r' that the named player controls.
-- The player's capital board contributes 1 for its faction; every
-- in-play card (unit, support, quest, legend) bearing the race adds
-- 1 more per instance.
raceSymbolCount :: Game -> PlayerKey -> Race -> Int
raceSymbolCount g pk r =
  capitalSymbol + boardSymbols
  where
    player = lookupPlayer pk g
    capitalSymbol = if player.race == r then 1 else 0
    boardSymbols =
      length [u | u <- g.units, u.controller == pk, r `elem` u.cardDef.races]
        + length [s | s <- g.supports, s.controller == pk, r `elem` s.cardDef.races]
        + length [q | q <- g.quests, q.controller == pk, r `elem` q.cardDef.races]
        + length [l | l <- g.legends, l.controller == pk, r `elem` l.cardDef.races]

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
-- play cost. Card-by-card switches live here; everything else returns
-- 0. Result may be negative — the final cost is clamped in
-- 'effectiveTotalCost'.
printedCostAdjustment :: Game -> PlayerKey -> CardDef k -> Int
printedCostAdjustment g pk cardDef =
  selfAdjust + globalAdjust + opponentAdjust
  where
    selfAdjust = case cardDef.code of
      -- Bloodcrusher: -1 per burning zone (across both capitals).
      CardCode "cataclysm-034" -> negate (burningZoneCount g)
      _ -> 0
    -- Imperial Crown (core-041): "Kingdom." prefix — active while
    -- the crown sits in your kingdom. Your Empire heroes cost 1 less.
    globalAdjust
      | Empire `elem` cardDef.races
      , Hero `elem` cardDef.traits
      , any
          ( \s ->
              s.controller == pk
                && s.cardDef.code == CardCode "core-041"
                && s.zone == KingdomZone
          )
          g.supports = -1
      | otherwise = 0
    -- Master Rune of Dismay (core-016): "Kingdom." prefix — must sit
    -- in the opponent's kingdom for the +1 cost-to-play tax to apply
    -- to your units.
    opponentAdjust
      | any
          ( \s ->
              s.controller == pk.next
                && s.cardDef.code == CardCode "core-016"
                && s.zone == KingdomZone
          )
          g.supports = 1
      | otherwise = 0

-- | Additional resource cost an effect must pay to target the unit
-- referenced in the supplied 'ActionTarget'. Currently only King
-- Kazador (core-007): opponents pay +3 resources per effect that
-- names him. Returns 0 for any non-targeted effect, or when the
-- caster is the unit's controller.
extraTargetTax :: PlayerKey -> ActionTarget -> Game -> Int
extraTargetTax caster target g = case target of
  TargetUnit k -> case findUnit k g of
    Just u
      | u.controller /= caster
      , u.cardDef.code == CardCode "core-007" -> 3
    _ -> 0
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
      loyalty = loyaltySurcharge g pk cardDef
   in adjustedPrinted + loyalty

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
-- after each engine step so 'effectivePower' and 'effectiveMaxHP'
-- always reflect current attachments, experiences, scoped modifiers,
-- and zone state.
recomputeUnitStats :: Game -> Game
recomputeUnitStats g = g {units = map update g.units}
  where
    update u =
      u {effectivePower = computePower u, effectiveMaxHP = computeMaxHP u}
        :: UnitDetails
    computePower u =
      u.cardDef.power
        + sum (map (attachmentPowerBonus u) u.attachments)
        + experiencePowerBonus u
        + modifierPowerBonus u
        + auraPowerBonus g u
        + selfScalingPowerBonus g u
    computeMaxHP u =
      unitPrintedHPFromDef u.cardDef
        + sum (map (attachmentHPBonus u) u.attachments)
    modifierPowerBonus u =
      let mods = fromMaybe [] (Map.lookup (UnitRef u.key) g.modifiers)
       in sum [n | Modifier (GainPower n) _ <- mods]

-- | Per-attachment power contribution. Hard-coded by 'CardCode'.
attachmentPowerBonus :: UnitDetails -> SupportDetails -> Int
attachmentPowerBonus _host s = case s.cardDef.code of
  CardCode "the-warpstone-chronicles-095" -> 3 -- Daemonsword
  CardCode "omens-of-ruin-013" -> 2 -- Mark of Chaos
  CardCode "core-042" -> 2 -- Hammer of Sigmar
  CardCode "core-043" -> 1 -- Banner of Sigmar
  CardCode "core-067" -> 2 -- Banner of Avelorn
  CardCode "core-098" -> 1 -- Eye of Tzeentch
  CardCode "core-122" -> 2 -- Choppa
  CardCode "core-149" -> 2 -- Whip of Agony
  CardCode "core-150" -> 1 -- Druchii Banner
  _ -> 0

-- | Per-attachment HP contribution.
attachmentHPBonus :: UnitDetails -> SupportDetails -> Int
attachmentHPBonus _host s = case s.cardDef.code of
  CardCode "the-warpstone-chronicles-095" -> 2 -- Daemonsword
  _ -> 0

-- | Continuous aura contributions to a unit's effective power, from
-- other in-play cards (units, supports). Read by 'recomputeUnitStats'
-- so the bonus shows up in every zone-dependent calculation
-- (resources, quest draw, combat).
auraPowerBonus :: Game -> UnitDetails -> Int
auraPowerBonus g u = sum
  [ karlFranzAura
  , templarOfSigmarAura
  , ironTowerAura
  , cauldronOfBloodAura
  , daBadMoonStaticAura
  ]
  where
    isFriendly v = v.controller == u.controller
    differentUnit v = v.key /= u.key
    hasCode code v = v.cardDef.code == CardCode code
    hasSupport code = any (\s -> isFriendlySupport s && hasCodeS s code) g.supports
    isFriendlySupport s = s.controller == u.controller
    hasCodeS s code = s.cardDef.code == CardCode code

    -- Karl Franz: your other Empire units gain {power}.
    karlFranzAura
      | Empire `elem` u.cardDef.races
      , any (\v -> isFriendly v && differentUnit v && hasCode "core-035" v) g.units = 1
      | otherwise = 0

    -- Templar of Sigmar: "Battlefield." prefix scopes the aura to
    -- the battlefield. Your other Warrior units in the battlefield
    -- gain {power} while the Templar is there.
    templarOfSigmarAura
      | Warrior `elem` u.cardDef.traits
      , u.zone == BattlefieldZone
      , any
          ( \v ->
              isFriendly v
                && differentUnit v
                && hasCode "core-028" v
                && v.zone == BattlefieldZone
          )
          g.units = 1
      | otherwise = 0

    -- Iron Tower: "Battlefield." prefix — only active while the
    -- Iron Tower itself sits in your battlefield. Your Chaos units
    -- in the battlefield then gain {power}.
    ironTowerAura
      | Chaos `elem` u.cardDef.races
      , u.zone == BattlefieldZone
      , hasSupportInZone "core-099" BattlefieldZone = 1
      | otherwise = 0

    -- Cauldron of Blood: "Battlefield." prefix — only active while
    -- the Cauldron sits in your battlefield.
    cauldronOfBloodAura
      | u.cardDef.code == CardCode "core-135"
      , hasSupportInZone "core-147" BattlefieldZone = 1
      | otherwise = 0

    hasSupportInZone code z =
      any
        ( \s ->
            isFriendlySupport s
              && hasCodeS s code
              && s.zone == z
        )
        g.supports

    -- Da Bad Moon: your other Orc units gain {power} while attacking.
    -- The "while attacking" part is handled in 'combatDamageOf'; here
    -- we only credit the always-on slice when the moon's a unit on the
    -- field (it's a support, so this is currently dead code — kept to
    -- mirror the symmetry with Iron Tower).
    daBadMoonStaticAura = 0

-- | Self-scaling power based on what else is in play. Read by
-- 'recomputeUnitStats' alongside 'auraPowerBonus' and
-- 'experiencePowerBonus'.
selfScalingPowerBonus :: Game -> UnitDetails -> Int
selfScalingPowerBonus g u = case u.cardDef.code of
  -- Troll Slayers (core-004): +2 power while you have at least 2
  -- developments in this zone.
  CardCode "core-004" | devsInZone g u >= 2 -> 2
  -- Durgnar the Bold (core-006): +2 power while one section of your
  -- capital is burning.
  CardCode "core-006" | controllerBurning -> 2
    where
      controllerBurning =
        let p = case u.controller of
              Player1 -> g.player1
              Player2 -> g.player2
         in any (.burning) p.capital.zones
  -- Korhil: gains {power} per other White Lion unit you control.
  CardCode "core-061" ->
    length
      [ ()
      | v <- g.units
      , v.controller == u.controller
      , v.key /= u.key
      , v.cardDef.code == CardCode "core-052"
      ]
  -- Crone Hellebron: gains {power} per Witch Elf unit you control.
  CardCode "core-133" ->
    length
      [ ()
      | v <- g.units
      , v.controller == u.controller
      , v.cardDef.code == CardCode "core-135"
      ]
  _ -> 0

-- | Power gained from experience markers. Only Skulltaker reads them
-- today.
experiencePowerBonus :: UnitDetails -> Int
experiencePowerBonus u = case u.cardDef.code of
  CardCode "faith-and-steel-113" -> length u.experiences
  _ -> 0

-- | Pull a Unit card matching the given 'UnitKey' out of a player's
-- discard pile. Used by Reckless Attack-style resurrection effects.
takeUnitFromDiscard :: UnitKey -> Player -> Maybe (CardDef Unit, Player)
takeUnitFromDiscard k p = go p.discard []
  where
    go :: [Card] -> [Card] -> Maybe (CardDef Unit, Player)
    go [] _ = Nothing
    go (c : rest) acc = case (c.key == k, c.def) of
      (True, UnitCardDef cd) ->
        Just (cd, p {discard = reverse acc ++ rest})
      _ -> go rest (c : acc)

-- | Pull a Tactic card matching the given 'UnitKey' out of a player's
-- hand.
takeTacticFromHand :: UnitKey -> Player -> Maybe (CardDef Tactic, Player)
takeTacticFromHand k p = go p.hand []
  where
    go :: [Card] -> [Card] -> Maybe (CardDef Tactic, Player)
    go [] _ = Nothing
    go (c : rest) acc = case (c.key == k, c.def) of
      (True, TacticCardDef cd) ->
        Just (cd, p {hand = reverse acc ++ rest})
      _ -> go rest (c : acc)

-- 'findSupport' / 'findQuest' / 'findLegend' are exported from
-- 'Invasion.Card'.

-- | Pull a Legend card matching the given 'CardCode' out of a player's
-- hand.
takeLegendFromHand :: UnitKey -> Player -> Maybe (CardDef Legend, Player)
takeLegendFromHand k p = go p.hand []
  where
    go :: [Card] -> [Card] -> Maybe (CardDef Legend, Player)
    go [] _ = Nothing
    go (c : rest) acc = case (c.key == k, c.def) of
      (True, LegendCardDef cd) ->
        Just (cd, p {hand = reverse acc ++ rest})
      _ -> go rest (c : acc)

-- | Printed HP for a legend card definition. Mirrors
-- 'unitPrintedHPFromDef'; 'Variable' falls back to 1 for now.
legendPrintedHPFromDef :: CardDef Legend -> Int
legendPrintedHPFromDef cd = case cd.hitPoints of
  Just (Fixed n) -> n
  Just Variable -> 1
  Nothing -> 1

replaceSupport :: SupportDetails -> [SupportDetails] -> [SupportDetails]
replaceSupport s = map \v -> if v.key == s.key then s else v

replaceQuest :: QuestDetails -> [QuestDetails] -> [QuestDetails]
replaceQuest q = map \v -> if v.key == q.key then q else v

replaceLegend :: LegendDetails -> [LegendDetails] -> [LegendDetails]
replaceLegend l = map \v -> if v.key == l.key then l else v

-- | Replace the unit whose key matches the supplied unit's key. No-op if
-- the unit is no longer present (e.g. concurrent destroy).
replaceUnit :: UnitDetails -> [UnitDetails] -> [UnitDetails]
replaceUnit u = map \v -> if v.key == u.key then u else v

-- | Dispatch a message to a snapshot of in-play units. The snapshot is
-- usually taken BEFORE 'Run Game.receive' runs, so a unit being
-- destroyed by this very message still sees the destruction notice and
-- can fire leave-play hooks. The current 'Player' records are sourced
-- from the post-receive 'Game' so log lines and re-reads of game state
-- see the latest mutations.
dispatchToInPlayUnits :: Message -> [UnitDetails] -> Game -> GameT ()
dispatchToInPlayUnits msg snapshot g = traverse_ deliver snapshot
  where
    deliver u = do
      let owner = lookupPlayer u.controller g
      case u.cardDef.receive of
        Receive f -> f msg owner u
      -- Also deliver to each attached support so attachment receives
      -- (Daemonsword, Branded by Khorne, Mark of Chaos, …) fire.
      traverse_ (deliverAttachment) u.attachments
    deliverAttachment s =
      let owner = lookupPlayer s.controller g
       in case s.cardDef.receive of
            Receive f -> f msg owner s

dispatchToInPlaySupports :: Message -> [SupportDetails] -> Game -> GameT ()
dispatchToInPlaySupports msg snapshot g = traverse_ deliver snapshot
  where
    deliver s =
      let owner = lookupPlayer s.controller g
       in case s.cardDef.receive of
            Receive f -> f msg owner s

dispatchToInPlayQuests :: Message -> [QuestDetails] -> Game -> GameT ()
dispatchToInPlayQuests msg snapshot g = traverse_ deliver snapshot
  where
    deliver q =
      let owner = lookupPlayer q.controller g
       in case q.cardDef.receive of
            Receive f -> f msg owner q

dispatchToInPlayLegends :: Message -> [LegendDetails] -> Game -> GameT ()
dispatchToInPlayLegends msg snapshot g = traverse_ deliver snapshot
  where
    deliver l =
      let owner = lookupPlayer l.controller g
       in case l.cardDef.receive of
            Receive f -> f msg owner l

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
-- mapping layer.
playerParam :: PlayerKey -> Text
playerParam = \case
  Player1 -> "Player1"
  Player2 -> "Player2"

phaseParam :: Phase -> Text
phaseParam = \case
  KingdomPhase -> "KingdomPhase"
  QuestPhase -> "QuestPhase"
  CapitalPhase -> "CapitalPhase"
  BattlefieldPhase -> "BattlefieldPhase"

triggerParam :: ActionWindowTrigger -> Text
triggerParam = \case
  KingdomActionWindow -> "KingdomActionWindow"
  QuestActionWindow -> "QuestActionWindow"
  CapitalActionWindow -> "CapitalActionWindow"
  BattlefieldActionWindow -> "BattlefieldActionWindow"
  AfterDeclareCombatTarget -> "AfterDeclareCombatTarget"
  AfterDeclareAttackers -> "AfterDeclareAttackers"
  AfterDeclareDefenders -> "AfterDeclareDefenders"
  AfterAssignCombatDamage -> "AfterAssignCombatDamage"
  AfterApplyCombatDamage -> "AfterApplyCombatDamage"

elimReasonParam :: EliminationReason -> Text
elimReasonParam = \case
  DeckedOut -> "DeckedOut"
  CapitalBurned -> "CapitalBurned"

winReasonParam :: WinReason -> Text
winReasonParam = \case
  OpponentDeckedOut -> "OpponentDeckedOut"
  OpponentCapitalBurned -> "OpponentCapitalBurned"

turnText :: Turn -> Text
turnText (Turn n) = T.pack (show n)

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
runSetup = case newGame dwarfStarterDeck dwarfStarterDeck of
  Left err -> pure $ Left err
  Right game -> Right <$> applyMessage game Setup
