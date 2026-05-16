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
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..), unitPrintedHP)
import Invasion.Game
import Invasion.Message
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
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
-- — an open action window stops emitting messages and the queue drains.
gameMain :: GameT Game
gameMain = do
  game <- getGame
  case game.lifecycle of
    GameFinished _ -> pure game
    _ -> do
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
    CollectResources k -> onKey k do
      -- Base power of the kingdom zone, plus power icons on cards
      -- placed there. No cards are implemented yet, so power icons
      -- contribute nothing.
      let Power n = basePower KingdomZone
      modify \p -> p {resources = Resources n}
    QuestDraw k -> onKey k do
      let Power n = basePower QuestZone
      replicateM_ n (send $ Draw $ Drawing StandardDraw k)
    Eliminate k reason -> onKey k $
      modify \p -> p {state = Eliminated reason}
    _ -> pure ()

instance Run Game where
  distribute msg g = do
    player1 <- distribute msg g.player1
    player2 <- distribute msg g.player2
    -- Snapshot in-play state BEFORE Game's receive runs so leaving
    -- cards still see the message that removed them. New cards entering
    -- during this message see the next one (typically the
    -- '*EnteredPlay' notice the handler emits).
    let preUnits = g.units
        preSupports = g.supports
        preQuests = g.quests
    g' <- execStateT (receive msg) (g {player1, player2})
    dispatchToInPlayUnits msg preUnits g'
    dispatchToInPlaySupports msg preSupports g'
    dispatchToInPlayQuests msg preQuests g'
    pure g'
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
      modify \g -> g {currentPlayer = k, turn = g.turn + Turn 1}
      t <- gets (.turn)
      logIt LogTurn
        "log.turn.begins"
        [("turn", turnText t), ("player", playerParam k)]
      send (BeginPhase KingdomPhase)
    EndTurn k -> do
      -- TODO once we have effects: clear UntilEndOfTurn modifiers,
      -- fire "after your turn ends" forced effects.
      modify \g -> g {phase = Nothing}
      logIt LogTurn "log.turn.ends" [("player", playerParam k)]
      send (BeginTurn k.next)
    BeginPhase phase -> do
      g <- get
      modify \gx -> gx {phase = Just phase}
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
              -- TODO restore one corrupt card the active player controls
              -- (no corruption tokens yet).
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
      modify \g -> g {actionWindow = Just aw}
      logIt LogSystem
        "log.window.open"
        [("trigger", triggerParam trigger), ("player", playerParam current)]
    PassPriority k -> do
      g <- get
      case g.actionWindow of
        Just aw | priorityHolder aw.awaiting == k -> do
          logIt LogPlayerAction "log.priority.pass" [("player", playerParam k)]
          case aw.awaiting of
            NoPasses _ -> do
              let aw' = aw {awaiting = OnePass k.next}
              modify \gx -> gx {actionWindow = Just aw'}
            OnePass _ ->
              -- Both players have now passed consecutively.
              send CloseActionWindow
        -- Invalid pass (no window open, or not the priority holder):
        -- silently ignore. The server should reject these at the
        -- protocol boundary; this is belt-and-braces.
        _ -> pure ()
    CloseActionWindow -> do
      g <- get
      modify \gx -> gx {actionWindow = Nothing}
      logIt LogSystem "log.window.close" []
      -- For the basic phase structure (no combat sub-steps yet) every
      -- action window is the final step of its phase. When combat is
      -- implemented this branch will dispatch on aw.trigger to advance
      -- to the next combat sub-step instead.
      case g.phase of
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
      -- Player.receive ran first and set the new resource total; read
      -- the updated value to include the count in the log line.
      g <- get
      let Resources n = case k of
            Player1 -> g.player1.resources
            Player2 -> g.player2.resources
      logIt LogSystem
        "log.resources.collected"
        [("player", playerParam k), ("count", T.pack (show n))]
    QuestDraw k ->
      logIt LogSystem "log.quest.draw" [("player", playerParam k)]
    PlayUnit pk code zone -> do
      g <- get
      let player = lookupPlayer pk g
      case takeUnitFromHand code player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard) -> case cardDef.cost of
          Variable -> pure ()
          Fixed n ->
            when (player.resources >= Resources n) $ do
              let UnitKey nextN = g.nextUnitKey
                  unitKey = UnitKey nextN
                  paidPlayer =
                    playerWithoutCard
                      {resources = player.resources - Resources n}
                  unitDetails =
                    UnitDetails
                      { key = unitKey
                      , controller = pk
                      , zone
                      , cardDef
                      , damage = Damage 0
                      , corrupted = False
                      , attachments = []
                      , experiences = []
                      }
              modify \gx ->
                (setPlayer pk paidPlayer gx)
                  { units = unitDetails : gx.units
                  , nextUnitKey = UnitKey (nextN + 1)
                  }
              logIt LogPlayerAction
                "log.unit.played"
                [ ("player", playerParam pk)
                , ("card", T.pack cardDef.title)
                , ("cost", T.pack (show n))
                ]
              send (UnitEnteredPlay pk unitKey)
    UnitEnteredPlay pk _key ->
      -- The card's own 'receive' fires via 'dispatchToInPlayUnits'.
      -- Game just narrates.
      logIt LogSystem "log.unit.entered_play" [("player", playerParam pk)]
    DealDamageToUnit ukey amount -> do
      g <- get
      case findUnit ukey g of
        Nothing -> pure ()
        Just u -> do
          -- Passive multiplier: Bloodletter (legends-031) doubles all
          -- damage assigned to units while it is in play. Future
          -- multipliers should be expressed the same way (a generic
          -- 'damageMultiplier g amount' helper).
          let inflated = applyDamageMultipliers g (max 0 amount)
              Damage existing = u.damage
              newDmg = Damage (existing + inflated)
              u' = u {damage = newDmg} :: UnitDetails
          modify \gx -> gx {units = replaceUnit u' gx.units}
          logIt LogSystem
            "log.unit.damaged"
            [ ("card", T.pack u.cardDef.title)
            , ("amount", T.pack (show inflated))
            ]
          let Damage total = newDmg
          when (total >= unitPrintedHP u) $
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
          -- its OWN controller's discard pile (attachments may be
          -- controlled by either side — Branded by Khorne is the
          -- canonical hostile attachment).
          let dropUnitCard pl =
                pl {discard = UnitCardDef u.cardDef : pl.discard}
              dropSupport gx attach =
                let owner = lookupPlayer attach.controller gx
                    owner' =
                      owner {discard = SupportCardDef attach.cardDef : owner.discard}
                 in setPlayer attach.controller owner' gx
              gAfterUnit =
                let owner = lookupPlayer u.controller g
                    owner' = dropUnitCard owner
                 in (setPlayer u.controller owner' g)
                      {units = filter (\v -> v.key /= ukey) g.units}
              gFinal = foldl dropSupport gAfterUnit u.attachments
          put gFinal
          logIt LogSystem
            "log.unit.destroyed"
            [ ("player", playerParam u.controller)
            , ("card", T.pack u.cardDef.title)
            ]
          send (UnitLeftPlay u.controller ukey u.zone u.cardDef.code)
    UnitLeftPlay _pk _ukey _zone _code ->
      -- Pure hook point: 'dispatchToInPlayUnits' runs cards' bespoke
      -- reactions; Game itself has nothing more to do.
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
    PlayAttachment pk code targetKey -> do
      g <- get
      let player = lookupPlayer pk g
      case (takeSupportFromHand code player, findUnit targetKey g) of
        (Just (cardDef, playerWithoutCard), Just host) -> case cardDef.cost of
          Variable -> pure ()
          Fixed n ->
            when (player.resources >= Resources n) $ do
              let UnitKey nextN = g.nextUnitKey
                  attachmentKey = UnitKey nextN
                  paidPlayer =
                    playerWithoutCard
                      {resources = player.resources - Resources n}
                  attachment =
                    SupportDetails
                      { key = attachmentKey
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
                  , nextUnitKey = UnitKey (nextN + 1)
                  }
              logIt LogPlayerAction
                "log.attachment.played"
                [ ("player", playerParam pk)
                , ("card", T.pack cardDef.title)
                , ("target", T.pack host.cardDef.title)
                , ("cost", T.pack (show n))
                ]
              send (SupportEnteredPlay pk attachmentKey)
        _ -> pure ()
    SupportEnteredPlay _pk _key ->
      -- 'dispatchToInPlayUnits' walks attachments via their host; any
      -- bespoke reaction lives in the support card's 'receive'.
      pure ()
    PlaySupport pk code zone -> do
      g <- get
      let player = lookupPlayer pk g
      case takeSupportFromHand code player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard) -> case cardDef.cost of
          Variable -> pure ()
          Fixed n ->
            when (player.resources >= Resources n) $ do
              let UnitKey nextN = g.nextUnitKey
                  supportKey = UnitKey nextN
                  paidPlayer =
                    playerWithoutCard
                      {resources = player.resources - Resources n}
                  support =
                    SupportDetails
                      { key = supportKey
                      , controller = pk
                      , zone
                      , cardDef
                      , attachedTo = Nothing
                      , tokens = 0
                      }
              modify \gx ->
                (setPlayer pk paidPlayer gx)
                  { supports = support : gx.supports
                  , nextUnitKey = UnitKey (nextN + 1)
                  }
              logIt LogPlayerAction
                "log.support.played"
                [ ("player", playerParam pk)
                , ("card", T.pack cardDef.title)
                , ("cost", T.pack (show n))
                ]
              send (SupportEnteredPlay pk supportKey)
    PlayQuest pk code -> do
      g <- get
      let player = lookupPlayer pk g
      case takeQuestFromHand code player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard) -> case cardDef.cost of
          Variable -> pure ()
          Fixed n ->
            when (player.resources >= Resources n) $ do
              let UnitKey nextN = g.nextUnitKey
                  questKey = UnitKey nextN
                  paidPlayer =
                    playerWithoutCard
                      {resources = player.resources - Resources n}
                  quest =
                    QuestDetails
                      { key = questKey
                      , controller = pk
                      , cardDef
                      , tokens = 0
                      }
              modify \gx ->
                (setPlayer pk paidPlayer gx)
                  { quests = quest : gx.quests
                  , nextUnitKey = UnitKey (nextN + 1)
                  }
              logIt LogPlayerAction
                "log.quest.played"
                [ ("player", playerParam pk)
                , ("card", T.pack cardDef.title)
                , ("cost", T.pack (show n))
                ]
              send (QuestEnteredPlay pk questKey)
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
                owner {discard = SupportCardDef s.cardDef : owner.discard}
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
    PlayTactic pk code -> do
      g <- get
      let player = lookupPlayer pk g
      case takeTacticFromHand code player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard) -> case cardDef.cost of
          Variable -> pure ()
          Fixed n ->
            when (player.resources >= Resources n) $ do
              let paidPlayer =
                    playerWithoutCard
                      { resources = player.resources - Resources n
                      , discard = TacticCardDef cardDef : playerWithoutCard.discard
                      }
              modify (setPlayer pk paidPlayer)
              logIt LogPlayerAction
                "log.tactic.played"
                [ ("player", playerParam pk)
                , ("card", T.pack cardDef.title)
                , ("cost", T.pack (show n))
                ]
              send (TacticResolved pk code)
    TacticResolved pk code -> do
      g <- get
      case Map.lookup code allCards of
        Just (TacticCardDef cardDef) -> do
          let ctx = TacticContext {controller = pk, cardDef}
              owner = lookupPlayer pk g
          case cardDef.receive of
            Receive f -> f (TacticResolved pk code) owner ctx
        _ -> pure ()

-- | First-turn penalty: the starting player skips Quest and Battlefield
-- on the very first turn of the game.
shouldSkipFirstTurnPhase :: Phase -> Game -> Bool
shouldSkipFirstTurnPhase phase g =
  g.turn == Turn 1
    && g.currentPlayer == g.firstPlayer
    && (phase == QuestPhase || phase == BattlefieldPhase)

newPlayer :: PlayerKey -> Deck -> Either DeckLoadError Player
newPlayer k cs = do
  (race, cards) <- loadDeck cs
  pure $
    Player
      { key = k
      , state = IdlePlayer
      , capital = newCapital
      , resources = Resources 0
      , hand = []
      , deck = cards
      , discard = []
      , race
      }

newGame :: Deck -> Deck -> Either DeckLoadError Game
newGame deck1 deck2 = do
  player1 <- newPlayer Player1 deck1
  player2 <- newPlayer Player2 deck2
  pure $
    Game
      { player1
      , player2
      , firstPlayer = Player1
      , currentPlayer = Player1
      , turn = Turn 0
      , phase = Nothing
      , actionWindow = Nothing
      , modifiers = mempty
      , lifecycle = GameSetup
      , log = []
      , units = []
      , supports = []
      , quests = []
      , nextUnitKey = UnitKey 0
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
takeUnitFromHand :: CardCode -> Player -> Maybe (CardDef Unit, Player)
takeUnitFromHand code p = go p.hand []
  where
    go :: [SomeCardDef] -> [SomeCardDef] -> Maybe (CardDef Unit, Player)
    go [] _ = Nothing
    go (s : rest) acc = case s of
      UnitCardDef cd
        | cd.code == code ->
            Just (cd, p {hand = reverse acc ++ rest})
      _ -> go rest (s : acc)

-- 'findUnit' is exported by 'Invasion.Card' for use in card receive
-- bodies; we reuse it here.

-- | Pull a Support card matching the given 'CardCode' out of a player's
-- hand. Mirror of 'takeUnitFromHand'.
takeSupportFromHand :: CardCode -> Player -> Maybe (CardDef Support, Player)
takeSupportFromHand code p = go p.hand []
  where
    go :: [SomeCardDef] -> [SomeCardDef] -> Maybe (CardDef Support, Player)
    go [] _ = Nothing
    go (s : rest) acc = case s of
      SupportCardDef cd
        | cd.code == code ->
            Just (cd, p {hand = reverse acc ++ rest})
      _ -> go rest (s : acc)

-- | Pull a Quest card matching the given 'CardCode' out of a player's
-- hand.
takeQuestFromHand :: CardCode -> Player -> Maybe (CardDef Quest, Player)
takeQuestFromHand code p = go p.hand []
  where
    go :: [SomeCardDef] -> [SomeCardDef] -> Maybe (CardDef Quest, Player)
    go [] _ = Nothing
    go (s : rest) acc = case s of
      QuestCardDef cd
        | cd.code == code ->
            Just (cd, p {hand = reverse acc ++ rest})
      _ -> go rest (s : acc)

-- | Apply any in-play passive damage multipliers to a raw damage
-- amount. Currently only Bloodletter (legends-031) doubles, but
-- additional multipliers (Slaaneshi mirror effects, etc.) plug in here.
applyDamageMultipliers :: Game -> Int -> Int
applyDamageMultipliers g amount =
  let hasBloodletter =
        any (\u -> u.cardDef.code == CardCode "legends-031") g.units
   in if hasBloodletter then amount * 2 else amount

-- | Pull a Tactic card matching the given 'CardCode' out of a player's
-- hand.
takeTacticFromHand :: CardCode -> Player -> Maybe (CardDef Tactic, Player)
takeTacticFromHand code p = go p.hand []
  where
    go :: [SomeCardDef] -> [SomeCardDef] -> Maybe (CardDef Tactic, Player)
    go [] _ = Nothing
    go (s : rest) acc = case s of
      TacticCardDef cd
        | cd.code == code ->
            Just (cd, p {hand = reverse acc ++ rest})
      _ -> go rest (s : acc)

-- 'findSupport' / 'findQuest' are exported from 'Invasion.Card'.

replaceSupport :: SupportDetails -> [SupportDetails] -> [SupportDetails]
replaceSupport s = map \v -> if v.key == s.key then s else v

replaceQuest :: QuestDetails -> [QuestDetails] -> [QuestDetails]
replaceQuest q = map \v -> if v.key == q.key then q else v

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
