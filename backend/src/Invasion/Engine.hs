{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Engine (module Invasion.Engine) where

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Traversable
import Invasion.Capital
import Invasion.Card
import Invasion.CardDef
import Invasion.Game
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

-- | Engine events. Every state change goes through a constructor here;
-- card text plugs in by handling, intercepting, or emitting these. New
-- behavior generally means a new constructor, not inlining work in a
-- handler.
data Message where
  -- Setup / lifecycle
  Setup :: Message
  BeginGame :: Message
  -- Player upkeep
  ShuffleDeck :: PlayerKey -> Message
  Draw :: Drawing -> Message
  Eliminate :: PlayerKey -> EliminationReason -> Message
  -- Turn structure
  BeginTurn :: PlayerKey -> Message
  EndTurn :: PlayerKey -> Message
  BeginPhase :: Phase -> Message
  EndPhase :: Phase -> Message
  -- Phase steps
  ReturnResources :: PlayerKey -> Message
  CollectResources :: PlayerKey -> Message
  QuestDraw :: PlayerKey -> Message
  -- Action windows
  OpenActionWindow :: ActionWindowTrigger -> Message
  PassPriority :: PlayerKey -> Message
  CloseActionWindow :: Message

deriving stock instance Show Message

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
    execStateT (receive msg) (g {player1, player2})
  receive = \case
    Setup -> do
      fp <- sample2 Player1 Player2
      modify \g -> g {firstPlayer = fp, currentPlayer = fp}
    BeginGame -> do
      fp <- gets (.firstPlayer)
      modify \g -> g {lifecycle = GamePlaying}
      send (BeginTurn fp)
    BeginTurn k -> do
      modify \g -> g {currentPlayer = k, turn = g.turn + Turn 1}
      send (BeginPhase KingdomPhase)
    EndTurn k -> do
      -- TODO once we have effects: clear UntilEndOfTurn modifiers,
      -- fire "after your turn ends" forced effects.
      modify \g -> g {phase = Nothing}
      send (BeginTurn k.next)
    BeginPhase phase -> do
      g <- get
      modify \gx -> gx {phase = Just phase}
      if shouldSkipFirstTurnPhase phase g
        then send (EndPhase phase)
        else do
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
      case nextPhase phase of
        Just np -> send (BeginPhase np)
        Nothing -> do
          current <- gets (.currentPlayer)
          send (EndTurn current)
    OpenActionWindow trigger -> do
      current <- gets (.currentPlayer)
      let aw = ActionWindow {trigger, awaiting = NoPasses current}
      modify \g -> g {actionWindow = Just aw}
    PassPriority k -> do
      g <- get
      case g.actionWindow of
        Just aw | priorityHolder aw.awaiting == k ->
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
        _ ->
          modify \gx ->
            gx
              { lifecycle =
                  GameFinished
                    GameResult
                      { winner = k.next
                      , reason = case reason of
                          DeckedOut -> OpponentDeckedOut
                          CapitalBurned -> OpponentCapitalBurned
                      }
              }
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
      }

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
