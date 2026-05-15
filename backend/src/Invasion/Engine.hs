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

gameMain :: GameT Game
gameMain = do
  mmsg <- pop
  case mmsg of
    Just msg -> do
      overGame $ distribute msg
      gameMain
    Nothing -> getGame

class Run a where
  distribute :: Message -> a -> GameT a
  distribute msg = execStateT (receive msg)
  receive :: Message -> StateT a GameT ()

data Message where
  Setup :: Message
  ShuffleDeck :: PlayerKey -> Message
  Draw :: Drawing -> Message
  BeginTurn :: PlayerKey -> Message
  BeginPhase :: Phase -> Message

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
        _ -> pure ()
    _ -> pure ()

instance Run Game where
  distribute msg g = do
    player1 <- distribute msg g.player1
    player2 <- distribute msg g.player2
    execStateT (receive msg) (g {player1, player2})
  receive = \case
    Setup -> do
      firstPlayer <- sample2 Player1 Player2
      modify \g -> g {firstPlayer}
    BeginTurn currentPlayer -> do
      send $ BeginPhase KingdomPhase
      modify \g -> g {currentPlayer}
    _ -> pure ()

newPlayer :: PlayerKey -> Deck -> Either DeckLoadError Player
newPlayer k cs = do
  (race, cards) <- loadDeck cs
  pure $ Player k False IdlePlayer (Capital (Battlefield 0 8 0) (QuestZone 0 8 0)) [] cards race

newGame :: Deck -> Deck -> Either DeckLoadError Game
newGame deck1 deck2 = do
  player1 <- newPlayer Player1 deck1
  player2 <- newPlayer Player2 deck2
  pure $ Game player1 player2 Player1 Player1 mempty IdleGame

runSetup :: IO (Either DeckLoadError Game)
runSetup =
  case newGame dwarfStarterDeck dwarfStarterDeck of
    Left err -> pure $ Left err
    Right game -> do
      env <- newEnv game
      Right <$> runGame (send Setup >> gameMain) env
