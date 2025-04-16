{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Queue
import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Traversable
import System.Random.Shuffle
import Invasion.Prelude
import Invasion.Player
import Invasion.Types
import Invasion.Game
import Invasion.CardDef
import Invasion.Card
import Invasion.Capital

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

dwarfStarterDeck :: [CardCode]
dwarfStarterDeck =
  replicate 3 "core-001" -- Defender of the Hold
    <> replicate 1 "core-002" -- Zhufbar Engineers
    <> replicate 3 "core-003" -- Hammerer of Karak Azul
    <> replicate 3 "core-004" -- Troll Slayers
    <> replicate 3 "core-005" -- Runesmith
    <> replicate 1 "core-006" -- Durgnar the Bold
    <> replicate 1 "core-007" -- King Kazador
    <> replicate 1 "core-008" -- Dwarf Cannon Crew
    <> replicate 1 "core-009" -- Dwarf Masons
    <> replicate 2 "core-010" -- Dwarf Ranger
    <> replicate 2 "core-011" -- Mountain Brigade
    <> replicate 1 "core-012" -- Ironbreakers of Ankhor
    <> replicate 1 "core-013" -- Rune of Fortitude
    <> replicate 3 "core-014" -- Keystone Forge
    <> replicate 1 "core-015" -- Organ Gun
    <> replicate 1 "core-016" -- Master Rune of Dismay
    <> replicate 2 "core-017" -- A Glorious Death
    <> replicate 2 "core-018" -- Grudge Thrower
    <> replicate 1 "core-019" -- Burying the Grudge
    <> replicate 1 "core-020" -- Stubborn Refusal
    <> replicate 2 "core-021" -- Striking the Grudge
    <> replicate 1 "core-022" -- Grudge Thrower Assault
    <> replicate 1 "core-023" -- Demolition!
    <> replicate 1 "core-024" -- Wake the Mountain
    <> replicate 1 "core-025" -- Master Rune of Valaya

type DeckLoadError = String

loadDeck :: [CardCode] -> Either DeckLoadError [SomeCardDef]
loadDeck cs = for cs \c ->
  case Map.lookup c allCards of
    Nothing -> Left $ "Card not found: " <> show c
    Just cardDef -> Right cardDef

runGame :: GameT a -> Env -> IO a
runGame (GameT inner) = evalStateT inner

overGame :: (Game -> GameT Game) -> GameT ()
overGame f = do
  game <- f =<< getGame
  modify \e -> e { game }

gameMain :: GameT Game
gameMain = do
  mmsg <- pop
  case mmsg of
    Just msg -> do
      liftIO $ print msg
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
      modify \g -> g { firstPlayer }
    BeginTurn currentPlayer -> do
      send $ BeginPhase KingdomPhase
      modify \g -> g { currentPlayer }
    _ -> pure ()

newPlayer :: PlayerKey -> [CardCode] -> Either DeckLoadError Player
newPlayer k cs = Player k False IdlePlayer (Capital (Battlefield 0 8 0) (QuestZone 0 8 0)) [] <$> loadDeck cs

newGame :: [CardCode] -> [CardCode] -> Either DeckLoadError Game
newGame deck1 deck2 = do
  player1 <- newPlayer Player1 deck1
  player2 <- newPlayer Player2 deck2
  pure $ Game player1 player2 Player1 Player1 mempty IdleGame

main :: IO ()
main = do
  case newGame dwarfStarterDeck dwarfStarterDeck of
    Left err -> error err
    Right game -> print =<< runGame (send Setup >> gameMain) =<< newEnv game
