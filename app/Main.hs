{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Traversable
import System.Random.Shuffle
import Invasion.Prelude
import Invasion.Player
import Invasion.Player qualified as Player
import Invasion.Types
import Invasion.Game
import Invasion.Game qualified as Game
import Invasion.CardDef
import Invasion.Card
import Invasion.Capital

newtype GameT a = GameT (StateT Game IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadState Game)

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

runGame :: GameT a -> Game -> IO a
runGame (GameT inner) = evalStateT inner

gameMain :: GameT ()
gameMain = do
  g <- get
  unless g.over do
    g' <- tick g
    put g'
    gameMain

class Tick a where
  tick :: a -> GameT a

overPlayer :: PlayerKey -> (Player -> Player) -> Game -> Game
overPlayer k f g =
  case k of
    Player1 -> g {player1 = f g.player1}
    Player2 -> g {player2 = f g.player2}

withPlayer :: Game -> PlayerKey -> (Player -> a) -> a
withPlayer g k f =
  case k of
    Player1 -> f g.player1
    Player2 -> f g.player2

class HasState a where
  type StateOf a
  updateState :: (StateOf a -> StateOf a) -> a -> a

instance HasState Player where
  type StateOf Player = PlayerState
  updateState f p = p {Player.state = f p.state}

instance HasState Game where
  type StateOf Game = GameState
  updateState f g = g {Game.state = f g.state}

instance Tick Game where
  tick g = case g.state of
    SetupGame nextState -> do
      firstPlayer <- sample2 Player1 Player2
      let player1 = withPlayer g Player1 $ updateState (ShuffleDeck . Draw (Drawing StartingHand))
      let player2 = withPlayer g Player2 $ updateState (ShuffleDeck . Draw (Drawing StartingHand))
      pure
        $ g
          { firstPlayer
          , Game.state = WaitOnPlayer Player1 (WaitOnPlayer Player2 (DoTurn firstPlayer nextState))
          , player1
          , player2
          }
    IdleGame -> do
      player1 <- tick g.player1
      player2 <- tick g.player2
      pure $ g {player1, player2}
    DoTurn currentPlayer nextState -> do
      pure
        $ g
          { Game.state =
              GamePhase KingdomPhase
                $ GamePhase QuestPhase
                $ GamePhase CapitalPhase
                $ GamePhase BattlefieldPhase
                $ DoTurn currentPlayer.next nextState
          , currentPlayer
          }
    GamePhase KingdomPhase nextState -> do
      liftIO $ putStrLn $ "Begin Kingdom Phase for: " <> show g.currentPlayer
      let update = overPlayer g.currentPlayer (updateState $ PerformPhase KingdomPhase)
      pure $ update $ g {Game.state = WaitOnPlayer g.currentPlayer nextState}
    GamePhase QuestPhase nextState -> do
      liftIO $ putStrLn $ "Begin Quest Phase for: " <> show g.currentPlayer
      pure $ g {Game.state = WaitOnPlayer g.currentPlayer nextState}
    GamePhase CapitalPhase nextState -> do
      liftIO $ putStrLn $ "Begin Capital Phase for: " <> show g.currentPlayer
      pure $ g {Game.state = WaitOnPlayer g.currentPlayer nextState}
    GamePhase BattlefieldPhase _nextState -> do
      liftIO $ putStrLn $ "Begin Battlefield Phase for: " <> show g.currentPlayer
      pure $ g {Game.state = FinishedGame g.currentPlayer}
    WaitOnPlayer currentPlayer nextState -> do
      case currentPlayer of
        Player1 | g.player1.idle -> pure $ g {Game.state = nextState}
        Player2 | g.player2.idle -> pure $ g {Game.state = nextState}
        _ -> do
          p1 <- tick g.player1
          p2 <- tick g.player2
          let
            nextState' =
              if p1.eliminated || p2.eliminated
                then FinishedGame currentPlayer
                else g.state
          pure $ g {player1 = p1, player2 = p2, Game.state = nextState'}
    FinishedGame _ -> pure g

instance Tick Player where
  tick p = case p.state of
    IdlePlayer -> pure p
    Eliminated -> pure p
    ShuffleDeck nextState -> do
      liftIO $ putStrLn $ "Shuffling deck " <> show p.key
      cards' <- liftIO $ shuffleM p.deck
      pure $ p {deck = cards', Player.state = nextState}
    PerformPhase KingdomPhase nextState -> do
      liftIO $ putStrLn $ "Performing Kingdom Phase for: " <> show p.key
      pure $ p {Player.state = nextState}
    PerformPhase QuestPhase nextState -> do
      liftIO $ putStrLn $ "Performing Quest Phase for: " <> show p.key
      pure $ p {Player.state = nextState}
    PerformPhase CapitalPhase nextState -> do
      liftIO $ putStrLn $ "Performing Capital Phase for: " <> show p.key
      pure $ p {Player.state = nextState}
    PerformPhase BattlefieldPhase nextState -> do
      liftIO $ putStrLn $ "Performing Battlefield Phase for: " <> show p.key
      pure $ p {Player.state = nextState}
    Draw drawing nextState -> do
      liftIO $ putStrLn $ "Drawing " <> show drawing.kind <> " for " <> show p.key
      case drawing.kind of
        StartingHand -> do
          let (hand, deck') = splitAt 7 p.deck
          pure $ p {hand, deck = deck', Player.state = nextState}
        StandardDraw -> do
          case p.deck of
            [] -> pure $ p {Player.state = Eliminated}
            (card : deck') -> pure $ p {hand = card : p.hand, deck = deck', state = nextState}

newPlayer :: PlayerKey -> [CardCode] -> Either DeckLoadError Player
newPlayer k cs = Player k False IdlePlayer (Capital (Battlefield 0 8 0) (QuestZone 0 8 0)) [] <$> loadDeck cs

newGame :: [CardCode] -> [CardCode] -> Either DeckLoadError Game
newGame deck1 deck2 = do
  player1 <- newPlayer Player1 deck1
  player2 <- newPlayer Player2 deck2
  pure $ Game player1 player2 Player1 Player1 mempty (SetupGame IdleGame)

main :: IO ()
main = do
  case newGame dwarfStarterDeck dwarfStarterDeck of
    Left err -> error err
    Right game -> runGame gameMain game
