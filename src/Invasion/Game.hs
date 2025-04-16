{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Game (module Invasion.Game) where

import Invasion.Player
import Invasion.Types
import Invasion.Capital
import Invasion.Prelude
import Invasion.Modifier
import Control.Monad.State.Strict
import Data.Map.Strict (Map)

class Monad m => HasGame m where
  getGame :: m Game

instance HasGame m => HasGame (StateT s m) where
  getGame = lift getGame

data GameState
  = SetupGame GameState
  | FinishedGame PlayerKey
  | IdleGame
  | DoTurn PlayerKey GameState
  | GamePhase Phase GameState
  | WaitOnPlayer PlayerKey GameState
  deriving stock Show

data Game = Game
  { player1 :: Player
  , player2 :: Player
  , firstPlayer :: PlayerKey
  , currentPlayer :: PlayerKey
  , modifiers :: Map (Ref Target) [ModifierDetails]
  , state :: GameState
  }
  deriving stock Show

instance HasField "over" Game Bool where
  getField g = case g.state of
    FinishedGame _ -> True
    _ -> False

getAllModifiers :: HasGame m => m (Map (Ref Target) [ModifierDetails])
getAllModifiers = do
  g <- getGame
  pure g.modifiers

getPlayer :: HasGame m => PlayerKey -> m Player
getPlayer pkey = do
  g <- getGame
  pure $ case pkey of
    Player1 -> g.player1
    Player2 -> g.player2

getBattleField :: HasGame m => PlayerKey -> m Battlefield
getBattleField pkey = do
  p <- getPlayer pkey
  pure $ p.battlefield

getCapital :: HasGame m => PlayerKey -> m Capital
getCapital pkey = do
  p <- getPlayer pkey
  pure $ p.capital

battlefield :: (HasGame m, HasField "controller" a PlayerKey) => a -> (Battlefield -> m ()) -> m ()
battlefield a f = getBattleField a.controller >>= f

capital :: HasGame m => PlayerKey -> (Capital -> m ()) -> m ()
capital pkey f = getCapital pkey >>= f
