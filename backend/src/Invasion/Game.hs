{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Game (module Invasion.Game) where

import Control.Monad.State.Strict
import Data.Aeson.TH
import Data.Map.Strict (Map)
import Invasion.Capital
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types

class Monad m => HasGame m where
  getGame :: m Game

instance HasGame m => HasGame (StateT s m) where
  getGame = lift getGame

data GameState = FinishedGame PlayerKey | IdleGame
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

mconcat
  [ deriveToJSON defaultOptions ''Game
  , deriveToJSON defaultOptions ''GameState
  ]
