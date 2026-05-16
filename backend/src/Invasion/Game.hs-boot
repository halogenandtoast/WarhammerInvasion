module Invasion.Game
  ( Game
  , HasGame (..)
  , getAllModifiers
  , getPlayer
  , getBattleField
  , getKingdom
  , getQuestZone
  , getCapital
  , battlefield
  , capital
  ) where

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Invasion.Capital
import Invasion.Modifier
import Invasion.Player (Player)
import Invasion.Prelude
import Invasion.Types

class Monad m => HasGame m where
  getGame :: m Game

instance HasGame m => HasGame (StateT s m)

data Game

getAllModifiers :: HasGame m => m (Map (Ref Target) [ModifierDetails])
getPlayer :: HasGame m => PlayerKey -> m Player
getBattleField :: HasGame m => PlayerKey -> m Zone
getKingdom :: HasGame m => PlayerKey -> m Zone
getQuestZone :: HasGame m => PlayerKey -> m Zone
getCapital :: HasGame m => PlayerKey -> m Capital
battlefield :: (HasGame m, HasField "controller" a PlayerKey) => a -> (Zone -> m ()) -> m ()
capital :: HasGame m => PlayerKey -> (Capital -> m ()) -> m ()
