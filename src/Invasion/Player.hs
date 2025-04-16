module Invasion.Player (module Invasion.Player) where

import {-# SOURCE #-} Invasion.Card
import Invasion.Capital
import Invasion.Prelude
import Invasion.Types

data Player = Player
  { key :: PlayerKey
  , eliminated :: Bool
  , state :: PlayerState
  , capital :: Capital
  , hand :: [SomeCardDef]
  , deck :: [SomeCardDef]
  }
  deriving stock Show

data PlayerState
  = IdlePlayer
  | Eliminated
  | Draw Drawing PlayerState
  | PerformPhase Phase PlayerState
  | ShuffleDeck PlayerState
  deriving stock Show

instance HasField "battlefield" Player Battlefield where
  getField p = p.capital.battlefield

instance HasField "idle" Player Bool where
  getField p = case p.state of
    IdlePlayer -> True
    _ -> False

data DrawingKind = StartingHand | StandardDraw
  deriving stock Show

newtype Drawing = Drawing
  { kind :: DrawingKind
  }
  deriving stock Show

