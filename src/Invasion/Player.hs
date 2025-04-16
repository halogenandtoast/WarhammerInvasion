{-# LANGUAGE TemplateHaskell #-}

module Invasion.Player (module Invasion.Player) where

import Data.Aeson.TH
import Invasion.Capital
import {-# SOURCE #-} Invasion.Card
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
  | PlayerDraw Drawing
  deriving stock Show

instance HasField "battlefield" Player Battlefield where
  getField p = p.capital.battlefield

instance HasField "idle" Player Bool where
  getField p = case p.state of
    IdlePlayer -> True
    _ -> False

data DrawingKind = StartingHand | StandardDraw
  deriving stock Show

data Drawing = Drawing
  { kind :: DrawingKind
  , player :: PlayerKey
  }
  deriving stock Show

mconcat
  [ deriveToJSON defaultOptions ''Player
  , deriveToJSON defaultOptions ''PlayerState
  , deriveToJSON defaultOptions ''Drawing
  , deriveToJSON defaultOptions ''DrawingKind
  ]
