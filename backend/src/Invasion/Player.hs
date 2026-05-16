{-# LANGUAGE TemplateHaskell #-}

module Invasion.Player (module Invasion.Player) where

import Data.Aeson (ToJSON)
import Data.Aeson.TH
import Invasion.Capital
import {-# SOURCE #-} Invasion.Card (Card)
import Invasion.Prelude
import Invasion.Types

-- | A player's resource pool. Filled during the kingdom phase, spent on
-- card costs. Unspent resources are returned to the central pool at the
-- start of the next kingdom phase.
newtype Resources = Resources Int
  deriving stock Show
  deriving newtype (Eq, Ord, Num, ToJSON)

data Player = Player
  { key :: PlayerKey
  , state :: PlayerState
  , capital :: Capital
  , resources :: Resources
  , hand :: [Card]
  , deck :: [Card]
  , discard :: [Card]
  , race :: Race
  }
  deriving stock Show

-- | Top-level lifecycle state of a player. 'Eliminated' is terminal: a
-- player loses if either two of their capital zones burn or their deck
-- runs out (decking out).
data PlayerState
  = IdlePlayer
  | Eliminated EliminationReason
  | PlayerDraw Drawing
  deriving stock Show

data EliminationReason
  = DeckedOut
  | CapitalBurned
  deriving stock Show

instance HasField "battlefield" Player Zone where
  getField p = p.capital.battlefield

instance HasField "idle" Player Bool where
  getField p = case p.state of
    IdlePlayer -> True
    _ -> False

instance HasField "eliminated" Player Bool where
  getField p = case p.state of
    Eliminated _ -> True
    _ -> False

data DrawingKind = StartingHand | StandardDraw
  deriving stock Show

data Drawing = Drawing
  { kind :: DrawingKind
  , player :: PlayerKey
  }
  deriving stock Show

mconcat
  [ deriveToJSON defaultOptions ''EliminationReason
  , deriveToJSON defaultOptions ''Player
  , deriveToJSON defaultOptions ''PlayerState
  , deriveToJSON defaultOptions ''Drawing
  , deriveToJSON defaultOptions ''DrawingKind
  ]
