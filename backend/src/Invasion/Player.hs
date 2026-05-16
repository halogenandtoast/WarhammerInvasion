{-# LANGUAGE TemplateHaskell #-}

module Invasion.Player (module Invasion.Player) where

import Data.Aeson (ToJSON)
import Data.Aeson.TH
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
  , developmentCards :: Map ZoneKind [Card]
    -- ^ The actual facedown cards placed as developments, keyed by
    -- the zone they sit in. The card identities are tracked here so
    -- that destroy-development effects (e.g. Demolition!) can route
    -- the card back to its owner's discard pile. The corresponding
    -- 'Zone.developments' count is the length of the list for that
    -- zone and is kept in sync by the engine.
  , race :: Race
  }
  deriving stock Show

-- | Initial empty per-zone development cards.
emptyDevelopmentCards :: Map ZoneKind [Card]
emptyDevelopmentCards =
  Map.fromList
    [ (KingdomZone, [])
    , (QuestZone, [])
    , (BattlefieldZone, [])
    ]

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
