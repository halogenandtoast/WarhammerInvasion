{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

module Invasion.Capital (module Invasion.Capital) where

import Data.Aeson (ToJSON)
import Data.Aeson.TH
import Invasion.Prelude
import Invasion.Types

-- | Number of facedown developments placed in a zone. Each one adds 1 HP
-- to its zone.
newtype Developments = Developments Int
  deriving stock Show
  deriving newtype (Eq, Ord, Num, ToJSON)

-- | Damage tokens applied to a zone or unit.
newtype Damage = Damage Int
  deriving stock Show
  deriving newtype (Eq, Ord, Num, ToJSON)

-- | Effective hit points of a zone (base 8 plus one per development).
-- Always computed; never stored directly.
newtype HitPoints = HitPoints Int
  deriving stock Show
  deriving newtype (Eq, Ord, Num, ToJSON)

-- | Combat / quest-draw / resource-collection "power". A zone's effective
-- power is the sum of its base power and the power icons on units and
-- supports occupying it.
newtype Power = Power Int
  deriving stock Show
  deriving newtype (Eq, Ord, Num, ToJSON)

-- | Base power printed on the capital board for each zone.
basePower :: ZoneKind -> Power
basePower = \case
  KingdomZone -> Power 3
  QuestZone -> Power 1
  BattlefieldZone -> Power 0

-- | A capital zone. The three zones in 'Capital' all share this shape;
-- 'kind' distinguishes them. Kingdom HP differ from quest/battlefield HP
-- only by virtue of which zone is hit, not by the structure of the zone.
data Zone = Zone
  { kind :: ZoneKind
  , developments :: Developments
  , damage :: Damage
  , burned :: Bool
    -- ^ A zone burns when applied damage meets or exceeds its HP. Once
    -- burned, all damage tokens are removed and a burn token is placed;
    -- two burns on the same capital ends the game.
  }
  deriving stock Show

newZone :: ZoneKind -> Zone
newZone k = Zone {kind = k, developments = 0, damage = 0, burned = False}

-- | The base HP printed on a capital board is 8 for every zone.
baseHitPoints :: HitPoints
baseHitPoints = HitPoints 8

-- | Effective max HP of a zone: 8 + number of developments. Exposed via
-- @z.hitPoints@; we don't export a standalone function so cards can
-- continue to use 'hitPoints' as a CardBuilder helper.
instance HasField "hitPoints" Zone HitPoints where
  getField z = case (baseHitPoints, z.developments) of
    (HitPoints base, Developments n) -> HitPoints (base + n)

-- | A zone is "burning" the moment applied damage crosses its HP — used
-- by card effects (e.g. Durgnar the Bold) and to decide when to flip
-- 'burned'. Distinct from 'burned', which is the permanent post-burn
-- flag.
instance HasField "burning" Zone Bool where
  getField z = case (z.damage, z.hitPoints) of
    (Damage d, HitPoints hp) -> d >= hp

data Capital = Capital
  { kingdom :: Zone
  , quest :: Zone
  , battlefield :: Zone
  }
  deriving stock Show

newCapital :: Capital
newCapital =
  Capital
    { kingdom = newZone KingdomZone
    , quest = newZone QuestZone
    , battlefield = newZone BattlefieldZone
    }

instance HasField "zones" Capital [Zone] where
  getField c = [c.kingdom, c.quest, c.battlefield]

-- | Number of burned zones on this capital. Two = game lost.
burnedZoneCount :: Capital -> Int
burnedZoneCount c = length $ filter (.burned) c.zones

mconcat
  [ deriveToJSON defaultOptions ''Zone
  , deriveToJSON defaultOptions ''Capital
  ]
