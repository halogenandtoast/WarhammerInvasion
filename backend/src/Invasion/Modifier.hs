{-# LANGUAGE TemplateHaskell #-}

module Invasion.Modifier (module Invasion.Modifier) where

import Data.Aeson.TH
import Invasion.Prelude

-- | Atomic effect a 'Modifier' carries on its target. Each constructor
-- is interpreted in one specific place in the engine — see
-- 'recomputeUnitStats' for power/HP math and the combat declarators
-- for the 'CannotAttack' / 'CannotDefend' gates.
data ModifierDetails
  = GainPower Int
    -- ^ @+N@ printed-power. Negative values debuff. Sums across
    -- stacked modifiers.
  | LoseHitPoints Int
    -- ^ @-N@ printed hit points (Vile Sorceress, Horrific Mutation).
    -- Negative HP cannot reduce a unit below 1.
  | CannotAttack
    -- ^ Excludes the unit from the legal attacker pool (Franz's Decree).
  | CannotDefend
    -- ^ Excludes the unit from the legal defender pool.
  | CannotBeCorrupted
    -- ^ The unit ignores 'CorruptUnit' messages (Blessing of Isha).
  | RedirectedThisTurn
    -- ^ A marker that some once-per-turn redirect has fired for
    -- this unit (Warrior Priests). Cleared at end of turn via
    -- 'EndOfTurn'-scoped install.
  | ActionUsedThisTurn
    -- ^ Marker that this in-play card's once-per-turn action has
    -- fired. Cards like Archmage of Saphery / Rock Lobber set this
    -- inside their action body and bail early if it's present.
  deriving stock (Show, Eq)

data ModifierScope = EndOfTurn | Permanent
  deriving stock (Show, Eq)

data Modifier = Modifier
  { details :: ModifierDetails
  , scope :: ModifierScope
  }
  deriving stock (Show, Eq)

mconcat
  [ deriveToJSON defaultOptions ''Modifier
  , deriveToJSON defaultOptions ''ModifierDetails
  , deriveToJSON defaultOptions ''ModifierScope
  ]
