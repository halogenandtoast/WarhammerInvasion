{-# LANGUAGE TemplateHaskell #-}

module Invasion.Modifier (module Invasion.Modifier) where

import Data.Aeson.TH
import Invasion.Prelude
import Invasion.Types (UnitKey)

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
  | GainHitPoints Int
    -- ^ @+N@ printed hit points until the scope expires (We Need Your
    -- Blood's beneficiary). Negative values are expressed with
    -- 'LoseHitPoints' instead.
  | DamageShield Int
    -- ^ "Cancel the next N damage that would be dealt to this unit."
    -- (Steel's Bane.) Consumed point-by-point on the cancellable
    -- damage path; the engine rewrites the remaining count in place.
  | RedirectShield Int UnitKey
    -- ^ "The next N damage dealt to this unit are redirected to
    -- [other unit]." (Blessing of Valaya.) Consumed like
    -- 'DamageShield'; the claimed points are re-dealt to the carried
    -- target. Expires silently if the target has left play.
  | LoseAllToughness
    -- ^ "This unit loses all Toughness." (Morathi's Pegasus's
    -- opponent-triggered ability.) 'totalToughness' returns 0 while
    -- present.
  | TargetTaxBonus Int
    -- ^ "Cancel any other action that targets this unit unless the
    -- action's controller pays an additional N resources." (Iron
    -- Discipline.) Summed into 'extraTargetTax' for any caster.
  | GainCombatDamage Int
    -- ^ "This unit deals +N damage in combat." (Naggaroth Spearmen.)
    -- Added by 'combatDamageOf' on top of effective power.
  | MustDefend
    -- ^ "Target unit must defend this turn, if able." (Animosity,
    -- Alluring Daemonettes.) The defender-declaration step force-
    -- includes eligible units carrying this marker.
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
