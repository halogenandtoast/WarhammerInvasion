module Invasion.Modifier (module Invasion.Modifier) where

import Invasion.Prelude

newtype ModifierDetails = GainPower Int
  deriving stock Show

data ModifierScope = UntilEndOfTurn | ConstantScope

data Modifier = Modifier
  { details :: ModifierDetails
  , scope :: ModifierScope
  }

