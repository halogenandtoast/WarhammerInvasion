{-# LANGUAGE TemplateHaskell #-}

module Invasion.Modifier (module Invasion.Modifier) where

import Data.Aeson.TH
import Invasion.Prelude

newtype ModifierDetails = GainPower Int
  deriving stock Show

data ModifierScope = UntilEndOfTurn | ConstantScope

data Modifier = Modifier
  { details :: ModifierDetails
  , scope :: ModifierScope
  }

mconcat
  [ deriveToJSON defaultOptions ''Modifier
  , deriveToJSON defaultOptions ''ModifierDetails
  , deriveToJSON defaultOptions ''ModifierScope
  ]
