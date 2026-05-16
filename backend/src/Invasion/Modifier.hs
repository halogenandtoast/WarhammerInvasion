{-# LANGUAGE TemplateHaskell #-}

module Invasion.Modifier (module Invasion.Modifier) where

import Data.Aeson.TH
import Invasion.Prelude

newtype ModifierDetails = GainPower Int
  deriving stock (Show, Eq)

data ModifierScope = UntilEndOfTurn | ConstantScope
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
