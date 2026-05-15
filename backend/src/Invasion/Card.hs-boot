module Invasion.Card (SomeCardDef) where

import Invasion.Prelude
import Data.Aeson

data SomeCardDef
instance Show SomeCardDef
instance ToJSON SomeCardDef
