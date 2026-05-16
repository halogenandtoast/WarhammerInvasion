module Invasion.Card (SomeCardDef, Card) where

import Invasion.Prelude
import Data.Aeson

data SomeCardDef
instance Show SomeCardDef
instance ToJSON SomeCardDef

data Card
instance Show Card
instance ToJSON Card
