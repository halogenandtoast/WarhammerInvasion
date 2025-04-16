module Invasion.Entity (Entity(..), Field(..), UnitDetails(..)) where

import {-# SOURCE #-} Invasion.Game
import Invasion.Types
import Invasion.Modifier
import Invasion.CardDef
import Invasion.Prelude
import Data.Map.Strict qualified as Map
import Data.Maybe

type family DetailsOfKind (k :: CardKind)
type family KeyOfKind (k :: CardKind)

type instance DetailsOfKind Unit = UnitDetails
type instance KeyOfKind Unit = UnitKey

data family Field (k :: CardKind) typ

data instance Field Unit typ where
  UnitController :: Field Unit PlayerKey
  UnitZone :: Field Unit Zone
  UnitPower :: Field Unit Int

class Entity (k :: CardKind) a where
  toDetails :: a -> DetailsOfKind k
  toKey :: a -> KeyOfKind k
  project :: HasGame m => Field k typ -> a -> m typ

data UnitDetails = UnitDetails
  { key :: UnitKey
  , controller :: PlayerKey
  , zone :: Zone
  , cardDef :: CardDef Unit
  }
  deriving stock Show

instance Reference UnitDetails where
  toRef details = UnitRef details.key

instance Entity Unit UnitDetails where
  toDetails = id
  toKey = (.key)
  project = \case
    UnitController -> pure . (.controller)
    UnitZone -> pure . (.zone)
    UnitPower -> \details -> do
      mods <- getModifiers details
      let additionalPower = sum [n | GainPower n <- mods]
      pure $ details.cardDef.power + additionalPower

getModifiers :: (HasGame m, Reference a) => a -> m [ModifierDetails]
getModifiers a = fromMaybe [] . Map.lookup (toRef a) <$> getAllModifiers

