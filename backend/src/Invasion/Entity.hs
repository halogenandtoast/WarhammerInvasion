{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Entity
  ( Entity (..)
  , Field (..)
  , UnitDetails (..)
  , SupportDetails (..)
  , QuestDetails (..)
  , TacticContext (..)
  , unitPrintedHP
  , getModifiers
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Maybe
import Invasion.Capital (Damage (..))
import Invasion.CardDef
import Invasion.Modifier
import Invasion.Prelude
import Invasion.Types
import {-# SOURCE #-} Invasion.Game

type family DetailsOfKind (k :: CardKind)
type family KeyOfKind (k :: CardKind)

type instance DetailsOfKind Unit = UnitDetails
type instance KeyOfKind Unit = UnitKey

data family Field (k :: CardKind) typ

data instance Field Unit typ where
  UnitController :: Field Unit PlayerKey
  UnitZone :: Field Unit ZoneKind
  UnitPower :: Field Unit Int

class Entity (k :: CardKind) a where
  toDetails :: a -> DetailsOfKind k
  toKey :: a -> KeyOfKind k
  project :: HasGame m => Field k typ -> a -> m typ

-- | A unit in play: the card definition together with its bookkeeping
-- (unique 'UnitKey', controlling player, zone it occupies, accumulated
-- damage).
data UnitDetails = UnitDetails
  { key :: UnitKey
  , controller :: PlayerKey
  , zone :: ZoneKind
  , cardDef :: CardDef Unit
  , damage :: Damage
  , corrupted :: Bool
    -- ^ A corrupted unit is "wrecked" — its abilities are suppressed
    -- and it doesn't quest, defend, or attack. Cleared during the
    -- kingdom phase restoration step (one per turn, controller's
    -- choice). Cards (Daemonsword, Festering Nurglings, Dominion of
    -- Chaos, …) flip this on.
  , attachments :: [SupportDetails]
    -- ^ Support cards attached to this unit (Daemonsword, Branded by
    -- Khorne, Mark of Chaos, …). When the unit leaves play, all
    -- attachments leave with it.
  , experiences :: [CardCode]
    -- ^ Cards (usually destroyed enemy units) facedown-attached as
    -- "experience" markers, e.g. Skulltaker. Each one is functionally a
    -- counter that the host card's text can reference.
  }
  deriving stock Show

-- | A unit's printed hit points. 'Variable' is treated as 1 until we
-- have an X-aware evaluator (Toughness X cards aren't in the active
-- decks yet).
unitPrintedHP :: UnitDetails -> Int
unitPrintedHP u = case u.cardDef.hitPoints of
  Just (Fixed n) -> n
  Just Variable -> 1
  Nothing -> 1

-- | A support card in play. Has the same shape as 'UnitDetails' for now;
-- when supports gain effects or attachments they'll grow distinct
-- fields.
data SupportDetails = SupportDetails
  { key :: UnitKey
  , controller :: PlayerKey
  , zone :: ZoneKind
  , cardDef :: CardDef Support
  , attachedTo :: Maybe UnitKey
    -- ^ 'Just' for supports that are attached to a unit; 'Nothing' for
    -- supports sitting freely in a zone. The host's controller is not
    -- necessarily this support's controller (Branded by Khorne can be
    -- played on an enemy unit).
  , tokens :: Int
    -- ^ Generic counter slot. Iron Throneroom counts down from 4 here;
    -- other tokenised supports (resource-storing siege engines, …)
    -- will reuse it.
  }
  deriving stock Show

-- | A quest card in play (always sits in the quest zone, hence no
-- 'zone' field).
data QuestDetails = QuestDetails
  { key :: UnitKey
  , controller :: PlayerKey
  , cardDef :: CardDef Quest
  , tokens :: Int
    -- ^ Token accumulator: Raiding Camps tracks none, A Glorious Death
    -- accumulates resource counters, Dominion of Chaos stores combat
    -- damage that's been routed here. The exact semantics live in each
    -- card's 'receive'.
  }
  deriving stock Show

-- | A tactic doesn't persist in play — it resolves and goes to the
-- discard. The "in-play" record for the duration of resolution carries
-- the player who's resolving and the card itself.
data TacticContext = TacticContext
  { controller :: PlayerKey
  , cardDef :: CardDef Tactic
  }
  deriving stock Show

-- Hook each in-play record into the open type family declared in
-- 'Invasion.CardDef'.
type instance InPlay Unit = UnitDetails
type instance InPlay Support = SupportDetails
type instance InPlay Quest = QuestDetails
type instance InPlay Tactic = TacticContext

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

instance ToJSON UnitDetails where
  toJSON d =
    object
      [ "key" .= d.key
      , "controller" .= d.controller
      , "zone" .= d.zone
      , "cardDef" .= d.cardDef
      , "damage" .= d.damage
      , "corrupted" .= d.corrupted
      , "attachments" .= d.attachments
      , "experiences" .= d.experiences
      ]

instance ToJSON SupportDetails where
  toJSON d =
    object
      [ "key" .= d.key
      , "controller" .= d.controller
      , "zone" .= d.zone
      , "cardDef" .= d.cardDef
      , "attachedTo" .= d.attachedTo
      , "tokens" .= d.tokens
      ]

instance ToJSON QuestDetails where
  toJSON d =
    object
      [ "key" .= d.key
      , "controller" .= d.controller
      , "cardDef" .= d.cardDef
      , "tokens" .= d.tokens
      ]

instance ToJSON TacticContext where
  toJSON d =
    object
      [ "controller" .= d.controller
      , "cardDef" .= d.cardDef
      ]

getModifiers :: (HasGame m, Reference a) => a -> m [ModifierDetails]
getModifiers a = fromMaybe [] . Map.lookup (toRef a) <$> getAllModifiers
