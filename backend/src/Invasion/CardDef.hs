{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.CardDef (module Invasion.CardDef) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Invasion.Player (Player)
import Invasion.Prelude
import Invasion.Types
import Queue (HasQueue)
import {-# SOURCE #-} Invasion.Engine (HasPromptIO)
import {-# SOURCE #-} Invasion.Game (HasGame)
import {-# SOURCE #-} Invasion.Message (Message)

data Keyword
  = Toughness Number
  | BattlefieldOnly
  | KingdomOnly
  | QuestOnly
  | Scout
  | Limited
  | DamageCannotBeCancelled
  | Counterstrike Int
  | PlayInOpponentArea
    -- ^ Quest enters play in the opponent's play area while remaining
    -- under the playing player's control. Used by Dominion of Chaos.
  | Ambush
    -- ^ Triggerable only while the host development is facedown
    -- (FAQ 2.2 v2.0 keyword). If an effect has flipped the development
    -- face-up, its Ambush ability cannot fire.
  | OrderOnly
    -- ^ Neutral-card restriction: cannot be included in a Destruction
    -- (Chaos / Orc / Dark Elf) deck.
  | DestructionOnly
    -- ^ Neutral-card restriction: cannot be included in an Order
    -- (Empire / Dwarf / High Elf) deck.
  | LimitOneHeroPerZone
    -- ^ Hero restriction. While a player controls a Hero in a given
    -- zone, neither player may put, play, or move another Hero into
    -- that same zone (FAQ 2.2 clarification).
  deriving stock (Show, Eq)

data Cost = PayResources Number | NoCost

data Trait
  = Warrior
  | Spell
  | Engineer
  | Elite
  | Slayer
  | Priest
  | Hero
  | Ranger
  | Rune
  | Building
  | Attachment
  | Weapon
  | Siege
  | Daemon
  | Creature
  | Sorcerer
  | Knight
  | Cavalry
  | Mission
  | QuestTrait
  | Wasteland
  | CapitalCenter
  | Rift
  | Relic
  deriving stock (Show, Eq)

mconcat
  [ deriveToJSON defaultOptions ''Keyword
  , deriveToJSON defaultOptions ''Trait
  ]

-- | Open type family of in-play self-references, indexed by card kind.
-- Instances are declared next to each kind's in-play record in
-- 'Invasion.Entity'.
type family InPlay (k :: CardKind)

-- | A card's reaction to engine events. Wrapped in a newtype because
-- record fields can't directly hold a polymorphic function. The
-- constraints (@HasGame@, @MonadIO@) describe the engine capabilities
-- card code is allowed to use; widen them when card behavior needs
-- more.
newtype Receive k = Receive
  { unReceive
      :: forall m
       . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
      => Message -> Player -> InPlay k -> m ()
  }

-- | No-op receiver: the default for cards without bespoke behavior.
noReceive :: Receive k
noReceive = Receive \_ _ _ -> pure ()

-- | What kind of target an action requires. The engine validates the
-- supplied 'ActionTarget' against this schema before invoking the
-- action's effect.
data TargetSchema
  = NoTargetSchema
    -- ^ The action takes no target.
  | AnyUnitTargetSchema
    -- ^ Any unit currently in play.
  | EnemyUnitTargetSchema
    -- ^ A unit controlled by the opponent.
  | FriendlyUnitTargetSchema
    -- ^ A unit controlled by the player triggering the action.
  | AnyZoneTargetSchema
    -- ^ A zone of any player.
  | EnemyZoneTargetSchema
    -- ^ A zone controlled by the opponent.
  | SupportTargetSchema
    -- ^ A free-standing support in play.
  deriving stock (Show, Eq)

-- | The concrete target supplied with a 'TriggerCardAction' message.
data ActionTarget
  = NoTarget
  | TargetUnit UnitKey
  | TargetZone PlayerKey ZoneKind
  | TargetSupport UnitKey
  deriving stock (Show, Eq)

mconcat
  [ deriveToJSON defaultOptions ''TargetSchema
  , deriveToJSON defaultOptions ''ActionTarget
  ]

-- | The bespoke effect a card's action runs once cost has been paid and
-- target has been validated. Same capability set as 'Receive'.
newtype ActionEffect k = ActionEffect
  { unEffect
      :: forall m
       . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
      => PlayerKey -> InPlay k -> ActionTarget -> m ()
  }

-- | A single Action ability printed on a card. Cards may declare zero
-- or more actions; the engine enumerates them during action windows.
-- Field names are prefixed with @action@ so they can't shadow
-- 'CardDef'\'s own @cost@/@target@ fields under
-- @DuplicateRecordFields@.
data ActionDef k = ActionDef
  { actionName :: Text
  , actionCost :: Int
  , actionTarget :: TargetSchema
  , actionEffect :: ActionEffect k
  }

data CardDef (k :: CardKind) = CardDef
  { code :: CardCode
  , title :: String
  , kind :: CardKind
  , races :: [Race]
  , cost :: Number
  , loyalty :: Int
  , power :: Int
  , hitPoints :: Maybe Number
  , traits :: [Trait]
  , text :: Maybe String
  , flavor :: Maybe String
  , keywords :: [Keyword]
  , unique :: Bool
  , actions :: [ActionDef k]
  , receive :: Receive k
  }

-- The 'receive' function field can't be 'Show'n, so we derive a manual
-- instance that prints just enough to identify the card in trace logs.
instance Show (CardDef k) where
  showsPrec d c =
    showParen (d > 10) $
      showString "CardDef "
        . shows c.code
        . showString " "
        . shows c.title

-- The 'receive' function field is not JSON-encodable; the frontend only
-- needs the static metadata anyway. Hand-roll the instance so the field
-- is silently dropped. Actions also serialize as just their static
-- metadata (name/cost/target schema) so the client can render the
-- available-actions list without seeing the effect closures.
instance ToJSON (CardDef k) where
  toJSON c =
    object
      [ "code" .= c.code
      , "title" .= c.title
      , "kind" .= c.kind
      , "races" .= c.races
      , "cost" .= c.cost
      , "loyalty" .= c.loyalty
      , "power" .= c.power
      , "hitPoints" .= c.hitPoints
      , "traits" .= c.traits
      , "text" .= c.text
      , "flavor" .= c.flavor
      , "keywords" .= c.keywords
      , "unique" .= c.unique
      , "actions" .= map actionDefMeta c.actions
      ]
    where
      actionDefMeta a =
        object
          [ "name" .= a.actionName
          , "cost" .= a.actionCost
          , "target" .= a.actionTarget
          ]
