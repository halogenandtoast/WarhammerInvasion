{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.CardDef (module Invasion.CardDef) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH
import Invasion.Player (Player)
import Invasion.Prelude
import Invasion.Types
import Queue (HasQueue)
import {-# SOURCE #-} Invasion.Game (HasGame)
import {-# SOURCE #-} Invasion.Message (Message)

data Keyword
  = Toughness Number
  | BattlefieldOnly
  | Scout
  | Limited
  | DamageCannotBeCancelled
  deriving stock Show

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
       . (HasGame m, MonadIO m, HasQueue Message m)
      => Message -> Player -> InPlay k -> m ()
  }

-- | No-op receiver: the default for cards without bespoke behavior.
noReceive :: Receive k
noReceive = Receive \_ _ _ -> pure ()

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
-- is silently dropped.
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
      ]
