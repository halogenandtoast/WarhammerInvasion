{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.CardDef (module Invasion.CardDef) where

import Data.String (IsString)
import Invasion.Prelude
import Invasion.Types

newtype CardCode = CardCode String
  deriving newtype (Eq, Ord, Show, IsString)

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
  }
  deriving stock Show

data Keyword = Toughness Number | BattlefieldOnly | Scout
  deriving stock Show

data Cost = Resources Number | NoCost

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
  deriving stock Show

data Race = Dwarf
  deriving stock Show
