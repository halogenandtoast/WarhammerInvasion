{-# LANGUAGE TemplateHaskell #-}
module Invasion.Types (module Invasion.Types) where

import Data.String (IsString)
import GHC.Records
import Invasion.Prelude
import Data.Aeson
import Data.Aeson.TH

newtype CardCode = CardCode String
  deriving newtype (Eq, Ord, Show, IsString, ToJSON, FromJSON)

data PlayerKey = Player1 | Player2
  deriving stock (Show, Eq, Ord)

instance HasField "next" PlayerKey PlayerKey where
  getField Player1 = Player2
  getField Player2 = Player1

data Number = Fixed Int | Variable
  deriving stock (Show, Eq)

data CardKind = Unit | Support | Quest | Tactic | Legend | DraftFormat
  deriving stock Show

-- | Identifies which of a capital's three zones something belongs to.
-- Used both to tag a 'Zone' (its identity) and to record where a unit
-- has been played.
data ZoneKind = KingdomZone | QuestZone | BattlefieldZone
  deriving stock (Show, Eq, Ord)

newtype UnitKey = UnitKey Int
  deriving stock (Show, Eq, Ord)

data RefKind = Target | Source

type role Ref phantom
newtype Ref (k :: RefKind) = UnitRef UnitKey
  deriving stock (Show, Eq, Ord)

class Reference a where
  toRef :: a -> Ref k

data Phase = KingdomPhase | QuestPhase | CapitalPhase | BattlefieldPhase
  deriving stock (Show, Eq, Ord)

-- | Phase that follows the given one, or 'Nothing' if the turn ends.
nextPhase :: Phase -> Maybe Phase
nextPhase = \case
  KingdomPhase -> Just QuestPhase
  QuestPhase -> Just CapitalPhase
  CapitalPhase -> Just BattlefieldPhase
  BattlefieldPhase -> Nothing

data Race = Dwarf | Chaos
  deriving stock (Show, Eq)


mconcat
  [ deriveToJSON defaultOptions ''Ref
  , deriveJSON defaultOptions ''UnitKey
  , deriveToJSON defaultOptions ''PlayerKey
  , deriveToJSON defaultOptions ''Number
  , deriveToJSON defaultOptions ''CardKind
  , -- Race is a single-constructor type today (just 'Dwarf'). The Aeson
    -- default unwraps single-constructor nullaries into '[]', which is
    -- useless for the frontend's capital-image lookup. Force tagging so
    -- it always serializes as the constructor name as a string.
    deriveToJSON
      defaultOptions {tagSingleConstructors = True, allNullaryToStringTag = True}
      ''Race
  , deriveToJSON defaultOptions ''Phase
  , deriveJSON defaultOptions ''ZoneKind
  ]

instance ToJSONKey (Ref k)
