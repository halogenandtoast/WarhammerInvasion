{-# LANGUAGE TemplateHaskell #-}
module Invasion.Types (module Invasion.Types) where

import GHC.Records
import Invasion.Prelude
import Data.Aeson.TH
import Data.Aeson.Types

data PlayerKey = Player1 | Player2
  deriving stock (Show, Eq)

instance HasField "next" PlayerKey PlayerKey where
  getField Player1 = Player2
  getField Player2 = Player1

data Number = Fixed Int | Variable
  deriving stock Show

data CardKind = Unit | Support | Quest | Tactic | DraftFormat
  deriving stock Show

data Zone = BattlefieldZone
  deriving stock Show

newtype UnitKey = UnitKey Int
  deriving stock (Show, Eq, Ord)

data RefKind = Target | Source

type role Ref phantom
newtype Ref (k :: RefKind) = UnitRef UnitKey
  deriving stock (Show, Eq, Ord)

class Reference a where
  toRef :: a -> Ref k

data Phase = KingdomPhase | QuestPhase | CapitalPhase | BattlefieldPhase
  deriving stock (Show, Eq)

mconcat
  [ deriveToJSON defaultOptions ''Ref
  , deriveToJSON defaultOptions ''UnitKey
  , deriveToJSON defaultOptions ''PlayerKey
  , deriveToJSON defaultOptions ''Number
  , deriveToJSON defaultOptions ''CardKind
  ]

instance ToJSONKey (Ref k)

