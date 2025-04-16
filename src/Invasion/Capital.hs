{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Capital (module Invasion.Capital) where

import Invasion.Prelude

data QuestZone = QuestZone {developments :: Int, hitPoints :: Int, damage :: Int}
  deriving stock Show

data Battlefield = Battlefield {developments :: Int, hitPoints :: Int, damage :: Int}
  deriving stock Show

data Capital = Capital
  { battlefield :: Battlefield
  , quest :: QuestZone
  }
  deriving stock Show

instance HasField "zones" Capital [Section] where
  getField capital = [BattlefieldSection capital.battlefield, QuestSection capital.quest]

data Section where
  BattlefieldSection :: Battlefield -> Section
  QuestSection :: QuestZone -> Section

instance HasField "burning" Section Bool where
  getField (BattlefieldSection bf) = bf.damage >= bf.hitPoints
  getField (QuestSection q) = q.damage >= q.hitPoints

