{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Dark Elf core cards (core-106..110). Only 5 cards — the rest of
-- the Dark Elf range arrives in later sets.
module Invasion.Card.Defs.DarkElf (module Invasion.Card.Defs.DarkElf) where

import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Entity (SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

discipleOfKhaine :: CardDef Unit
discipleOfKhaine = unitCard "core-106" "Disciple of Khaine" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Warrior, Priest]
  body "Action: Spend 2 resources to redirect one combat damage just assigned to this unit to another target unit."
  -- Action is selectable any time it's the controller's priority
  -- (the "just assigned" window isn't expressible without a real
  -- action stack). We guard the redirect on actually having a
  -- cancellable pending-damage assignment on self in the current
  -- combat — outside combat (or when nothing's been assigned to
  -- self) the action is a no-op rather than a free "deal 1 to a
  -- target" exploit.
  action "Redirect blow" 2 \usage -> do
    g <- getGame
    let pendingOnSelf = case g.combat of
          Just cs ->
            sum
              [ pd.cancellable
              | pd <- cs.pendingAssignments
              , PDUnit k <- [pd.target]
              , k == usage.self.key
              ]
          Nothing -> 0
    when (pendingOnSelf > 0) $
      withTarget usage.user AnyUnit \target -> do
        push (CancelAssignedDamageOnUnit usage.self.key 1)
        dealDamage target 1

vileSorceress :: CardDef Unit
vileSorceress = unitCard "core-107" "Vile Sorceress" do
  race DarkElf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Sorceror
  body "Quest. Forced: After your turn begins, one target unit gets -1 hit points until the end of the turn."
  onMyTurnBegin \_owner self ->
    when (self.zone == QuestZone) $
      withTarget self.controller AnyUnit \k ->
        until EndOfTurn $ debuffHP k 1

coldOneRiders :: CardDef Unit
coldOneRiders = unitCard "core-108" "Cold One Riders" do
  race DarkElf
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  traits [Cavalry, Elite]
  counterstrike 1
  body "Counterstrike 1 (this unit deals 1 combat damage immediately after defending)."

cauldronOfBlood :: CardDef Support
cauldronOfBlood = supportCard "core-109" "Cauldron of Blood" do
  race DarkElf
  cost 4
  loyalty 1
  power 2
  trait Siege
  body "Kingdom. Forced: When this zone is attacked, deal 1 damage to one target attacking unit."
  onMyZoneAttacked \_owner self cs ->
    case cs.attackers of
      [] -> pure ()
      _ ->
        withTarget self.controller
          (UnitMatching \_ _ u -> u.key `elem` cs.attackers)
          (`dealDamage` 1)

hate :: CardDef Tactic
hate = tacticCard "core-110" "Hate" do
  race DarkElf
  cost 0
  loyalty 1
  body "Action: Take 1 resource from each opponent and add it to your available resources."
  whenResolved \self -> do
    let pk = self.controller
        opp = pk.next
    -- Spend 1 from opponent (clamps at 0), gain 1 to self per opp.
    push (SpendResources opp 1)
    push (GainResources pk 1)
