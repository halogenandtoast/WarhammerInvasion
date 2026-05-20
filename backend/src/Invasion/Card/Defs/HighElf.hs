{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | High Elf core cards (core-051..055). Only 5 cards — the rest of
-- the High Elf range arrives in later sets.
module Invasion.Card.Defs.HighElf (module Invasion.Card.Defs.HighElf) where

import Data.Map.Strict qualified as Map
import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Entity (SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Prelude
import Invasion.Types
import Queue (push)

silverHelmBrigade :: CardDef Unit
silverHelmBrigade = unitCard "core-051" "Silver Helm Brigade" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Elite, Noble, Cavalry]
  body "Forced: After this unit takes 1 or more damage, draw a card."
  onSelfDamaged \_owner self _n -> drawCard self.controller

archmageOfSaphery :: CardDef Unit
archmageOfSaphery = unitCard "core-052" "Archmage of Saphery" do
  race HighElf
  cost 1
  loyalty 1
  power 0
  hitPoints 1
  trait Mage
  body "Quest. Action: During your quest phase, you may heal 1 damage on one target unit. (Limit once per turn.)"
  quest $ action "Mend" 0 \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used do
      until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
      withTarget usage.user AnyUnit \k -> healUnit k 1

blessingOfIsha :: CardDef Support
blessingOfIsha = supportCard "core-053" "Blessing of Isha" do
  race HighElf
  cost 0
  loyalty 1
  traits [Attachment, Spell]
  body "Attach to a target unit. Restore that unit, if able. Attached unit cannot be corrupted."
  -- "Restore" = remove damage + cleanse corruption when the support
  -- enters play. The "cannot be corrupted" rider lasts while the
  -- attachment is in play, modelled via a Permanent modifier on the
  -- host (the modifier is dropped if the host leaves play; we don't
  -- yet auto-clear it when only the attachment leaves — small gap
  -- to revisit when Permanent-modifier lifecycle gets tightened).
  onEnterPlay \_owner self ->
    case self.attachedTo of
      Just host -> do
        healUnit host 999
        push (CleanseUnit host)
        until Permanent $ shieldFromCorruption host
      Nothing -> pure ()

radiantGaze :: CardDef Tactic
radiantGaze = tacticCard "core-054" "Radiant Gaze" do
  race HighElf
  cost 2
  loyalty 3
  trait Spell
  body "Action: Choose an opponent's zone. All units in that zone lose {power} until the end of the turn."
  playableWhen \g _pk -> any (\u -> True) g.units
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk enemyCapital \(_owner, zk) -> do
      g <- getGame
      let targets = filter
            (\u -> u.controller /= pk && u.zone == zk) g.units
      for_ targets \u ->
        until EndOfTurn $ buffPower u.key (-1)

greaterHeal :: CardDef Tactic
greaterHeal = tacticCard "core-055" "Greater Heal" do
  race HighElf
  cost 3
  loyalty 1
  trait Spell
  body "Action: Heal all damage on your units."
  whenResolved \self -> do
    g <- getGame
    let mine = filter (\u -> u.controller == self.controller) g.units
    for_ mine \u -> healUnit u.key 999
