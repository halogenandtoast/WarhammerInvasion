{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card.Defs.DarkElf (module Invasion.Card.Defs.DarkElf) where

import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Entity (SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types

malekith :: CardDef Unit
malekith = unitCard "core-131" "Malekith" do
  unique
  race DarkElf
  cost 7
  loyalty 5
  power 4
  hitPoints 6
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit damages an enemy unit, corrupt that unit."
  -- Approximation: when a combat we participate in resolves, let Malekith's
  -- controller pick one defender to corrupt. Real card requires
  -- per-damage-event source tracking.
  onCombatResolveAsAttacker \_owner self _cs ->
    withTarget self.controller defendingUnit corrupt

morathi :: CardDef Unit
morathi = unitCard "core-132" "Morathi" do
  unique
  race DarkElf
  cost 5
  loyalty 4
  power 2
  hitPoints 4
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Action: Spend 2 resources to corrupt one target unit."
  actionEnemyUnit "Corrupt" 2 \_ -> corrupt

croneHellebron :: CardDef Unit
croneHellebron = unitCard "core-133" "Crone Hellebron" do
  unique
  race DarkElf
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. This unit gains {power} for each Witch Elf unit you control."
  selfPower \g u ->
    length
      [ ()
      | v <- g.units
      , v.controller == u.controller
      , v.cardDef.code == CardCode "core-135"
      ]

lokhirFellheart :: CardDef Unit
lokhirFellheart = unitCard "core-134" "Lokhir Fellheart" do
  unique
  race DarkElf
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit damages an enemy zone, draw a card."
  -- Approximation: any combat we participate in where the attacker
  -- side wins damage to a zone (spillover) → draw. The damage-source
  -- granularity isn't tracked, so just fire whenever this unit is one
  -- of the attackers and combat resolves.
  onCombatResolveAsAttacker \_owner self _cs ->
    drawCard self.controller

witchElves :: CardDef Unit
witchElves = unitCard "core-135" "Witch Elves" do
  race DarkElf
  cost 3
  loyalty 2
  power 3
  hitPoints 1
  trait Warrior
  body "Battlefield only. Damage dealt by this unit cannot be cancelled."
  keyword BattlefieldOnly
  keyword DamageCannotBeCancelled

blackGuardOfNaggarond :: CardDef Unit
blackGuardOfNaggarond = unitCard "core-136" "Black Guard of Naggarond" do
  race DarkElf
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

executioners :: CardDef Unit
executioners = unitCard "core-137" "Executioners" do
  race DarkElf
  cost 4
  loyalty 2
  power 4
  hitPoints 2
  traits [Warrior, Elite]
  body "Battlefield only. Damage dealt by this unit cannot be cancelled."
  keyword BattlefieldOnly
  keyword DamageCannotBeCancelled

corsairs :: CardDef Unit
corsairs = unitCard "core-138" "Corsairs" do
  race DarkElf
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  trait Warrior
  body "Forced: When this unit damages an enemy zone, draw a card."
  -- Same approximation as Lokhir: fire on ResolveCombat when attacking.
  onCombatResolveAsAttacker \_owner self _cs ->
    drawCard self.controller

coldOneKnights :: CardDef Unit
coldOneKnights = unitCard "core-139" "Cold One Knights" do
  race DarkElf
  cost 5
  loyalty 3
  power 3
  hitPoints 3
  traits [Warrior, Cavalry, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

darkRiders :: CardDef Unit
darkRiders = unitCard "core-140" "Dark Riders" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Cavalry]
  body "Scout."
  scout

darkSorceress :: CardDef Unit
darkSorceress = unitCard "core-141" "Dark Sorceress" do
  race DarkElf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Action: Spend 2 resources to deal 2 damage to a target unit."
  actionEnemyUnit "Cast" 2 \_ k -> dealDamage k 2

assassinsOfKhaine :: CardDef Unit
assassinsOfKhaine = unitCard "core-142" "Assassins of Khaine" do
  race DarkElf
  cost 3
  loyalty 2
  power 2
  hitPoints 1
  trait Warrior
  body "Forced: When this unit enters play, destroy one target unit with 1 hit point."
  onEnterPlay \_owner self ->
    withTarget self.controller (unitWhere \u -> u.effectiveMaxHP <= 1) \k ->
      destroyUnit k

repeaterCrossbowmen :: CardDef Unit
repeaterCrossbowmen = unitCard "core-143" "Repeater Crossbowmen" do
  race DarkElf
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to a target unit."
  actionEnemyUnit "Loose bolts" 1 \_ k -> dealDamage k 1

bloodwrackMedusa :: CardDef Unit
bloodwrackMedusa = unitCard "core-144" "Bloodwrack Medusa" do
  race DarkElf
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  trait Creature
  body "Forced: When this unit enters play, deal 1 damage to each enemy unit in target zone."
  onEnterPlay \_owner self ->
    withTarget self.controller AnyCapital \(_, z) ->
      damageEachEnemyInZone self.controller z 1

blackDragon :: CardDef Unit
blackDragon = unitCard "core-145" "Black Dragon" do
  race DarkElf
  cost 6
  loyalty 3
  power 4
  hitPoints 5
  trait Creature
  body "Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

manticore :: CardDef Unit
manticore = unitCard "core-146" "Manticore" do
  race DarkElf
  cost 5
  loyalty 2
  power 4
  hitPoints 3
  trait Creature
  body "Battlefield only."
  keyword BattlefieldOnly

cauldronOfBlood :: CardDef Support
cauldronOfBlood = supportCard "core-147" "Cauldron of Blood" do
  unique
  race DarkElf
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Battlefield. Your Witch Elf units gain {power}."
  supportAura \_g s u ->
    if s.controller == u.controller
       && s.zone == BattlefieldZone
       && u.cardDef.code == CardCode "core-135"
       then 1
       else 0

theBlackArk :: CardDef Support
theBlackArk = supportCard "core-148" "The Black Ark" do
  unique
  race DarkElf
  cost 3
  loyalty 3
  power 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, draw a card."
  onMyTurnBegin \_owner self ->
    when (self.zone == KingdomZone) $
      drawCard self.controller

whipOfAgony :: CardDef Support
whipOfAgony = supportCard "core-149" "Whip of Agony" do
  race DarkElf
  cost 2
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target Dark Elf unit. Attached unit gains {power}{power}."
  attachmentPower 2

druchiiBanner :: CardDef Support
druchiiBanner = supportCard "core-150" "Druchii Banner" do
  race DarkElf
  cost 1
  loyalty 1
  trait Attachment
  body "Attach to a target unit. Attached unit gains {power}; opponents pay 1 additional resource to target it."
  attachmentPower 1

witchbrew :: CardDef Support
witchbrew = supportCard "core-151" "Witchbrew" do
  race DarkElf
  cost 1
  loyalty 1
  trait Attachment
  body "Attach to a target Dark Elf unit. Action: Sacrifice this card to give attached unit {power}{power}{power} until the end of the turn."
  action "Brew (sacrifice)" 0 \u ->
    for_ u.self.attachedTo \hostKey -> do
      until EndOfTurn $ buffPower hostKey 3
      destroySupport u.self.key

slaughterAtLustria :: CardDef Quest
slaughterAtLustria = questCard "core-152" "Slaughter at Lustria" do
  race DarkElf
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 token here if a unit is questing here.\n\
    \Action: Spend 3 tokens to corrupt up to 2 target units."
  forced accrueTokenWhileQuesting
  spendTokens "Corrupt up to 2 units" 3 \u ->
    withUpTo u.user 2 (unitWhere (not . (.corrupted))) (traverse_ corrupt)

khainesEmbrace :: CardDef Tactic
khainesEmbrace = tacticCard "core-153" "Khaine's Embrace" do
  race DarkElf
  cost 2
  loyalty 2
  body "Action: Destroy target unit with 2 or fewer hit points."
  playableWhen $ hasTarget weakUnit
  whenResolved \self ->
    withTarget self.controller weakUnit \k -> destroyUnit k
  where
    weakUnit = unitWhere \u -> u.effectiveMaxHP <= 2

murderousProwess :: CardDef Tactic
murderousProwess = tacticCard "core-154" "Murderous Prowess" do
  race DarkElf
  cost 1
  loyalty 2
  body "Action: Each of your Dark Elf units gains {power} until the end of the turn."
  playableWhen $ hasTarget ownDarkElves
  whenResolved \self -> buffEachUntilEoT self.controller ownDarkElves 1
  where
    ownDarkElves = UnitMatching \pk _ u -> u.controller == pk && u `isRace` DarkElf

coldBloodedSlaughter :: CardDef Tactic
coldBloodedSlaughter = tacticCard "core-155" "Cold Blooded Slaughter" do
  race DarkElf
  cost 3
  loyalty 3
  body "Action: Deal damage equal to your hand size to a target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    withTarget pk AnyUnit \k -> dealDamage k (length me.hand)
