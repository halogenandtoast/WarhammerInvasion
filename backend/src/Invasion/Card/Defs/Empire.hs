{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card.Defs.Empire (module Invasion.Card.Defs.Empire) where

import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.CardDef
import Invasion.Entity (SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Modifier
import Invasion.Prelude
import Invasion.Types

spearmenOfWissenland :: CardDef Unit
spearmenOfWissenland = unitCard "core-026" "Spearmen of Wissenland" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Battlefield. This unit gains {power} while defending."

stateTroops :: CardDef Unit
stateTroops = unitCard "core-027" "State Troops" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  flavor "The backbone of the Empire's vast standing army."

templarOfSigmar :: CardDef Unit
templarOfSigmar = unitCard "core-028" "Templar of Sigmar" do
  race Empire
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Warrior, Priest]
  body "Battlefield. Your other Warrior units gain {power} while in this zone."
  unitAura \_g self target ->
    if self.zone == BattlefieldZone
       && target.zone == BattlefieldZone
       && self.controller == target.controller
       && target.key /= self.key
       && Warrior `elem` target.cardDef.traits
       then 1
       else 0

witchHunter :: CardDef Unit
witchHunter = unitCard "core-029" "Witch Hunter" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Forced: When this unit enters play, destroy one target corrupted unit."
  onEnterPlay \_owner self ->
    withTarget self.controller (unitWhere (.corrupted)) \k ->
      destroyUnit k

greatswordsOfNuln :: CardDef Unit
greatswordsOfNuln = unitCard "core-030" "Greatswords of Nuln" do
  race Empire
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

knightsPanther :: CardDef Unit
knightsPanther = unitCard "core-031" "Knights Panther" do
  race Empire
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  traits [Warrior, Cavalry]
  body "Battlefield. Action: When this unit attacks, it gains {power}{power} for this attack."

reiksguard :: CardDef Unit
reiksguard = unitCard "core-032" "Reiksguard" do
  race Empire
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Cavalry, Elite]
  body "Battlefield. Toughness 1."
  toughness 1

rieklandMarksmen :: CardDef Unit
rieklandMarksmen = unitCard "core-033" "Riekland Marksmen" do
  race Empire
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to target unit."
  actionEnemyUnit "Shoot" 1 \_ k -> dealDamage k 1

thyrusGorman :: CardDef Unit
thyrusGorman = unitCard "core-034" "Thyrus Gorman" do
  unique
  race Empire
  cost 3
  loyalty 3
  power 2
  hitPoints 2
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Action: Spend 2 resources to deal 2 damage to target unit."
  actionEnemyUnit "Cast" 2 \_ k -> dealDamage k 2

karlFranz :: CardDef Unit
karlFranz = unitCard "core-035" "Karl Franz" do
  unique
  race Empire
  cost 7
  loyalty 5
  power 4
  hitPoints 6
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Your other Empire units gain {power}."
  unitAura \_g self target ->
    if self.controller == target.controller
       && target.key /= self.key
       && Empire `elem` target.cardDef.races
       then 1
       else 0

volkmarTheGrim :: CardDef Unit
volkmarTheGrim = unitCard "core-036" "Volkmar the Grim" do
  unique
  race Empire
  cost 5
  loyalty 3
  power 2
  hitPoints 4
  traits [Hero, Priest]
  body
    "Limit one Hero per zone.\n\
    \Forced: At the beginning of your turn, heal 2 damage from your capital."
  onMyTurnBegin \_owner self -> healCapital self.controller 2

mariusLeitdorf :: CardDef Unit
mariusLeitdorf = unitCard "core-037" "Marius Leitdorf" do
  unique
  race Empire
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  -- FAQ 2.2: "After" → "When".
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit enters play, draw a card."
  onEnterPlay \_owner self ->
    drawCard self.controller

lectorOfSigmar :: CardDef Unit
lectorOfSigmar = unitCard "core-038" "Lector of Sigmar" do
  race Empire
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Priest
  body "Kingdom. While in your kingdom, your capital gains +1 hit points in each zone."

imperialEngineers :: CardDef Unit
imperialEngineers = unitCard "core-039" "Imperial Engineers" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Engineer
  body "Forced: When this unit enters play, draw a card."
  onEnterPlay \_owner self -> drawCard self.controller

pegasusKnights :: CardDef Unit
pegasusKnights = unitCard "core-040" "Pegasus Knights" do
  race Empire
  cost 5
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Cavalry]
  body "Battlefield. This unit can attack the turn it enters play."

theImperialCrown :: CardDef Support
theImperialCrown = supportCard "core-041" "The Imperial Crown" do
  unique
  race Empire
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Kingdom. Your Empire heroes cost 1 less to play."
  globalCostAdjust \_g s playing tgt ->
    if playing == s.controller
       && s.zone == KingdomZone
       && Empire `elem` tgt.cfRaces
       && Hero `elem` tgt.cfTraits
       then -1
       else 0

hammerOfSigmar :: CardDef Support
hammerOfSigmar = supportCard "core-042" "The Hammer of Sigmar" do
  race Empire
  cost 2
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target Empire unit. Attached unit gains {power}{power}; its damage cannot be cancelled."
  attachmentPower 2
  grantsUncancellable

bannerOfSigmar :: CardDef Support
bannerOfSigmar = supportCard "core-043" "Banner of Sigmar" do
  race Empire
  cost 1
  loyalty 2
  trait Attachment
  body "Attach to a target unit. Attached unit gains {power}."
  attachmentPower 1

altdorf :: CardDef Support
altdorf = supportCard "core-044" "Altdorf" do
  unique
  race Empire
  cost 2
  loyalty 2
  power 1
  trait Building
  body "Kingdom. While in play, non-combat damage to your capital is reduced by 1 (minimum 0)."

defendingTheEmpire :: CardDef Quest
defendingTheEmpire = questCard "core-045" "Defending the Empire" do
  race Empire
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 resource token on this card if a unit is questing here.\n\
    \Action: Spend 3 resource tokens from this card to heal all damage on your capital."
  forced accrueTokenWhileQuesting
  spendTokens "Heal your capital" 3 \u ->
    healCapital u.user 99

forSigmar :: CardDef Tactic
forSigmar = tacticCard "core-046" "For Sigmar!" do
  race Empire
  cost 2
  loyalty 1
  body "Action: Each of your units gains {power} until the end of the turn."
  playableWhen $ hasTarget ownUnit
  whenResolved \self -> buffEachUntilEoT self.controller ownUnit 1

sigmarsWrath :: CardDef Tactic
sigmarsWrath = tacticCard "core-047" "Sigmar's Wrath" do
  race Empire
  cost 3
  loyalty 2
  body "Action: Deal 3 damage to target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> dealDamage k 3

counterCharge :: CardDef Tactic
counterCharge = tacticCard "core-048" "Counter-charge" do
  race Empire
  cost 1
  loyalty 2
  body "Play during combat. Action: Target defending unit gains {power}{power} until the end of the turn."
  playableWhen $ hasTarget defendingUnit
  whenResolved \self ->
    withTarget self.controller defendingUnit \k ->
      until EndOfTurn $ buffPower k 2

battleOfTheReik :: CardDef Tactic
battleOfTheReik = tacticCard "core-049" "Battle of the Reik" do
  race Empire
  cost 2
  loyalty 2
  body "Action: Deal 1 damage to each attacking and each defending unit."
  playableWhen inCombat
  whenResolved \_ -> damageEachUnitInCombat 1

defendersOfTheFaith :: CardDef Tactic
defendersOfTheFaith = tacticCard "core-050" "Defenders of the Faith" do
  race Empire
  cost 1
  loyalty 1
  body "Action: Cancel up to 2 damage assigned to a unit you control."
  playableWhen $ hasTarget ownUnit
  whenResolved \self ->
    withTarget self.controller ownUnit \k ->
      cancelDamageOnUnit k 2
