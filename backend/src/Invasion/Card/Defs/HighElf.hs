{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card.Defs.HighElf (module Invasion.Card.Defs.HighElf) where

import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.CardDef
import Invasion.Entity (SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Modifier
import Invasion.Prelude
import Invasion.Types

phoenixGuard :: CardDef Unit
phoenixGuard = unitCard "core-051" "Phoenix Guard" do
  race HighElf
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield. Toughness 1."
  toughness 1

whiteLionsOfChrace :: CardDef Unit
whiteLionsOfChrace = unitCard "core-052" "White Lions of Chrace" do
  race HighElf
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

swordmastersOfHoeth :: CardDef Unit
swordmastersOfHoeth = unitCard "core-053" "Swordmasters of Hoeth" do
  race HighElf
  cost 5
  loyalty 3
  power 4
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

highElfArchers :: CardDef Unit
highElfArchers = unitCard "core-054" "High Elf Archers" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to target unit."
  actionEnemyUnit "Loose arrow" 1 \_ k -> dealDamage k 1

seaGuardOfLothern :: CardDef Unit
seaGuardOfLothern = unitCard "core-055" "Sea Guard of Lothern" do
  race HighElf
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  trait Warrior

silverHelms :: CardDef Unit
silverHelms = unitCard "core-056" "Silver Helms" do
  race HighElf
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Warrior, Cavalry]

dragonPrincesOfCaledor :: CardDef Unit
dragonPrincesOfCaledor = unitCard "core-057" "Dragon Princes of Caledor" do
  race HighElf
  cost 5
  loyalty 3
  power 3
  hitPoints 3
  traits [Warrior, Cavalry, Elite]
  body "Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

princeTyrion :: CardDef Unit
princeTyrion = unitCard "core-058" "Prince Tyrion" do
  unique
  race HighElf
  cost 6
  loyalty 5
  power 4
  hitPoints 5
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

teclis :: CardDef Unit
teclis = unitCard "core-059" "Teclis" do
  unique
  race HighElf
  cost 6
  loyalty 5
  power 2
  hitPoints 4
  traits [Hero, Sorcerer]
  -- FAQ 2.2: "After" → "When".
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit enters play, draw 2 cards."
  onEnterPlay \_owner self -> drawCards self.controller 2

eltharionTheGrim :: CardDef Unit
eltharionTheGrim = unitCard "core-060" "Eltharion the Grim" do
  unique
  race HighElf
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When an opponent's unit enters play, deal 1 damage to that unit."
  onOpponentUnitEnterPlay \_owner _self ukey ->
    dealDamage ukey 1

korhil :: CardDef Unit
korhil = unitCard "core-061" "Korhil" do
  unique
  race HighElf
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. This unit gains {power} for each other White Lion unit you control."
  selfPower \g u ->
    length
      [ ()
      | v <- g.units
      , v.controller == u.controller
      , v.key /= u.key
      , v.cardDef.code == CardCode "core-052"
      ]

loremasterOfHoeth :: CardDef Unit
loremasterOfHoeth = unitCard "core-062" "Loremaster of Hoeth" do
  race HighElf
  cost 4
  loyalty 2
  power 1
  hitPoints 3
  traits [Sorcerer]
  body "Forced: When this unit enters play, draw a card."
  onEnterPlay \_owner self ->
    drawCard self.controller

mageOfTheWhiteTower :: CardDef Unit
mageOfTheWhiteTower = unitCard "core-063" "Mage of the White Tower" do
  race HighElf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Quest. Action: Spend 2 resources to look at the top 3 cards of your deck."

spearmenOfLothern :: CardDef Unit
spearmenOfLothern = unitCard "core-064" "Spearmen of Lothern" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior

reaverKnights :: CardDef Unit
reaverKnights = unitCard "core-065" "Reaver Knights" do
  race HighElf
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  traits [Warrior, Cavalry]
  body "Scout."
  scout

lighthouseOfLothern :: CardDef Support
lighthouseOfLothern = supportCard "core-066" "The Lighthouse of Lothern" do
  unique
  race HighElf
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Quest. Your quest zone gains +1 power."
  zonePowerAura \_g s zone ->
    if s.zone == QuestZone && zone == QuestZone then 1 else 0

bannerOfAvelorn :: CardDef Support
bannerOfAvelorn = supportCard "core-067" "Banner of Avelorn" do
  race HighElf
  cost 2
  loyalty 2
  trait Attachment
  body "Attach to a target High Elf unit. Attached unit gains {power}{power}."
  attachmentPower 2

bowOfAvelorn :: CardDef Support
bowOfAvelorn = supportCard "core-068" "Bow of Avelorn" do
  race HighElf
  cost 1
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target High Elf unit. Action: Sacrifice this card to deal 2 damage to target unit."
  actionEnemyUnit "Loose arrow (sacrifice)" 0 \u k -> do
    dealDamage k 2
    destroySupport u.self.key

hoethsWisdom :: CardDef Support
hoethsWisdom = supportCard "core-069" "Hoeth's Wisdom" do
  race HighElf
  cost 2
  loyalty 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, draw a card."
  onMyTurnBegin \_owner self ->
    when (self.zone == KingdomZone) $
      drawCard self.controller

theWhiteTower :: CardDef Quest
theWhiteTower = questCard "core-070" "The White Tower" do
  race HighElf
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 resource token here if a unit is questing here.\n\
    \Action: Spend 3 tokens to draw 3 cards."
  forced accrueTokenWhileQuesting
  spendTokens "Draw 3 cards" 3 \u ->
    drawCards u.user 3

voiceOfCommand :: CardDef Tactic
voiceOfCommand = tacticCard "core-071" "Voice of Command" do
  race HighElf
  cost 2
  loyalty 2
  traits [Spell]
  body "Action: Target unit gains {power}{power} until the end of the turn."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k ->
      until EndOfTurn $ buffPower k 2

dragonBreath :: CardDef Tactic
dragonBreath = tacticCard "core-072" "Dragon Breath" do
  race HighElf
  cost 3
  loyalty 3
  traits [Spell]
  body "Action: Deal 2 damage to each enemy unit in target zone."
  playableWhen $ hasTarget enemyCapital
  whenResolved \self ->
    withTarget self.controller enemyCapital \(_, z) ->
      damageEachEnemyInZone self.controller z 2

magicOfTheOldOnes :: CardDef Tactic
magicOfTheOldOnes = tacticCard "core-073" "Magic of the Old Ones" do
  race HighElf
  cost 1
  loyalty 1
  traits [Spell]
  body "Action: Draw 2 cards."
  whenResolved \self -> drawCards self.controller 2

battleMagic :: CardDef Tactic
battleMagic = tacticCard "core-074" "Battle Magic" do
  race HighElf
  cost 2
  loyalty 2
  traits [Spell]
  body "Action: Deal 2 damage to target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> dealDamage k 2

sacredIncantations :: CardDef Tactic
sacredIncantations = tacticCard "core-075" "Sacred Incantations" do
  race HighElf
  cost 1
  loyalty 2
  traits [Spell]
  body "Action: Cancel a target tactic that is being played."
