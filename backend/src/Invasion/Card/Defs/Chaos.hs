{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card.Defs.Chaos (module Invasion.Card.Defs.Chaos) where

import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

servantsOfKhorne :: CardDef Unit
servantsOfKhorne = unitCard "core-081" "Servants of Khorne" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield only."
  keyword BattlefieldOnly

savageMarauders :: CardDef Unit
savageMarauders = unitCard "core-082" "Savage Marauders" do
  race Chaos
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

valkiaTheBloody :: CardDef Unit
valkiaTheBloody = unitCard "core-087" "Valkia the Bloody" do
  unique
  race Chaos
  cost 4
  loyalty 3
  power 2
  hitPoints 4
  traits [Hero, Daemon]
  body
    "Limit one Hero per zone.\n\
    \Quest. Action: Spend 2 resources to move any number of damage on this unit to a target corrupted unit."
  quest $ actionWith "Sluice damage" 2 [] \u -> do
    let self = u.self
    when (isDamaged self) $
      withTarget self.controller corruptedEnemy \k ->
        moveAllDamage self.key k
  where
    corruptedEnemy =
      UnitMatching \pk _ u -> u.controller /= pk && u.corrupted

bloodthirster :: CardDef Unit
bloodthirster = unitCard "core-092" "Bloodthirster" do
  unique
  race Chaos
  cost 8
  loyalty 5
  power 5
  hitPoints 8
  trait Daemon
  keyword DamageCannotBeCancelled
  -- FAQ 2.2 General: "After your turn begins" → "At the beginning of
  -- your turn". The trigger fires in Phase 0 (Beginning of the Turn).
  body
    "Damage cannot be cancelled.\n\
    \Forced: At the beginning of your turn, each player must sacrifice a unit in this corresponding zone."
  onMyTurnBegin \_owner self ->
    eachPlayer \pk ->
      mustSacrificeInZone pk self.zone
        "Sacrifice one of your units in this zone."

bloodForTheBloodGod :: CardDef Tactic
bloodForTheBloodGod = tacticCard "core-103" "Blood for the Blood God" do
  race Chaos
  cost 2
  loyalty 2
  body
    "Action: Choose a target unit in any battlefield. Deal damage to that unit equal to its power."
  playableWhen $ hasTarget unitInBattlefield
  whenResolved \self ->
    withTarget self.controller unitInBattlefield \k ->
      withUnit k \u -> dealDamage u.key u.cardDef.power
  where
    unitInBattlefield = unitWhere \u -> u.zone == BattlefieldZone

bloodletter :: CardDef Unit
bloodletter = unitCard "legends-031" "Bloodletter" do
  race Chaos
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  trait Daemon
  body "Double all damage assigned to units as it is being assigned."
  damageMultiplier 2

bloodsworn :: CardDef Unit
bloodsworn = unitCard "path-of-the-zealot-031" "Bloodsworn" do
  race Chaos
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  trait Warrior
  body "Forced: When an opponent's unit enters a discard pile from play, heal all damage on Bloodsworn."
  -- Heal all damage. A large constant beats threading the current HP
  -- into the message; 'HealUnit' clamps to 0.
  onOpponentUnitLeavePlay \_owner self _uk _zone _code ->
    healUnit self.key 999

bloodcrusher :: CardDef Unit
bloodcrusher = unitCard "cataclysm-034" "Bloodcrusher" do
  race Chaos
  cost 5
  loyalty 3
  power 3
  hitPoints 5
  trait Daemon
  body "Lower the cost to play this unit by 1 for each burning zone."
  selfCostAdjust \g _pk -> negate (burningZoneCount g)

swornOfKhorne :: CardDef Unit
swornOfKhorne = unitCard "fragments-of-power-031" "Sworn of Khorne" do
  race Chaos
  cost 2
  loyalty 1
  power 3
  hitPoints 1
  trait Warrior
  keyword BattlefieldOnly
  body "Battlefield only. This unit cannot attack unless the defending zone has at least 1 corrupted unit."
  canAttack \g defender zone _u ->
    any
      ( \v ->
          v.controller == defender
            && v.zone == zone
            && v.corrupted
      )
      g.units

viciousMarauder :: CardDef Unit
viciousMarauder = unitCard "the-fourth-waystone-091" "Vicious Marauder" do
  race Chaos
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  trait Warrior
  keyword BattlefieldOnly
  body "Battlefield only. This unit must attack during your battlefield phase, if able."
  -- Approximation: when the battlefield action window opens for the
  -- controller's turn, auto-initiate an attack on the opposing
  -- battlefield with every friendly unit in that zone (the marauder
  -- itself plus any others) — strictly, the rules force only the
  -- marauder, but auto-batching with others keeps the line of play
  -- coherent until action prompts exist.
  onActionWindow BattlefieldActionWindow \_owner self -> do
    g <- getGame
    when (g.currentPlayer == self.controller && self.zone == BattlefieldZone) do
      let attackers =
            [ u.key
            | u <- g.units
            , u.controller == self.controller
            , u.zone == BattlefieldZone
            , not u.corrupted
            ]
      unless (null attackers) $
        push $ BeginCombat self.controller BattlefieldZone attackers

warhounds :: CardDef Unit
warhounds = unitCard "legends-032" "Warhounds" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Creature
  body
    "Action: When this unit enters play, reveal a {chaos} legend or unit from your hand. \
    \If you do, deal 2 damage to target unit in any corresponding zone."
  onEnterPlay \_owner self ->
    revealFromHand self.controller chaosLegendOrUnit
      "Reveal a Chaos legend or unit to deal 2 damage." \_revealed ->
        withTarget self.controller (unitWhere \u -> u.zone == self.zone) \k ->
          dealDamage k 2
  where
    chaosLegendOrUnit c = case c.def of
      UnitCardDef cd -> Chaos `elem` cd.races
      LegendCardDef cd -> Chaos `elem` cd.races
      _ -> False

wolvesOfTheNorth :: CardDef Quest
wolvesOfTheNorth = questCard "path-of-the-zealot-032" "Wolves of the North" do
  race Chaos
  cost 0
  loyalty 2
  trait QuestTrait
  body
    "Action: During your quest phase, the unit questing on this card can initiate a single attack against a single zone controlled by an opponent."
  actionEnemyZone "Out-of-phase attack" 0 \u (_, z) ->
    withQuest u.self.key \q ->
      for_ q.questingUnit \attackerKey ->
        push $ BeginCombat u.user z [attackerKey]

doombull :: CardDef Unit
doombull = unitCard "the-chaos-moon-032" "Doombull" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Action: When this unit leaves play, deal 4 damage to target unit in any corresponding zone."
  onSelfDestroyed \_owner self ->
    withTarget self.controller (unitWhere \u -> u.zone == self.zone) \k ->
      dealDamage k 4

skulltaker :: CardDef Unit
skulltaker = unitCard "faith-and-steel-113" "Skulltaker" do
  unique
  race Chaos
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Daemon
  body
    "This unit gains {power} for each experience attached to it. \
    \Action: When an opponent's unit leaves play, spend 1 resource to attach it facedown to this unit as an experience."
  selfPower \_g u -> length u.experiences
  onOpponentUnitLeavePlay \_owner self _uk _zone code ->
    mayPay self.controller 1
      "Spend 1 resource to attach the departing unit as an experience on Skulltaker?" $
        attachExperience self.key code

lordOfKhorne :: CardDef Unit
lordOfKhorne = unitCard "cataclysm-033" "Lord of Khorne" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Warrior
  body "This unit deals +1 damage in combat for each burning zone."
  combatPower \g _u -> burningZoneCount g

berserkFury :: CardDef Tactic
berserkFury = tacticCard "the-warpstone-chronicles-094" "Berserk Fury" do
  race Chaos
  cost 2
  loyalty 3
  body
    "Action: One target Unit gains 3 Power until the end of the turn. At the end of the turn, that unit takes 2 damage."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      until EndOfTurn $ buffPower k 3
      queueEoTDamage k 2

daemonsword :: CardDef Support
daemonsword = supportCard "the-warpstone-chronicles-095" "Daemonsword" do
  race Chaos
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {chaos} unit. Corrupt that unit. \
    \Attached unit gains 3 Power and gets +2 Hit Points."
  attachmentPower 3
  attachmentHp 2
  onEnterPlay \_owner self -> for_ self.attachedTo corrupt

brandedByKhorne :: CardDef Support
brandedByKhorne = supportCard "the-eclipse-of-hope-093" "Branded by Khorne" do
  race Chaos
  cost 0
  loyalty 2
  trait Attachment
  body "Attach to a target unit. If attached unit is damaged, destroy that unit."
  onHostDamaged \_owner _self hostKey _n -> destroyUnit hostKey

markOfChaos :: CardDef Support
markOfChaos = supportCard "omens-of-ruin-013" "Mark of Chaos" do
  race Chaos
  cost 1
  loyalty 2
  traits [Attachment, Spell]
  body
    "Attach to a target unit. Attached unit gains {power}{power}. \
    \Forced: At the beginning of your turn, attached unit takes 1 uncancellable damage."
  attachmentPower 2
  -- The +2 power half waits on dynamic modifiers; for now wire the
  -- turn-start damage tick on the host's controller's turn.
  onAttachedHostTurnBegin \_owner _self host ->
    dealUncancellableDamage host.key 1

northernWastes :: CardDef Support
northernWastes = supportCard "the-ruinous-hordes-083" "Northern Wastes" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  trait Wasteland
  body "If you control a faceup non-{chaos} unit or support card, sacrifice this card."
  -- Self-check on every message tick: if controller has any faceup
  -- non-Chaos unit or support, sacrifice. (Attachments inherit their
  -- host's faceup status; we don't track facedown explicitly so every
  -- in-play card is treated as faceup for this check.)
  onAnyMessage \_owner self -> do
    g <- getGame
    let nonChaosUnit u =
          u.controller == self.controller
            && Chaos `notElem` u.cardDef.races
        nonChaosSupport s =
          s.key /= self.key
            && s.controller == self.controller
            && Chaos `notElem` s.cardDef.races
    when
      ( any nonChaosUnit g.units
          || any nonChaosSupport g.supports
      )
      (destroySupport self.key)

ironThroneroom :: CardDef Support
ironThroneroom = supportCard "the-inevitable-city-013" "Iron Throneroom" do
  unique
  race Chaos
  cost 3
  loyalty 5
  power 2
  trait CapitalCenter
  body
    "This card enters play with 4 resource tokens on it. \
    \Action: At the beginning of your turn, remove a resource token from this card. \
    \Then, if there are no resource tokens on this card, put up to 3 {chaos} units into play from your hand or discard pile."
  onEnterPlay \_owner self -> adjustSupportTokens self.key 4
  onMyTurnBegin \_owner self -> when (self.tokens > 0) do
    adjustSupportTokens self.key (-1)
    when (self.tokens == 1) do
      let pk = self.controller
      me <- playerOf pk <$> getGame
      let isChaosUnit c = case c.def of
            UnitCardDef cd -> Chaos `elem` cd.races
            _ -> False
          handChaos = filter isChaosUnit me.hand
          discardChaos = filter isChaosUnit me.discard
          candidates = handChaos <> discardChaos
          inHandKeys = map (.key) handChaos
      chooseFromCards pk 0 3 candidates
        "Choose up to 3 Chaos units from your hand or discard pile to put into play." \chosen ->
          for_ chosen \c ->
            putUnitIntoPlay pk
              (if c.key `elem` inHandKeys then FromHand else FromDiscard)
              c.key KingdomZone

raidingCamps :: CardDef Quest
raidingCamps = questCard "the-inevitable-city-020" "Raiding Camps" do
  race Chaos
  cost 0
  loyalty 3
  body
    "Quest. Action: When this card enters play, draw a card. \
    \Quest. Action: When you play a {chaos} non-Attachment support card from your hand, \
    \destroy target support card in a zone with no units if a unit is questing here."
  -- "When this card enters play, draw a card." The second ability
  -- needs unit-questing-here tracking and is parked.
  onEnterPlay \_owner self ->
    drawCard self.controller

riftOfBattle :: CardDef Support
riftOfBattle = supportCard "the-accursed-dead-052" "Rift of Battle" do
  race Chaos
  cost 1
  loyalty 2
  trait Rift
  body "Units in all corresponding zones deal +1 damage in combat."
  supportCombat \_g _s _u -> 1

riftOfChaos :: CardDef Support
riftOfChaos = supportCard "cataclysm-037" "Rift of Chaos" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  trait Rift
  body "This card gains {power} for each burning zone."
  zonePowerAura \g s zone ->
    if s.zone == zone then burningZoneCount g else 0

recklessAttack :: CardDef Tactic
recklessAttack = tacticCard "days-of-blood-018" "Reckless Attack" do
  race Chaos
  cost 1
  loyalty 2
  keyword Limited
  body
    "Limited. Action: When your opponent declares at least 1 defender against your attack, \
    \put target unit in your discard pile into play in your battlefield declared as an attacker. \
    \At the end of the phase, sacrifice all units that attacked this phase."
  playableWhen \g pk ->
    isMyAttackWithDefenders g pk
      && any isUnitCard (playerOf pk g).discard
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    chooseFromCards pk 1 1 (filter isUnitCard me.discard)
      "Choose a unit in your discard pile to put into your battlefield." \chosen ->
        for_ chosen \c -> do
          putUnitIntoPlay pk FromDiscard c.key BattlefieldZone
          push ScheduleAttackerSacrifice
  where
    isUnitCard c = case c.def of
      UnitCardDef _ -> True
      _ -> False
    isMyAttackWithDefenders g pk = case g.combat of
      Just cs -> cs.attackingPlayer == pk && not (null cs.defenders)
      _ -> False

dominionOfChaos :: CardDef Quest
dominionOfChaos = questCard "the-ruinous-hordes-082" "Dominion of Chaos" do
  race Chaos
  cost 0
  loyalty 3
  trait Mission
  keyword PlayInOpponentArea
  body
    "Play in any opponent's zone under your control. \
    \When you assign combat damage to this zone, you may place any number of that combat damage on this quest instead. \
    \Forced: When the 3rd damage token is placed here, sacrifice this quest to corrupt up to 3 target units."
  onMyQuestTokensAdjusted \_owner self _delta ->
    withQuest self.key \q -> when (q.tokens >= 3) $
      withUpTo self.controller 3 (unitWhere (not . (.corrupted))) \chosen -> do
        traverse_ corrupt chosen
        destroyQuest self.key

tzeentchSorcerer :: CardDef Unit
tzeentchSorcerer = unitCard "core-083" "Tzeentch Sorcerer" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  traits [Sorcerer]
  body "Action: Spend 2 resources to deal 1 damage to a target unit and draw a card."

slaaneshiMarauders :: CardDef Unit
slaaneshiMarauders = unitCard "core-084" "Slaaneshi Marauders" do
  race Chaos
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  traits [Warrior]
  body "Battlefield only."
  keyword BattlefieldOnly

plaguebearersOfNurgle :: CardDef Unit
plaguebearersOfNurgle = unitCard "core-085" "Plaguebearers of Nurgle" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 4
  trait Daemon
  body "Forced: When this unit damages an enemy unit in combat, corrupt that unit."
  corruptsOnDamage

festeringNurglings :: CardDef Unit
festeringNurglings = unitCard "core-086" "Festering Nurglings" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  traits [Creature]
  body "Forced: When this unit leaves play, corrupt target enemy unit."
  onSelfDestroyed \_owner self ->
    withTarget self.controller enemyUnit corrupt

archaonTheEverchosen :: CardDef Unit
archaonTheEverchosen = unitCard "core-088" "Archaon the Everchosen" do
  unique
  race Chaos
  cost 7
  loyalty 5
  power 4
  hitPoints 6
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit attacks, corrupt one defending unit."
  onMyAttackDeclared \_owner self _zone _attackers ->
    withTarget self.controller defendingUnit corrupt

chaosKnights :: CardDef Unit
chaosKnights = unitCard "core-089" "Chaos Knights" do
  race Chaos
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Cavalry, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

chaosWarriors :: CardDef Unit
chaosWarriors = unitCard "core-090" "Chaos Warriors" do
  race Chaos
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

maraudersOfTheNorth :: CardDef Unit
maraudersOfTheNorth = unitCard "core-091" "Marauders of the North" do
  race Chaos
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

chaosSorcerer :: CardDef Unit
chaosSorcerer = unitCard "core-093" "Chaos Sorcerer" do
  race Chaos
  cost 4
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Quest. Action: Spend 2 resources to corrupt target enemy unit."
  actionEnemyUnit "Corrupt" 2 \_ -> corrupt

horrorOfTzeentch :: CardDef Unit
horrorOfTzeentch = unitCard "core-094" "Horror of Tzeentch" do
  race Chaos
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Daemon]
  body "Forced: When this unit enters play, you may discard a card to deal 2 damage to a target unit."
  onEnterPlay \owner self -> when (not (null owner.hand)) do
    let pk = self.controller
    may pk "Discard a card to deal 2 damage to a target unit?" $
      withTarget pk AnyUnit \k -> do
        discardRandom pk
        dealDamage k 2

daemonettesOfSlaanesh :: CardDef Unit
daemonettesOfSlaanesh = unitCard "core-095" "Daemonettes of Slaanesh" do
  race Chaos
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Daemon]
  body "Battlefield. This unit cannot be assigned more than 1 damage per turn."
  perTurnDamageCap 1

beastsOfNurgle :: CardDef Unit
beastsOfNurgle = unitCard "core-096" "Beasts of Nurgle" do
  race Chaos
  cost 4
  loyalty 2
  power 1
  hitPoints 5
  traits [Creature, Daemon]
  body "Forced: When this unit damages an enemy unit, corrupt that unit."
  corruptsOnDamage

chaosSpawn :: CardDef Unit
chaosSpawn = unitCard "core-097" "Chaos Spawn" do
  race Chaos
  cost 2
  loyalty 1
  power 2
  hitPoints 3
  trait Creature
  body "Forced: At the end of your turn, deal 1 damage to this unit."
  onMyTurnEnd \_owner self -> dealDamage self.key 1

eyeOfTzeentch :: CardDef Support
eyeOfTzeentch = supportCard "core-098" "Eye of Tzeentch" do
  race Chaos
  cost 2
  loyalty 2
  traits [Attachment, Spell]
  body "Attach to a target Chaos unit. Attached unit gains {power}; you may draw a card whenever it attacks."
  attachmentPower 1

theIronTower :: CardDef Support
theIronTower = supportCard "core-099" "The Iron Tower" do
  unique
  race Chaos
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Battlefield. Your Chaos units gain {power} while in this zone."
  supportAura \_g s u ->
    if s.controller == u.controller
       && s.zone == BattlefieldZone
       && u.zone == BattlefieldZone
       && Chaos `elem` u.cardDef.races
       then 1
       else 0

pyreOfTcharzanek :: CardDef Support
pyreOfTcharzanek = supportCard "core-100" "Pyre of Tchar'zanek" do
  race Chaos
  cost 2
  loyalty 2
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, deal 1 damage to a target zone."
  kingdom $ forced \self ->
    onTurnBegin self.controller $
      withTarget self.controller AnyCapital \(owner, z) ->
        dealZoneDamage owner z 1

tidesOfChaos :: CardDef Tactic
tidesOfChaos = tacticCard "core-101" "Tides of Chaos" do
  race Chaos
  cost 2
  loyalty 2
  body "Action: Corrupt target unit."
  playableWhen $ hasTarget uncorruptedUnit
  whenResolved \self ->
    withTarget self.controller uncorruptedUnit corrupt
  where
    uncorruptedUnit = unitWhere (not . (.corrupted))

doomOfTheEmpire :: CardDef Tactic
doomOfTheEmpire = tacticCard "core-102" "Doom of the Empire" do
  race Chaos
  cost 3
  loyalty 3
  body "Action: Deal 2 damage to target zone."
  playableWhen $ hasTarget enemyCapital
  whenResolved \self ->
    withTarget self.controller enemyCapital \(owner, z) ->
      dealZoneDamage owner z 2
