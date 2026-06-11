{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Chaos core cards (core-081..105). Heavy on corruption interactions
-- so the engine's existing 'CorruptUnit' / 'CleanseUnit' messages do a
-- lot of work here.
module Invasion.Card.Defs.Chaos (module Invasion.Card.Defs.Chaos) where

import Data.Text qualified as T
import Invasion.Capital
import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
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
  battlefieldOnly

savageMarauders :: CardDef Unit
savageMarauders = unitCard "core-082" "Savage Marauders" do
  race Chaos
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

festeringNurglings :: CardDef Unit
festeringNurglings = unitCard "core-083" "Festering Nurglings" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Daemon
  body "Forced: After this unit enters play, corrupt one target unit in any player's corresponding zone."
  onEnterPlay \_owner self ->
    withTarget self.controller
      (UnitMatching \_ _ u -> u.zone == self.zone)
      \k -> push (CorruptUnit k)

nurgleSorcerer :: CardDef Unit
nurgleSorcerer = unitCard "core-084" "Nurgle Sorcerer" do
  race Chaos
  cost 3
  loyalty 3
  power 1
  hitPoints 2
  trait Sorceror
  body "Quest. Action: Spend 3 resources to deal 1 damage to one target unit. Deal an additional damage to the target if that unit is corrupted."
  quest $ action "Plague them" 3 \usage ->
    withTarget usage.user AnyUnit \k ->
      withUnit k \u ->
        dealDamage k (if u.corrupted then 2 else 1)

chaosKnights :: CardDef Unit
chaosKnights = unitCard "core-085" "Chaos Knights" do
  race Chaos
  cost 5
  loyalty 2
  power 3
  hitPoints 4
  traits [Knight, Cavalry]

cultistOfSlaanesh :: CardDef Unit
cultistOfSlaanesh = unitCard "core-086" "Cultist of Slaanesh" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Zealot
  body "Quest. Action: Spend 4 resources to destroy one target development."
  quest $ action "Defile" 4 \usage ->
    withTarget usage.user AnyDevelopmentZone \(owner, zk) ->
      destroyDevelopment owner zk

valkiaTheBloody :: CardDef Unit
valkiaTheBloody = unitCard "core-087" "Valkia the Bloody" do
  hero
  trait Daemon
  race Chaos
  cost 4
  loyalty 3
  power 2
  hitPoints 4
  body "Limit one Hero per zone. Quest. Action: Spend 2 resources to move any number of damage on this unit to a target corrupted unit."
  quest $ action "Spite" 2 \usage ->
    withTarget usage.user
      (UnitMatching \_ _ u -> u.corrupted)
      \dst -> moveAllDamage usage.self.key dst

melekhTheChanger :: CardDef Unit
melekhTheChanger = unitCard "core-088" "Melekh the Changer" do
  hero
  trait Mage
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  body "Limit one Hero per zone. This unit gains {power} for each corrupted card controlled by an opponent."
  effects \self owner -> do
    let opp = owner.key.next
    units <- (.units) <$> getGame
    let n = length [u | u <- units, u.controller == opp, u.corrupted]
    when (n > 0) (gainPower self n)

fledglingChaosSpawn :: CardDef Unit
fledglingChaosSpawn = unitCard "core-089" "Fledgling Chaos Spawn" do
  race Chaos
  cost 0
  loyalty 1
  power 0
  hitPoints 1
  trait Daemon
  body "Battlefield. Forced: After this unit is destroyed, deal 1 damage to one target unit in any player's battlefield."
  onSelfDestroyed \_owner self ->
    withTarget self.controller
      (UnitMatching \_ _ u -> u.zone == BattlefieldZone)
      \k -> dealDamage k 1

savageGors :: CardDef Unit
savageGors = unitCard "core-090" "Savage Gors" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield. This unit deals 2 additional damage while attacking if you have 2 or more developments in your battlefield."
  effects \self owner ->
    let devs = case owner.capital.battlefield.developments of
          Developments n -> n
     in when (self.zone == BattlefieldZone && self.attacking && devs >= 2) $
          gainPower self 2

darkZealot :: CardDef Unit
darkZealot = unitCard "core-091" "Dark Zealot" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Zealot

bloodthirster :: CardDef Unit
bloodthirster = unitCard "core-092" "Bloodthirster" do
  race Chaos
  cost 8
  loyalty 5
  power 5
  hitPoints 8
  trait Daemon
  keyword DamageCannotBeCancelled
  body "Damage cannot be cancelled. Forced: After your turn begins, each player must sacrifice a unit in this corresponding zone."
  onMyTurnBegin \_owner self -> do
    g <- getGame
    -- One sacrifice per player, in the same zone as this unit.
    for_ [self.controller, self.controller.next] \pk ->
      withTarget pk
        (UnitMatching \_ _ u -> u.controller == pk && u.zone == self.zone)
        destroyUnit

cloudOfFlies :: CardDef Support
cloudOfFlies = supportCard "core-093" "Cloud of Flies" do
  race Chaos
  cost 0
  loyalty 1
  traits [Attachment, Spell]
  body "Attach to a target unit you control. At the beginning of your turn, you may deal 1 uncancellable damage to this unit and to one target unit."
  -- "You may": the whole package (damage to the host AND to a target)
  -- is optional, gated behind one yes/no.
  onAttachedHostTurnBegin \_owner self host ->
    may self.controller
      "Cloud of Flies: deal 1 uncancellable damage to the attached unit and one target unit?"
      do
        dealUncancellableDamage host.key 1
        withTarget self.controller AnyUnit \k -> dealUncancellableDamage k 1

horrificMutation :: CardDef Support
horrificMutation = supportCard "core-094" "Horrific Mutation" do
  race Chaos
  cost 1
  loyalty 1
  traits [Attachment, Mutation]
  body "Attach to a target unit you control. While attached unit is attacking, defending units get -1 hit points."
  supportHPAura \g self target -> case self.attachedTo of
    Just hostKey | hostKey `elem` maybe [] (.attackers) g.combat ->
      if target.key `elem` maybe [] (.defenders) g.combat then -1 else 0
    _ -> 0

sadisticMutation :: CardDef Support
sadisticMutation = supportCard "core-095" "Sadistic Mutation" do
  race Chaos
  cost 2
  loyalty 2
  traits [Attachment, Mutation]
  body "Attach to a target unit you control. Forced: After the attached unit deals damage in combat, deal 1 damage to one target unit or capital."
  -- Fires whenever combat resolves with the host on the attacker
  -- side. We don't check "the host actually dealt damage" beyond
  -- being a non-corrupted attacker — covers the common case.
  onReceive $ Receive \msg _owner self -> case msg of
    ResolveCombat -> case self.attachedTo of
      Just hostKey -> do
        g <- getGame
        case g.combat of
          Just cs | hostKey `elem` cs.attackers ->
            withTarget self.controller (AnyUnit `Or` AnyCapital) \case
              TargetUnitOption u -> dealDamage u 1
              TargetZoneOption owner z -> dealZoneDamage owner z 1
          _ -> pure ()
      Nothing -> pure ()
    _ -> pure ()

warpstoneMeteor :: CardDef Support
warpstoneMeteor = supportCard "core-096" "Warpstone Meteor" do
  race Chaos
  cost 3
  loyalty 2
  power 2
  trait Warpstone
  body "Forced: After your turn begins, each player must corrupt one of his units in this corresponding zone or deal 1 damage to his capital. (Players decide where their own damage is assigned.)"
  -- Each player picks: corrupt one of their own units in this zone
  -- or take 1 indirect damage. With no eligible unit, the damage
  -- is mandatory.
  onMyTurnBegin \_owner self -> do
    g <- getGame
    for_ [self.controller, self.controller.next] \pk -> do
      let candidates =
            [ u.key
            | u <- g.units
            , u.controller == pk
            , u.zone == self.zone
            , not u.corrupted
            ]
      case candidates of
        [] -> indirectDamage pk 1
        _ -> do
          corruptIt <- askYesNo pk "Corrupt one of your units in this zone instead of taking 1 capital damage?"
          if corruptIt
            then withTarget pk
              (UnitMatching \_ _ u ->
                u.controller == pk && u.zone == self.zone && not u.corrupted)
              \k -> push (CorruptUnit k)
            else indirectDamage pk 1

journeyToTheGate :: CardDef Quest
journeyToTheGate = questCard "core-097" "Journey to the Gate" do
  race Chaos
  cost 2
  loyalty 2
  body "Quest. Action: Sacrifice the unit on this quest to force each opponent to discard his hand. Use this ability on your turn, and only if Journey to the Gate has 3 or more resource tokens on it. Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  action "Journey" 0 \usage ->
    withQuest usage.self.key \q ->
      when (q.tokens >= 3) $
        for_ q.questingUnit \quester -> do
          destroyUnit quester
          discardHand usage.user.next

shrineToNurgle :: CardDef Support
shrineToNurgle = supportCard "core-098" "Shrine to Nurgle" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Forced: After an opponent's unit is damaged during combat, corrupt that unit."
  onReceive $ Receive \msg _owner self -> case msg of
    DealDamageToUnit uk n
      | n > 0, self.zone == KingdomZone -> do
          g <- getGame
          when (isJust g.combat) $
            case findUnit uk g of
              Just u | u.controller /= self.controller ->
                push (CorruptUnit uk)
              _ -> pure ()
    _ -> pure ()

seducedByDarkness :: CardDef Tactic
seducedByDarkness = tacticCard "core-099" "Seduced by Darkness" do
  race Chaos
  cost 0
  loyalty 1
  body "Action: Corrupt one target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> push (CorruptUnit k)

willOfTzeentch :: CardDef Tactic
willOfTzeentch = tacticCard "core-100" "Will of Tzeentch" do
  race Chaos
  cost 3
  loyalty 3
  body "Play during your turn. Action: Each player discards his hand and draws three cards."
  whenResolved \self -> do
    eachPlayer \pk -> do
      discardHand pk
      drawCards pk 3
    -- Silence unused-self warning.
    let _ = self
    pure ()

nurglesPestilence :: CardDef Tactic
nurglesPestilence = tacticCard "core-101" "Nurgle's Pestilence" do
  race Chaos
  cost 3
  loyalty 2
  trait Spell
  body "Action: Each unit in play takes 1 damage. Corrupted units take an additional damage."
  whenResolved \_ -> do
    g <- getGame
    for_ g.units \u -> dealDamage u.key (if u.corrupted then 2 else 1)

flamesOfTzeentch :: CardDef Tactic
flamesOfTzeentch = tacticCard "core-102" "Flames of Tzeentch" do
  race Chaos
  costVariable
  loyalty 3
  trait Spell
  body "Play during your turn. Action: Deal X damage to one target unit."
  whenResolved \self ->
    when (self.xValue > 0) $
      withTarget self.controller AnyUnit \k -> dealDamage k self.xValue

bloodForTheBloodGod :: CardDef Tactic
bloodForTheBloodGod = tacticCard "core-103" "Blood for the Blood God" do
  race Chaos
  cost 2
  loyalty 2
  body "Action: Choose a target unit in any battlefield. Deal damage to that unit equal to its power."
  whenResolved \self ->
    withTarget self.controller
      (UnitMatching \_ _ u -> u.zone == BattlefieldZone)
      \k ->
        withUnit k \u -> dealDamage k u.effectivePower

cullingTheWeak :: CardDef Tactic
cullingTheWeak = tacticCard "core-104" "Culling the Weak" do
  race Chaos
  cost 2
  loyalty 1
  body "Action: Sacrifice a unit to have all units in your battlefield gain {power} until the end of the turn."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk ownUnit \k -> do
      destroyUnit k
      g <- getGame
      let mine =
            [ u | u <- g.units
                , u.controller == pk
                , u.zone == BattlefieldZone
            ]
      for_ mine \u -> until EndOfTurn $ buffPower u.key 1

slaaneshsDomination :: CardDef Tactic
slaaneshsDomination = tacticCard "core-105" "Slaanesh's Domination" do
  race Chaos
  cost 2
  loyalty 2
  body "Action: Reveal up to three cards at random from one target opponent's hand. You may play any tactics thus revealed as though they were in your hand for no cost."
  -- Engine-side handler does the random shuffle (no 'MonadRandom'
  -- in TriggerM) and the per-tactic opt-in prompt loop. Cards stay
  -- in the opponent's hand; only the effects fire on the
  -- controller's behalf.
  whenResolved \self ->
    push (SlaaneshDominate self.controller self.controller.next 3)
