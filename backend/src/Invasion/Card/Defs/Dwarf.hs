{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Dwarf core cards (core-001..025). Static data matches @cards.json@
-- and every printed ability has a functional implementation against
-- the engine's effect / trigger / modifier primitives.
module Invasion.Card.Defs.Dwarf (module Invasion.Card.Defs.Dwarf) where

import Data.Map.Strict qualified as Map
import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Capital
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

defenderOfTheHold :: CardDef Unit
defenderOfTheHold = unitCard "core-001" "Defender of the Hold" do
  race Dwarf
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield only."
  battlefieldOnly

zhufbarEngineers :: CardDef Unit
zhufbarEngineers = unitCard "core-002" "Zhufbar Engineers" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Engineer
  body "Forced: After this unit leaves play, each opponent must sacrifice a unit in this corresponding zone."
  onSelfLeavesPlay \_owner self ->
    -- Forced: the opponent MUST sacrifice (their choice of which
    -- unit); a declinable target prompt would let them skip it.
    mustSacrificeInZone self.controller.next self.zone
      "Zhufbar Engineers: sacrifice a unit in this zone."

hammererOfKarakAzul :: CardDef Unit
hammererOfKarakAzul = unitCard "core-003" "Hammerer of Karak Azul" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Elite]
  toughness 1
  body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."

trollSlayers :: CardDef Unit
trollSlayers = unitCard "core-004" "Troll Slayers" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Slayer
  body "Battlefield. This unit gains {power}{power} while you have at least two developments in this zone."
  battlefield $ constant \self ->
    withZoneOf self \z ->
      when (z.developments >= 2) $ gainPower self 2

runesmith :: CardDef Unit
runesmith = unitCard "core-005" "Runesmith" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Priest
  body "Quest. Action: Spend 2 resources to have a target unit gain {power} until the end of the turn."
  quest $ action "Buff a unit" 2 \usage ->
    withTarget usage.user AnyUnit \t -> until EndOfTurn $ buffPower t 1

durgnarTheBold :: CardDef Unit
durgnarTheBold = unitCard "core-006" "Durgnar the Bold" do
  hero
  trait Warrior
  race Dwarf
  cost 3
  loyalty 3
  power 2
  hitPoints 2
  body "Limit one Hero per zone. This unit gains {power}{power} while one section of your capital is burning."
  effects \self owner ->
    when (capitalBurning owner) $ gainPower self 2

kingKazador :: CardDef Unit
kingKazador = unitCard "core-007" "King Kazador" do
  hero
  trait Warrior
  race Dwarf
  cost 6
  loyalty 5
  power 3
  hitPoints 6
  toughness 2
  body "Limit one Hero per zone. Toughness 2. Opponents cannot target this unit with card effects unless they pay an additional 3 resources per effect."
  targetTax \_g caster self ->
    if caster /= self.controller then 3 else 0

dwarfCannonCrew :: CardDef Unit
dwarfCannonCrew = unitCard "core-008" "Dwarf Cannon Crew" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Engineer
  body "Forced: After this unit enters play, search the top five cards of your deck for a support card with cost 2 or lower and put it into this zone, if able. Then shuffle your deck."
  onEnterPlay \_owner self -> do
    let pk = self.controller
    searchTopOfDeck pk 5 \result -> do
      let matches = filterSupportsIn result.cards (costAtMost 2)
      chooseFromCards pk 0 1 matches
        "Choose a support to put into play (or skip)." \chosen ->
          for_ chosen \c -> playSupportFromDeck pk c.key self.zone
      shuffleDeck pk

dwarfMasons :: CardDef Unit
dwarfMasons = unitCard "core-009" "Dwarf Masons" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Engineer
  body "Forced: After this unit enters play, put the top card of your deck facedown into this zone as a development."
  onEnterPlay \_owner self ->
    addDevelopment self.controller self.zone

dwarfRanger :: CardDef Unit
dwarfRanger = unitCard "core-010" "Dwarf Ranger" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Ranger
  scout
  body "Scout. Quest. Forced: After one of your other {dwarf} units leaves play, deal 1 damage to one target unit or capital."
  quest $ forced \self ->
    onUnitOfLeavesPlay self.controller \unit ->
      when (unit.key /= self.key && unit `isRace` Dwarf) $
        withTarget self.controller (AnyUnit `Or` AnyCapital) \case
          TargetUnitOption u -> dealDamage u 1
          TargetZoneOption owner z -> dealZoneDamage owner z 1

mountainBrigade :: CardDef Unit
mountainBrigade = unitCard "core-011" "Mountain Brigade" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 6
  trait Warrior

ironbreakersOfAnkhor :: CardDef Unit
ironbreakersOfAnkhor = unitCard "core-012" "Ironbreakers of Ankhor" do
  race Dwarf
  cost 5
  loyalty 2
  power 2
  hitPoints 3
  traits [Warrior, Elite]
  toughnessX
  body "Toughness X (whenever this unit is assigned damage, cancel X of that damage). X is the number of development cards in this zone."

runeOfFortitude :: CardDef Support
runeOfFortitude = supportCard "core-013" "Rune of Fortitude" do
  race Dwarf
  cost 2
  loyalty 1
  trait Rune
  body "Each unit attacking this zone loses {power} unless its controller pays 1 resource per unit."
  imposesRuneOfFortitudeTax

keystoneForge :: CardDef Support
keystoneForge = supportCard "core-014" "Keystone Forge" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Forced: After your turn begins, heal 1 damage to your capital."
  kingdom $ forced \self ->
    onTurnBegin self.controller $
      healCapital self.controller 1

organGun :: CardDef Support
organGun = supportCard "core-015" "Organ Gun" do
  race Dwarf
  cost 0
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target unit. Attached unit gains {power}{power} while defending."
  attachedTo \_self unit ->
    when unit.defending $ gainPower unit 2

masterRuneOfDismay :: CardDef Support
masterRuneOfDismay = supportCard "core-016" "Master Rune of Dismay" do
  race Dwarf
  cost 4
  loyalty 3
  power 2
  trait Rune
  body "Kingdom. Opponent's units cost 1 additional resource to play."
  globalCostAdjust \_g s playing _filter ->
    if playing /= s.controller && s.zone == KingdomZone then 1 else 0

aGloriousDeath :: CardDef Quest
aGloriousDeath = questCard "core-017" "A Glorious Death" do
  race Dwarf
  cost 0
  loyalty 2
  body "Quest. Action: Sacrifice the unit on this quest to destroy up to two target attacking units. Use this ability only if A Glorious Death has 3 or more resource tokens on it. Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  action "Glorious sacrifice" 0 \usage ->
    withQuest usage.self.key \q -> when (q.tokens >= 3) $
      for_ q.questingUnit \quester -> do
        destroyUnit quester
        withCombat \cs ->
          when (cs.attackingPlayer /= usage.user) $
            chooseUpTo usage.user 2 cs.attackers (traverse_ destroyUnit)

grudgeThrower :: CardDef Support
grudgeThrower = supportCard "core-018" "Grudge Thrower" do
  race Dwarf
  cost 1
  loyalty 2
  trait Siege
  body "Battlefield. Action: Spend 1 resource and sacrifice a unit to have each attacking or defending unit gain {power} until the end of the turn."
  battlefield $ actionWith "Volley" 1 [SacrificeUnit] \_usage ->
    withCombat \cs ->
      for_ (cs.attackers <> cs.defenders) \k ->
        until EndOfTurn $ buffPower k 1

buryingTheGrudge :: CardDef Tactic
buryingTheGrudge = tacticCard "core-019" "Burying the Grudge" do
  race Dwarf
  cost 0
  loyalty 2
  body "Action: Gain 1 resource for each unit that entered a discard pile this turn."
  playableWhen \g _ ->
    maybe 0 (.unitsDiscarded) (Map.lookup ThisTurn g.history) > 0
  whenResolved \self ->
    withHistory ThisTurn \h ->
      gainResources self.controller h.unitsDiscarded

stubbornRefusal :: CardDef Tactic
stubbornRefusal = tacticCard "core-020" "Stubborn Refusal" do
  race Dwarf
  cost 2
  loyalty 1
  body "Action: Move all damage from one target unit to another target unit in any player's corresponding zone."
  playableWhen \g _pk ->
    any (\u -> isDamaged u && hasPeerInZone g u) g.units
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \src ->
      withUnit src \srcUnit -> do
        peers <- filter (\u -> u.key /= src) <$> unitsInZone srcUnit.zone
        chooseUpTo pk 1 (map (.key) peers) \chosen ->
          for_ chosen \dst -> moveAllDamage src dst

strikingTheGrudge :: CardDef Tactic
strikingTheGrudge = tacticCard "core-021" "Striking the Grudge" do
  race Dwarf
  cost 1
  loyalty 3
  body "Action: One target attacking or defending unit gains {power}{power} until the end of the turn."
  playableWhen $ hasTarget (Or attackingUnit defendingUnit)
  whenResolved \self ->
    withTarget self.controller (Or attackingUnit defendingUnit) \case
      TargetUnitOption k -> until EndOfTurn $ buffPower k 2
      _ -> pure ()

grudgeThrowerAssault :: CardDef Tactic
grudgeThrowerAssault = tacticCard "core-022" "Grudge Thrower Assault" do
  race Dwarf
  cost 2
  loyalty 3
  body "Play during combat, after damage has been assigned. Action: Destroy one target attacking unit."
  -- "After damage has been assigned": only the assign-response and
  -- post-apply windows qualify. Without the window gate this could
  -- kill an attacker before its damage was pooled, which is strictly
  -- stronger than the printed timing.
  playableWhen \g pk ->
    hasEnemyAttacker g pk
      && case g.actionWindow of
        Just aw ->
          aw.trigger
            `elem` [AfterAssignCombatDamage, AfterApplyCombatDamage]
        Nothing -> False
  whenResolved \self ->
    withTarget self.controller attackingUnit \k -> destroyUnit k

demolition :: CardDef Tactic
demolition = tacticCard "core-023" "Demolition!" do
  race Dwarf
  cost 2
  loyalty 1
  body "Action: Destroy one target support card or development."
  -- One unified picker over both prongs: any in-play support card
  -- (friendly or enemy, free-standing or attached) or any development
  -- (either player's). The printed text has no "enemy" restriction
  -- and the player must be free to pick a development even when
  -- supports exist.
  playableWhen \g pk ->
    hasTarget (AnySupportCard `Or` AnyDevelopmentZone) g pk
  whenResolved \self ->
    withTarget self.controller (AnySupportCard `Or` AnyDevelopmentZone) \case
      TargetSupportOption k -> destroySupport k
      TargetZoneOption owner zk -> destroyDevelopment owner zk
      _ -> pure ()

wakeTheMountain :: CardDef Tactic
wakeTheMountain = tacticCard "core-024" "Wake the Mountain" do
  race Dwarf
  cost 3
  loyalty 2
  body "Action: Put the top three cards of your deck into your battlefield or kingdom facedown as developments. (All three developments must go in the same zone.)"
  playableWhen \g pk -> hasDeckSize 3 g pk && canDevelop g pk
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk MyDevZone \zone ->
      replicateM_ 3 (addDevelopment pk zone)

masterRuneOfValaya :: CardDef Tactic
masterRuneOfValaya = tacticCard "core-025" "Master Rune of Valaya" do
  race Dwarf
  cost 2
  loyalty 1
  traits [Spell, Rune]
  body "Action: Cancel all damage assigned during the battlefield phase this turn."
  -- Clears every pending combat assignment (units and zone spillover)
  -- but leaves the combat itself in flight: the apply step then
  -- commits nothing, and Scout / end-of-combat hooks still run as
  -- normal.
  whenResolved \_ ->
    push CancelAllAssignedDamage
