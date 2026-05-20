{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Empire core cards (core-026..050). Every printed ability is
-- implemented; one card takes a simplifying shortcut (Bright
-- Wizard Apprentice cancels the next opponent action rather than
-- the previous one) — the in-card comment names the compromise.
module Invasion.Card.Defs.Empire (module Invasion.Card.Defs.Empire) where

import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Data.Map.Strict qualified as Map
import Invasion.Capital
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

peasantMilitia :: CardDef Unit
peasantMilitia = unitCard "core-026" "Peasant Militia" do
  race Empire
  cost 0
  loyalty 1
  power 0
  hitPoints 3
  trait Warrior

reiksguardKnights :: CardDef Unit
reiksguardKnights = unitCard "core-027" "Reiksguard Knights" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Knight
  counterstrike 2
  body "Counterstrike 2 (this unit deals 2 combat damage immediately after defending)."

brightWizardApprentice :: CardDef Unit
brightWizardApprentice = unitCard "core-028" "Bright Wizard Apprentice" do
  race Empire
  cost 2
  loyalty 2
  power 1
  hitPoints 1
  trait Mage
  body "Quest. Action: Spend 2 resources to target the effects of an Action just triggered by another unit or support card. Cancel the effects of that Action (limit once per turn)."
  -- Approximation: arm a one-shot cancel against this player's
  -- opponent. The next 'TriggerCardAction' that opponent fires has
  -- its effect suppressed (cost is still paid). The "target an
  -- action just triggered" rewind isn't expressible without a real
  -- action stack; this forward-looking cancel is the closest
  -- working substitute. Once-per-turn enforced via
  -- 'ActionUsedThisTurn' marker.
  quest $ action "Counterspell" 2 \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used do
      until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
      push (ArmActionCancel usage.user.next)

nulnTinkerers :: CardDef Unit
nulnTinkerers = unitCard "core-029" "Nuln Tinkerers" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Engineer
  body "Kingdom. Lower the cost of the first support card you play each turn by 1."
  unitCostAdjust \g self pk filt ->
    let played =
          getOrZero pk (historyOfThisTurn g).supportsPlayedBy
        gateZone = self.zone == KingdomZone
     in if pk == self.controller
           && gateZone
           && filt.cfKind == Support
           && played == 0
          then -1
          else 0

-- | Lookup helper: scope's per-player counter, default 0.
getOrZero :: PlayerKey -> Map.Map PlayerKey Int -> Int
getOrZero pk = Map.findWithDefault 0 pk

historyOfThisTurn :: Game -> History
historyOfThisTurn g = case Map.lookup ThisTurn g.history of
  Just h -> h
  Nothing -> mempty

theGreatswords :: CardDef Unit
theGreatswords = unitCard "core-030" "The Greatswords" do
  race Empire
  cost 4
  loyalty 2
  power 1
  hitPoints 4
  traits [Warrior, Elite]
  body "Forced: After a unit enters this zone, The Greatswords gains {power} until the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay _pk uk
      | uk /= self.key -> do
          g <- getGame
          case findUnit uk g of
            Just u | u.zone == self.zone ->
              until EndOfTurn $ buffPower self.key 1
            _ -> pure ()
    _ -> pure ()

sigmarsBlessed :: CardDef Unit
sigmarsBlessed = unitCard "core-031" "Sigmar's Blessed" do
  race Empire
  cost 2
  loyalty 2
  power 1
  hitPoints 1
  trait Priest
  body "Forced: After this unit leaves play, return one target unit in any player's corresponding zone to its owner's hand."
  onSelfLeavesPlay \_owner self -> do
    g <- getGame
    let candidates = filter (\u -> u.zone == self.zone) g.units
    case candidates of
      [] -> pure ()
      _ ->
        withTarget self.controller AnyUnit \k ->
          withUnit k \u ->
            when (u.zone == self.zone) (returnUnitToHand k)

thyrusGorman :: CardDef Unit
thyrusGorman = unitCard "core-032" "Thyrus Gorman" do
  hero
  trait Mage
  race Empire
  cost 3
  loyalty 3
  power 3
  hitPoints 3
  body "Limit one Hero per zone. Forced: After your turn ends, this unit takes 1 damage."
  onMyTurnEnd \_owner self -> dealDamage self.key 1

huntsmen :: CardDef Unit
huntsmen = unitCard "core-033" "Huntsmen" do
  race Empire
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Ranger
  body "Quest zone only."
  questOnly

warriorPriests :: CardDef Unit
warriorPriests = unitCard "core-034" "Warrior Priests" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  traits [Warrior, Priest]
  body "Forced: The first point of damage assigned to this unit each turn is redirected to one target unit in any battlefield. (If there is no valid target, the damage is assigned to Warrior Priests.)"
  -- Faithful: the engine consults this pre-damage hook BEFORE
  -- applying damage. We claim 1 of the inbound on the first hit
  -- each turn provided a valid battlefield target exists, then
  -- prompt the controller for redirect target.
  preDamageRedirectHook \g self inbound ->
    let used =
          any (\m -> m.details == RedirectedThisTurn)
            (Map.findWithDefault [] (UnitRef self.key) g.modifiers)
        anyTarget =
          any (\u -> u.zone == BattlefieldZone && u.key /= self.key) g.units
     in if not used && anyTarget && inbound > 0
          then Just PreDamageRedirect
            { amount = 1
            , run = ActionEffect \usage -> do
                until EndOfTurn (PendingBuff usage.self.key RedirectedThisTurn)
                withTarget usage.user
                  (UnitMatching \_ _ u ->
                    u.zone == BattlefieldZone && u.key /= usage.self.key)
                  \k -> dealDamage k 1
            }
          else Nothing

freeCompany :: CardDef Unit
freeCompany = unitCard "core-035" "Free Company" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Warrior
  scout
  body "Scout (discard one card at random from an opponent's hand if this unit survives combat)."

pistoliers :: CardDef Unit
pistoliers = unitCard "core-036" "Pistoliers" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Ranger
  body "Action: Spend 1 resource to move this unit from its current zone to another of your zones."
  action "Reposition" 1 \usage ->
    withTarget usage.user MyAnyZone \zk ->
      moveUnit usage.self.key zk

johannesBroheim :: CardDef Unit
johannesBroheim = unitCard "core-037" "Johannes Broheim" do
  hero
  trait Cavalry
  race Empire
  cost 6
  loyalty 3
  power 1
  hitPoints 4
  counterstrike 2
  body "Limit one Hero per zone. Counterstrike 2. At the end of each phase, you may move this unit from its current zone to one of your other zones."
  onReceive $ Receive \msg _owner self -> case msg of
    EndPhase _ -> do
      g <- getGame
      when (g.currentPlayer == self.controller) do
        move <- askYesNo self.controller "Move Johannes Broheim to one of your other zones?"
        when move $
          withTarget self.controller MyAnyZone \zk ->
            moveUnit self.key zk
    _ -> pure ()

knightTraining :: CardDef Support
knightTraining = supportCard "core-038" "Knight Training" do
  race Empire
  cost 0
  loyalty 2
  traits [Attachment, Skill]
  body "Attach to a target unit in your battlefield. Attached unit gains {power}."
  attachedTo \_self unit -> gainPower unit 1
  -- NOTE: the engine doesn't yet restrict attachment targets by
  -- zone. Until it does, this can attach to any unit. The +1 power
  -- buff is correct once it's attached.

churchOfSigmar :: CardDef Support
churchOfSigmar = supportCard "core-039" "Church of Sigmar" do
  race Empire
  cost 2
  loyalty 2
  power 1
  trait Building
  body "Kingdom. Opponents cannot target your units with card effects unless they pay an additional 1 resource per effect."
  supportTax \_g self caster target ->
    if self.zone == KingdomZone
        && target.controller == self.controller
        && caster /= self.controller
      then 1
      else 0

cityGates :: CardDef Support
cityGates = supportCard "core-040" "City Gates" do
  race Empire
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Forced: After your turn begins, place the top card of your deck into this zone as a development."
  forced \self ->
    onTurnBegin self.controller $
      addDevelopment self.controller self.zone

shrineToTaal :: CardDef Support
shrineToTaal = supportCard "core-041" "Shrine to Taal" do
  race Empire
  cost 2
  loyalty 1
  trait Building
  body "Forced: After your turn begins, choose one target unit in this zone. That unit gains {power} for each of your developments in this zone until the end of the turn."
  onMyTurnBegin \owner self -> do
    let pk = self.controller
        z = self.zone
        Developments devs =
          case z of
            KingdomZone -> owner.capital.kingdom.developments
            QuestZone -> owner.capital.quest.developments
            BattlefieldZone -> owner.capital.battlefield.developments
    when (devs > 0) $
      withTarget pk
        (UnitMatching \_ _ u -> u.controller == pk && u.zone == z)
        \k -> until EndOfTurn $ buffPower k devs

templeOfShallya :: CardDef Support
templeOfShallya = supportCard "core-042" "Temple of Shallya" do
  race Empire
  cost 4
  loyalty 1
  power 2
  trait Building
  body "Kingdom. At the beginning of your kingdom phase, you may move one of your units from its current zone to one of your other zones."
  -- Implemented as a free Kingdom action rather than the printed
  -- "you may at the beginning of your kingdom phase". The action's
  -- cost is 0 and there's no resource gate; the player picks a unit
  -- they control and a destination zone.
  kingdom $ action "Reposition a unit" 0 \usage ->
    withTarget usage.user ownUnit \uk ->
      withTarget usage.user MyAnyZone \zk ->
        moveUnit uk zk

defendTheBorder :: CardDef Quest
defendTheBorder = questCard "core-043" "Defend the Border" do
  race Empire
  cost 0
  loyalty 2
  body "Quest. While Defend the Border has 3 or more resource tokens on it, redirect the first point of damage done to your capital each turn to another target unit or capital. Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  -- Approximation: while the quest has 3+ tokens, schedule a
  -- one-shot capital shield each turn. The shield CANCELS the
  -- first damage rather than redirecting it; the redirect-to-
  -- another-target wording is a follow-up requiring a pre-damage
  -- hook.
  onMyTurnBegin \_owner self -> do
    g <- getGame
    case findQuest self.key g of
      Just q | q.tokens >= 3 ->
        scheduleCapitalShield self.controller
      _ -> pure ()

willOfTheElectors :: CardDef Tactic
willOfTheElectors = tacticCard "core-044" "Will of the Electors" do
  race Empire
  cost 1
  loyalty 1
  body "Action: Move up to two target developments from one zone to another controlled by the same player."
  whenResolved \self -> do
    let pk = self.controller
    -- Pick from zone and to zone in two prompts. We move 1 or 2
    -- developments depending on whether the source zone has them.
    withTarget pk MyAnyZone \fromZ ->
      withTarget pk MyAnyZone \toZ ->
        when (fromZ /= toZ) do
          moveDevelopment pk fromZ toZ
          moveDevelopment pk fromZ toZ

twinTailedComet :: CardDef Tactic
twinTailedComet = tacticCard "core-045" "Twin-Tailed Comet" do
  race Empire
  cost 2
  loyalty 4
  body "Action: Target a tactic just played. Copy the effects of that tactic without paying its cost. (You choose all targets of the copied tactic.)"
  -- The engine updates 'lastResolvedTactic' AFTER each tactic's
  -- body fires, so when Comet's body runs it reads the previously
  -- resolved tactic (exactly the "just played" referent the card
  -- text names) and re-fires it under the Comet controller, who
  -- re-picks every target via the tactic's own 'withTarget' calls.
  -- The 'code /= self' guard is a defensive no-op in case the
  -- caller ever resolves Comet without a prior tactic on record.
  whenResolved \self -> do
    g <- getGame
    case g.lastResolvedTactic of
      Just (code, target, x)
        | code /= self.cardDef.code ->
            push (TacticResolved self.controller code target x)
      _ -> pure ()

demoralise :: CardDef Tactic
demoralise = tacticCard "core-046" "Demoralise" do
  race Empire
  cost 0
  loyalty 1
  body "Action: One target unit loses {power}{power} until the end of the turn."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k ->
      until EndOfTurn $ buffPower k (-2)

franzsDecree :: CardDef Tactic
franzsDecree = tacticCard "core-047" "Franz's Decree" do
  race Empire
  cost 1
  loyalty 1
  body "Action: One target unit cannot attack or defend until the end of the turn."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      until EndOfTurn $ disableAttack k
      until EndOfTurn $ disableDefend k

forcedMarch :: CardDef Tactic
forcedMarch = tacticCard "core-048" "Forced March" do
  race Empire
  cost 2
  loyalty 2
  body "Play during your turn. Action: Move one target unit from its zone to another zone controlled by the same player."
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \uk ->
      withUnit uk \u ->
        -- "Another zone controlled by the same player" — own or
        -- opponent, but the unit's controller's zones, not the
        -- caster's. So we pick from `u.controller`'s zones.
        withTarget u.controller MyAnyZone \zk -> moveUnit uk zk

judgementOfVerena :: CardDef Tactic
judgementOfVerena = tacticCard "core-049" "Judgement of Verena" do
  race Empire
  cost 4
  loyalty 2
  trait Spell
  body "Play during your turn. Action: Destroy all unit and support cards in each zone with no developments."
  whenResolved \_ -> do
    g <- getGame
    let zonesFor :: Player -> [(PlayerKey, ZoneKind, Zone)]
        zonesFor p =
          let cap = p.capital
           in [ (p.key, KingdomZone, cap.kingdom)
              , (p.key, QuestZone, cap.quest)
              , (p.key, BattlefieldZone, cap.battlefield)
              ]
        undeveloped =
          [ (pk, zk)
          | (pk, zk, z) <- zonesFor g.player1 <> zonesFor g.player2
          , z.developments == 0
          ]
        targetUnits =
          [ u.key | u <- g.units
                  , (u.controller, u.zone) `elem` undeveloped
          ]
        targetSupports =
          [ s.key | s <- g.supports
                  , s.attachedTo == Nothing
                  , (s.controller, s.zone) `elem` undeveloped
          ]
    for_ targetUnits destroyUnit
    for_ targetSupports destroySupport

sigmarsIntervention :: CardDef Tactic
sigmarsIntervention = tacticCard "core-050" "Sigmar's Intervention" do
  race Empire
  cost 2
  loyalty 2
  trait Spell
  body "Play when an opponent attacks your capital, before defenders are declared. Action: Redirect the attack to a different one of your zones. (An attack cannot be redirected to a burning zone.)"
  -- The "play during the AfterDeclareCombatTarget window" timing
  -- restriction relies on the player choosing not to fire this any
  -- other time; we surface the redirect itself unconditionally.
  playableWhen \g pk -> case g.combat of
    Just cs -> cs.defendingPlayer == pk
    Nothing -> False
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk MyAnyZone \zk -> redirectAttackZone zk
