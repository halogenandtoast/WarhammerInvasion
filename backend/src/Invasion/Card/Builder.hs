{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card.Builder (module Invasion.Card.Builder) where

import Control.Monad.State.Strict
import Invasion.Card.Effects
import Invasion.CardDef
import Invasion.CardDef qualified as CardDef
import Invasion.Entity (SupportDetails (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Player
import Invasion.Prelude
import Invasion.Types

newtype CardBuilder k a = CardBuilder (State (CardDef k) a)
  deriving newtype (Functor, Applicative, Monad, MonadState (CardDef k))

emptyCardDef :: forall k. HasDefaultExtras k => CardCode -> String -> CardKind -> CardDef k
emptyCardDef code title kind =
  CardDef
    code
    title
    kind
    []
    (Fixed 0)
    0
    0
    Nothing
    []
    Nothing
    Nothing
    []
    False
    []
    noReceive
    (defaultExtras @k)
    (\_ _ -> 0)
    (\_ _ -> True)

unitCard :: CardCode -> String -> CardBuilder Unit () -> CardDef Unit
unitCard code title = buildCard $ (emptyCardDef code title Unit) {CardDef.hitPoints = Just (Fixed 1)}

supportCard :: CardCode -> String -> CardBuilder Support a -> CardDef Support
supportCard code title = buildCard $ emptyCardDef code title Support

questCard :: CardCode -> String -> CardBuilder Quest a -> CardDef Quest
questCard code title = buildCard $ emptyCardDef code title Quest

tacticCard :: CardCode -> String -> CardBuilder Tactic a -> CardDef Tactic
tacticCard code title = buildCard $ emptyCardDef code title Tactic

-- | Legends are persistent like units (they have hit points and live on
-- the capital board) but they're their own card type and are not
-- targeted by unit/support/tactic effects. The HP default of 1 mirrors
-- 'unitCard'; legend cards will normally override via 'hitPoints'.
legendCard :: CardCode -> String -> CardBuilder Legend () -> CardDef Legend
legendCard code title = buildCard $ (emptyCardDef code title Legend) {CardDef.hitPoints = Just (Fixed 1)}

buildCard :: CardDef k -> CardBuilder k a -> CardDef k
buildCard def (CardBuilder inner) = execState inner def

unique :: CardBuilder k ()
unique = modify \cardDef -> cardDef {unique = True}

race :: Race -> CardBuilder k ()
race r = modify \cardDef -> cardDef {races = r : cardDef.races}

cost :: Int -> CardBuilder k ()
cost c = modify \cardDef -> cardDef {cost = Fixed c}

loyalty :: Int -> CardBuilder k ()
loyalty l = modify \cardDef -> cardDef {loyalty = l}

power :: Int -> CardBuilder k ()
power p = modify \cardDef -> cardDef {power = p}

-- | Cards that carry hit points: units and legends. Other kinds reject
-- 'hitPoints' at the type level so a tactic builder can't silently set
-- an HP value that the engine would never read.
class HasHitPoints (k :: CardKind)
instance HasHitPoints 'Unit
instance HasHitPoints 'Legend

hitPoints :: HasHitPoints k => Int -> CardBuilder k ()
hitPoints hp = modify \cardDef -> cardDef {CardDef.hitPoints = Just (Fixed hp)}

trait :: Trait -> CardBuilder k ()
trait t = modify \cardDef -> cardDef {traits = t : cardDef.traits}

traits :: [Trait] -> CardBuilder k ()
traits = traverse_ trait

body :: String -> CardBuilder k ()
body f = modify \cardDef -> cardDef {CardDef.text = Just f}

flavor :: String -> CardBuilder k ()
flavor f = modify \cardDef -> cardDef {flavor = Just f}

keyword :: Keyword -> CardBuilder k ()
keyword k = modify \cardDef -> cardDef {keywords = k : cardDef.keywords}

toughness :: Int -> CardBuilder Unit ()
toughness n = keyword (Toughness $ Fixed n)

toughnessX :: CardBuilder Unit ()
toughnessX = keyword (Toughness Variable)

scout :: CardBuilder Unit ()
scout = keyword Scout

-- | Counterstrike N keyword: while declared as a defender, this unit
-- immediately deals N uncancellable damage to one attacker of its
-- choice before regular combat damage assigns.
counterstrike :: Int -> CardBuilder Unit ()
counterstrike n = keyword (Counterstrike n)

-- | Ambush keyword (FAQ 2.2 v2.0): only triggerable while the host
-- development is face-down. Used by Cataclysm-era developments.
ambush :: CardBuilder k ()
ambush = keyword Ambush

-- | Order Only keyword: cannot be included in a Destruction deck.
orderOnly :: CardBuilder k ()
orderOnly = keyword OrderOnly

-- | Destruction Only keyword: cannot be included in an Order deck.
destructionOnly :: CardBuilder k ()
destructionOnly = keyword DestructionOnly

-- | Limit one Hero per zone: while a player controls this Hero in a
-- zone, neither player may put another Hero into that same zone.
limitOneHeroPerZone :: CardBuilder Unit ()
limitOneHeroPerZone = keyword LimitOneHeroPerZone

-- | Hero meta-builder. Sets 'unique', adds the 'Hero' trait, and
-- installs the "Limit one Hero per zone" keyword in one step. Combine
-- with additional traits ('trait Warrior', 'trait Sorcerer', …).
--
-- > durgnarTheBold = unitCard "core-006" "Durgnar the Bold" do
-- >   hero
-- >   trait Warrior
-- >   race Dwarf
-- >   ...
hero :: CardBuilder Unit ()
hero = do
  unique
  trait Hero
  limitOneHeroPerZone

-- | Zone-restriction keywords for cards that may only enter play in a
-- specific zone.
kingdomOnly :: CardBuilder k ()
kingdomOnly = keyword KingdomOnly

questOnly :: CardBuilder k ()
questOnly = keyword QuestOnly

battlefieldOnly :: CardBuilder k ()
battlefieldOnly = keyword BattlefieldOnly

-- | Append a 'Receive' handler. The existing receiver runs first, then
-- the new one; this lets a card stack several event-specific hooks
-- without one stomping the previous one. 'emptyCardDef' starts with
-- 'noReceive', so the first append is effectively a set.
onReceive :: Receive k -> CardBuilder k ()
onReceive (Receive new) = modify \cardDef ->
  let Receive prev = cardDef.receive
   in cardDef
        { receive = Receive \msg owner self -> do
            prev msg owner self
            new msg owner self
        }

-- | Compose two 'Receive' handlers sequentially. The first runs, then
-- the second.
composeReceive :: Receive k -> Receive k -> Receive k
composeReceive (Receive a) (Receive b) = Receive \msg owner self -> do
  a msg owner self
  b msg owner self

-- | Wrap a 'Receive' handler so it only fires while the host is in
-- the given zone.
gateReceive
  :: HasField "zone" (InPlay k) ZoneKind
  => ZoneKind -> Receive k -> Receive k
gateReceive z (Receive r) = Receive \msg owner self ->
  when (self.zone == z) (r msg owner self)

-- ---------------------------------------------------------------------
-- Extras builders
--
-- The engine reads kind-specific 'extras' fields instead of casing on a
-- card's 'code'. Each helper below sets a single slice; absence keeps
-- the corresponding default from 'defaultExtras' (no-op for that
-- behavior).
-- ---------------------------------------------------------------------

modifyUnitExtras :: (UnitExtras -> UnitExtras) -> CardBuilder Unit ()
modifyUnitExtras f =
  modify \cd -> cd {extras = f cd.extras}

modifySupportExtras :: (SupportExtras -> SupportExtras) -> CardBuilder Support ()
modifySupportExtras f =
  modify \cd -> cd {extras = f cd.extras}

-- | Game-state-derived self power bonus (Troll Slayers, Korhil, …).
selfPower :: (Game -> UnitDetails -> Int) -> CardBuilder Unit ()
selfPower f = modifyUnitExtras \e -> e {selfPowerBonus = f}

-- | Extra combat damage this unit deals (Lord of Khorne, Gorbad).
combatPower :: (Game -> UnitDetails -> Int) -> CardBuilder Unit ()
combatPower f = modifyUnitExtras \e -> e {combatPowerBonus = f}

-- | Power this unit grants to other in-play units (Karl Franz, Templar
-- of Sigmar). Args to @f@: game, this unit, target unit.
unitAura :: (Game -> UnitDetails -> UnitDetails -> Int) -> CardBuilder Unit ()
unitAura f = modifyUnitExtras \e -> e {unitAuraPower = f}

-- | Predicate gating attacker eligibility (Sworn of Khorne).
canAttack :: (Game -> PlayerKey -> ZoneKind -> UnitDetails -> Bool) -> CardBuilder Unit ()
canAttack f = modifyUnitExtras \e -> e {canAttackZone = f}

-- | Per-turn damage cap (Daemonettes of Slaanesh).
perTurnDamageCap :: Int -> CardBuilder Unit ()
perTurnDamageCap n = modifyUnitExtras \e -> e {damageCap = Just n}

-- | Mark the unit as corrupting any enemy it deals non-zero combat
-- damage to (Plaguebearers of Nurgle, Beasts of Nurgle).
corruptsOnDamage :: CardBuilder Unit ()
corruptsOnDamage = modifyUnitExtras \e -> e {corruptsOnCombatDamage = True}

-- | Extra resources a non-controller must spend to target this unit
-- (King Kazador).
targetTax :: (Game -> PlayerKey -> UnitDetails -> Int) -> CardBuilder Unit ()
targetTax f = modifyUnitExtras \e -> e {extraTargetTax = f}

-- | Multiplier on ALL applied damage while this unit is in play
-- (Bloodletter).
damageMultiplier :: Int -> CardBuilder Unit ()
damageMultiplier n = modifyUnitExtras \e -> e {damageMultiplierWhileInPlay = n}

-- | Self-cost adjustment when playing this card (Bloodcrusher: -1
-- per burning zone). May be negative.
selfCostAdjust :: (Game -> PlayerKey -> Int) -> CardBuilder k ()
selfCostAdjust f = modify \cd -> cd {selfCostAdjustment = f}

-- | Per-card playability predicate. The engine refuses to play this
-- card if the predicate returns 'False'. Stacks with the engine's
-- baseline checks (resources, unique, Limited).
--
-- > playableWhen \g _pk ->
-- >   any (\u -> ... source candidate ...) g.units
playableWhen :: (Game -> PlayerKey -> Bool) -> CardBuilder k ()
playableWhen pred = modify \cd -> cd {canPlay = pred}

-- | Static power contribution while attached (Daemonsword, Banner of
-- Sigmar, …).
attachmentPower :: Int -> CardBuilder Support ()
attachmentPower n = modifySupportExtras \e -> e {attachmentPowerBonus = n}

-- | Static HP contribution while attached (Daemonsword).
attachmentHp :: Int -> CardBuilder Support ()
attachmentHp n = modifySupportExtras \e -> e {attachmentHPBonus = n}

-- | While attached, the host unit's combat damage is uncancellable
-- (Hammer of Sigmar).
grantsUncancellable :: CardBuilder Support ()
grantsUncancellable =
  modifySupportExtras \e -> e {grantsUncancellableDamage = True}

-- | Power this support grants to a unit (Iron Tower, Cauldron of
-- Blood, Da Bad Moon static slice).
supportAura :: (Game -> SupportDetails -> UnitDetails -> Int) -> CardBuilder Support ()
supportAura f = modifySupportExtras \e -> e {supportAuraPower = f}

-- | Extra combat damage this support adds to a unit (Rift of Battle,
-- Organ Gun while defending, Da Bad Moon and Big Boss's Banner for Orc
-- attackers).
supportCombat :: (Game -> SupportDetails -> UnitDetails -> Int) -> CardBuilder Support ()
supportCombat f = modifySupportExtras \e -> e {supportCombatBonus = f}

-- | "Attach to a target unit. Attached unit gains …" Runs the body
-- each engine recompute with the support and the unit it's attached
-- to; 'gainPower unit n' (and future buff verbs) emit contributions
-- that flow into the attached unit's effective stats. Body fires
-- only while attached.
--
-- > organGun = supportCard "core-015" "Organ Gun" do
-- >   ...
-- >   attachedTo \_self unit ->
-- >     when unit.defending $ gainPower unit 2
attachedTo
  :: (SupportDetails -> UnitDetails -> EffectM ())
  -> CardBuilder Support ()
attachedTo body = modifySupportExtras \e -> e
  { supportAuraPower = \g s u ->
      let prev = e.supportAuraPower g s u
          extra = case s.attachedTo of
            Just k | k == u.key ->
              activeBonusPower (execEffectM g (body s u))
            _ -> 0
       in prev + extra
  }

-- | Bonus power this support contributes to a zone of its controller
-- (Lighthouse of Lothern, Rift of Chaos).
zonePowerAura :: (Game -> SupportDetails -> ZoneKind -> Int) -> CardBuilder Support ()
zonePowerAura f = modifySupportExtras \e -> e {zonePowerBonus = f}

-- | Cost-of-play adjustment this support imposes on other cards being
-- played (Imperial Crown, Master Rune of Dismay).
globalCostAdjust
  :: (Game -> SupportDetails -> PlayerKey -> CardCodeFilter -> Int)
  -> CardBuilder Support ()
globalCostAdjust f = modifySupportExtras \e -> e {globalCostAdjustment = f}

-- | Mark the printed Rune of Fortitude effect on this support.
imposesRuneOfFortitudeTax :: CardBuilder Support ()
imposesRuneOfFortitudeTax =
  modifySupportExtras \e -> e {runeOfFortitudeTax = True}

-- | Non-gated constant block. The body fires every engine tick
-- regardless of which zone this unit is in. Use for "this unit gains
-- X while CONDITION" effects whose condition isn't zone-specific
-- (Durgnar the Bold, …).
--
-- > effects \self owner ->
-- >   when (capitalBurning owner) $ gainPower self 2
effects
  :: (UnitDetails -> Player -> EffectM ())
  -> CardBuilder Unit ()
effects body = modifyUnitExtras \e -> e
  { runtimeEffects = \g self ->
      let prev = e.runtimeEffects g self
          owner = playerOf self.controller g
       in prev <> execEffectM g (body self owner)
  }
