{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | High Elf core cards (core-051..055). Only 5 cards — the rest of
-- the High Elf range arrives in later sets.
module Invasion.Card.Defs.HighElf (module Invasion.Card.Defs.HighElf) where

import Data.Map.Strict qualified as Map
import Invasion.Capital
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

-- The Corruption cycle ------------------------------------------------

tyriel :: CardDef Unit
tyriel = unitCard "the-skavenblight-threat-005" "Tyriel" do
  hero
  race HighElf
  cost 5
  loyalty 3
  power 2
  hitPoints 4
  body
    "Limit one Hero per zone. Forced: Whenever an opponent attacks this zone, he must \
    \return one of his attacking units to its owner's hand at the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    EndTurn _ -> do
      g <- getGame
      let h = Map.findWithDefault mempty ThisTurn g.history
          opp = self.controller.next
          relevant =
            [ rec
            | rec <- h.combats
            , rec.attacker == opp
            , rec.defender == self.controller
            , rec.zone == self.zone
            ]
      for_ relevant \rec -> do
        gNow <- getGame
        let alive =
              [ k
              | k <- rec.attackerKeys
              , Just u <- [findUnit k gNow]
              , u.controller == opp
              ]
        forcePickUnit opp alive
          "Tyriel: return one of your attacking units to your hand."
          returnUnitToHand
    _ -> pure ()

steelsBane :: CardDef Tactic
steelsBane = tacticCard "the-skavenblight-threat-006" "Steel's Bane" do
  race HighElf
  cost 1
  loyalty 4
  body "Action: Cancel the next 10 damage that would be dealt to one target {highelf} unit this turn."
  playableWhen $ hasTarget (unitWhere (`isRace` HighElf))
  whenResolved \self ->
    withTarget self.controller (unitWhere (`isRace` HighElf)) \k ->
      until EndOfTurn $ damageShield k 10

repairTheWaystones :: CardDef Quest
repairTheWaystones = questCard "the-skavenblight-threat-007" "Repair the Waystones" do
  race HighElf
  cost 0
  loyalty 3
  body
    "Quest. Action: Discard 3 resource tokens from this card to target a support card in your \
    \discard pile, and put it into play in your quest zone. \
    \Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  spendTokens "Restore a waystone" 3 \usage -> do
    let pk = usage.user
    me <- playerOf pk <$> getGame
    -- Attachments need a host to enter play, so only free-standing
    -- supports are recoverable through this quest.
    let candidates =
          [ c
          | c <- me.discard
          , Just cd <- [asSupport c.def]
          , Attachment `notElem` cd.traits
          ]
    chooseFromCards pk 0 1 candidates
      "Choose a support card to put into your quest zone." \chosen ->
        for_ chosen \c -> push (PlaySupportFromDiscard pk c.key QuestZone)

vaulsUnmaking :: CardDef Tactic
vaulsUnmaking = tacticCard "path-of-the-zealot-026" "Vaul's Unmaking" do
  race HighElf
  cost 0
  loyalty 1
  trait Spell
  body "Action: Destroy one target Attachment card."
  playableWhen $ hasTarget attachmentCard
  whenResolved \self ->
    withTarget self.controller attachmentCard destroySupport
  where
    attachmentCard = SupportMatching \_ _ s ->
      isJust s.attachedTo || Attachment `elem` s.cardDef.traits

repeaterBoltThrower :: CardDef Support
repeaterBoltThrower = supportCard "path-of-the-zealot-027" "Repeater Bolt Thrower" do
  race HighElf
  cost 3
  loyalty 3
  trait Siege
  body
    "Battlefield. Action: Spend X resources to deal X indirect damage to target opponent. \
    \X is the number of your developments in this zone. (Players assign their own indirect damage.)"
  battlefield $ action "Volley" 0 \usage -> do
    g <- getGame
    let me = playerOf usage.user g
        Developments x = me.capital.battlefield.developments
        Resources r = me.resources
    when (x > 0 && r >= x) do
      payResources usage.user x
      indirectDamage usage.user.next x

dragonmage :: CardDef Unit
dragonmage = unitCard "tooth-and-claw-046" "Dragonmage" do
  race HighElf
  cost 5
  loyalty 3
  power 2
  hitPoints 3
  traits [Mage, Elite]
  body "Whenever this unit is assigned damage, cancel all but 1 of that damage."
  perHitCap 1

giftsOfAenarion :: CardDef Tactic
giftsOfAenarion = tacticCard "tooth-and-claw-047" "Gifts of Aenarion" do
  race HighElf
  cost 4
  loyalty 2
  trait Spell
  body
    "Action: Cancel all damage that would be dealt to your capital until the end of the turn. \
    \For each damage thus cancelled, gain 1 resource."
  whenResolved \self ->
    push (ArmCapitalShield self.controller Nothing 1)

silverHelmDetachment :: CardDef Unit
silverHelmDetachment = unitCard "the-deathmaster-s-dance-067" "Silver Helm Detachment" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Elite]
  body
    "This unit enters play with 3 resource tokens on it. Action: Remove a resource token \
    \from this unit to gain {power} until the end of the turn (limit once per turn)."
  onEnterPlay \_owner self -> push (AdjustUnitTokens self.key 3)
  action "Spur the helms" 0 \usage -> do
    g <- getGame
    whenJust (findUnit usage.self.key g) \u -> do
      let used =
            any (\m -> m.details == ActionUsedThisTurn)
              (Map.findWithDefault [] (UnitRef u.key) g.modifiers)
      when (u.tokens > 0 && not used) do
        until EndOfTurn (PendingBuff u.key ActionUsedThisTurn)
        push (AdjustUnitTokens u.key (-1))
        until EndOfTurn $ buffPower u.key 1

ishasGaze :: CardDef Support
ishasGaze = supportCard "the-deathmaster-s-dance-068" "Isha's Gaze" do
  race HighElf
  cost 0
  loyalty 1
  traits [Attachment, Spell]
  body "Attach to a target unit. Whenever a unit is healed, attached unit gains {power} until the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    HealUnit _ n
      | n > 0 ->
          for_ self.attachedTo \host ->
            until EndOfTurn $ buffPower host 1
    _ -> pure ()

banish :: CardDef Tactic
banish = tacticCard "the-deathmaster-s-dance-069" "Banish" do
  race HighElf
  cost 3
  loyalty 2
  body "Action: Return one target unit without any Attachment cards on it to its owner's hand."
  playableWhen $ hasTarget (unitWhere (null . (.attachments)))
  whenResolved \self ->
    withTarget self.controller (unitWhere (null . (.attachments))) returnUnitToHand

finreirsGuard :: CardDef Unit
finreirsGuard = unitCard "the-warpstone-chronicles-087" "Finreir's Guard" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  toughness 1
  body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."

warCrownOfSaphery :: CardDef Support
warCrownOfSaphery = supportCard "the-warpstone-chronicles-088" "War Crown of Saphery" do
  unique
  race HighElf
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {highelf} unit. Attached unit gains {power} for each resource token \
    \on this card. Forced: At the beginning of your turn, place a resource token on this card."
  attachedTo \self unit ->
    when (self.tokens > 0) $ gainPower unit self.tokens
  onMyTurnBegin \_owner self ->
    adjustSupportTokens self.key 1

secondSight :: CardDef Tactic
secondSight = tacticCard "the-warpstone-chronicles-089" "Second Sight" do
  race HighElf
  cost 1
  loyalty 1
  trait Spell
  body "Action: Look at each opponent's hand. Then, draw a card."
  whenResolved \self -> do
    let pk = self.controller
    opp <- playerOf pk.next <$> getGame
    -- A zero-pick card prompt doubles as the reveal.
    chooseFromCards pk 0 0 opp.hand "Second Sight: the opponent's hand." \_ -> pure ()
    drawCard pk

ellyrianReavers :: CardDef Unit
ellyrianReavers = unitCard "arcane-fire-107" "Ellyrian Reavers" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Warrior, Elite]
  body "Forced: When this unit enters play, put the top card of your deck into your battlefield as a development."
  onEnterPlay \_owner self ->
    addDevelopment self.controller BattlefieldZone

morvaelsLegacy :: CardDef Tactic
morvaelsLegacy = tacticCard "arcane-fire-108" "Morvael's Legacy" do
  race HighElf
  cost 10
  loyalty 3
  traits [Epic, Spell]
  body
    "Play only during your turn. Action: Put into play all units in your discard pile. \
    \(You choose which zone each unit enters.)"
  playableWhen \g pk ->
    g.currentPlayer == pk
      && any (isJust . asUnit . (.def)) (playerOf pk g).discard
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    let units = [c | c <- me.discard, isJust (asUnit c.def)]
    for_ units \c ->
      withTarget pk MyAnyZone \zk ->
        putUnitIntoPlay pk FromDiscard c.key zk

chargeOfTheSilverHelms :: CardDef Tactic
chargeOfTheSilverHelms = tacticCard "arcane-fire-109" "Charge of the Silver Helms" do
  race HighElf
  cost 1
  loyalty 2
  body "Action: One of your target units gets -1 hit points and gains {power}{power}{power} until the end of the turn."
  playableWhen $ hasTarget ownUnit
  whenResolved \self ->
    withTarget self.controller ownUnit \k -> do
      until EndOfTurn $ debuffHP k 1
      until EndOfTurn $ buffPower k 3

-- Days of Blood --------------------------------------------------------

greatFireDragon :: CardDef Unit
greatFireDragon = unitCard "days-of-blood-010" "Great Fire Dragon" do
  race HighElf
  cost 5
  loyalty 2
  power 3
  hitPoints 4
  trait Creature
  battlefieldOnly
  body
    "Battlefield only. Action: When this unit attacks, put 1 resource token on it. \
    \Then, you may remove X resource tokens from this unit to deal X damage to target \
    \unit in the attacked zone."
  onMyAttackDeclared \_owner self zone _attackers -> do
    push (AdjustUnitTokens self.key 1)
    g <- getGame
    let avail = maybe 0 (.tokens) (findUnit self.key g) + 1
        inZone u = u.zone == zone && u.controller /= self.controller
        targets = [u | u <- g.units, inZone u]
    when (avail > 0 && not (null targets)) $
      may self.controller "Great Fire Dragon: remove resource tokens to deal damage?" do
        x <- chooseAmount self.controller 1 avail "Remove how many resource tokens?"
        withTarget self.controller (UnitMatching \_ _ u -> inZone u) \k -> do
          push (AdjustUnitTokens self.key (negate x))
          dealDamage k x

-- Oaths of Vengeance ---------------------------------------------------

outlyingTower :: CardDef Support
outlyingTower = supportCard "oaths-of-vengeance-023" "Outlying Tower" do
  race HighElf
  cost 1
  loyalty 1
  power 1
  trait Building
  body "If you control a non-[High Elf] card, sacrifice this card."
  sacrificeIfControlsOffFaction HighElf

-- Battle for the Old World ---------------------------------------------

lilea :: CardDef Unit
lilea = unitCard "battle-for-the-old-world-050" "Lilea" do
  unique
  race HighElf
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  traits [Elite, Ranger]
  body
    "Action: When this unit attacks, put 1 resource token on it. Then, deal X indirect \
    \damage to target opponent. X is the number of resource tokens on this unit."
  onMyAttackDeclared \_owner self _zone _attackers -> do
    push (AdjustUnitTokens self.key 1)
    g <- getGame
    let x = maybe 0 (.tokens) (findUnit self.key g) + 1
    when (x > 0) $ indirectDamage self.controller.next x
