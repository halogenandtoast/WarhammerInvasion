{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Dark Elf core cards (core-106..110). Only 5 cards — the rest of
-- the Dark Elf range arrives in later sets.
module Invasion.Card.Defs.DarkElf (module Invasion.Card.Defs.DarkElf) where

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
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

discipleOfKhaine :: CardDef Unit
discipleOfKhaine = unitCard "core-106" "Disciple of Khaine" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Warrior, Priest]
  body "Action: Spend 2 resources to redirect one combat damage just assigned to this unit to another target unit."
  -- Action is selectable any time it's the controller's priority
  -- (the "just assigned" window isn't expressible without a real
  -- action stack). We guard the redirect on actually having a
  -- cancellable pending-damage assignment on self in the current
  -- combat — outside combat (or when nothing's been assigned to
  -- self) the action is a no-op rather than a free "deal 1 to a
  -- target" exploit.
  action "Redirect blow" 2 \usage -> do
    g <- getGame
    let pendingOnSelf = case g.combat of
          Just cs ->
            sum
              [ pd.cancellable
              | pd <- cs.pendingAssignments
              , PDUnit k <- [pd.target]
              , k == usage.self.key
              ]
          Nothing -> 0
    when (pendingOnSelf > 0) $
      withTarget usage.user AnyUnit \target -> do
        push (CancelAssignedDamageOnUnit usage.self.key 1)
        dealDamage target 1

vileSorceress :: CardDef Unit
vileSorceress = unitCard "core-107" "Vile Sorceress" do
  race DarkElf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Sorceror
  body "Quest. Forced: After your turn begins, one target unit gets -1 hit points until the end of the turn."
  onMyTurnBegin \_owner self ->
    when (self.zone == QuestZone) $
      withTarget self.controller AnyUnit \k ->
        until EndOfTurn $ debuffHP k 1

coldOneRiders :: CardDef Unit
coldOneRiders = unitCard "core-108" "Cold One Riders" do
  race DarkElf
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  traits [Cavalry, Elite]
  counterstrike 1
  body "Counterstrike 1 (this unit deals 1 combat damage immediately after defending)."

cauldronOfBlood :: CardDef Support
cauldronOfBlood = supportCard "core-109" "Cauldron of Blood" do
  race DarkElf
  cost 4
  loyalty 1
  power 2
  trait Siege
  body "Kingdom. Forced: When this zone is attacked, deal 1 damage to one target attacking unit."
  onMyZoneAttacked \_owner self cs ->
    case cs.attackers of
      [] -> pure ()
      _ ->
        withTarget self.controller
          (UnitMatching \_ _ u -> u.key `elem` cs.attackers)
          (`dealDamage` 1)

hate :: CardDef Tactic
hate = tacticCard "core-110" "Hate" do
  race DarkElf
  cost 0
  loyalty 1
  body "Action: Take 1 resource from each opponent and add it to your available resources."
  whenResolved \self -> do
    let pk = self.controller
        opp = pk.next
    -- Spend 1 from opponent (clamps at 0), gain 1 to self per opp.
    push (SpendResources opp 1)
    push (GainResources pk 1)

-- The Corruption cycle ------------------------------------------------

malusDarkblade :: CardDef Unit
malusDarkblade = unitCard "the-skavenblight-threat-012" "Malus Darkblade" do
  hero
  race DarkElf
  cost 5
  loyalty 4
  power 3
  hitPoints 3
  body
    "Limit one Hero per zone. At the end of your battlefield phase, deal 1 damage to each \
    \of your opponent's units that could have defended but did not."
  onMyPhaseEnd BattlefieldPhase \_owner self -> do
    g <- getGame
    let h = Map.findWithDefault mempty ThisPhase g.history
        myAttacks =
          [ (rec.defender, rec.zone, rec.defenderKeys)
          | rec <- h.combats
          , rec.attacker == self.controller
          ]
        defended = concat [ks | (_, _, ks) <- myAttacks]
        couldNot u =
          u.corrupted
            || (not u.blanked && u.cardDef.extras.cannotDefend)
            || any
              (\m -> m.details == CannotDefend)
              (Map.findWithDefault [] (UnitRef u.key) g.modifiers)
        shirkers =
          [ u.key
          | u <- g.units
          , u.controller == self.controller.next
          , any (\(d, z, _) -> d == u.controller && z == u.zone) myAttacks
          , u.key `notElem` defended
          , not (couldNot u)
          ]
    for_ shirkers \k -> dealDamage k 1

morathisPegasus :: CardDef Unit
morathisPegasus = unitCard "the-skavenblight-threat-013" "Morathi's Pegasus" do
  race DarkElf
  cost 3
  loyalty 4
  power 1
  hitPoints 3
  trait Cavalry
  toughness 3
  body
    "Toughness 3 (whenever this unit is assigned damage, cancel 3 of that damage). \
    \Action: Spend 3 resources to have this unit lose all Toughness until the end of the \
    \turn. Only an opponent may trigger this ability."
  actionOpponent "Ground the pegasus" 3 \usage ->
    until EndOfTurn $ loseAllToughness usage.self.key

weNeedYourBlood :: CardDef Tactic
weNeedYourBlood = tacticCard "the-skavenblight-threat-014" "We Need Your Blood" do
  race DarkElf
  cost 1
  loyalty 1
  body
    "Action: One target unit gets -1 hit points and another target unit gets +1 hit points \
    \until the end of the turn."
  playableWhen \g _pk -> length g.units >= 2
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \loser -> do
      until EndOfTurn $ debuffHP loser 1
      withTarget pk (unitWhere \u -> u.key /= loser) \winner ->
        until EndOfTurn $ buffHP winner 1

darkRiders :: CardDef Unit
darkRiders = unitCard "path-of-the-zealot-033" "Dark Riders" do
  race DarkElf
  cost 4
  loyalty 1
  power 1
  hitPoints 4
  trait Cavalry
  body "This unit gains {power} while any unit in play is corrupted."
  effects \self _owner -> do
    g <- getGame
    when (any (.corrupted) g.units) $ gainPower self 1

callTheBlood :: CardDef Tactic
callTheBlood = tacticCard "path-of-the-zealot-034" "Call the Blood" do
  race DarkElf
  cost 1
  loyalty 2
  trait Spell
  body "Action: Destroy one target damaged unit."
  playableWhen $ hasTarget (unitWhere isDamaged)
  whenResolved \self ->
    withTarget self.controller (unitWhere isDamaged) destroyUnit

coldOneChariot :: CardDef Unit
coldOneChariot = unitCard "tooth-and-claw-053" "Cold One Chariot" do
  race DarkElf
  cost 3
  loyalty 2
  power 2
  hitPointsX
  trait Cavalry
  body "X is the number of developments in this zone."
  selfHP \g u -> devsInZone g u

graspingDarkness :: CardDef Tactic
graspingDarkness = tacticCard "tooth-and-claw-054" "Grasping Darkness" do
  race DarkElf
  cost 3
  loyalty 2
  trait Spell
  body
    "Action: Until the end of the turn, take control of target unit with printed cost 2 or \
    \lower. Move the unit to your corresponding zone."
  playableWhen $ hasTarget cheapEnemy
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk cheapEnemy \k ->
      withUnit k \u -> do
        push (TakeControlOfUnit pk k)
        push (ScheduleControlReturn k u.controller)
  where
    cheapEnemy = UnitMatching \me _ u ->
      u.controller /= me && costAtMost 2 u.cardDef

warHydra :: CardDef Unit
warHydra = unitCard "the-deathmaster-s-dance-076" "War Hydra" do
  race DarkElf
  cost 5
  loyalty 1
  power 2
  hitPoints 1
  trait Creature
  body
    "Place 5 resource tokens on this unit when it enters play. Action: Remove a resource \
    \token from this unit to cancel 1 damage assigned to it. Then, add 1 resource to your pool."
  onEnterPlay \_owner self -> push (AdjustUnitTokens self.key 5)
  action "Regrow a head" 0 \usage -> do
    g <- getGame
    whenJust (findUnit usage.self.key g) \u -> do
      let pending = case g.combat of
            Just cs ->
              sum
                [ pd.cancellable
                | pd <- cs.pendingAssignments
                , pd.target == PDUnit u.key
                ]
            Nothing -> 0
      when (u.tokens > 0 && pending > 0) do
        push (AdjustUnitTokens u.key (-1))
        push (CancelAssignedDamageOnUnit u.key 1)
        gainResources usage.user 1

reaperBoltThrower :: CardDef Support
reaperBoltThrower = supportCard "the-deathmaster-s-dance-077" "Reaper Bolt Thrower" do
  race DarkElf
  cost 2
  loyalty 2
  trait Siege
  body
    "Battlefield. Action: Spend 2 resources to deal 2 indirect damage to each opponent. \
    \(Players assign their own indirect damage.)"
  battlefield $ action "Reap" 2 \usage ->
    indirectDamage usage.user.next 2

caughtTheScent :: CardDef Tactic
caughtTheScent = tacticCard "the-deathmaster-s-dance-078" "Caught the Scent" do
  race DarkElf
  cost 2
  loyalty 3
  body
    "Play during your turn. Action: Look at one target opponent's hand. You may choose and \
    \discard one card from that hand."
  playableWhen \g pk -> g.currentPlayer == pk
  whenResolved \self -> do
    let pk = self.controller
        opp = pk.next
    oppPlayer <- playerOf opp <$> getGame
    chooseFromCards pk 0 1 oppPlayer.hand
      "Caught the Scent: the opponent's hand — discard one card (or none)." \chosen ->
        unless (null chosen) $
          push (DiscardCardsFromHand opp (map (.key) chosen))

naggarothSpearmen :: CardDef Unit
naggarothSpearmen = unitCard "the-warpstone-chronicles-096" "Naggaroth Spearmen" do
  race DarkElf
  cost 3
  loyalty 3
  power 1
  hitPoints 2
  trait Warrior
  body
    "Battlefield. Action: Spend X resources to have this unit deal +X damage in combat \
    \until the end of the turn. X is the number of developments in this zone."
  battlefield $ action "Wall of spears" 0 \usage -> do
    g <- getGame
    whenJust (findUnit usage.self.key g) \u -> do
      let x = devsInZone g u
          Resources r = (playerOf usage.user g).resources
      when (x > 0 && r >= x) do
        payResources usage.user x
        until EndOfTurn $ buffCombatDamage u.key x

hydraBlade :: CardDef Support
hydraBlade = supportCard "the-warpstone-chronicles-097" "Hydra Blade" do
  unique
  race DarkElf
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {darkelf} unit. Corrupt that unit. Attached unit gains \
    \{power}{power}. If attached unit would be destroyed, you may pay 2 resources to \
    \(instead of destroying it) leave it in play and remove all damage from it."
  attachmentPower 2
  onEnterPlay \_owner self -> for_ self.attachedTo corrupt
  hostDestroyRansomOf 2

slaverRaid :: CardDef Quest
slaverRaid = questCard "the-warpstone-chronicles-098" "Slaver Raid" do
  race DarkElf
  cost 1
  loyalty 3
  trait QuestTrait
  body
    "Quest. Action: Discard 3 resource tokens from this card to put a unit from an \
    \opponent's discard pile into play, corrupt, in your quest zone. \
    \Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  spendTokens "Raid for slaves" 3 \usage -> do
    let pk = usage.user
        opp = pk.next
    oppPlayer <- playerOf opp <$> getGame
    let units = [c | c <- oppPlayer.discard, isJust (asUnit c.def)]
    chooseFromCards pk 0 1 units
      "Choose a unit from the opponent's discard pile to enslave." \chosen ->
        for_ chosen \c ->
          push (StealUnitFromDiscard pk opp c.key QuestZone True)

slaveDriver :: CardDef Unit
slaveDriver = unitCard "arcane-fire-116" "Slave Driver" do
  race DarkElf
  cost 2
  loyalty 4
  power 1
  hitPoints 1
  trait Warrior
  body
    "Kingdom. Action: Spend 2 resources to choose one target unit with printed cost 2 or \
    \lower. That unit cannot attack or defend this turn."
  kingdom $ action "Crack the whip" 2 \usage ->
    withTarget usage.user (unitWhere \u -> costAtMost 2 u.cardDef) \k -> do
      until EndOfTurn $ disableAttack k
      until EndOfTurn $ disableDefend k

yourWillIsMine :: CardDef Tactic
yourWillIsMine = tacticCard "arcane-fire-117" "Your Will Is Mine" do
  race DarkElf
  cost 10
  loyalty 3
  traits [Epic, Spell]
  body
    "Play only during your turn. Action: Choose a zone. Take control of each opponent's \
    \units in that zone. (Those units move to your corresponding zone.)"
  playableWhen \g pk -> g.currentPlayer == pk
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk MyAnyZone \zk -> do
      g <- getGame
      for_ [u.key | u <- g.units, u.controller /= pk, u.zone == zk] \k ->
        push (TakeControlOfUnit pk k)

witchHagsCurse :: CardDef Support
witchHagsCurse = supportCard "arcane-fire-118" "Witch Hag's Curse" do
  race DarkElf
  cost 1
  loyalty 2
  traits [Attachment, Hex]
  body "Attach to a target unit. Treat attached unit as though its printed text box were blank (except for Traits)."
  blanksAttachedUnit

-- Days of Blood --------------------------------------------------------

chillSeaWatchtower :: CardDef Support
chillSeaWatchtower = supportCard "days-of-blood-004" "Chill Sea Watchtower" do
  race DarkElf
  cost 1
  loyalty 1
  power 1
  trait Building
  body "If you control a non-[Dark Elf] card, sacrifice this card."
  sacrificeIfControlsOffFaction DarkElf

-- Oaths of Vengeance ---------------------------------------------------

vaedraBloodsworn :: CardDef Unit
vaedraBloodsworn = unitCard "oaths-of-vengeance-035" "Vaedra Bloodsworn" do
  unique
  race DarkElf
  cost 3
  loyalty 2
  power 0
  hitPoints 3
  traits [Warrior]
  body
    "Action: When this unit attacks or defends, discard the top card of target opponent's \
    \deck. This unit gains {power} equal to the cost of the discarded card until the end of \
    \the phase."
  onMyAttackOrDefend \_owner self -> drainTopCard self
  where
    drainTopCard :: TriggerM m => UnitDetails -> m ()
    drainTopCard self = do
      let opp = self.controller.next
      oppPlayer <- playerOf opp <$> getGame
      case oppPlayer.deck of
        [] -> pure ()
        (top : _) -> do
          millFromDeck opp 1
          let c = someCardCost top.def
          when (c > 0) $ until EndOfTurn $ buffPower self.key c

-- Glory of Days Past ---------------------------------------------------

markedForDeath :: CardDef Support
markedForDeath = supportCard "glory-of-days-past-078" "Marked for Death" do
  race DarkElf
  cost 0
  loyalty 2
  trait Attachment
  body
    "Attach to a target unit. When attached unit leaves play, attached unit's controller \
    \must discard X cards from the top of his deck. X is the attached unit's cost."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitLeftPlay du
      | Just du.key == self.attachedTo ->
          let x = case du.cardDef.cost of
                Fixed n -> n
                Variable -> 0
           in when (x > 0) $ millFromDeck du.controller x
    _ -> pure ()

hagGraefKnights :: CardDef Unit
hagGraefKnights = unitCard "oaths-of-vengeance-036" "Hag Graef Knights" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Cavalry
  raider 2
  body "Raider 2."

coldOneChampion :: CardDef Unit
coldOneChampion = unitCard "the-ruinous-hordes-096" "Cold One Champion" do
  race DarkElf
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  trait Cavalry
  raider 2
  scout
  body "Raider 2. Scout."

-- Bloodquest: Rising Dawn -----------------------------------------------

towerOfOblivion :: CardDef Support
towerOfOblivion = supportCard "rising-dawn-015" "Tower of Oblivion" do
  race DarkElf
  cost 2
  loyalty 2
  power 1
  body
    "Quest. Action: Discard the top card of your deck to have target unit lose {power} until \
    \the end of the turn. Then, put 1 resource token on this card. (Limit once per turn)."
  quest $ action "Tower of Oblivion" 0 \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used $
      withTarget usage.user AnyUnit \k -> do
        until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
        millFromDeck usage.user 1
        until EndOfTurn $ buffPower k (-1)
        adjustSupportTokens usage.self.key 1

-- Bloodquest: The Accursed Dead -----------------------------------------

treasureThieves :: CardDef Unit
treasureThieves = unitCard "the-accursed-dead-053" "Treasure Thieves" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Sorceror
  body
    "Action: When this unit enters play, discard the top card of your deck to discard the top \
    \card of target opponent's deck. Gain resources equal to the difference in printed cost \
    \between the discarded cards."
  onEnterPlay \owner self -> do
    let pk = self.controller
        opp = pk.next
    oppP <- playerOf opp <$> getGame
    case (owner.deck, oppP.deck) of
      (mine : _, theirs : _) -> do
        millFromDeck pk 1
        millFromDeck opp 1
        let diff = abs (someCardCost mine.def - someCardCost theirs.def)
        when (diff > 0) $ gainResources pk diff
      _ -> pure ()
