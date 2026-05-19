{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card.Defs.Orc (module Invasion.Card.Defs.Orc) where

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

grimgorIronhide :: CardDef Unit
grimgorIronhide = unitCard "core-106" "Grimgor Ironhide" do
  unique
  race Orc
  cost 6
  loyalty 5
  power 4
  hitPoints 5
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. Your other Orc units gain {power} while attacking."

skarsnik :: CardDef Unit
skarsnik = unitCard "core-107" "Skarsnik" do
  unique
  race Orc
  cost 4
  loyalty 3
  power 2
  hitPoints 3
  traits [Hero, Warrior]
  -- FAQ 2.2: "After" → "When".
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit enters play, search the top 3 cards of your deck for a Goblin unit and put it into play. Shuffle your deck."

gorbadIronclaw :: CardDef Unit
gorbadIronclaw = unitCard "core-108" "Gorbad Ironclaw" do
  unique
  race Orc
  cost 5
  loyalty 4
  power 3
  hitPoints 4
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit damages a zone, deal 1 additional damage to that zone."
  -- Approximated as a flat +1 to its own combat damage while
  -- attacking; spillover to the zone scales naturally with that.
  combatPower \g u -> if unitIsAttacking g u then 1 else 0

orcBigUns :: CardDef Unit
orcBigUns = unitCard "core-109" "Orc Big 'Uns" do
  race Orc
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]

blackOrcs :: CardDef Unit
blackOrcs = unitCard "core-110" "Black Orcs" do
  race Orc
  cost 5
  loyalty 3
  power 4
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

savageOrcs :: CardDef Unit
savageOrcs = unitCard "core-111" "Savage Orcs" do
  race Orc
  cost 3
  loyalty 1
  power 3
  hitPoints 2
  trait Warrior
  body "Battlefield only."
  keyword BattlefieldOnly

orcBoyz :: CardDef Unit
orcBoyz = unitCard "core-112" "Orc Boyz" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

boarBoyz :: CardDef Unit
boarBoyz = unitCard "core-113" "Boar Boyz" do
  race Orc
  cost 3
  loyalty 2
  power 3
  hitPoints 2
  traits [Warrior, Cavalry]
  body "Battlefield only."
  keyword BattlefieldOnly

nightGoblins :: CardDef Unit
nightGoblins = unitCard "core-114" "Night Goblins" do
  race Orc
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior

goblinWolfRiders :: CardDef Unit
goblinWolfRiders = unitCard "core-115" "Goblin Wolf Riders" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  traits [Warrior, Cavalry]
  body "Scout."
  scout

squigHoppers :: CardDef Unit
squigHoppers = unitCard "core-116" "Squig Hoppers" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Creature

orcShaman :: CardDef Unit
orcShaman = unitCard "core-117" "Orc Shaman" do
  race Orc
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Action: Spend 2 resources to deal 2 damage to a target unit; deal 1 damage to this unit."
  actionEnemyUnit "Sorcery" 2 \u k -> do
    dealDamage k 2
    dealUncancellableDamage u.self.key 1

trolls :: CardDef Unit
trolls = unitCard "core-118" "Trolls" do
  race Orc
  cost 4
  loyalty 2
  power 3
  hitPoints 4
  trait Creature
  body "Forced: At the beginning of your turn, heal 1 damage from this unit."
  onMyTurnBegin \_owner self -> healUnit self.key 1

forestGoblinSpiderRiders :: CardDef Unit
forestGoblinSpiderRiders = unitCard "core-119" "Forest Goblin Spider Riders" do
  race Orc
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  traits [Warrior, Cavalry]
  body "Scout."
  scout

snotlings :: CardDef Unit
snotlings = unitCard "core-120" "Snotlings" do
  race Orc
  cost 0
  loyalty 1
  power 1
  hitPoints 1
  trait Creature

daBadMoon :: CardDef Support
daBadMoon = supportCard "core-121" "Da Bad Moon" do
  unique
  race Orc
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Battlefield. Your other Orc units gain {power} while attacking."
  -- "Battlefield." prefix scopes the aura to the controller's
  -- battlefield. The aura fires on combat damage, not on resting
  -- power, so it's a 'supportCombat' bonus.
  supportCombat \g s u ->
    if s.controller == u.controller
       && s.zone == BattlefieldZone
       && Orc `elem` u.cardDef.races
       && unitIsAttacking g u
       then 1
       else 0

choppa :: CardDef Support
choppa = supportCard "core-122" "Choppa" do
  race Orc
  cost 1
  loyalty 1
  traits [Attachment, Weapon]
  body "Attach to a target Orc unit. Attached unit gains {power}{power}."
  attachmentPower 2

bigBossesBanner :: CardDef Support
bigBossesBanner = supportCard "core-123" "Big Boss's Banner" do
  race Orc
  cost 2
  loyalty 2
  trait Attachment
  body "Attach to a target Orc unit. While attached unit is attacking, your other Orc attackers gain {power}."
  -- The banner buffs the host's other Orc attackers — never its own
  -- host. Grant +1 to any friendly Orc attacker different from the
  -- host while the host is itself attacking.
  supportCombat \g s u ->
    case s.attachedTo of
      Just hostKey
        | hostKey /= u.key
        , s.controller == u.controller
        , Orc `elem` u.cardDef.races
        , unitIsAttacking g u
        , Just host <- findUnit hostKey g
        , unitIsAttacking g host ->
            1
      _ -> 0

daMorksEye :: CardDef Support
daMorksEye = supportCard "core-124" "Da Mork's Eye" do
  race Orc
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, deal 1 damage to one target enemy zone."
  kingdom $ forced \self ->
    onTurnBegin self.controller $
      withTarget self.controller enemyCapital \(owner, z) ->
        dealZoneDamage owner z 1

orcWarmachine :: CardDef Support
orcWarmachine = supportCard "core-125" "Orc Warmachine" do
  race Orc
  cost 3
  loyalty 2
  trait Siege
  body "Battlefield. Action: Sacrifice a unit to deal 2 damage to a target zone."

greenskinRush :: CardDef Quest
greenskinRush = questCard "core-126" "Greenskin Rush" do
  race Orc
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 resource token here if a unit is questing here.\n\
    \Action: Spend 2 tokens to put a Goblin unit from your hand into play."
  forced accrueTokenWhileQuesting
  spendTokens "Summon a Goblin unit" 2 \u -> do
    me <- playerOf u.user <$> getGame
    let goblinsInHand = filter (cardIs goblinCodes) me.hand
    chooseFromCards u.user 0 1 goblinsInHand
      "Choose a Goblin unit to put into play." \chosen ->
        for_ chosen \c ->
          putUnitIntoPlay u.user FromHand c.key BattlefieldZone
  where
    goblinCodes =
      [ CardCode "core-114"  -- Night Goblins
      , CardCode "core-115"  -- Goblin Wolf Riders
      , CardCode "core-119"  -- Forest Goblin Spider Riders
      , CardCode "core-120"  -- Snotlings
      ]
    cardIs codes c = case c.def of
      UnitCardDef cd -> cd.code `elem` codes
      _ -> False

waaagh :: CardDef Tactic
waaagh = tacticCard "core-127" "Waaagh!" do
  race Orc
  cost 2
  loyalty 2
  body "Action: Each of your Orc units gains {power}{power} until the end of the turn."
  playableWhen $ hasTarget ownOrcs
  whenResolved \self -> buffEachUntilEoT self.controller ownOrcs 2
  where
    ownOrcs = UnitMatching \pk _ u -> u.controller == pk && u `isRace` Orc

crushEm :: CardDef Tactic
crushEm = tacticCard "core-128" "Crush 'Em" do
  race Orc
  cost 1
  loyalty 1
  body "Action: Target unit gains {power}{power}{power} until the end of the turn; sacrifice it at end of turn."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      until EndOfTurn $ buffPower k 3
      queueEoTDamage k 99

runEmDown :: CardDef Tactic
runEmDown = tacticCard "core-129" "Run 'Em Down" do
  race Orc
  cost 2
  loyalty 1
  body "Action: Deal 1 damage to each enemy unit in target zone."
  playableWhen $ hasTarget enemyCapital
  whenResolved \self ->
    withTarget self.controller enemyCapital \(_, z) ->
      damageEachEnemyInZone self.controller z 1

daBigStomp :: CardDef Tactic
daBigStomp = tacticCard "core-130" "Da Big Stomp" do
  race Orc
  cost 3
  loyalty 2
  body "Action: Destroy target support card or development."
  playableWhen hasEnemySupport
  tacticTargets SupportTargetSchema
  onResolveEnemySupport \_pk s -> destroySupport s.key
