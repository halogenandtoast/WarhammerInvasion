{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Neutral core cards (core-111..127, no core-119). 16 cards.
-- Neutrals don't belong to a faction (no 'race' call), so they
-- contribute no race symbol toward loyalty and pay the full loyalty
-- surcharge — which for every core neutral is 0 since they're
-- printed with loyalty 0.
module Invasion.Card.Defs.Neutral (module Invasion.Card.Defs.Neutral) where

import Invasion.Capital
import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

contestedVillage :: CardDef Support
contestedVillage = supportCard "core-111" "Contested Village" do
  cost 1
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn)."

contestedFortress :: CardDef Support
contestedFortress = supportCard "core-112" "Contested Fortress" do
  cost 3
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn). Cancel 1 damage to your capital each turn."
  onMyTurnBegin \_owner self -> scheduleCapitalShield self.controller

contestedStronghold :: CardDef Support
contestedStronghold = supportCard "core-113" "Contested Stronghold" do
  cost 4
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn). This support gains {power} for each of your developments in this zone."
  zonePowerAura \g self zk ->
    if zk == self.zone
      then let me = playerOf self.controller g
               Developments n = zoneDevs me.capital zk
            in n
      else 0

armoury :: CardDef Support
armoury = supportCard "core-114" "Armoury" do
  cost 2
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn). Kingdom. This card gains {power} while you have at least two developments in this zone."
  zonePowerAura \g self zk ->
    if zk == self.zone && self.zone == KingdomZone
      then let me = playerOf self.controller g
               Developments n = zoneDevs me.capital zk
            in if n >= 2 then 1 else 0
      else 0

forgottenCemetery :: CardDef Support
forgottenCemetery = supportCard "core-115" "Forgotten Cemetery" do
  cost 2
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn). Quest. This card gains {power} while you have at least two developments in this zone."
  zonePowerAura \g self zk ->
    if zk == self.zone && self.zone == QuestZone
      then let me = playerOf self.controller g
               Developments n = zoneDevs me.capital zk
            in if n >= 2 then 1 else 0
      else 0

warpstoneExcavation :: CardDef Support
warpstoneExcavation = supportCard "core-116" "Warpstone Excavation" do
  cost 0
  loyalty 0
  power 1
  trait Warpstone
  body "Your units enter this zone corrupted."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk uk
      | pk == self.controller -> do
          g <- getGame
          case findUnit uk g of
            Just u | u.zone == self.zone -> push (CorruptUnit uk)
            _ -> pure ()
    _ -> pure ()

pilgrimage :: CardDef Tactic
pilgrimage = tacticCard "core-117" "Pilgrimage" do
  cost 4
  loyalty 0
  body "Lower the cost to play this card by 1 for each development in your quest zone. Action: Return one target unit to its owner's hand."
  selfCostAdjust \g pk ->
    let me = playerOf pk g
        Developments n = me.capital.quest.developments
     in negate n
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \k -> returnUnitToHand k

burnItDown :: CardDef Tactic
burnItDown = tacticCard "core-118" "Burn It Down" do
  cost 2
  loyalty 0
  body "Action: Destroy one target support card with printed cost X or lower. X is the number of developments in your battlefield."
  whenResolved \self -> do
    g <- getGame
    let pk = self.controller
        me = playerOf pk g
        Developments x = me.capital.battlefield.developments
        eligible s = case s.cardDef.cost of
          Fixed c -> c <= x
          Variable -> False
        candidates = [s | s <- g.supports, eligible s, s.attachedTo == Nothing]
    case candidates of
      [] -> pure ()
      (s : _) -> destroySupport s.key

infiltrate :: CardDef Quest
infiltrate = questCard "core-120" "Infiltrate!" do
  cost 2
  loyalty 0
  body "Quest. Forced: At the beginning of your turn, discard the top X cards from each opponent's deck. X is the number of resource tokens on this quest. Quest. Forced: At the beginning of your turn, place 1 resource token on this quest if a unit is questing here."
  forced accrueTokenWhileQuesting
  onMyTurnBegin \_owner self -> do
    g <- getGame
    case findQuest self.key g of
      Just q | q.tokens > 0 -> millFromDeck self.controller.next q.tokens
      _ -> pure ()

prepareForWar :: CardDef Quest
prepareForWar = questCard "core-121" "Prepare for War!" do
  cost 0
  loyalty 0
  body "Quest. Action: Sacrifice the unit on this quest to shuffle X target cards from your discard pile into your deck, where X is the number of resource tokens on this quest. Quest. Forced: At the beginning of your turn, place 1 resource token on this quest if a unit is questing here."
  forced accrueTokenWhileQuesting
  action "March to war" 0 \usage ->
    withQuest usage.self.key \q ->
      for_ q.questingUnit \quester -> do
        destroyUnit quester
        recycleDiscard usage.user q.tokens

-- Alliance supports list BOTH races they support so the engine's
-- 'raceSymbolCount' (which reads 'cardDef.races') credits the
-- controller with one symbol of each. That's how the printed
-- "provides both X and Y loyalty symbols" rider takes effect with no
-- additional engine work.

allianceDwarfEmpire :: CardDef Support
allianceDwarfEmpire = supportCard "core-122" "Alliance - Dwarf and Empire" do
  race Dwarf
  race Empire
  cost 2
  loyalty 0
  power 1
  trait Banner
  orderOnly
  body "Order only. (This Alliance provides both a Dwarf and an Empire loyalty symbol.)"

allianceEmpireHighElf :: CardDef Support
allianceEmpireHighElf = supportCard "core-123" "Alliance - Empire and High Elf" do
  race Empire
  race HighElf
  cost 2
  loyalty 0
  power 1
  trait Banner
  orderOnly
  body "Order only. (This Alliance provides both an Empire and a High Elf loyalty symbol.)"

allianceDwarfHighElf :: CardDef Support
allianceDwarfHighElf = supportCard "core-124" "Alliance - Dwarf and High Elf" do
  race Dwarf
  race HighElf
  cost 2
  loyalty 0
  power 1
  trait Banner
  orderOnly
  body "Order only. (This Alliance provides both a Dwarf and a High Elf loyalty symbol.)"

allianceChaosOrc :: CardDef Support
allianceChaosOrc = supportCard "core-125" "Alliance - Chaos and Orc" do
  race Chaos
  race Orc
  cost 2
  loyalty 0
  power 1
  trait Banner
  destructionOnly
  body "Destruction only. (This Alliance provides both a Chaos and an Orc loyalty symbol.)"

allianceChaosDarkElf :: CardDef Support
allianceChaosDarkElf = supportCard "core-126" "Alliance - Chaos and Dark Elf" do
  race Chaos
  race DarkElf
  cost 2
  loyalty 0
  power 1
  trait Banner
  destructionOnly
  body "Destruction only. (This alliance provides both a Chaos and a Dark Elf loyalty symbol.)"

allianceOrcDarkElf :: CardDef Support
allianceOrcDarkElf = supportCard "core-127" "Alliance - Orc and Dark Elf" do
  race Orc
  race DarkElf
  cost 2
  loyalty 0
  power 1
  trait Banner
  destructionOnly
  body "Destruction only. (This Alliance provides both an Orc and a Dark Elf loyalty symbol.)"

-- | Helper: developments count in the named zone of a capital.
zoneDevs :: Capital -> ZoneKind -> Developments
zoneDevs cap = \case
  KingdomZone -> cap.kingdom.developments
  QuestZone -> cap.quest.developments
  BattlefieldZone -> cap.battlefield.developments
