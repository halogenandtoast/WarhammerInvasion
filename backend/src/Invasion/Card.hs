{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Public surface for card definitions and the @allCards@ registry.
-- 'SomeCardDef', 'Card', their JSON/Show instances and the
-- 'HasRaces SomeCardDef' instance are defined here so the existing
-- 'Invasion.Card.hs-boot' stays valid; the DSL surface
-- (CardBuilder, EffectM, triggers, action builders, etc.) is
-- re-exported from the submodules listed below.
module Invasion.Card
  ( module Invasion.Card.Effects
  , module Invasion.Card.Builder
  , module Invasion.Card.Triggers
  , module Invasion.Card.Types
  , hasRace
  , allCards
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Invasion.Card.Builder
import Invasion.Card.Defs.Chaos
import Invasion.Card.Defs.DarkElf
import Invasion.Card.Defs.Dwarf
import Invasion.Card.Defs.Empire
import Invasion.Card.Defs.HighElf
import Invasion.Card.Defs.Neutral
import Invasion.Card.Defs.Orc
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.Prelude
import Invasion.Types

-- | True iff the named card carries the given race. Looks the card up
-- in 'allCards'; returns 'False' if the code is unknown (defensive —
-- should never happen for in-play card codes).
hasRace :: Race -> CardCode -> Bool
hasRace r code = case Map.lookup code allCards of
  Just sd -> sd `isRace` r
  Nothing -> False

-- | The registry of every card the engine knows about, keyed by its
-- printed product code (e.g. @core-014@). Mirrors @cards.json@; the
-- two must stay in sync. Currently covers the core set only.
allCards :: Map CardCode SomeCardDef
allCards =
  Map.fromList
    -- Dwarf — core-001..025
    [ ("core-001", UnitCardDef defenderOfTheHold)
    , ("core-002", UnitCardDef zhufbarEngineers)
    , ("core-003", UnitCardDef hammererOfKarakAzul)
    , ("core-004", UnitCardDef trollSlayers)
    , ("core-005", UnitCardDef runesmith)
    , ("core-006", UnitCardDef durgnarTheBold)
    , ("core-007", UnitCardDef kingKazador)
    , ("core-008", UnitCardDef dwarfCannonCrew)
    , ("core-009", UnitCardDef dwarfMasons)
    , ("core-010", UnitCardDef dwarfRanger)
    , ("core-011", UnitCardDef mountainBrigade)
    , ("core-012", UnitCardDef ironbreakersOfAnkhor)
    , ("core-013", SupportCardDef runeOfFortitude)
    , ("core-014", SupportCardDef keystoneForge)
    , ("core-015", SupportCardDef organGun)
    , ("core-016", SupportCardDef masterRuneOfDismay)
    , ("core-017", QuestCardDef aGloriousDeath)
    , ("core-018", SupportCardDef grudgeThrower)
    , ("core-019", TacticCardDef buryingTheGrudge)
    , ("core-020", TacticCardDef stubbornRefusal)
    , ("core-021", TacticCardDef strikingTheGrudge)
    , ("core-022", TacticCardDef grudgeThrowerAssault)
    , ("core-023", TacticCardDef demolition)
    , ("core-024", TacticCardDef wakeTheMountain)
    , ("core-025", TacticCardDef masterRuneOfValaya)
    -- Empire — core-026..050
    , ("core-026", UnitCardDef peasantMilitia)
    , ("core-027", UnitCardDef reiksguardKnights)
    , ("core-028", UnitCardDef brightWizardApprentice)
    , ("core-029", UnitCardDef nulnTinkerers)
    , ("core-030", UnitCardDef theGreatswords)
    , ("core-031", UnitCardDef sigmarsBlessed)
    , ("core-032", UnitCardDef thyrusGorman)
    , ("core-033", UnitCardDef huntsmen)
    , ("core-034", UnitCardDef warriorPriests)
    , ("core-035", UnitCardDef freeCompany)
    , ("core-036", UnitCardDef pistoliers)
    , ("core-037", UnitCardDef johannesBroheim)
    , ("core-038", SupportCardDef knightTraining)
    , ("core-039", SupportCardDef churchOfSigmar)
    , ("core-040", SupportCardDef cityGates)
    , ("core-041", SupportCardDef shrineToTaal)
    , ("core-042", SupportCardDef templeOfShallya)
    , ("core-043", QuestCardDef defendTheBorder)
    , ("core-044", TacticCardDef willOfTheElectors)
    , ("core-045", TacticCardDef twinTailedComet)
    , ("core-046", TacticCardDef demoralise)
    , ("core-047", TacticCardDef franzsDecree)
    , ("core-048", TacticCardDef forcedMarch)
    , ("core-049", TacticCardDef judgementOfVerena)
    , ("core-050", TacticCardDef sigmarsIntervention)
    -- High Elf — core-051..055 (only 5 in core)
    , ("core-051", UnitCardDef silverHelmBrigade)
    , ("core-052", UnitCardDef archmageOfSaphery)
    , ("core-053", SupportCardDef blessingOfIsha)
    , ("core-054", TacticCardDef radiantGaze)
    , ("core-055", TacticCardDef greaterHeal)
    -- Orc — core-056..080
    , ("core-056", UnitCardDef crookedTeefGoblins)
    , ("core-057", UnitCardDef squigHerders)
    , ("core-058", UnitCardDef ironclawsHorde)
    , ("core-059", UnitCardDef followersOfMork)
    , ("core-060", UnitCardDef blackOrcSquad)
    , ("core-061", UnitCardDef boarBoyz)
    , ("core-062", UnitCardDef urguck)
    , ("core-063", UnitCardDef grimgorIronhide)
    , ("core-064", UnitCardDef nightGoblins)
    , ("core-065", UnitCardDef doomDivers)
    , ("core-066", UnitCardDef lobberCrew)
    , ("core-067", UnitCardDef bigUns)
    , ("core-068", SupportCardDef rockLobber)
    , ("core-069", SupportCardDef choppa)
    , ("core-070", SupportCardDef totemOfGork)
    , ("core-071", SupportCardDef bannaOfDaRedSunz)
    , ("core-072", QuestCardDef smashEmAll)
    , ("core-073", SupportCardDef grimgorsCamp)
    , ("core-074", TacticCardDef smashGoBoom)
    , ("core-075", TacticCardDef ripDereEadsOff)
    , ("core-076", TacticCardDef wezBigga)
    , ("core-077", TacticCardDef favourOfMork)
    , ("core-078", TacticCardDef pillage)
    , ("core-079", TacticCardDef waaagh)
    , ("core-080", TacticCardDef trollVomit)
    -- Chaos — core-081..105
    , ("core-081", UnitCardDef servantsOfKhorne)
    , ("core-082", UnitCardDef savageMarauders)
    , ("core-083", UnitCardDef festeringNurglings)
    , ("core-084", UnitCardDef nurgleSorcerer)
    , ("core-085", UnitCardDef chaosKnights)
    , ("core-086", UnitCardDef cultistOfSlaanesh)
    , ("core-087", UnitCardDef valkiaTheBloody)
    , ("core-088", UnitCardDef melekhTheChanger)
    , ("core-089", UnitCardDef fledglingChaosSpawn)
    , ("core-090", UnitCardDef savageGors)
    , ("core-091", UnitCardDef darkZealot)
    , ("core-092", UnitCardDef bloodthirster)
    , ("core-093", SupportCardDef cloudOfFlies)
    , ("core-094", SupportCardDef horrificMutation)
    , ("core-095", SupportCardDef sadisticMutation)
    , ("core-096", SupportCardDef warpstoneMeteor)
    , ("core-097", QuestCardDef journeyToTheGate)
    , ("core-098", SupportCardDef shrineToNurgle)
    , ("core-099", TacticCardDef seducedByDarkness)
    , ("core-100", TacticCardDef willOfTzeentch)
    , ("core-101", TacticCardDef nurglesPestilence)
    , ("core-102", TacticCardDef flamesOfTzeentch)
    , ("core-103", TacticCardDef bloodForTheBloodGod)
    , ("core-104", TacticCardDef cullingTheWeak)
    , ("core-105", TacticCardDef slaaneshsDomination)
    -- Chaos — Cataclysm cycle
    , ("cataclysm-033", UnitCardDef lordOfKhorne)
    , ("faith-and-steel-113", UnitCardDef skulltaker)
    , ("cataclysm-034", UnitCardDef bloodcrusher)
    , ("cataclysm-037", SupportCardDef riftOfChaos)
    , ("days-of-blood-018", TacticCardDef recklessAttack)
    , ("fragments-of-power-031", UnitCardDef swornOfKhorne)
    , ("legends-031", UnitCardDef bloodletter)
    , ("legends-032", UnitCardDef warhounds)
    , ("path-of-the-zealot-031", UnitCardDef bloodsworn)
    , ("path-of-the-zealot-032", QuestCardDef wolvesOfTheNorth)
    , ("the-fourth-waystone-091", UnitCardDef viciousMarauder)
    , ("the-chaos-moon-032", UnitCardDef doombull)
    , ("the-warpstone-chronicles-094", TacticCardDef berserkFury)
    , ("the-warpstone-chronicles-095", SupportCardDef daemonsword)
    , ("the-eclipse-of-hope-093", SupportCardDef brandedByKhorne)
    , ("omens-of-ruin-013", SupportCardDef markOfChaos)
    , ("the-ruinous-hordes-083", SupportCardDef northernWastes)
    , ("the-ruinous-hordes-082", QuestCardDef dominionOfChaos)
    , ("the-inevitable-city-013", SupportCardDef ironThroneroom)
    , ("the-inevitable-city-020", QuestCardDef raidingCamps)
    , ("the-accursed-dead-052", SupportCardDef riftOfBattle)
    -- Dark Elf — core-106..110 (only 5 in core)
    , ("core-106", UnitCardDef discipleOfKhaine)
    , ("core-107", UnitCardDef vileSorceress)
    , ("core-108", UnitCardDef coldOneRiders)
    , ("core-109", SupportCardDef cauldronOfBlood)
    , ("core-110", TacticCardDef hate)
    -- Neutral — core-111..127 (no core-119)
    , ("core-111", SupportCardDef contestedVillage)
    , ("core-112", SupportCardDef contestedFortress)
    , ("core-113", SupportCardDef contestedStronghold)
    , ("core-114", SupportCardDef armoury)
    , ("core-115", SupportCardDef forgottenCemetery)
    , ("core-116", SupportCardDef warpstoneExcavation)
    , ("core-117", TacticCardDef pilgrimage)
    , ("core-118", TacticCardDef burnItDown)
    , ("core-120", QuestCardDef infiltrate)
    , ("core-121", QuestCardDef prepareForWar)
    , ("core-122", SupportCardDef allianceDwarfEmpire)
    , ("core-123", SupportCardDef allianceEmpireHighElf)
    , ("core-124", SupportCardDef allianceDwarfHighElf)
    , ("core-125", SupportCardDef allianceChaosOrc)
    , ("core-126", SupportCardDef allianceChaosDarkElf)
    , ("core-127", SupportCardDef allianceOrcDarkElf)
    ]
