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

allCards :: Map CardCode SomeCardDef
allCards =
  Map.fromList
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
    , ("core-081", UnitCardDef servantsOfKhorne)
    , ("core-082", UnitCardDef savageMarauders)
    , ("core-087", UnitCardDef valkiaTheBloody)
    , ("core-092", UnitCardDef bloodthirster)
    , ("core-103", TacticCardDef bloodForTheBloodGod)
    , ("legends-031", UnitCardDef bloodletter)
    , ("path-of-the-zealot-031", UnitCardDef bloodsworn)
    , ("cataclysm-034", UnitCardDef bloodcrusher)
    , ("fragments-of-power-031", UnitCardDef swornOfKhorne)
    , ("the-fourth-waystone-091", UnitCardDef viciousMarauder)
    , ("legends-032", UnitCardDef warhounds)
    , ("path-of-the-zealot-032", QuestCardDef wolvesOfTheNorth)
    , ("the-chaos-moon-032", UnitCardDef doombull)
    , ("faith-and-steel-113", UnitCardDef skulltaker)
    , ("cataclysm-033", UnitCardDef lordOfKhorne)
    , ("the-warpstone-chronicles-094", TacticCardDef berserkFury)
    , ("the-warpstone-chronicles-095", SupportCardDef daemonsword)
    , ("the-eclipse-of-hope-093", SupportCardDef brandedByKhorne)
    , ("omens-of-ruin-013", SupportCardDef markOfChaos)
    , ("the-ruinous-hordes-083", SupportCardDef northernWastes)
    , ("the-inevitable-city-013", SupportCardDef ironThroneroom)
    , ("the-inevitable-city-020", QuestCardDef raidingCamps)
    , ("the-accursed-dead-052", SupportCardDef riftOfBattle)
    , ("cataclysm-037", SupportCardDef riftOfChaos)
    , ("days-of-blood-018", TacticCardDef recklessAttack)
    , ("the-ruinous-hordes-082", QuestCardDef dominionOfChaos)
    -- ----------------------------------------------------------------------
    -- Empire (core-026 to core-050)
    , ("core-026", UnitCardDef spearmenOfWissenland)
    , ("core-027", UnitCardDef stateTroops)
    , ("core-028", UnitCardDef templarOfSigmar)
    , ("core-029", UnitCardDef witchHunter)
    , ("core-030", UnitCardDef greatswordsOfNuln)
    , ("core-031", UnitCardDef knightsPanther)
    , ("core-032", UnitCardDef reiksguard)
    , ("core-033", UnitCardDef rieklandMarksmen)
    , ("core-034", UnitCardDef thyrusGorman)
    , ("core-035", UnitCardDef karlFranz)
    , ("core-036", UnitCardDef volkmarTheGrim)
    , ("core-037", UnitCardDef mariusLeitdorf)
    , ("core-038", UnitCardDef lectorOfSigmar)
    , ("core-039", UnitCardDef imperialEngineers)
    , ("core-040", UnitCardDef pegasusKnights)
    , ("core-041", SupportCardDef theImperialCrown)
    , ("core-042", SupportCardDef hammerOfSigmar)
    , ("core-043", SupportCardDef bannerOfSigmar)
    , ("core-044", SupportCardDef altdorf)
    , ("core-045", QuestCardDef defendingTheEmpire)
    , ("core-046", TacticCardDef forSigmar)
    , ("core-047", TacticCardDef sigmarsWrath)
    , ("core-048", TacticCardDef counterCharge)
    , ("core-049", TacticCardDef battleOfTheReik)
    , ("core-050", TacticCardDef defendersOfTheFaith)
    -- ----------------------------------------------------------------------
    -- High Elf (core-051 to core-075)
    , ("core-051", UnitCardDef phoenixGuard)
    , ("core-052", UnitCardDef whiteLionsOfChrace)
    , ("core-053", UnitCardDef swordmastersOfHoeth)
    , ("core-054", UnitCardDef highElfArchers)
    , ("core-055", UnitCardDef seaGuardOfLothern)
    , ("core-056", UnitCardDef silverHelms)
    , ("core-057", UnitCardDef dragonPrincesOfCaledor)
    , ("core-058", UnitCardDef princeTyrion)
    , ("core-059", UnitCardDef teclis)
    , ("core-060", UnitCardDef eltharionTheGrim)
    , ("core-061", UnitCardDef korhil)
    , ("core-062", UnitCardDef loremasterOfHoeth)
    , ("core-063", UnitCardDef mageOfTheWhiteTower)
    , ("core-064", UnitCardDef spearmenOfLothern)
    , ("core-065", UnitCardDef reaverKnights)
    , ("core-066", SupportCardDef lighthouseOfLothern)
    , ("core-067", SupportCardDef bannerOfAvelorn)
    , ("core-068", SupportCardDef bowOfAvelorn)
    , ("core-069", SupportCardDef hoethsWisdom)
    , ("core-070", QuestCardDef theWhiteTower)
    , ("core-071", TacticCardDef voiceOfCommand)
    , ("core-072", TacticCardDef dragonBreath)
    , ("core-073", TacticCardDef magicOfTheOldOnes)
    , ("core-074", TacticCardDef battleMagic)
    , ("core-075", TacticCardDef sacredIncantations)
    -- ----------------------------------------------------------------------
    -- Chaos (fill the core-083..core-102 gaps)
    , ("core-083", UnitCardDef tzeentchSorcerer)
    , ("core-084", UnitCardDef slaaneshiMarauders)
    , ("core-085", UnitCardDef plaguebearersOfNurgle)
    , ("core-086", UnitCardDef festeringNurglings)
    , ("core-088", UnitCardDef archaonTheEverchosen)
    , ("core-089", UnitCardDef chaosKnights)
    , ("core-090", UnitCardDef chaosWarriors)
    , ("core-091", UnitCardDef maraudersOfTheNorth)
    , ("core-093", UnitCardDef chaosSorcerer)
    , ("core-094", UnitCardDef horrorOfTzeentch)
    , ("core-095", UnitCardDef daemonettesOfSlaanesh)
    , ("core-096", UnitCardDef beastsOfNurgle)
    , ("core-097", UnitCardDef chaosSpawn)
    , ("core-098", SupportCardDef eyeOfTzeentch)
    , ("core-099", SupportCardDef theIronTower)
    , ("core-100", SupportCardDef pyreOfTcharzanek)
    , ("core-101", TacticCardDef tidesOfChaos)
    , ("core-102", TacticCardDef doomOfTheEmpire)
    -- ----------------------------------------------------------------------
    -- Orc (core-106 to core-130)
    , ("core-106", UnitCardDef grimgorIronhide)
    , ("core-107", UnitCardDef skarsnik)
    , ("core-108", UnitCardDef gorbadIronclaw)
    , ("core-109", UnitCardDef orcBigUns)
    , ("core-110", UnitCardDef blackOrcs)
    , ("core-111", UnitCardDef savageOrcs)
    , ("core-112", UnitCardDef orcBoyz)
    , ("core-113", UnitCardDef boarBoyz)
    , ("core-114", UnitCardDef nightGoblins)
    , ("core-115", UnitCardDef goblinWolfRiders)
    , ("core-116", UnitCardDef squigHoppers)
    , ("core-117", UnitCardDef orcShaman)
    , ("core-118", UnitCardDef trolls)
    , ("core-119", UnitCardDef forestGoblinSpiderRiders)
    , ("core-120", UnitCardDef snotlings)
    , ("core-121", SupportCardDef daBadMoon)
    , ("core-122", SupportCardDef choppa)
    , ("core-123", SupportCardDef bigBossesBanner)
    , ("core-124", SupportCardDef daMorksEye)
    , ("core-125", SupportCardDef orcWarmachine)
    , ("core-126", QuestCardDef greenskinRush)
    , ("core-127", TacticCardDef waaagh)
    , ("core-128", TacticCardDef crushEm)
    , ("core-129", TacticCardDef runEmDown)
    , ("core-130", TacticCardDef daBigStomp)
    -- ----------------------------------------------------------------------
    -- Dark Elf (core-131 to core-155)
    , ("core-131", UnitCardDef malekith)
    , ("core-132", UnitCardDef morathi)
    , ("core-133", UnitCardDef croneHellebron)
    , ("core-134", UnitCardDef lokhirFellheart)
    , ("core-135", UnitCardDef witchElves)
    , ("core-136", UnitCardDef blackGuardOfNaggarond)
    , ("core-137", UnitCardDef executioners)
    , ("core-138", UnitCardDef corsairs)
    , ("core-139", UnitCardDef coldOneKnights)
    , ("core-140", UnitCardDef darkRiders)
    , ("core-141", UnitCardDef darkSorceress)
    , ("core-142", UnitCardDef assassinsOfKhaine)
    , ("core-143", UnitCardDef repeaterCrossbowmen)
    , ("core-144", UnitCardDef bloodwrackMedusa)
    , ("core-145", UnitCardDef blackDragon)
    , ("core-146", UnitCardDef manticore)
    , ("core-147", SupportCardDef cauldronOfBlood)
    , ("core-148", SupportCardDef theBlackArk)
    , ("core-149", SupportCardDef whipOfAgony)
    , ("core-150", SupportCardDef druchiiBanner)
    , ("core-151", SupportCardDef witchbrew)
    , ("core-152", QuestCardDef slaughterAtLustria)
    , ("core-153", TacticCardDef khainesEmbrace)
    , ("core-154", TacticCardDef murderousProwess)
    , ("core-155", TacticCardDef coldBloodedSlaughter)
    ]
