{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card (module Invasion.Card) where

import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Invasion.CardDef
import Invasion.CardDef qualified as CardDef
import {-# SOURCE #-} Invasion.Engine (HasPromptIO (..))
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Capital
import Invasion.Game
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

data SomeCardDef where
  UnitCardDef :: CardDef 'Unit -> SomeCardDef
  SupportCardDef :: CardDef 'Support -> SomeCardDef
  QuestCardDef :: CardDef 'Quest -> SomeCardDef
  TacticCardDef :: CardDef 'Tactic -> SomeCardDef
  LegendCardDef :: CardDef 'Legend -> SomeCardDef

instance Show SomeCardDef where
  show (UnitCardDef card) = show card
  show (SupportCardDef card) = show card
  show (QuestCardDef card) = show card
  show (TacticCardDef card) = show card
  show (LegendCardDef card) = show card

instance ToJSON SomeCardDef where
  toJSON (UnitCardDef card) = toJSON card
  toJSON (SupportCardDef card) = toJSON card
  toJSON (QuestCardDef card) = toJSON card
  toJSON (TacticCardDef card) = toJSON card
  toJSON (LegendCardDef card) = toJSON card

-- | A card instance: a definition paired with the stable 'UnitKey' that
-- identifies this specific copy across the entire game. The same key is
-- minted when the card is shuffled into the deck, carried through the
-- hand, into play (as a 'UnitDetails' / 'SupportDetails' / …), and back
-- into discard if the card leaves play. Keys never recycle.
--
-- The frontend uses the key as a CSS view-transition name so a card
-- visually morphs from one location to another as state updates.
data Card = Card
  { key :: UnitKey
  , def :: SomeCardDef
  }
  deriving stock Show

-- | Flatten the card definition into a JSON object and splice in the
-- 'key' field, so the wire shape is the existing 'CardDef' surface
-- (code, title, traits, …) plus a stable integer key. Falls back to a
-- tagged object if the inner JSON is somehow not an object (defensive;
-- 'CardDef' always serializes as one today).
instance ToJSON Card where
  toJSON c = case toJSON c.def of
    Object o -> Object (KeyMap.insert (AesonKey.fromString "key") (toJSON c.key) o)
    other -> object ["key" .= c.key, "def" .= other]

-- | Convenience: lift a definition to a 'Card' with the given key.
mkCard :: UnitKey -> SomeCardDef -> Card
mkCard k d = Card {key = k, def = d}

newtype CardBuilder k a = CardBuilder (State (CardDef k) a)
  deriving newtype (Functor, Applicative, Monad, MonadState (CardDef k))

emptyCardDef :: CardCode -> String -> CardKind -> CardDef k
emptyCardDef code title kind =
  CardDef code title kind [] (Fixed 0) 0 0 Nothing [] Nothing Nothing [] False [] noReceive

unit :: CardCode -> String -> CardBuilder Unit () -> CardDef Unit
unit code title = buildCard $ (emptyCardDef code title Unit) {CardDef.hitPoints = Just (Fixed 1)}

support :: CardCode -> String -> CardBuilder Support a -> CardDef Support
support code title = buildCard $ emptyCardDef code title Support

quest :: CardCode -> String -> CardBuilder Quest a -> CardDef Quest
quest code title = buildCard $ emptyCardDef code title Quest

tactic :: CardCode -> String -> CardBuilder Tactic a -> CardDef Tactic
tactic code title = buildCard $ emptyCardDef code title Tactic

-- | Legends are persistent like units (they have hit points and live on
-- the capital board) but they're their own card type and are not
-- targeted by unit/support/tactic effects. The HP default of 1 mirrors
-- 'unit'; legend cards will normally override via 'hitPoints'.
legend :: CardCode -> String -> CardBuilder Legend () -> CardDef Legend
legend code title = buildCard $ (emptyCardDef code title Legend) {CardDef.hitPoints = Just (Fixed 1)}

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

-- | Builder setter for a card's bespoke 'Receive' handler. The default
-- (set by 'emptyCardDef') is 'noReceive' — a no-op.
onReceive :: Receive k -> CardBuilder k ()
onReceive r = modify \cardDef -> cardDef {receive = r}

-- | Append an 'ActionDef' to the card's action list. Multiple actions
-- can be declared; the engine surfaces them to the client by index.
action :: ActionDef k -> CardBuilder k ()
action a = modify \cardDef -> cardDef {actions = cardDef.actions ++ [a]}

-- | Convenience builder for declaring a tactic's target schema. The
-- effect closure isn't used for tactics — the engine fires the
-- card's 'receive' with 'TacticResolved'; the action metadata is
-- there purely for client display and target validation.
tacticTargets :: TargetSchema -> CardBuilder Tactic ()
tacticTargets schema = action ActionDef
  { actionName = "Play"
  , actionCost = 0  -- the actual cost lives on CardDef.cost
  , actionTarget = schema
  , actionEffect = ActionEffect \_pk _ctx _tgt -> pure ()
  }

-- | First in-play unit controlled by an opponent of 'pk'. Used as an
-- auto-target placeholder when the supplied 'ActionTarget' is
-- 'NoTarget' or invalid.
firstEnemyUnit :: PlayerKey -> Game -> Maybe UnitDetails
firstEnemyUnit pk g = case filter (\u -> u.controller /= pk) g.units of
  (u : _) -> Just u
  [] -> Nothing

-- | Resolve an enemy-unit target: prefer the explicit pick if it
-- references an in-play opposing unit; otherwise fall back to the
-- first enemy in play. Returns 'Nothing' if neither is available.
resolveEnemyUnit :: PlayerKey -> ActionTarget -> Game -> Maybe UnitDetails
resolveEnemyUnit pk t g = case t of
  TargetUnit k -> case findUnit k g of
    Just u | u.controller /= pk -> Just u
    _ -> firstEnemyUnit pk g
  _ -> firstEnemyUnit pk g

-- | Resolve a friendly-unit target with the same fallback discipline
-- as 'resolveEnemyUnit'.
resolveFriendlyUnit :: PlayerKey -> ActionTarget -> Game -> Maybe UnitDetails
resolveFriendlyUnit pk t g = case t of
  TargetUnit k -> case findUnit k g of
    Just u | u.controller == pk -> Just u
    _ -> firstMine
  _ -> firstMine
  where
    firstMine = case filter (\u -> u.controller == pk) g.units of
      (u : _) -> Just u
      [] -> Nothing

-- | Resolve a zone target: prefer the explicit pick; otherwise fall
-- back to the opponent's most-damaged unburned zone.
resolveEnemyZone :: PlayerKey -> ActionTarget -> Game -> Maybe (PlayerKey, ZoneKind)
resolveEnemyZone pk t g = case t of
  TargetZone owner z | owner /= pk -> Just (owner, z)
  _ ->
    let opp = case pk of
          Player1 -> g.player2
          Player2 -> g.player1
        scored =
          [ (d, z)
          | (z, zL) <-
              [ (KingdomZone, opp.capital.kingdom)
              , (QuestZone, opp.capital.quest)
              , (BattlefieldZone, opp.capital.battlefield)
              ]
          , not zL.burned
          , let Damage d = zL.damage
          ]
     in case scored of
          [] -> Nothing
          xs ->
            let (_, z) = maximum xs
             in Just (pk.next, z)

-- | Resolve an enemy-support target: prefer the explicit pick;
-- otherwise fall back to the first enemy support in play.
resolveEnemySupport :: PlayerKey -> ActionTarget -> Game -> Maybe SupportDetails
resolveEnemySupport pk t g = case t of
  TargetSupport k -> case findSupport k g of
    Just s | s.controller /= pk -> Just s
    _ -> fallback
  _ -> fallback
  where
    fallback = case filter (\s -> s.controller /= pk) g.supports of
      (s : _) -> Just s
      [] -> Nothing

-- | First unit controlled by 'pk' sitting in the given zone.
firstUnitOfInZone :: PlayerKey -> ZoneKind -> Game -> Maybe UnitDetails
firstUnitOfInZone pk z g =
  case filter (\u -> u.controller == pk && u.zone == z) g.units of
    (u : _) -> Just u
    [] -> Nothing

-- | Look up an in-play unit by its 'UnitKey'. Mirror of the Engine-side
-- helper so card receive bodies can resolve their attachment hosts
-- without importing 'Invasion.Engine'.
findUnit :: UnitKey -> Game -> Maybe UnitDetails
findUnit ukey g = case filter (\u -> u.key == ukey) g.units of
  (u : _) -> Just u
  [] -> Nothing

-- | Look up an in-play free-standing support by key.
findSupport :: UnitKey -> Game -> Maybe SupportDetails
findSupport skey g = case filter (\s -> s.key == skey) g.supports of
  (s : _) -> Just s
  [] -> Nothing

-- | Look up an in-play quest by key.
findQuest :: UnitKey -> Game -> Maybe QuestDetails
findQuest qkey g = case filter (\q -> q.key == qkey) g.quests of
  (q : _) -> Just q
  [] -> Nothing

-- | Look up an in-play legend by key.
findLegend :: UnitKey -> Game -> Maybe LegendDetails
findLegend lkey g = case filter (\l -> l.key == lkey) g.legends of
  (l : _) -> Just l
  [] -> Nothing

-- | The legend currently in play for the given player, if any. Each
-- player may control at most one legend at a time.
legendOf :: PlayerKey -> Game -> Maybe LegendDetails
legendOf pk g = case filter (\l -> l.controller == pk) g.legends of
  (l : _) -> Just l
  [] -> Nothing

-- | Total number of currently-burning zones across both capitals. Used
-- by Chaos cards that scale with burning (Bloodcrusher, Lord of Khorne,
-- Rift of Chaos, Durgnar). Not yet wired into cost / power calculation
-- — see the per-card TODOs.
burningZoneCount :: Game -> Int
burningZoneCount g =
  length
    [ ()
    | p <- [g.player1, g.player2]
    , z <- p.capital.zones
    , z.burning
    ]

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

-- The +2-power-while-≥2-developments effect is wired in
-- 'Invasion.Engine.selfScalingPowerBonus'; recomputeUnitStats
-- consults it after every message so the bonus stays in sync with
-- the live development count.
trollSlayers :: CardDef Unit
trollSlayers = unit "core-004" "Troll Slayers" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Slayer
  body
    "Battlefield. This unit gains {power}{power} while you have at least two developments in this zone."

runesmith :: CardDef Unit
runesmith = unit "core-005" "Runesmith" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Priest
  body
    "Quest. Action: Spend 2 resources to have a target unit gain {power} until the end of the turn."
  action ActionDef
    { actionName = "Buff a unit"
    , actionCost = 2
    , actionTarget = AnyUnitTargetSchema
    , actionEffect = ActionEffect \_pk _self target -> case target of
        TargetUnit k ->
          push
            ( InstallModifier
                (UnitRef k)
                (Modifier (GainPower 1) UntilEndOfTurn)
            )
        _ -> pure ()
    }

-- The +2-power-while-burning effect is wired in
-- 'Invasion.Engine.selfScalingPowerBonus'.
durgnarTheBold :: CardDef Unit
durgnarTheBold = unit "core-006" "Durgnar the Bold" do
  unique
  race Dwarf
  cost 3
  loyalty 3
  power 2
  hitPoints 2
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\nThis unit gains {power}{power} while one section of your capital is burning."

kingKazador :: CardDef Unit
kingKazador = unit "core-007" "King Kazador" do
  unique
  race Dwarf
  cost 6
  loyalty 5
  power 3
  hitPoints 6
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\nToughness 2.\nOpponents cannot target this unit with card effects unless they pay an additional 3 resources per effect."
  toughness 2

dwarfCannonCrew :: CardDef Unit
dwarfCannonCrew = unit "core-008" "Dwarf Cannon Crew" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Engineer
  body
    "Forced: When this unit enters play, search the top five cards of your deck for a support card with cost 2 or lower. You may put that card into this zone. Then, shuffle your deck."
  onReceive $ Receive \msg owner self -> case msg of
    UnitEnteredPlay pk ukey | pk == self.controller && ukey == self.key -> do
      let scanFor =
            [ c.key
            | c <- take 5 owner.deck
            , SupportCardDef cd <- [c.def]
            , Fixed n <- [cd.cost]
            , n <= 2
            ]
      case scanFor of
        (sKey : _) -> do
          -- Pull the picked card to the front of the deck (so the
          -- engine's hand-or-deck lookups can find it), then play it
          -- into this unit's zone via the existing PlaySupport path.
          push (PlaySupportFromDeck self.controller sKey self.zone)
          push (ShuffleDeck self.controller)
        [] ->
          push (ShuffleDeck self.controller)
    _ -> pure ()

dwarfMasons :: CardDef Unit
dwarfMasons = unit "core-009" "Dwarf Masons" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Engineer
  body
    "Forced: After this unit enters play, put the top card of your deck facedown into this zone as a development."
  flavor "Put your faith in the rock, lad."
  -- AddDevelopment doesn't actually consume the top deck card today
  -- (deck draws happen via Draw; placing as a facedown dev just bumps
  -- the developments counter). Close enough for the +1 HP effect; the
  -- consumed-card bookkeeping can land later.
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey | pk == self.controller && ukey == self.key ->
      push (AddDevelopment self.controller self.zone)
    _ -> pure ()

dwarfRanger :: CardDef Unit
dwarfRanger = unit "core-010" "Dwarf Ranger" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Ranger
  scout
  body
    "Scout.\nQuest. Forced: When one of your other {dwarf} units leaves play, deal I damage to one target unit or capital."
  -- Auto-target the first enemy unit. We have no "deal to capital" auto
  -- choice yet; pick a unit if any, otherwise no-op.
  onReceive $ Receive \msg _owner self -> case msg of
    UnitLeftPlay leftBy ukey _zone _code
      | leftBy == self.controller
      , ukey /= self.key -> do
          g <- getGame
          case firstEnemyUnit self.controller g of
            Just target -> push (DealDamageToUnit target.key 1)
            Nothing -> pure ()
    _ -> pure ()

mountainBrigade :: CardDef Unit
mountainBrigade = unit "core-011" "Mountain Brigade" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 6
  trait Warrior
  flavor "These stout dwarfs are the first line of defence."

ironbreakersOfAnkhor :: CardDef Unit
ironbreakersOfAnkhor = unit "core-012" "Ironbreakers of Ankhor" do
  race Dwarf
  cost 5
  loyalty 2
  power 2
  hitPoints 3
  traits [Warrior, Elite]
  toughnessX
  body
    "Toughness X (whenever this unit is assigned damage, cancel X of that damage).\nX is the number of developments in this zone."

runeOfFortitude :: CardDef Support
runeOfFortitude = support "core-013" "Rune of Fortitude" do
  race Dwarf
  cost 2
  loyalty 1
  trait Rune
  body "Each unit attacking this zone loses {power} unless its controller pays 1 resource per unit."

keystoneForge :: CardDef Support
keystoneForge = support "core-014" "Keystone Forge" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, heal 1 damage to your capital."
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k
      | k == self.controller
      , self.zone == KingdomZone ->
          push (HealCapital self.controller 1)
    _ -> pure ()

organGun :: CardDef Support
organGun = support "core-015" "Organ Gun" do
  race Dwarf
  cost 0
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target unit.\n Attached unit gains {power}{power} while defending."
  flavor "A dwarf device deadly and reliable forever."

masterRuneOfDismay :: CardDef Support
masterRuneOfDismay = support "core-016" "Master Rune of Dismay" do
  race Dwarf
  cost 4
  loyalty 3
  trait Rune
  body "Kingdom. Opponent's units cost 1 additional resource to play."
  flavor "Enemies of the dwarfs beware, your fears are returned to you a hundredfold!"

aGloriousDeath :: CardDef Quest
aGloriousDeath = quest "core-017" "A Glorious Death" do
  race Dwarf
  cost 0
  loyalty 2
  body
    "Quest. Action: Sacrifice the unit on this quest to destroy up to two target attacking units. Use this ability only if A Glorious Death has 3 or more resource tokens on it.\nQuest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  -- Forced: tick a token at controller-turn-begin when a unit is
  -- questing here.
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k | k == self.controller -> do
      g <- getGame
      case findQuest self.key g of
        Just q | q.questingUnit /= Nothing ->
          push (AdjustQuestTokens self.key 1)
        _ -> pure ()
    _ -> pure ()
  -- Action: sacrifice the questing unit + destroy up to 2 attackers.
  -- Gated on >= 3 tokens.
  action ActionDef
    { actionName = "Glorious sacrifice"
    , actionCost = 0
    , actionTarget = NoTargetSchema
    , actionEffect = ActionEffect \pk self _tgt -> do
        g <- getGame
        case findQuest self.key g of
          Just q
            | q.tokens >= 3
            , Just questerKey <- q.questingUnit -> do
                push (DestroyUnit questerKey)
                case g.combat of
                  Just cs | cs.attackingPlayer /= pk ->
                    traverse_
                      (\k -> push (DestroyUnit k))
                      (take 2 cs.attackers)
                  _ -> pure ()
                push (AdjustQuestTokens self.key (-3))
          _ -> pure ()
    }

grudgeThrower :: CardDef Support
grudgeThrower = support "core-018" "Grudge Thrower" do
  race Dwarf
  cost 1
  loyalty 2
  trait Siege
  body
    "Battlefield. Action: Spend 1 resource and sacrifice a unit to have each attacking or defending unit gain {power} until the end of the turn."

buryingTheGrudge :: CardDef Tactic
buryingTheGrudge = tactic "core-019" "Burying the Grudge" do
  race Dwarf
  cost 0
  loyalty 2
  body "Action: Gain 1 resource for each unit that entered a discard pile this turn."
  flavor "Grudges are best buried with the corpse of the wrongdoer."
  -- Read Game.unitsDiscardedThisTurn and credit the controller's
  -- resource pool. No 'AddResources' message exists yet, so we mutate
  -- the player record directly via the dispatch hook.
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      let gain = g.unitsDiscardedThisTurn
      when (gain > 0) $
        push (GainResources pk gain)
    _ -> pure ()

stubbornRefusal :: CardDef Tactic
stubbornRefusal = tactic "core-020" "Stubborn Refusal" do
  race Dwarf
  cost 2
  loyalty 1
  body
    "Action: Move all damage from one target unit to another target unit in any player's corresponding zone."
  -- Auto-target: move damage from the controller's most-damaged unit
  -- onto the first enemy unit in the same corresponding zone.
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      let mine =
            [ u
            | u <- g.units
            , u.controller == pk
            , let Damage d = u.damage
            , d > 0
            ]
      case mine of
        (src : _) ->
          case
            [ v
            | v <- g.units
            , v.controller /= pk
            , v.zone == src.zone
            ]
            of
              (dst : _) -> push (MoveAllDamage src.key dst.key)
              [] -> pure ()
        [] -> pure ()
    _ -> pure ()

strikingTheGrudge :: CardDef Tactic
strikingTheGrudge = tactic "core-021" "Striking the Grudge" do
  race Dwarf
  cost 1
  loyalty 3
  body
    "Action: One target attacking or defending unit gains {power}{power} until the end of the turn."
  flavor "Honour redeemed, oaths fulfilled."
  -- Auto-target the first friendly combatant.
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      case g.combat of
        Just cs ->
          let mine =
                [ k
                | k <-
                    if cs.attackingPlayer == pk
                      then cs.attackers
                      else cs.defenders
                ]
           in case mine of
                (t : _) ->
                  push
                    ( InstallModifier
                        (UnitRef t)
                        (Modifier (GainPower 2) UntilEndOfTurn)
                    )
                [] -> pure ()
        Nothing -> pure ()
    _ -> pure ()

grudgeThrowerAssault :: CardDef Tactic
grudgeThrowerAssault = tactic "core-022" "Grudge Thrower Assault" do
  race Dwarf
  cost 2
  loyalty 3
  body
    "Play during combat, after damage has been assigned.\nAction: Destroy one target attacking unit."
  -- Auto-target the first enemy attacker.
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      case g.combat of
        Just cs | cs.attackingPlayer /= pk ->
          case cs.attackers of
            (t : _) -> push (DestroyUnit t)
            [] -> pure ()
        _ -> pure ()
    _ -> pure ()

demolition :: CardDef Tactic
demolition = tactic "core-023" "Demolition!" do
  race Dwarf
  cost 2
  loyalty 1
  body "Action: Destroy one target support card or development."
  flavor "KABOOM!"
  tacticTargets SupportTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveEnemySupport pk target g of
        Just s -> push (DestroySupport s.key)
        Nothing -> pure ()
    _ -> pure ()

wakeTheMountain :: CardDef Tactic
wakeTheMountain = tactic "core-024" "Wake the Mountain" do
  race Dwarf
  cost 3
  loyalty 2
  body
    "Action: Put the top three cards of your deck into your battlefield or kingdom facedown as developments. (All three developments must go in the same zone.)"
  -- Auto-pick: drop the three devs in the most-damaged (or default,
  -- battlefield) of the two eligible zones. AddDevelopment doesn't
  -- consume deck cards today; same approximation as Dwarf Masons.
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      let me = case pk of
            Player1 -> g.player1
            Player2 -> g.player2
          Damage kd = me.capital.kingdom.damage
          Damage bd = me.capital.battlefield.damage
          target = if kd >= bd then KingdomZone else BattlefieldZone
      push (AddDevelopment pk target)
      push (AddDevelopment pk target)
      push (AddDevelopment pk target)
    _ -> pure ()

masterRuneOfValaya :: CardDef Tactic
masterRuneOfValaya = tactic "core-025" "Master Rune of Valaya" do
  race Dwarf
  cost 2
  loyalty 1
  traits [Spell, Rune]
  body "Action: Cancel all damage assigned during the battlefield phase this turn."
  flavor "Valaya preseve and protect us in our hour of need!"
  -- Drop the entire pending-damage list for the current combat. The
  -- rule technically suppresses ALL further battlefield-phase damage
  -- this turn; subsequent combats this phase still happen — for
  -- that, CancelAllBattlefieldDamageThisTurn is the bigger hammer
  -- that nukes the in-flight combat entirely.
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      push CancelAllAssignedDamage
      push CancelAllBattlefieldDamageThisTurn
    _ -> pure ()

defenderOfTheHold :: CardDef Unit
defenderOfTheHold = unit "core-001" "Defender of the Hold" do
  race Dwarf
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield only."
  flavor "My blood for the hold, 'tis a fair trade."
  keyword BattlefieldOnly

zhufbarEngineers :: CardDef Unit
zhufbarEngineers = unit "core-002" "Zhufbar Engineers" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Engineer
  body "Forced: After this unit leaves play, each opponent must sacrifice a unit in this corresponding zone."

hammererOfKarakAzul :: CardDef Unit
hammererOfKarakAzul = unit "core-003" "Hammerer of Karak Azul" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Elite]
  toughness 1
  body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."
  flavor "\"The hammer blow rings out doom to our foe.\"\n-Ancient Hammerer saying"

-- ----------------------------------------------------------------------------
-- Chaos cards

servantsOfKhorne :: CardDef Unit
servantsOfKhorne = unit "core-081" "Servants of Khorne" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield only."
  keyword BattlefieldOnly

savageMarauders :: CardDef Unit
savageMarauders = unit "core-082" "Savage Marauders" do
  race Chaos
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

valkiaTheBloody :: CardDef Unit
valkiaTheBloody = unit "core-087" "Valkia the Bloody" do
  unique
  race Chaos
  cost 4
  loyalty 3
  power 2
  hitPoints 4
  traits [Hero, Daemon]
  body
    "Limit one Hero per zone.\n\
    \Quest. Action: Spend 2 resources to move any number of damage on this unit to a target corrupted unit."
  -- Auto-fires at end of her controller's quest phase: if she has any
  -- damage and a corrupted enemy unit exists, shuffle all of it over.
  -- Skips the 2-resource cost (no clean way to debit without a free
  -- action prompt). Real implementation needs the action-prompt
  -- subsystem.
  onReceive $ Receive \msg owner self -> case msg of
    EndPhase QuestPhase
      | self.controller == owner.key
      , Damage d <- self.damage
      , d > 0 -> do
          g <- getGame
          let corruptedEnemies =
                filter
                  (\u -> u.controller /= self.controller && u.corrupted)
                  g.units
          case corruptedEnemies of
            (target : _) -> push (MoveAllDamage self.key target.key)
            [] -> pure ()
    _ -> pure ()

bloodthirster :: CardDef Unit
bloodthirster = unit "core-092" "Bloodthirster" do
  unique
  race Chaos
  cost 8
  loyalty 5
  power 5
  hitPoints 8
  trait Daemon
  keyword DamageCannotBeCancelled
  body
    "Damage cannot be cancelled.\n\
    \Forced: After your turn begins, each player must sacrifice a unit in this corresponding zone."
  -- "Each player must sacrifice a unit in this corresponding zone"
  -- — block on each player's prompt in sequence. The engine
  -- suspends until each one answers; if a player has no eligible
  -- unit, they reply PickNone (or the auto-resolver fires).
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn turnOwner
      | turnOwner == self.controller -> do
          let askSacrifice pk = do
                answer <-
                  askPrompt Prompt
                    { player = pk
                    , kind =
                        ChooseSacrifice
                          { zone = self.zone
                          , optional = False
                          , description =
                              "Sacrifice one of your units in this zone."
                          }
                    , callback = CallbackBloodthirsterSacrifice pk self.key
                    }
                case answer of
                  PickUnits (chosen : _) -> do
                    g <- getGame
                    case findUnit chosen g of
                      Just u | u.controller == pk -> push (DestroyUnit u.key)
                      _ -> pure ()
                  _ -> pure ()
          askSacrifice Player1
          askSacrifice Player2
    _ -> pure ()

bloodForTheBloodGod :: CardDef Tactic
bloodForTheBloodGod = tactic "core-103" "Blood for the Blood God" do
  race Chaos
  cost 2
  loyalty 2
  body
    "Action: Choose a target unit in any battlefield. Deal damage to that unit equal to its power."
  -- Auto-target the first enemy unit and deal damage equal to its
  -- printed power. (Real card chooses "any battlefield"; we'd want a
  -- target prompt for that.)
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      case firstEnemyUnit self.controller g of
        Just target -> push (DealDamageToUnit target.key target.cardDef.power)
        Nothing -> pure ()
    _ -> pure ()

bloodletter :: CardDef Unit
bloodletter = unit "legends-031" "Bloodletter" do
  race Chaos
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  trait Daemon
  body "Double all damage assigned to units as it is being assigned."

bloodsworn :: CardDef Unit
bloodsworn = unit "path-of-the-zealot-031" "Bloodsworn" do
  race Chaos
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  trait Warrior
  body "Forced: When an opponent's unit enters a discard pile from play, heal all damage on Bloodsworn."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitLeftPlay leftBy _key _zone _code
      | leftBy /= self.controller ->
          -- Heal all damage. A large constant beats threading the
          -- current HP into the message; 'HealUnit' clamps to 0.
          push (HealUnit self.key 999)
    _ -> pure ()

bloodcrusher :: CardDef Unit
bloodcrusher = unit "cataclysm-034" "Bloodcrusher" do
  race Chaos
  cost 5
  loyalty 3
  power 3
  hitPoints 5
  trait Daemon
  body "Lower the cost to play this unit by 1 for each burning zone."

swornOfKhorne :: CardDef Unit
swornOfKhorne = unit "fragments-of-power-031" "Sworn of Khorne" do
  race Chaos
  cost 2
  loyalty 1
  power 3
  hitPoints 1
  trait Warrior
  keyword BattlefieldOnly
  body "Battlefield only. This unit cannot attack unless the defending zone has at least 1 corrupted unit."

viciousMarauder :: CardDef Unit
viciousMarauder = unit "the-fourth-waystone-091" "Vicious Marauder" do
  race Chaos
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  trait Warrior
  keyword BattlefieldOnly
  body "Battlefield only. This unit must attack during your battlefield phase, if able."
  -- Approximation: when the battlefield action window opens for the
  -- controller's turn, auto-initiate an attack on the opposing
  -- battlefield with every friendly unit in that zone (the marauder
  -- itself plus any others) — strictly, the rules force only the
  -- marauder, but auto-batching with others keeps the line of play
  -- coherent until action prompts exist.
  onReceive $ Receive \msg _owner self -> case msg of
    OpenActionWindow BattlefieldActionWindow -> do
      g <- getGame
      when
        (g.currentPlayer == self.controller && self.zone == BattlefieldZone)
        $ do
          let attackers =
                [ u.key
                | u <- g.units
                , u.controller == self.controller
                , u.zone == BattlefieldZone
                , not u.corrupted
                ]
          unless (null attackers) $
            push (BeginCombat self.controller BattlefieldZone attackers)
    _ -> pure ()

warhounds :: CardDef Unit
warhounds = unit "legends-032" "Warhounds" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Creature
  body
    "Action: When this unit enters play, reveal a {chaos} legend or unit from your hand. \
    \If you do, deal 2 damage to target unit in any corresponding zone."
  -- Approximation: skip the reveal requirement (no hand-reveal mechanic
  -- yet) and the corresponding-zone targeting (no targeting prompts);
  -- just deal 2 damage to the first enemy unit in play.
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey
      | pk == self.controller && ukey == self.key -> do
          g <- getGame
          case firstEnemyUnit self.controller g of
            Just target -> push (DealDamageToUnit target.key 2)
            Nothing -> pure ()
    _ -> pure ()

wolvesOfTheNorth :: CardDef Quest
wolvesOfTheNorth = quest "path-of-the-zealot-032" "Wolves of the North" do
  race Chaos
  cost 0
  loyalty 2
  trait QuestTrait
  body
    "Action: During your quest phase, the unit questing on this card can initiate a single attack against a single zone controlled by an opponent."
  -- Action: the questing unit initiates an out-of-phase attack
  -- against the chosen enemy zone. The target schema is an enemy
  -- zone; the questing unit is read off the quest itself.
  action ActionDef
    { actionName = "Out-of-phase attack"
    , actionCost = 0
    , actionTarget = EnemyZoneTargetSchema
    , actionEffect = ActionEffect \pk self target -> do
        g <- getGame
        case (findQuest self.key g, target) of
          (Just q, TargetZone owner z)
            | owner /= pk
            , Just attackerKey <- q.questingUnit ->
                push (BeginCombat pk z [attackerKey])
          _ -> pure ()
    }

doombull :: CardDef Unit
doombull = unit "the-chaos-moon-032" "Doombull" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Action: When this unit leaves play, deal 4 damage to target unit in any corresponding zone."
  -- Auto-target the first enemy unit. (Card text actually requires a
  -- target in the *corresponding* zone, but until the engine surfaces
  -- targeting prompts, pick any.)
  onReceive $ Receive \msg _owner self -> case msg of
    DestroyUnit ukey
      | ukey == self.key -> do
          g <- getGame
          case firstEnemyUnit self.controller g of
            Just target -> push (DealDamageToUnit target.key 4)
            Nothing -> pure ()
    _ -> pure ()

skulltaker :: CardDef Unit
skulltaker = unit "faith-and-steel-113" "Skulltaker" do
  unique
  race Chaos
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Daemon
  body
    "This unit gains {power} for each experience attached to it. \
    \Action: When an opponent's unit leaves play, spend 1 resource to attach it facedown to this unit as an experience."
  -- When an opponent's unit leaves play, ask Skulltaker's controller
  -- whether to pay 1 resource to attach it as an experience. The
  -- engine blocks until the controller answers; on yes (and still
  -- affording the 1-resource cost) we debit and attach.
  onReceive $ Receive \msg owner self -> case msg of
    UnitLeftPlay leftBy _ukey _zone code
      | leftBy /= self.controller
      , Resources r0 <- owner.resources
      , r0 >= 1 -> do
          answer <-
            askPrompt Prompt
              { player = self.controller
              , kind =
                  ChooseYesNo
                    { description =
                        "Spend 1 resource to attach the departing unit as an experience on Skulltaker?"
                    }
              , callback = CallbackSkulltakerPayToAttach self.key code
              }
          case answer of
            PickBool True -> do
              push (SpendResources self.controller 1)
              push (AttachExperience self.key code)
            _ -> pure ()
    _ -> pure ()

lordOfKhorne :: CardDef Unit
lordOfKhorne = unit "cataclysm-033" "Lord of Khorne" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Warrior
  body "This unit deals +1 damage in combat for each burning zone."

berserkFury :: CardDef Tactic
berserkFury = tactic "the-warpstone-chronicles-094" "Berserk Fury" do
  race Chaos
  cost 2
  loyalty 3
  body
    "Action: One target Unit gains 3 Power until the end of the turn. At the end of the turn, that unit takes 2 damage."
  -- Auto-target the first friendly unit. Install a +3 power
  -- UntilEndOfTurn modifier, then defer the 2-damage tick to end of
  -- turn. EndTurn clears the modifier in one go via
  -- 'ClearScopedModifiers UntilEndOfTurn'.
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      case filter (\u -> u.controller == self.controller) g.units of
        (target : _) -> do
          push
            ( InstallModifier
                (UnitRef target.key)
                (Modifier (GainPower 3) UntilEndOfTurn)
            )
          push (DeferDamageToUnitUntilEoT target.key 2)
        [] -> pure ()
    _ -> pure ()

daemonsword :: CardDef Support
daemonsword = support "the-warpstone-chronicles-095" "Daemonsword" do
  race Chaos
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {chaos} unit. Corrupt that unit. \
    \Attached unit gains 3 Power and gets +2 Hit Points."
  -- On entering play, corrupt the host. The +3 power / +2 HP buff
  -- waits on the modifier-recompute iteration; for now the body text
  -- documents the unmet half.
  onReceive $ Receive \msg _owner self -> case msg of
    SupportEnteredPlay _pk key
      | key == self.key
      , Just hostKey <- self.attachedTo ->
          push (CorruptUnit hostKey)
    _ -> pure ()

brandedByKhorne :: CardDef Support
brandedByKhorne = support "the-eclipse-of-hope-093" "Branded by Khorne" do
  race Chaos
  cost 0
  loyalty 2
  trait Attachment
  body "Attach to a target unit. If attached unit is damaged, destroy that unit."
  -- Watch for any 'DealDamageToUnit' targeting our host. If the host
  -- ends up with at least 1 damage, destroy it.
  onReceive $ Receive \msg _owner self -> case msg of
    DealDamageToUnit ukey n
      | Just hostKey <- self.attachedTo
      , ukey == hostKey
      , n > 0 ->
          push (DestroyUnit hostKey)
    _ -> pure ()

markOfChaos :: CardDef Support
markOfChaos = support "omens-of-ruin-013" "Mark of Chaos" do
  race Chaos
  cost 1
  loyalty 2
  traits [Attachment, Spell]
  body
    "Attach to a target unit. Attached unit gains {power}{power}. \
    \Forced: At the beginning of your turn, attached unit takes 1 uncancellable damage."
  -- The +2 power half waits on dynamic modifiers; for now wire the
  -- turn-start damage tick on the host's controller's turn.
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn turnOwner
      | Just hostKey <- self.attachedTo -> do
          g <- getGame
          case findUnit hostKey g of
            Just host | host.controller == turnOwner ->
              push (DealDamageToUnitUncancellable hostKey 1)
            _ -> pure ()
    _ -> pure ()

northernWastes :: CardDef Support
northernWastes = support "the-ruinous-hordes-083" "Northern Wastes" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  trait Wasteland
  body "If you control a faceup non-{chaos} unit or support card, sacrifice this card."
  -- Self-check on every message tick: if controller has any faceup
  -- non-Chaos unit or support, sacrifice. (Attachments inherit their
  -- host's faceup status; we don't track facedown explicitly so every
  -- in-play card is treated as faceup for this check.)
  onReceive $ Receive \_msg _owner self -> do
    g <- getGame
    let nonChaosUnit u =
          u.controller == self.controller
            && Chaos `notElem` u.cardDef.races
        nonChaosSupport s =
          s.key /= self.key
            && s.controller == self.controller
            && Chaos `notElem` s.cardDef.races
    when
      ( any nonChaosUnit g.units
          || any nonChaosSupport g.supports
      )
      (push (DestroySupport self.key))

ironThroneroom :: CardDef Support
ironThroneroom = support "the-inevitable-city-013" "Iron Throneroom" do
  unique
  race Chaos
  cost 3
  loyalty 5
  power 2
  trait CapitalCenter
  body
    "This card enters play with 4 resource tokens on it. \
    \Action: At the beginning of your turn, remove a resource token from this card. \
    \Then, if there are no resource tokens on this card, put up to 3 {chaos} units into play from your hand or discard pile."
  -- On entry: load 4 tokens. On your turn-begin: tick one off and, on
  -- the transition to 0, fire the payoff (auto-pick first 3 Chaos
  -- units from hand and put them into play in the kingdom zone free).
  onReceive $ Receive \msg owner self -> case msg of
    SupportEnteredPlay _pk key
      | key == self.key ->
          push (AdjustSupportTokens self.key 4)
    BeginTurn turnOwner
      | turnOwner == self.controller && self.tokens > 0 -> do
          push (AdjustSupportTokens self.key (-1))
          -- On the transition to 0 tokens, prompt the controller for
          -- up to 3 Chaos unit cards from hand or discard, then
          -- inline-route hand picks through PutUnitIntoPlay and
          -- discard picks through PutUnitIntoPlayFromDiscard.
          when (self.tokens == 1) $ do
            answer <-
              askPrompt Prompt
                { player = self.controller
                , kind =
                    ChooseUnits
                      { filterSpec =
                          OwnUnitsFromHandOrDiscardByRace Chaos
                      , minPick = 0
                      , maxPick = 3
                      , description =
                          "Choose up to 3 Chaos units from your hand or discard pile to put into play."
                      }
                , callback = CallbackIronThroneroomPayoff self.key
                }
            case answer of
              PickUnits chosen -> do
                g <- getGame
                let me = case self.controller of
                      Player1 -> g.player1
                      Player2 -> g.player2
                    picks = take 3 chosen
                    inHandChaos =
                      [ c.key
                      | c <- me.hand
                      , UnitCardDef cd <- [c.def]
                      , Chaos `elem` cd.races
                      ]
                    inDiscardChaos =
                      [ c.key
                      | c <- me.discard
                      , UnitCardDef cd <- [c.def]
                      , Chaos `elem` cd.races
                      ]
                    handPicks = [k | k <- picks, k `elem` inHandChaos]
                    discardPicks =
                      [ k
                      | k <- picks
                      , k `notElem` handPicks
                      , k `elem` inDiscardChaos
                      ]
                traverse_
                  (\k -> push (PutUnitIntoPlay self.controller k KingdomZone))
                  handPicks
                traverse_
                  (\k -> push (PutUnitIntoPlayFromDiscard self.controller k KingdomZone))
                  discardPicks
              _ -> pure ()
    _ -> pure ()

raidingCamps :: CardDef Quest
raidingCamps = quest "the-inevitable-city-020" "Raiding Camps" do
  race Chaos
  cost 0
  loyalty 3
  body
    "Quest. Action: When this card enters play, draw a card. \
    \Quest. Action: When you play a {chaos} non-Attachment support card from your hand, \
    \destroy target support card in a zone with no units if a unit is questing here."
  -- "When this card enters play, draw a card." The second ability
  -- needs unit-questing-here tracking and is parked.
  onReceive $ Receive \msg _owner self -> case msg of
    QuestEnteredPlay pk key
      | key == self.key && pk == self.controller ->
          push (Draw (Drawing StandardDraw self.controller))
    _ -> pure ()

riftOfBattle :: CardDef Support
riftOfBattle = support "the-accursed-dead-052" "Rift of Battle" do
  race Chaos
  cost 1
  loyalty 2
  trait Rift
  body "Units in all corresponding zones deal +1 damage in combat."

riftOfChaos :: CardDef Support
riftOfChaos = support "cataclysm-037" "Rift of Chaos" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  trait Rift
  body "This card gains {power} for each burning zone."

recklessAttack :: CardDef Tactic
recklessAttack = tactic "days-of-blood-018" "Reckless Attack" do
  race Chaos
  cost 1
  loyalty 2
  keyword Limited
  body
    "Limited. Action: When your opponent declares at least 1 defender against your attack, \
    \put target unit in your discard pile into play in your battlefield declared as an attacker. \
    \At the end of the phase, sacrifice all units that attacked this phase."
  -- Auto-resolution: when this tactic resolves, if a combat is in
  -- progress with our controller as attacker, pull the first Chaos
  -- unit from our discard, summon it into the battlefield (where the
  -- handler auto-adds it to the combat attackers and to
  -- 'attackersThisPhase'), and schedule the end-of-phase sacrifice.
  onReceive $ Receive \msg owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      let pickFromDiscard =
            [ c.key
            | c <- owner.discard
            , UnitCardDef cd <- pure c.def
            , Chaos `elem` cd.races
            ]
      case (g.combat, pickFromDiscard) of
        (Just cs, (k : _))
          | cs.attackingPlayer == pk -> do
              push (PutUnitIntoPlayFromDiscard pk k BattlefieldZone)
              push ScheduleAttackerSacrifice
        _ -> pure ()
    _ -> pure ()

dominionOfChaos :: CardDef Quest
dominionOfChaos = quest "the-ruinous-hordes-082" "Dominion of Chaos" do
  race Chaos
  cost 0
  loyalty 3
  trait Mission
  body
    "Play in any opponent's zone under your control. \
    \When you assign combat damage to this zone, you may place any number of that combat damage on this quest instead. \
    \Forced: When the 3rd damage token is placed here, sacrifice this quest to corrupt up to 3 target units."
  -- Trigger on token adjustment: once the count crosses to >= 3,
  -- corrupt up to 3 enemy units. The combat-damage-routing half is
  -- not yet wired, so this only fires if some other effect feeds the
  -- quest tokens.
  onReceive $ Receive \msg _owner self -> case msg of
    AdjustQuestTokens qkey _delta
      | qkey == self.key -> do
          g <- getGame
          case findQuest self.key g of
            Just q | q.tokens >= 3 -> do
              let enemies =
                    take 3 $
                      filter (\u -> u.controller /= self.controller) g.units
              traverse_ (\u -> push (CorruptUnit u.key)) enemies
              push (DestroyQuest self.key)
            _ -> pure ()
    _ -> pure ()

-- ============================================================================
-- Empire (core-026 to core-050)
-- ============================================================================

spearmenOfWissenland :: CardDef Unit
spearmenOfWissenland = unit "core-026" "Spearmen of Wissenland" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Battlefield. This unit gains {power} while defending."

stateTroops :: CardDef Unit
stateTroops = unit "core-027" "State Troops" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  flavor "The backbone of the Empire's vast standing army."

templarOfSigmar :: CardDef Unit
templarOfSigmar = unit "core-028" "Templar of Sigmar" do
  race Empire
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Warrior, Priest]
  body "Battlefield. Your other Warrior units gain {power} while in this zone."

witchHunter :: CardDef Unit
witchHunter = unit "core-029" "Witch Hunter" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Forced: When this unit enters play, destroy one target corrupted unit."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey | pk == self.controller && ukey == self.key -> do
      g <- getGame
      case filter (\u -> u.corrupted && u.controller /= self.controller) g.units of
        (target : _) -> push (DestroyUnit target.key)
        [] -> pure ()
    _ -> pure ()

greatswordsOfNuln :: CardDef Unit
greatswordsOfNuln = unit "core-030" "Greatswords of Nuln" do
  race Empire
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

knightsPanther :: CardDef Unit
knightsPanther = unit "core-031" "Knights Panther" do
  race Empire
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  traits [Warrior, Cavalry]
  body "Battlefield. Action: When this unit attacks, it gains {power}{power} for this attack."

reiksguard :: CardDef Unit
reiksguard = unit "core-032" "Reiksguard" do
  race Empire
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Cavalry, Elite]
  body "Battlefield. Toughness 1."
  toughness 1

rieklandMarksmen :: CardDef Unit
rieklandMarksmen = unit "core-033" "Riekland Marksmen" do
  race Empire
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to target unit."
  action ActionDef
    { actionName = "Shoot"
    , actionCost = 1
    , actionTarget = EnemyUnitTargetSchema
    , actionEffect = ActionEffect \_pk _self target -> case target of
        TargetUnit k -> push (DealDamageToUnit k 1)
        _ -> pure ()
    }

thyrusGorman :: CardDef Unit
thyrusGorman = unit "core-034" "Thyrus Gorman" do
  unique
  race Empire
  cost 3
  loyalty 3
  power 2
  hitPoints 2
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Action: Spend 2 resources to deal 2 damage to target unit."
  action ActionDef
    { actionName = "Cast"
    , actionCost = 2
    , actionTarget = EnemyUnitTargetSchema
    , actionEffect = ActionEffect \_pk _self target -> case target of
        TargetUnit k -> push (DealDamageToUnit k 2)
        _ -> pure ()
    }

karlFranz :: CardDef Unit
karlFranz = unit "core-035" "Karl Franz" do
  unique
  race Empire
  cost 7
  loyalty 5
  power 4
  hitPoints 6
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Your other Empire units gain {power}."

volkmarTheGrim :: CardDef Unit
volkmarTheGrim = unit "core-036" "Volkmar the Grim" do
  unique
  race Empire
  cost 5
  loyalty 3
  power 2
  hitPoints 4
  traits [Hero, Priest]
  body
    "Limit one Hero per zone.\n\
    \Forced: At the beginning of your turn, heal 2 damage from your capital."
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k | k == self.controller -> push (HealCapital self.controller 2)
    _ -> pure ()

mariusLeitdorf :: CardDef Unit
mariusLeitdorf = unit "core-037" "Marius Leitdorf" do
  unique
  race Empire
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: After this unit enters play, draw a card."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey | pk == self.controller && ukey == self.key ->
      push (Draw (Drawing StandardDraw self.controller))
    _ -> pure ()

lectorOfSigmar :: CardDef Unit
lectorOfSigmar = unit "core-038" "Lector of Sigmar" do
  race Empire
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Priest
  body "Kingdom. While in your kingdom, your capital gains +1 hit points in each zone."

imperialEngineers :: CardDef Unit
imperialEngineers = unit "core-039" "Imperial Engineers" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Engineer
  body "Forced: When this unit enters play, draw a card."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey | pk == self.controller && ukey == self.key ->
      push (Draw (Drawing StandardDraw self.controller))
    _ -> pure ()

pegasusKnights :: CardDef Unit
pegasusKnights = unit "core-040" "Pegasus Knights" do
  race Empire
  cost 5
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Cavalry]
  body "Battlefield. This unit can attack the turn it enters play."

theImperialCrown :: CardDef Support
theImperialCrown = support "core-041" "The Imperial Crown" do
  unique
  race Empire
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Kingdom. Your Empire heroes cost 1 less to play."

hammerOfSigmar :: CardDef Support
hammerOfSigmar = support "core-042" "The Hammer of Sigmar" do
  race Empire
  cost 2
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target Empire unit. Attached unit gains {power}{power}; its damage cannot be cancelled."

bannerOfSigmar :: CardDef Support
bannerOfSigmar = support "core-043" "Banner of Sigmar" do
  race Empire
  cost 1
  loyalty 2
  trait Attachment
  body "Attach to a target unit. Attached unit gains {power}."

altdorf :: CardDef Support
altdorf = support "core-044" "Altdorf" do
  unique
  race Empire
  cost 2
  loyalty 2
  power 1
  trait Building
  body "Kingdom. While in play, non-combat damage to your capital is reduced by 1 (minimum 0)."

defendingTheEmpire :: CardDef Quest
defendingTheEmpire = quest "core-045" "Defending the Empire" do
  race Empire
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 resource token on this card if a unit is questing here.\n\
    \Action: Spend 3 resource tokens from this card to heal all damage on your capital."
  -- Tokens only accrue while a unit is questing here. On reaching 3
  -- tokens, auto-fire the heal-capital payoff and reset.
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k | k == self.controller -> do
      g <- getGame
      case findQuest self.key g of
        Just q | q.questingUnit /= Nothing -> do
          push (AdjustQuestTokens self.key 1)
          when (q.tokens + 1 >= 3) $ do
            push (AdjustQuestTokens self.key (-3))
            push (HealCapital self.controller 99)
        _ -> pure ()
    _ -> pure ()

forSigmar :: CardDef Tactic
forSigmar = tactic "core-046" "For Sigmar!" do
  race Empire
  cost 2
  loyalty 1
  body "Action: Each of your units gains {power} until the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      let mine = [u | u <- g.units, u.controller == pk]
      traverse_
        ( \u ->
            push
              ( InstallModifier
                  (UnitRef u.key)
                  (Modifier (GainPower 1) UntilEndOfTurn)
              )
        )
        mine
    _ -> pure ()

sigmarsWrath :: CardDef Tactic
sigmarsWrath = tactic "core-047" "Sigmar's Wrath" do
  race Empire
  cost 3
  loyalty 2
  body "Action: Deal 3 damage to target unit."
  tacticTargets EnemyUnitTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveEnemyUnit pk target g of
        Just t -> push (DealDamageToUnit t.key 3)
        Nothing -> pure ()
    _ -> pure ()

counterCharge :: CardDef Tactic
counterCharge = tactic "core-048" "Counter-charge" do
  race Empire
  cost 1
  loyalty 2
  body "Play during combat. Action: Target defending unit gains {power}{power} until the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      case g.combat of
        Just cs ->
          case [k | k <- cs.defenders, Just _ <- [findUnit k g]] of
            (target : _) ->
              push
                ( InstallModifier
                    (UnitRef target)
                    (Modifier (GainPower 2) UntilEndOfTurn)
                )
            [] -> pure ()
        Nothing -> pure ()
    _ -> pure ()

battleOfTheReik :: CardDef Tactic
battleOfTheReik = tactic "core-049" "Battle of the Reik" do
  race Empire
  cost 2
  loyalty 2
  body "Action: Deal 1 damage to each attacking and each defending unit."
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller ->
      push (DealDamageToEachUnitInCombat 1)
    _ -> pure ()

defendersOfTheFaith :: CardDef Tactic
defendersOfTheFaith = tactic "core-050" "Defenders of the Faith" do
  race Empire
  cost 1
  loyalty 1
  body "Action: Cancel up to 2 damage assigned to a unit you control."
  tacticTargets FriendlyUnitTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveFriendlyUnit pk target g of
        Just t -> push (CancelAssignedDamageOnUnit t.key 2)
        Nothing -> pure ()
    _ -> pure ()

-- ============================================================================
-- High Elf (core-051 to core-075)
-- ============================================================================

phoenixGuard :: CardDef Unit
phoenixGuard = unit "core-051" "Phoenix Guard" do
  race HighElf
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield. Toughness 1."
  toughness 1

whiteLionsOfChrace :: CardDef Unit
whiteLionsOfChrace = unit "core-052" "White Lions of Chrace" do
  race HighElf
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

swordmastersOfHoeth :: CardDef Unit
swordmastersOfHoeth = unit "core-053" "Swordmasters of Hoeth" do
  race HighElf
  cost 5
  loyalty 3
  power 4
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

highElfArchers :: CardDef Unit
highElfArchers = unit "core-054" "High Elf Archers" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to target unit."
  action ActionDef
    { actionName = "Loose arrow"
    , actionCost = 1
    , actionTarget = EnemyUnitTargetSchema
    , actionEffect = ActionEffect \_pk _self target -> case target of
        TargetUnit k -> push (DealDamageToUnit k 1)
        _ -> pure ()
    }

seaGuardOfLothern :: CardDef Unit
seaGuardOfLothern = unit "core-055" "Sea Guard of Lothern" do
  race HighElf
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  trait Warrior

silverHelms :: CardDef Unit
silverHelms = unit "core-056" "Silver Helms" do
  race HighElf
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Warrior, Cavalry]

dragonPrincesOfCaledor :: CardDef Unit
dragonPrincesOfCaledor = unit "core-057" "Dragon Princes of Caledor" do
  race HighElf
  cost 5
  loyalty 3
  power 3
  hitPoints 3
  traits [Warrior, Cavalry, Elite]
  body "Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

princeTyrion :: CardDef Unit
princeTyrion = unit "core-058" "Prince Tyrion" do
  unique
  race HighElf
  cost 6
  loyalty 5
  power 4
  hitPoints 5
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

teclis :: CardDef Unit
teclis = unit "core-059" "Teclis" do
  unique
  race HighElf
  cost 6
  loyalty 5
  power 2
  hitPoints 4
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Forced: After this unit enters play, draw 2 cards."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey | pk == self.controller && ukey == self.key -> do
      push (Draw (Drawing StandardDraw self.controller))
      push (Draw (Drawing StandardDraw self.controller))
    _ -> pure ()

eltharionTheGrim :: CardDef Unit
eltharionTheGrim = unit "core-060" "Eltharion the Grim" do
  unique
  race HighElf
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When an opponent's unit enters play, deal 1 damage to that unit."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey | pk /= self.controller && ukey /= self.key ->
      push (DealDamageToUnit ukey 1)
    _ -> pure ()

korhil :: CardDef Unit
korhil = unit "core-061" "Korhil" do
  unique
  race HighElf
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. This unit gains {power} for each other White Lion unit you control."

loremasterOfHoeth :: CardDef Unit
loremasterOfHoeth = unit "core-062" "Loremaster of Hoeth" do
  race HighElf
  cost 4
  loyalty 2
  power 1
  hitPoints 3
  traits [Sorcerer]
  body "Forced: When this unit enters play, draw a card."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey | pk == self.controller && ukey == self.key ->
      push (Draw (Drawing StandardDraw self.controller))
    _ -> pure ()

mageOfTheWhiteTower :: CardDef Unit
mageOfTheWhiteTower = unit "core-063" "Mage of the White Tower" do
  race HighElf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Quest. Action: Spend 2 resources to look at the top 3 cards of your deck."

spearmenOfLothern :: CardDef Unit
spearmenOfLothern = unit "core-064" "Spearmen of Lothern" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior

reaverKnights :: CardDef Unit
reaverKnights = unit "core-065" "Reaver Knights" do
  race HighElf
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  traits [Warrior, Cavalry]
  body "Scout."
  scout

lighthouseOfLothern :: CardDef Support
lighthouseOfLothern = support "core-066" "The Lighthouse of Lothern" do
  unique
  race HighElf
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Quest. Your quest zone gains +1 power."

bannerOfAvelorn :: CardDef Support
bannerOfAvelorn = support "core-067" "Banner of Avelorn" do
  race HighElf
  cost 2
  loyalty 2
  trait Attachment
  body "Attach to a target High Elf unit. Attached unit gains {power}{power}."

bowOfAvelorn :: CardDef Support
bowOfAvelorn = support "core-068" "Bow of Avelorn" do
  race HighElf
  cost 1
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target High Elf unit. Action: Sacrifice this card to deal 2 damage to target unit."
  action ActionDef
    { actionName = "Loose arrow (sacrifice)"
    , actionCost = 0
    , actionTarget = EnemyUnitTargetSchema
    , actionEffect = ActionEffect \_pk self target -> case target of
        TargetUnit k -> do
          push (DealDamageToUnit k 2)
          push (DestroySupport self.key)
        _ -> pure ()
    }

hoethsWisdom :: CardDef Support
hoethsWisdom = support "core-069" "Hoeth's Wisdom" do
  race HighElf
  cost 2
  loyalty 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, draw a card."
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k
      | k == self.controller
      , self.zone == KingdomZone ->
          push (Draw (Drawing StandardDraw self.controller))
    _ -> pure ()

theWhiteTower :: CardDef Quest
theWhiteTower = quest "core-070" "The White Tower" do
  race HighElf
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 resource token here if a unit is questing here.\n\
    \Action: Spend 3 tokens to draw 3 cards."
  -- Tokens only accrue with a questing unit. On reaching 3, auto-fire
  -- the draw-3 payoff and reset.
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k | k == self.controller -> do
      g <- getGame
      case findQuest self.key g of
        Just q | q.questingUnit /= Nothing -> do
          push (AdjustQuestTokens self.key 1)
          when (q.tokens + 1 >= 3) $ do
            push (AdjustQuestTokens self.key (-3))
            push (Draw (Drawing StandardDraw self.controller))
            push (Draw (Drawing StandardDraw self.controller))
            push (Draw (Drawing StandardDraw self.controller))
        _ -> pure ()
    _ -> pure ()

voiceOfCommand :: CardDef Tactic
voiceOfCommand = tactic "core-071" "Voice of Command" do
  race HighElf
  cost 2
  loyalty 2
  traits [Spell]
  body "Action: Target unit gains {power}{power} until the end of the turn."
  tacticTargets AnyUnitTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      let pick = case target of
            TargetUnit k -> findUnit k g
            _ -> case [u | u <- g.units, u.controller == pk] of
              (u : _) -> Just u
              [] -> Nothing
      case pick of
        Just t ->
          push
            ( InstallModifier
                (UnitRef t.key)
                (Modifier (GainPower 2) UntilEndOfTurn)
            )
        Nothing -> pure ()
    _ -> pure ()

dragonBreath :: CardDef Tactic
dragonBreath = tactic "core-072" "Dragon Breath" do
  race HighElf
  cost 3
  loyalty 3
  traits [Spell]
  body "Action: Deal 2 damage to each enemy unit in target zone."
  tacticTargets EnemyZoneTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveEnemyZone pk target g of
        Just (_, z) -> push (DealDamageToEachEnemyUnitInZone pk z 2)
        Nothing -> pure ()
    _ -> pure ()

magicOfTheOldOnes :: CardDef Tactic
magicOfTheOldOnes = tactic "core-073" "Magic of the Old Ones" do
  race HighElf
  cost 1
  loyalty 1
  traits [Spell]
  body "Action: Draw 2 cards."
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      push (Draw (Drawing StandardDraw self.controller))
      push (Draw (Drawing StandardDraw self.controller))
    _ -> pure ()

battleMagic :: CardDef Tactic
battleMagic = tactic "core-074" "Battle Magic" do
  race HighElf
  cost 2
  loyalty 2
  traits [Spell]
  body "Action: Deal 2 damage to target unit."
  tacticTargets EnemyUnitTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveEnemyUnit pk target g of
        Just t -> push (DealDamageToUnit t.key 2)
        Nothing -> pure ()
    _ -> pure ()

sacredIncantations :: CardDef Tactic
sacredIncantations = tactic "core-075" "Sacred Incantations" do
  race HighElf
  cost 1
  loyalty 2
  traits [Spell]
  body "Action: Cancel a target tactic that is being played."

-- ============================================================================
-- Chaos (fill core-083..core-102 gaps)
-- ============================================================================

tzeentchSorcerer :: CardDef Unit
tzeentchSorcerer = unit "core-083" "Tzeentch Sorcerer" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  traits [Sorcerer]
  body "Action: Spend 2 resources to deal 1 damage to a target unit and draw a card."

slaaneshiMarauders :: CardDef Unit
slaaneshiMarauders = unit "core-084" "Slaaneshi Marauders" do
  race Chaos
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  traits [Warrior]
  body "Battlefield only."
  keyword BattlefieldOnly

plaguebearersOfNurgle :: CardDef Unit
plaguebearersOfNurgle = unit "core-085" "Plaguebearers of Nurgle" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 4
  trait Daemon
  body "Forced: When this unit damages an enemy unit in combat, corrupt that unit."
  -- Engine-handled: firePerSourceCombatEffects at 'EndCombat' walks
  -- the damagedInCurrentCombat list and corrupts every enemy that
  -- took damage in any combat this unit participated in.

festeringNurglings :: CardDef Unit
festeringNurglings = unit "core-086" "Festering Nurglings" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  traits [Creature]
  body "Forced: When this unit leaves play, corrupt target enemy unit."
  onReceive $ Receive \msg _owner self -> case msg of
    DestroyUnit ukey | ukey == self.key -> do
      g <- getGame
      case firstEnemyUnit self.controller g of
        Just target -> push (CorruptUnit target.key)
        Nothing -> pure ()
    _ -> pure ()

archaonTheEverchosen :: CardDef Unit
archaonTheEverchosen = unit "core-088" "Archaon the Everchosen" do
  unique
  race Chaos
  cost 7
  loyalty 5
  power 4
  hitPoints 6
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit attacks, corrupt one defending unit."
  onReceive $ Receive \msg _owner self -> case msg of
    BeginCombat attacker _zone attackers
      | attacker == self.controller && self.key `elem` attackers -> do
          g <- getGame
          case g.combat of
            Just cs ->
              case [d | d <- cs.defenders, Just _ <- [findUnit d g]] of
                (t : _) -> push (CorruptUnit t)
                [] -> pure ()
            Nothing -> pure ()
    _ -> pure ()

chaosKnights :: CardDef Unit
chaosKnights = unit "core-089" "Chaos Knights" do
  race Chaos
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Cavalry, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

chaosWarriors :: CardDef Unit
chaosWarriors = unit "core-090" "Chaos Warriors" do
  race Chaos
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

maraudersOfTheNorth :: CardDef Unit
maraudersOfTheNorth = unit "core-091" "Marauders of the North" do
  race Chaos
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

chaosSorcerer :: CardDef Unit
chaosSorcerer = unit "core-093" "Chaos Sorcerer" do
  race Chaos
  cost 4
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Quest. Action: Spend 2 resources to corrupt target enemy unit."
  action ActionDef
    { actionName = "Corrupt"
    , actionCost = 2
    , actionTarget = EnemyUnitTargetSchema
    , actionEffect = ActionEffect \_pk _self target -> case target of
        TargetUnit k -> push (CorruptUnit k)
        _ -> pure ()
    }

horrorOfTzeentch :: CardDef Unit
horrorOfTzeentch = unit "core-094" "Horror of Tzeentch" do
  race Chaos
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Daemon]
  body "Forced: When this unit enters play, you may discard a card to deal 2 damage to a target unit."
  -- Inline two-stage prompt: yes/no to commit to the discard; if
  -- yes, ask the controller for the damage target. The engine
  -- blocks on each prompt independently, so the second is asked
  -- only after the first resolves.
  onReceive $ Receive \msg owner self -> case msg of
    UnitEnteredPlay pk ukey
      | pk == self.controller
      , ukey == self.key
      , not (null owner.hand) -> do
          yes <-
            askPrompt Prompt
              { player = self.controller
              , kind =
                  ChooseYesNo
                    { description =
                        "Discard a card to deal 2 damage to a target unit?"
                    }
              , callback = CallbackHorrorOfTzeentchDiscard self.key
              }
          case yes of
            PickBool True -> do
              tgt <-
                askPrompt Prompt
                  { player = self.controller
                  , kind =
                      ChooseUnits
                        { filterSpec = AnyOwnUnit
                        , minPick = 1
                        , maxPick = 1
                        , description = "Choose a target unit to take 2 damage."
                        }
                  , callback = CallbackHorrorOfTzeentchTarget self.key
                  }
              case tgt of
                PickUnits (k : _) -> do
                  push (DiscardRandomFromHand self.controller)
                  push (DealDamageToUnit k 2)
                _ -> pure ()
            _ -> pure ()
    _ -> pure ()

daemonettesOfSlaanesh :: CardDef Unit
daemonettesOfSlaanesh = unit "core-095" "Daemonettes of Slaanesh" do
  race Chaos
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  traits [Daemon]
  body "Battlefield. This unit cannot be assigned more than 1 damage per turn."

beastsOfNurgle :: CardDef Unit
beastsOfNurgle = unit "core-096" "Beasts of Nurgle" do
  race Chaos
  cost 4
  loyalty 2
  power 1
  hitPoints 5
  traits [Creature, Daemon]
  body "Forced: When this unit damages an enemy unit, corrupt that unit."
  -- Engine-handled by firePerSourceCombatEffects (see Plaguebearers).

chaosSpawn :: CardDef Unit
chaosSpawn = unit "core-097" "Chaos Spawn" do
  race Chaos
  cost 2
  loyalty 1
  power 2
  hitPoints 3
  trait Creature
  body "Forced: At the end of your turn, deal 1 damage to this unit."
  onReceive $ Receive \msg _owner self -> case msg of
    EndTurn k | k == self.controller -> push (DealDamageToUnit self.key 1)
    _ -> pure ()

eyeOfTzeentch :: CardDef Support
eyeOfTzeentch = support "core-098" "Eye of Tzeentch" do
  race Chaos
  cost 2
  loyalty 2
  traits [Attachment, Spell]
  body "Attach to a target Chaos unit. Attached unit gains {power}; you may draw a card whenever it attacks."

theIronTower :: CardDef Support
theIronTower = support "core-099" "The Iron Tower" do
  unique
  race Chaos
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Battlefield. Your Chaos units gain {power} while in this zone."

pyreOfTcharzanek :: CardDef Support
pyreOfTcharzanek = support "core-100" "Pyre of Tchar'zanek" do
  race Chaos
  cost 2
  loyalty 2
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, deal 1 damage to a target zone."
  -- Auto-target the opponent's most-damaged zone. Gated on Pyre
  -- sitting in your kingdom ("Kingdom." prefix).
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k
      | k == self.controller
      , self.zone == KingdomZone -> do
      g <- getGame
      let opp = case self.controller of
            Player1 -> g.player2
            Player2 -> g.player1
          scored =
            [ (d, z)
            | (z, zL) <-
                [ (KingdomZone, opp.capital.kingdom)
                , (QuestZone, opp.capital.quest)
                , (BattlefieldZone, opp.capital.battlefield)
                ]
            , not zL.burned
            , let Damage d = zL.damage
            ]
      case scored of
        [] -> pure ()
        xs ->
          let (_, z) = maximum xs
           in push (DealDamageToZone self.controller.next z 1)
    _ -> pure ()

tidesOfChaos :: CardDef Tactic
tidesOfChaos = tactic "core-101" "Tides of Chaos" do
  race Chaos
  cost 2
  loyalty 2
  body "Action: Corrupt target unit."
  tacticTargets EnemyUnitTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveEnemyUnit pk target g of
        Just t -> push (CorruptUnit t.key)
        Nothing -> pure ()
    _ -> pure ()

doomOfTheEmpire :: CardDef Tactic
doomOfTheEmpire = tactic "core-102" "Doom of the Empire" do
  race Chaos
  cost 3
  loyalty 3
  body "Action: Deal 2 damage to target zone."
  tacticTargets EnemyZoneTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveEnemyZone pk target g of
        Just (owner, z) -> push (DealDamageToZone owner z 2)
        Nothing -> pure ()
    _ -> pure ()

-- ============================================================================
-- Orc (core-106 to core-130)
-- ============================================================================

grimgorIronhide :: CardDef Unit
grimgorIronhide = unit "core-106" "Grimgor Ironhide" do
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
skarsnik = unit "core-107" "Skarsnik" do
  unique
  race Orc
  cost 4
  loyalty 3
  power 2
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: After this unit enters play, search the top 3 cards of your deck for a Goblin unit and put it into play. Shuffle your deck."

gorbadIronclaw :: CardDef Unit
gorbadIronclaw = unit "core-108" "Gorbad Ironclaw" do
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

orcBigUns :: CardDef Unit
orcBigUns = unit "core-109" "Orc Big 'Uns" do
  race Orc
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  traits [Warrior, Elite]

blackOrcs :: CardDef Unit
blackOrcs = unit "core-110" "Black Orcs" do
  race Orc
  cost 5
  loyalty 3
  power 4
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

savageOrcs :: CardDef Unit
savageOrcs = unit "core-111" "Savage Orcs" do
  race Orc
  cost 3
  loyalty 1
  power 3
  hitPoints 2
  trait Warrior
  body "Battlefield only."
  keyword BattlefieldOnly

orcBoyz :: CardDef Unit
orcBoyz = unit "core-112" "Orc Boyz" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

boarBoyz :: CardDef Unit
boarBoyz = unit "core-113" "Boar Boyz" do
  race Orc
  cost 3
  loyalty 2
  power 3
  hitPoints 2
  traits [Warrior, Cavalry]
  body "Battlefield only."
  keyword BattlefieldOnly

nightGoblins :: CardDef Unit
nightGoblins = unit "core-114" "Night Goblins" do
  race Orc
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior

goblinWolfRiders :: CardDef Unit
goblinWolfRiders = unit "core-115" "Goblin Wolf Riders" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  traits [Warrior, Cavalry]
  body "Scout."
  scout

squigHoppers :: CardDef Unit
squigHoppers = unit "core-116" "Squig Hoppers" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Creature

orcShaman :: CardDef Unit
orcShaman = unit "core-117" "Orc Shaman" do
  race Orc
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Action: Spend 2 resources to deal 2 damage to a target unit; deal 1 damage to this unit."
  action ActionDef
    { actionName = "Sorcery"
    , actionCost = 2
    , actionTarget = EnemyUnitTargetSchema
    , actionEffect = ActionEffect \_pk self target -> case target of
        TargetUnit k -> do
          push (DealDamageToUnit k 2)
          push (DealDamageToUnitUncancellable self.key 1)
        _ -> pure ()
    }

trolls :: CardDef Unit
trolls = unit "core-118" "Trolls" do
  race Orc
  cost 4
  loyalty 2
  power 3
  hitPoints 4
  trait Creature
  body "Forced: At the beginning of your turn, heal 1 damage from this unit."
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k | k == self.controller -> push (HealUnit self.key 1)
    _ -> pure ()

forestGoblinSpiderRiders :: CardDef Unit
forestGoblinSpiderRiders = unit "core-119" "Forest Goblin Spider Riders" do
  race Orc
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  traits [Warrior, Cavalry]
  body "Scout."
  scout

snotlings :: CardDef Unit
snotlings = unit "core-120" "Snotlings" do
  race Orc
  cost 0
  loyalty 1
  power 1
  hitPoints 1
  trait Creature

daBadMoon :: CardDef Support
daBadMoon = support "core-121" "Da Bad Moon" do
  unique
  race Orc
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Battlefield. Your other Orc units gain {power} while attacking."

choppa :: CardDef Support
choppa = support "core-122" "Choppa" do
  race Orc
  cost 1
  loyalty 1
  traits [Attachment, Weapon]
  body "Attach to a target Orc unit. Attached unit gains {power}{power}."

bigBossesBanner :: CardDef Support
bigBossesBanner = support "core-123" "Big Boss's Banner" do
  race Orc
  cost 2
  loyalty 2
  trait Attachment
  body "Attach to a target Orc unit. While attached unit is attacking, your other Orc attackers gain {power}."

daMorksEye :: CardDef Support
daMorksEye = support "core-124" "Da Mork's Eye" do
  race Orc
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, deal 1 damage to one target enemy zone."
  -- Auto-target the opponent's most-damaged unburned zone. Gated on
  -- Da Mork's Eye being in the controller's kingdom.
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k
      | k == self.controller
      , self.zone == KingdomZone -> do
      g <- getGame
      let opp = case self.controller of
            Player1 -> g.player2
            Player2 -> g.player1
          scored =
            [ (d, z)
            | (z, zL) <-
                [ (KingdomZone, opp.capital.kingdom)
                , (QuestZone, opp.capital.quest)
                , (BattlefieldZone, opp.capital.battlefield)
                ]
            , not zL.burned
            , let Damage d = zL.damage
            ]
      case scored of
        [] -> pure ()
        xs ->
          let (_, z) = maximum xs
           in push (DealDamageToZone self.controller.next z 1)
    _ -> pure ()

orcWarmachine :: CardDef Support
orcWarmachine = support "core-125" "Orc Warmachine" do
  race Orc
  cost 3
  loyalty 2
  trait Siege
  body "Battlefield. Action: Sacrifice a unit to deal 2 damage to a target zone."

greenskinRush :: CardDef Quest
greenskinRush = quest "core-126" "Greenskin Rush" do
  race Orc
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 resource token here if a unit is questing here.\n\
    \Action: Spend 2 tokens to put a Goblin unit from your hand into play."
  -- Tokens only accrue while a unit is questing here. Once 2 tokens
  -- accrue, auto-summon the first Goblin-trait Orc unit from hand
  -- (Night Goblins, Wolf Riders, Spider Riders, Snotlings) into the
  -- battlefield.
  onReceive $ Receive \msg owner self -> case msg of
    BeginTurn k | k == self.controller -> do
      g <- getGame
      case findQuest self.key g of
        Just q | q.questingUnit /= Nothing -> do
          push (AdjustQuestTokens self.key 1)
          when (q.tokens + 1 >= 2) $ do
            let goblinCodes =
                  [ CardCode "core-114"  -- Night Goblins
                  , CardCode "core-115"  -- Goblin Wolf Riders
                  , CardCode "core-119"  -- Forest Goblin Spider Riders
                  , CardCode "core-120"  -- Snotlings
                  ]
                pickHandUnit =
                  [ c.key
                  | c <- owner.hand
                  , UnitCardDef cd <- [c.def]
                  , cd.code `elem` goblinCodes
                  ]
            case pickHandUnit of
              (uKey : _) -> do
                push (AdjustQuestTokens self.key (-2))
                push (PutUnitIntoPlay self.controller uKey BattlefieldZone)
              [] -> pure ()
        _ -> pure ()
    _ -> pure ()

waaagh :: CardDef Tactic
waaagh = tactic "core-127" "Waaagh!" do
  race Orc
  cost 2
  loyalty 2
  body "Action: Each of your Orc units gains {power}{power} until the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      let mine =
            [ u
            | u <- g.units
            , u.controller == pk
            , Orc `elem` u.cardDef.races
            ]
      traverse_
        ( \u ->
            push
              ( InstallModifier
                  (UnitRef u.key)
                  (Modifier (GainPower 2) UntilEndOfTurn)
              )
        )
        mine
    _ -> pure ()

crushEm :: CardDef Tactic
crushEm = tactic "core-128" "Crush 'Em" do
  race Orc
  cost 1
  loyalty 1
  body "Action: Target unit gains {power}{power}{power} until the end of the turn; sacrifice it at end of turn."
  -- Auto-target the first friendly unit. Defer the sacrifice via
  -- DeferDamageToUnitUntilEoT for "lots of damage" → relies on the
  -- existing end-of-turn pump to trigger destroy.
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      case [u | u <- g.units, u.controller == pk] of
        (target : _) -> do
          push
            ( InstallModifier
                (UnitRef target.key)
                (Modifier (GainPower 3) UntilEndOfTurn)
            )
          -- Schedule a destroy by piling 99 damage at EoT.
          push (DeferDamageToUnitUntilEoT target.key 99)
        [] -> pure ()
    _ -> pure ()

runEmDown :: CardDef Tactic
runEmDown = tactic "core-129" "Run 'Em Down" do
  race Orc
  cost 2
  loyalty 1
  body "Action: Deal 1 damage to each enemy unit in target zone."
  tacticTargets EnemyZoneTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveEnemyZone pk target g of
        Just (_, z) -> push (DealDamageToEachEnemyUnitInZone pk z 1)
        Nothing -> pure ()
    _ -> pure ()

daBigStomp :: CardDef Tactic
daBigStomp = tactic "core-130" "Da Big Stomp" do
  race Orc
  cost 3
  loyalty 2
  body "Action: Destroy target support card or development."
  tacticTargets SupportTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveEnemySupport pk target g of
        Just s -> push (DestroySupport s.key)
        Nothing -> pure ()
    _ -> pure ()

-- ============================================================================
-- Dark Elf (core-131 to core-155)
-- ============================================================================

malekith :: CardDef Unit
malekith = unit "core-131" "Malekith" do
  unique
  race DarkElf
  cost 7
  loyalty 5
  power 4
  hitPoints 6
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit damages an enemy unit, corrupt that unit."
  -- Approximation: corrupt the first enemy defender on every combat we
  -- attack in. Real card needs per-damage-event source tracking.
  onReceive $ Receive \msg _owner self -> case msg of
    ResolveCombat -> do
      g <- getGame
      case g.combat of
        Just cs
          | self.key `elem` cs.attackers ->
              case cs.defenders of
                (t : _) -> push (CorruptUnit t)
                [] -> pure ()
        _ -> pure ()
    _ -> pure ()

morathi :: CardDef Unit
morathi = unit "core-132" "Morathi" do
  unique
  race DarkElf
  cost 5
  loyalty 4
  power 2
  hitPoints 4
  traits [Hero, Sorcerer]
  body
    "Limit one Hero per zone.\n\
    \Action: Spend 2 resources to corrupt one target unit."
  action ActionDef
    { actionName = "Corrupt"
    , actionCost = 2
    , actionTarget = EnemyUnitTargetSchema
    , actionEffect = ActionEffect \_pk _self target -> case target of
        TargetUnit k -> push (CorruptUnit k)
        _ -> pure ()
    }

croneHellebron :: CardDef Unit
croneHellebron = unit "core-133" "Crone Hellebron" do
  unique
  race DarkElf
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Battlefield. This unit gains {power} for each Witch Elf unit you control."

lokhirFellheart :: CardDef Unit
lokhirFellheart = unit "core-134" "Lokhir Fellheart" do
  unique
  race DarkElf
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  traits [Hero, Warrior]
  body
    "Limit one Hero per zone.\n\
    \Forced: When this unit damages an enemy zone, draw a card."
  -- Approximation: any combat we participate in where the attacker
  -- side wins damage to a zone (spillover) → draw. The damage-source
  -- granularity isn't tracked, so just fire whenever this unit is one
  -- of the attackers and combat resolves.
  onReceive $ Receive \msg _owner self -> case msg of
    ResolveCombat -> do
      g <- getGame
      case g.combat of
        Just cs
          | self.key `elem` cs.attackers ->
              push (Draw (Drawing StandardDraw self.controller))
        _ -> pure ()
    _ -> pure ()

witchElves :: CardDef Unit
witchElves = unit "core-135" "Witch Elves" do
  race DarkElf
  cost 3
  loyalty 2
  power 3
  hitPoints 1
  trait Warrior
  body "Battlefield only. Damage dealt by this unit cannot be cancelled."
  keyword BattlefieldOnly
  keyword DamageCannotBeCancelled

blackGuardOfNaggarond :: CardDef Unit
blackGuardOfNaggarond = unit "core-136" "Black Guard of Naggarond" do
  race DarkElf
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

executioners :: CardDef Unit
executioners = unit "core-137" "Executioners" do
  race DarkElf
  cost 4
  loyalty 2
  power 4
  hitPoints 2
  traits [Warrior, Elite]
  body "Battlefield only. Damage dealt by this unit cannot be cancelled."
  keyword BattlefieldOnly
  keyword DamageCannotBeCancelled

corsairs :: CardDef Unit
corsairs = unit "core-138" "Corsairs" do
  race DarkElf
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  trait Warrior
  body "Forced: When this unit damages an enemy zone, draw a card."
  -- Same approximation as Lokhir: fire on ResolveCombat when attacking.
  onReceive $ Receive \msg _owner self -> case msg of
    ResolveCombat -> do
      g <- getGame
      case g.combat of
        Just cs
          | self.key `elem` cs.attackers ->
              push (Draw (Drawing StandardDraw self.controller))
        _ -> pure ()
    _ -> pure ()

coldOneKnights :: CardDef Unit
coldOneKnights = unit "core-139" "Cold One Knights" do
  race DarkElf
  cost 5
  loyalty 3
  power 3
  hitPoints 3
  traits [Warrior, Cavalry, Elite]
  body "Battlefield only."
  keyword BattlefieldOnly

darkRiders :: CardDef Unit
darkRiders = unit "core-140" "Dark Riders" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Cavalry]
  body "Scout."
  scout

darkSorceress :: CardDef Unit
darkSorceress = unit "core-141" "Dark Sorceress" do
  race DarkElf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorcerer
  body "Action: Spend 2 resources to deal 2 damage to a target unit."
  action ActionDef
    { actionName = "Cast"
    , actionCost = 2
    , actionTarget = EnemyUnitTargetSchema
    , actionEffect = ActionEffect \_pk _self target -> case target of
        TargetUnit k -> push (DealDamageToUnit k 2)
        _ -> pure ()
    }

assassinsOfKhaine :: CardDef Unit
assassinsOfKhaine = unit "core-142" "Assassins of Khaine" do
  race DarkElf
  cost 3
  loyalty 2
  power 2
  hitPoints 1
  trait Warrior
  body "Forced: When this unit enters play, destroy one target unit with 1 hit point."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey | pk == self.controller && ukey == self.key -> do
      g <- getGame
      let onePip =
            [ u
            | u <- g.units
            , u.controller /= self.controller
            , u.effectiveMaxHP <= 1
            ]
      case onePip of
        (t : _) -> push (DestroyUnit t.key)
        [] -> pure ()
    _ -> pure ()

repeaterCrossbowmen :: CardDef Unit
repeaterCrossbowmen = unit "core-143" "Repeater Crossbowmen" do
  race DarkElf
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to a target unit."
  action ActionDef
    { actionName = "Loose bolts"
    , actionCost = 1
    , actionTarget = EnemyUnitTargetSchema
    , actionEffect = ActionEffect \_pk _self target -> case target of
        TargetUnit k -> push (DealDamageToUnit k 1)
        _ -> pure ()
    }

bloodwrackMedusa :: CardDef Unit
bloodwrackMedusa = unit "core-144" "Bloodwrack Medusa" do
  race DarkElf
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  trait Creature
  body "Forced: When this unit enters play, deal 1 damage to each enemy unit in target zone."
  -- Auto-pick the zone with the most enemy units.
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk ukey | pk == self.controller && ukey == self.key -> do
      g <- getGame
      let counts =
            [ ( length
                  [ ()
                  | u <- g.units
                  , u.controller /= pk
                  , u.zone == z
                  ]
              , z
              )
            | z <- [BattlefieldZone, QuestZone, KingdomZone]
            ]
          (_, bestZone) = maximum counts
      push (DealDamageToEachEnemyUnitInZone pk bestZone 1)
    _ -> pure ()

blackDragon :: CardDef Unit
blackDragon = unit "core-145" "Black Dragon" do
  race DarkElf
  cost 6
  loyalty 3
  power 4
  hitPoints 5
  trait Creature
  body "Battlefield. Damage dealt by this unit cannot be cancelled."
  keyword DamageCannotBeCancelled

manticore :: CardDef Unit
manticore = unit "core-146" "Manticore" do
  race DarkElf
  cost 5
  loyalty 2
  power 4
  hitPoints 3
  trait Creature
  body "Battlefield only."
  keyword BattlefieldOnly

cauldronOfBlood :: CardDef Support
cauldronOfBlood = support "core-147" "Cauldron of Blood" do
  unique
  race DarkElf
  cost 3
  loyalty 4
  power 2
  trait CapitalCenter
  body "Battlefield. Your Witch Elf units gain {power}."

theBlackArk :: CardDef Support
theBlackArk = support "core-148" "The Black Ark" do
  unique
  race DarkElf
  cost 3
  loyalty 3
  power 1
  trait Building
  body "Kingdom. Forced: At the beginning of your turn, draw a card."
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k
      | k == self.controller
      , self.zone == KingdomZone ->
          push (Draw (Drawing StandardDraw self.controller))
    _ -> pure ()

whipOfAgony :: CardDef Support
whipOfAgony = support "core-149" "Whip of Agony" do
  race DarkElf
  cost 2
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target Dark Elf unit. Attached unit gains {power}{power}."

druchiiBanner :: CardDef Support
druchiiBanner = support "core-150" "Druchii Banner" do
  race DarkElf
  cost 1
  loyalty 1
  trait Attachment
  body "Attach to a target unit. Attached unit gains {power}; opponents pay 1 additional resource to target it."

witchbrew :: CardDef Support
witchbrew = support "core-151" "Witchbrew" do
  race DarkElf
  cost 1
  loyalty 1
  trait Attachment
  body "Attach to a target Dark Elf unit. Action: Sacrifice this card to give attached unit {power}{power}{power} until the end of the turn."
  action ActionDef
    { actionName = "Brew (sacrifice)"
    , actionCost = 0
    , actionTarget = NoTargetSchema
    , actionEffect = ActionEffect \_pk self _tgt -> case self.attachedTo of
        Just hostKey -> do
          push
            ( InstallModifier
                (UnitRef hostKey)
                (Modifier (GainPower 3) UntilEndOfTurn)
            )
          push (DestroySupport self.key)
        Nothing -> pure ()
    }

slaughterAtLustria :: CardDef Quest
slaughterAtLustria = quest "core-152" "Slaughter at Lustria" do
  race DarkElf
  cost 0
  loyalty 2
  body
    "Quest. Forced: At the beginning of your turn, place 1 token here if a unit is questing here.\n\
    \Action: Spend 3 tokens to corrupt up to 2 target units."
  -- Tokens only accrue while a unit is questing here. On crossing 3
  -- tokens, auto-corrupt 2 enemy units and reset.
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn k | k == self.controller -> do
      g <- getGame
      case findQuest self.key g of
        Just q | q.questingUnit /= Nothing -> do
          push (AdjustQuestTokens self.key 1)
          when (q.tokens + 1 >= 3) $ do
            push (AdjustQuestTokens self.key (-3))
            let enemies =
                  take 2 $
                    filter (\u -> u.controller /= self.controller && not u.corrupted) g.units
            traverse_ (\u -> push (CorruptUnit u.key)) enemies
        _ -> pure ()
    _ -> pure ()

khainesEmbrace :: CardDef Tactic
khainesEmbrace = tactic "core-153" "Khaine's Embrace" do
  race DarkElf
  cost 2
  loyalty 2
  body "Action: Destroy target unit with 2 or fewer hit points."
  tacticTargets EnemyUnitTargetSchema
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      let pick = case target of
            TargetUnit k -> case findUnit k g of
              Just u | u.controller /= pk && u.effectiveMaxHP <= 2 -> Just u
              _ -> firstWeak
            _ -> firstWeak
          firstWeak = case
            [u | u <- g.units, u.controller /= pk, u.effectiveMaxHP <= 2]
            of
              (u : _) -> Just u
              [] -> Nothing
      case pick of
        Just t -> push (DestroyUnit t.key)
        Nothing -> pure ()
    _ -> pure ()

murderousProwess :: CardDef Tactic
murderousProwess = tactic "core-154" "Murderous Prowess" do
  race DarkElf
  cost 1
  loyalty 2
  body "Action: Each of your Dark Elf units gains {power} until the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target | pk == self.controller -> do
      g <- getGame
      let mine =
            [ u
            | u <- g.units
            , u.controller == pk
            , DarkElf `elem` u.cardDef.races
            ]
      traverse_
        ( \u ->
            push
              ( InstallModifier
                  (UnitRef u.key)
                  (Modifier (GainPower 1) UntilEndOfTurn)
              )
        )
        mine
    _ -> pure ()

coldBloodedSlaughter :: CardDef Tactic
coldBloodedSlaughter = tactic "core-155" "Cold Blooded Slaughter" do
  race DarkElf
  cost 3
  loyalty 3
  body "Action: Deal damage equal to your hand size to a target unit."
  tacticTargets EnemyUnitTargetSchema
  onReceive $ Receive \msg owner self -> case msg of
    TacticResolved pk _code target | pk == self.controller -> do
      g <- getGame
      case resolveEnemyUnit pk target g of
        Just t -> push (DealDamageToUnit t.key (length owner.hand))
        Nothing -> pure ()
    _ -> pure ()

