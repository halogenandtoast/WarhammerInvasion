{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card (module Invasion.Card) where

import Control.Monad.State.Strict
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Invasion.CardDef
import Invasion.CardDef qualified as CardDef
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Capital
import Invasion.Game
import Invasion.Message
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

data SomeCardDef where
  UnitCardDef :: CardDef 'Unit -> SomeCardDef
  SupportCardDef :: CardDef 'Support -> SomeCardDef
  QuestCardDef :: CardDef 'Quest -> SomeCardDef
  TacticCardDef :: CardDef 'Tactic -> SomeCardDef

instance Show SomeCardDef where
  show (UnitCardDef card) = show card
  show (SupportCardDef card) = show card
  show (QuestCardDef card) = show card
  show (TacticCardDef card) = show card

instance ToJSON SomeCardDef where
  toJSON (UnitCardDef card) = toJSON card
  toJSON (SupportCardDef card) = toJSON card
  toJSON (QuestCardDef card) = toJSON card
  toJSON (TacticCardDef card) = toJSON card

newtype CardBuilder k a = CardBuilder (State (CardDef k) a)
  deriving newtype (Functor, Applicative, Monad, MonadState (CardDef k))

emptyCardDef :: CardCode -> String -> CardKind -> CardDef k
emptyCardDef code title kind =
  CardDef code title kind [] (Fixed 0) 0 0 Nothing [] Nothing Nothing [] False noReceive

unit :: CardCode -> String -> CardBuilder Unit () -> CardDef Unit
unit code title = buildCard $ (emptyCardDef code title Unit) {CardDef.hitPoints = Just (Fixed 1)}

support :: CardCode -> String -> CardBuilder Support a -> CardDef Support
support code title = buildCard $ emptyCardDef code title Support

quest :: CardCode -> String -> CardBuilder Quest a -> CardDef Quest
quest code title = buildCard $ emptyCardDef code title Quest

tactic :: CardCode -> String -> CardBuilder Tactic a -> CardDef Tactic
tactic code title = buildCard $ emptyCardDef code title Tactic

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

hitPoints :: Int -> CardBuilder Unit ()
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

-- | Builder setter for a card's bespoke 'Receive' handler. The default
-- (set by 'emptyCardDef') is 'noReceive' — a no-op.
onReceive :: Receive k -> CardBuilder k ()
onReceive r = modify \cardDef -> cardDef {receive = r}

-- | First in-play unit controlled by an opponent of 'pk'. Used as an
-- auto-target placeholder until the engine surfaces real action
-- prompts.
firstEnemyUnit :: PlayerKey -> Game -> Maybe UnitDetails
firstEnemyUnit pk g = case filter (\u -> u.controller /= pk) g.units of
  (u : _) -> Just u
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
    ]

-- TODO: re-implement the "+2 power while >= 2 developments in this zone"
-- effect on top of CardDef.receive once the engine surfaces
-- development-count-change events.
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

-- TODO: re-implement the "Action: pay 2 -> target unit +1 power until end
-- of turn" ability once the engine exposes action prompts on top of
-- CardDef.receive.
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

-- TODO: re-implement the "+2 power while a capital section is burning"
-- effect once burning is wired through CardDef.receive.
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

stubbornRefusal :: CardDef Tactic
stubbornRefusal = tactic "core-020" "Stubborn Refusal" do
  race Dwarf
  cost 2
  loyalty 1
  body
    "Action: Move all damage from one target unit to another target unit in any player's corresponding zone."

strikingTheGrudge :: CardDef Tactic
strikingTheGrudge = tactic "core-021" "Striking the Grudge" do
  race Dwarf
  cost 1
  loyalty 3
  body
    "Action: One target attacking or defending unit gains {power}{power} until the end of the turn."
  flavor "Honour redeemed, oaths fulfilled."

grudgeThrowerAssault :: CardDef Tactic
grudgeThrowerAssault = tactic "core-022" "Grudge Thrower Assault" do
  race Dwarf
  cost 2
  loyalty 3
  body
    "Play during combat, after damage has been assigned.\nAction: Destroy one target attacking unit."

demolition :: CardDef Tactic
demolition = tactic "core-023" "Demolition!" do
  race Dwarf
  cost 2
  loyalty 1
  body "Action: Destroy one target support card or development."
  flavor "KABOOM!"

wakeTheMountain :: CardDef Tactic
wakeTheMountain = tactic "core-024" "Wake the Mountain" do
  race Dwarf
  cost 3
  loyalty 2
  body
    "Action: Put the top three cards of your deck into your battlefield or kingdom facedown as developments. (All three developments must go in the same zone.)"

masterRuneOfValaya :: CardDef Tactic
masterRuneOfValaya = tactic "core-025" "Master Rune of Valaya" do
  race Dwarf
  cost 2
  loyalty 1
  traits [Spell, Rune]
  body "Action: Cancel all damage assigned during the battlefield phase this turn."
  flavor "Valaya preseve and protect us in our hour of need!"

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
  -- Auto-targeting: the engine doesn't surface a player choice yet, so
  -- on each player's turn-begin we sacrifice the first unit found in
  -- the matching zone for each player. Real targeting waits on the
  -- action-prompt iteration.
  onReceive $ Receive \msg _owner self -> case msg of
    BeginTurn _turnOwner
      | _turnOwner == self.controller -> do
          g <- getGame
          let pick pk = firstUnitOfInZone pk self.zone g
          case pick Player1 of
            Just u -> push (DestroyUnit u.key)
            Nothing -> pure ()
          case pick Player2 of
            Just u -> push (DestroyUnit u.key)
            Nothing -> pure ()
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
    TacticResolved pk _code | pk == self.controller -> do
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
  -- Approximation: when an opponent's unit leaves play we auto-attach
  -- if the controller can afford the 1-resource cost. The +power buff
  -- per experience waits on dynamic-power computation; for now we just
  -- track the list, so the gain shows up only if combat code reads
  -- 'experiences'.
  onReceive $ Receive \msg owner self -> case msg of
    UnitLeftPlay leftBy _ukey _zone code
      | leftBy /= self.controller
      , Resources r <- owner.resources
      , r >= 1 -> do
          -- Pay the resource by sending a synthetic effect: cards
          -- can't directly debit resources today, so we approximate by
          -- attaching without payment. The TODO remains.
          push (AttachExperience self.key code)
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
  -- Compressed: target the first friendly unit and immediately apply 2
  -- damage (the +3 power buff needs dynamic-power computation; the
  -- "until end of turn" timing needs deferred-effect support). The
  -- damage-half is the more impactful half of the rules text.
  onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code | pk == self.controller -> do
      g <- getGame
      case filter (\u -> u.controller == self.controller) g.units of
        (target : _) -> push (DealDamageToUnit target.key 2)
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
              push (DealDamageToUnit hostKey 1)
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
  -- On entry: load 4 tokens. On your turn-begin: tick one off. The
  -- "put up to 3 Chaos units into play" payoff is a player action and
  -- waits on the action-prompt iteration; for now it just empties.
  onReceive $ Receive \msg _owner self -> case msg of
    SupportEnteredPlay _pk key
      | key == self.key ->
          push (AdjustSupportTokens self.key 4)
    BeginTurn turnOwner
      | turnOwner == self.controller && self.tokens > 0 ->
          push (AdjustSupportTokens self.key (-1))
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
              -- TODO: also remove this quest from play (no destroy-quest
              -- message yet).
            _ -> pure ()
    _ -> pure ()

