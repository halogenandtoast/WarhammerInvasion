{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card (module Invasion.Card) where

import Control.Monad.State.Strict
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy
import Invasion.Capital
import Invasion.CardDef
import Invasion.CardDef qualified as CardDef
import Invasion.Entity
import {-# SOURCE #-} Invasion.Game
import Invasion.Matcher
import Invasion.Modifier
import Invasion.Prelude
import Invasion.Types

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
  CardDef code title kind [] (Fixed 0) 0 0 Nothing [] Nothing Nothing [] False

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
body f = modify \cardDef -> cardDef {text = Just f}

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

data ConstantAbility = Modified (Ref Target) Modifier

newtype ConstantAbilityBuilder m a = ConstantAbilityBuilder (StateT [ConstantAbility] m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState [ConstantAbility], HasGame)

data Action = Action
  { source :: Ref Source
  , actionCost :: Cost
  , targets :: Maybe UnitMatcher
  , withTarget :: forall m. HasGame m => UnitDetails -> ConstantAbilityBuilder m ()
  }

actionCost :: Cost -> ActionBuilder ()
actionCost c = modify \action -> action {actionCost = c}

targets :: UnitMatcher -> ActionBuilder ()
targets t = modify \action -> action {targets = Just t}

withTarget
  :: (forall m. HasGame m => UnitDetails -> ConstantAbilityBuilder m ()) -> ActionBuilder ()
withTarget f = modify \action -> action {withTarget = f}

newtype ActionsBuilder a = ActionsBuilder (State [Action] a)
  deriving newtype (Functor, Applicative, Monad, MonadState [Action])

newtype ActionBuilder a = ActionBuilder (State Action a)
  deriving newtype (Functor, Applicative, Monad, MonadState Action)

runActionBuilder :: Reference a => a -> ActionBuilder () -> ActionsBuilder Action
runActionBuilder source (ActionBuilder inner) = pure $ execState inner (Action (toRef source) NoCost Nothing (const (pure ())))

questAction :: Reference a => a -> ActionBuilder () -> ActionsBuilder ()
questAction a f = do
  action <- runActionBuilder a f
  modify (<> [action])

class SomeKindOfCard (KindOfCard a) => Card a where
  type KindOfCard a :: CardKind
  asName :: proxy a -> String
  asCardCode :: proxy a -> CardCode
  asCardDef :: proxy a -> CardDef (KindOfCard a)
  constant :: HasGame m => a -> ConstantAbilityBuilder m ()
  constant _ = pure ()
  actions :: a -> ActionsBuilder ()
  actions _ = pure ()

class SomeKindOfCard a where
  toSomeKindOfCard :: CardDef a -> SomeCardDef

someCardDef :: forall a. Card a => SomeCardDef
someCardDef = toSomeKindOfCard $ asCardDef (Proxy @a)

instance SomeKindOfCard Unit where
  toSomeKindOfCard = UnitCardDef

instance SomeKindOfCard Support where
  toSomeKindOfCard = SupportCardDef

allCards :: Map CardCode SomeCardDef
allCards =
  Map.fromList
    [ ("core-001", someCardDef @DefenderOfTheHold)
    , ("core-002", someCardDef @ZhufbarEngineers)
    , ("core-003", someCardDef @HammererOfKarakAzul)
    , ("core-004", someCardDef @TrollSlayers)
    , ("core-005", someCardDef @Runesmith)
    , ("core-006", someCardDef @DurgnarTheBold)
    , ("core-007", UnitCardDef kingKazador)
    , ("core-008", UnitCardDef dwarfCannonCrew)
    , ("core-009", UnitCardDef dwarfMasons)
    , ("core-010", UnitCardDef dwarfRanger)
    , ("core-011", UnitCardDef mountainBrigade)
    , ("core-012", UnitCardDef ironbreakersOfAnkhor)
    , ("core-013", someCardDef @RuneOfFortitude)
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
    ]

newtype TrollSlayers = TrollSlayers UnitDetails
  deriving stock Show
  deriving newtype (Reference, Entity Unit)

instance HasField "controller" TrollSlayers PlayerKey where
  getField (TrollSlayers details) = details.controller

instance Card TrollSlayers where
  type KindOfCard TrollSlayers = Unit
  asCardCode _ = "core-004"
  asName _ = "Troll Slayers"
  asCardDef = unit' do
    race Dwarf
    cost 3
    loyalty 1
    power 1
    hitPoints 3
    trait Slayer
    body
      "Battlefield. This unit gains {power}{power} while you have at least two developments in this zone."
  constant this = do
    battlefield this \zone -> when (zone.developments >= 2) (constantly $ gainPower this 2)

resources :: Int -> Cost
resources n = Resources (Fixed n)

newtype Runesmith = Runesmith UnitDetails
  deriving stock Show
  deriving newtype (Reference, Entity Unit)

instance HasField "controller" Runesmith PlayerKey where
  getField (Runesmith details) = details.controller

instance Card Runesmith where
  type KindOfCard Runesmith = Unit
  asCardCode _ = "core-005"
  asName _ = "Runesmith"
  asCardDef = unit' do
    race Dwarf
    cost 2
    loyalty 1
    power 1
    hitPoints 1
    trait Priest
    body
      "Quest. Action: Spend 2 resources to have a target unit gain {power} until the end of the turn."
  actions this = do
    questAction this do
      actionCost $ resources 2
      targets AnyUnit
      withTarget \target -> untilEndOfTurn (gainPower target 1)

newtype DurgnarTheBold = DurgnarTheBold UnitDetails
  deriving stock Show
  deriving newtype (Reference, Entity Unit)

instance HasField "controller" DurgnarTheBold PlayerKey where
  getField (DurgnarTheBold details) = details.controller

instance Card DurgnarTheBold where
  type KindOfCard DurgnarTheBold = Unit
  asCardCode _ = "core-006"
  asName _ = "Durgnar the Bold"
  asCardDef = unit' do
    unique
    race Dwarf
    cost 3
    loyalty 3
    power 2
    hitPoints 2
    traits [Hero, Warrior]
    body
      "Limit one Hero per zone.\nThis unit gains {power}{power} while one section of your capital is burning."
  constant this = do
    capital this.controller \controllerCapital -> when (any (.burning) controllerCapital.zones) (constantly $ gainPower this 2)

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

data RuneOfFortitude
instance Card RuneOfFortitude where
  type KindOfCard RuneOfFortitude = Support
  asName _ = "Rune of Fortitude"
  asCardCode _ = "core-013"
  asCardDef = support' do
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

data DefenderOfTheHold
instance Card DefenderOfTheHold where
  type KindOfCard DefenderOfTheHold = Unit
  asCardCode _ = "core-001"
  asName _ = "Defender of the Hold"
  asCardDef = unit' do
    race Dwarf
    cost 1
    loyalty 1
    power 1
    hitPoints 1
    trait Warrior
    body "Battlefield only."
    flavor "My blood for the hold, 'tis a fair trade."
    keyword BattlefieldOnly

data ZhufbarEngineers
instance Card ZhufbarEngineers where
  type KindOfCard ZhufbarEngineers = Unit
  asCardCode _ = "core-002"
  asName _ = "Zhufbar Engineers"
  asCardDef = unit' do
    race Dwarf
    cost 3
    loyalty 1
    power 1
    hitPoints 3
    trait Engineer
    flavor
      "The engineers of Zhufbar are the finest in the Old World. They can build anything, and they can destroy anything."

data HammererOfKarakAzul
instance Card HammererOfKarakAzul where
  type KindOfCard HammererOfKarakAzul = Unit
  asCardCode _ = "core-003"
  asName _ = "Hammerer of Karak Azul"
  asCardDef = unit' do
    race Dwarf
    cost 2
    loyalty 1
    power 1
    hitPoints 2
    traits [Warrior, Elite]
    toughness 1
    body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."
    flavor "\"The hammer blow rings out doom to our foe.\"\n-Ancient Hammerer saying"

unit' :: forall a proxy. Card a => CardBuilder Unit () -> proxy a -> CardDef Unit
unit' builder _ = unit (asCardCode (Proxy @a)) (asName (Proxy @a)) builder

support' :: forall a proxy. Card a => CardBuilder Support () -> proxy a -> CardDef Support
support' builder _ = support (asCardCode (Proxy @a)) (asName (Proxy @a)) builder

modified
  :: (Reference a, HasGame m, ?scope :: ModifierScope)
  => a -> ModifierDetails -> ConstantAbilityBuilder m ()
modified a details = modify (<> [Modified (toRef a) (Modifier details ?scope)])

gainPower
  :: (Reference a, HasGame m, ?scope :: ModifierScope) => a -> Int -> ConstantAbilityBuilder m ()
gainPower a n = modified a (GainPower n)

untilEndOfTurn :: ((?scope :: ModifierScope) => m ()) -> m ()
untilEndOfTurn f = let ?scope = UntilEndOfTurn in f

constantly :: ((?scope :: ModifierScope) => m ()) -> m ()
constantly f = let ?scope = ConstantScope in f
