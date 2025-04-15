{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Proxy
import Data.String (IsString)
import Data.Traversable
import GHC.Records
import Queue
import System.Random.Shuffle

sample :: MonadRandom m => NonEmpty a -> m a
sample xs = do
  idx <- getRandomR (0, NE.length xs - 1)
  pure $ xs NE.!! idx

sample2 :: MonadRandom m => a -> a -> m a
sample2 x y = sample (x :| [y])

newtype CardCode = CardCode String
  deriving newtype (Eq, Ord, Show, IsString)

class Monad m => HasGame m where
  getGame :: m Game

data GameState
  = SetupGame GameState
  | FinishedGame PlayerKey
  | IdleGame
  | DoTurn PlayerKey GameState
  | GamePhase Phase GameState
  | WaitOnPlayer PlayerKey GameState
  deriving stock Show

data Game = Game
  { player1 :: Player
  , player2 :: Player
  , firstPlayer :: PlayerKey
  , currentPlayer :: PlayerKey
  , modifiers :: Map (Ref Target) [ModifierDetails]
  , state :: GameState
  }
  deriving stock Show

instance HasField "over" Game Bool where
  getField g = case g.state of
    FinishedGame _ -> True
    _ -> False

data Phase = KingdomPhase | QuestPhase | CapitalPhase | BattlefieldPhase
  deriving stock (Show, Eq)

data PlayerState
  = IdlePlayer
  | Eliminated
  | Draw Drawing PlayerState
  | PerformPhase Phase PlayerState
  | ShuffleDeck PlayerState
  deriving stock Show

data Player = Player
  { key :: PlayerKey
  , eliminated :: Bool
  , state :: PlayerState
  , capital :: Capital
  , hand :: [SomeCardDef]
  , deck :: [SomeCardDef]
  }
  deriving stock Show

instance HasField "battlefield" Player Battlefield where
  getField p = p.capital.battlefield

instance HasField "idle" Player Bool where
  getField p = case p.state of
    IdlePlayer -> True
    _ -> False

newtype Capital = Capital
  { battlefield :: Battlefield
  }
  deriving stock Show

instance HasField "sections" Capital [Section] where
  getField (Capital bf) = [BattlefieldSection bf]

data Section where
  BattlefieldSection :: Battlefield -> Section

instance HasField "burning" Section Bool where
  getField (BattlefieldSection bf) = bf.burning

newtype GameT a = GameT (StateT Game IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadState Game)

data Number = Fixed Int | Variable
  deriving stock Show

data CardKind = Unit | Support | Quest | Tactic | DraftFormat
  deriving stock Show

data Trait
  = Warrior
  | Spell
  | Engineer
  | Elite
  | Slayer
  | Priest
  | Hero
  | Ranger
  | Rune
  | Building
  | Attachment
  | Weapon
  | Siege
  deriving stock Show

data Race = Dwarf
  deriving stock Show

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

data CardDef (k :: CardKind) = CardDef
  { code :: CardCode
  , title :: String
  , kind :: CardKind
  , races :: [Race]
  , cost :: Number
  , loyalty :: Int
  , power :: Int
  , hitPoints :: Int
  , traits :: [Trait]
  , text :: Maybe String
  , flavor :: Maybe String
  , keywords :: [Keyword]
  , unique :: Bool
  }
  deriving stock Show

data Keyword = Toughness Number | BattlefieldOnly | Scout
  deriving stock Show

newtype CardBuilder k a = CardBuilder (State (CardDef k) a)
  deriving newtype (Functor, Applicative, Monad, MonadState (CardDef k))

unit :: CardCode -> String -> CardBuilder Unit () -> CardDef Unit
unit code title builder = runCardBuilder builder (CardDef code title Unit [] (Fixed 0) 0 0 0 [] Nothing Nothing [] False)

support :: CardCode -> String -> CardBuilder Support a -> CardDef Support
support code title builder =
  runCardBuilder builder (CardDef code title Support [] (Fixed 0) 0 0 0 [] Nothing Nothing [] False)

quest :: CardCode -> String -> CardBuilder Quest a -> CardDef Quest
quest code title builder = runCardBuilder builder (CardDef code title Quest [] (Fixed 0) 0 0 0 [] Nothing Nothing [] False)

tactic :: CardCode -> String -> CardBuilder Tactic a -> CardDef Tactic
tactic code title builder = runCardBuilder builder (CardDef code title Tactic [] (Fixed 0) 0 0 0 [] Nothing Nothing [] False)

runCardBuilder :: CardBuilder k a -> CardDef k -> CardDef k
runCardBuilder (CardBuilder inner) = execState inner

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
hitPoints hp = modify \cardDef -> cardDef {hitPoints = hp}

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

newtype ModifierDetails = GainPower Int
  deriving stock Show

data ModifierScope = UntilEndOfTurn | ConstantScope

data Modifier = Modifier
  { details :: ModifierDetails
  , scope :: ModifierScope
  }

data ConstantAbility = Modified (Ref Target) Modifier

instance HasGame m => HasGame (StateT s m) where
  getGame = lift getGame

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

getPlayer :: HasGame m => PlayerKey -> m Player
getPlayer pkey = do
  g <- getGame
  pure $ case pkey of
    Player1 -> g.player1
    Player2 -> g.player2

getBattleField :: HasGame m => PlayerKey -> m Battlefield
getBattleField pkey = do
  p <- getPlayer pkey
  pure $ p.battlefield

getCapital :: HasGame m => PlayerKey -> m Capital
getCapital pkey = do
  p <- getPlayer pkey
  pure $ p.capital

battlefield :: (HasGame m, HasField "controller" a PlayerKey) => a -> (Battlefield -> m ()) -> m ()
battlefield a f = getBattleField a.controller >>= f

capital :: HasGame m => PlayerKey -> (Capital -> m ()) -> m ()
capital pkey f = getCapital pkey >>= f

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

data Cost = Resources Number | NoCost

newtype QuestZone = QuestZone {developments :: Int}
  deriving stock Show

data Battlefield = Battlefield {developments :: Int, burning :: Bool}
  deriving stock Show

unit' :: forall a proxy. Card a => CardBuilder Unit () -> proxy a -> CardDef Unit
unit' builder _ = unit (asCardCode (Proxy @a)) (asName (Proxy @a)) builder

support' :: forall a proxy. Card a => CardBuilder Support () -> proxy a -> CardDef Support
support' builder _ = support (asCardCode (Proxy @a)) (asName (Proxy @a)) builder

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

data Zone = BattlefieldZone
  deriving stock Show

newtype UnitKey = UnitKey Int
  deriving stock (Show, Eq, Ord)

data RefKind = Target | Source

newtype Ref (k :: RefKind) = UnitRef UnitKey
  deriving stock (Show, Eq, Ord)

class Reference a where
  toRef :: a -> Ref k

data UnitMatcher = AnyUnit

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

data UnitDetails = UnitDetails
  { key :: UnitKey
  , controller :: PlayerKey
  , zone :: Zone
  , cardDef :: CardDef Unit
  }
  deriving stock Show

instance Reference UnitDetails where
  toRef details = UnitRef details.key

type family DetailsOfKind (k :: CardKind)
type family KeyOfKind (k :: CardKind)

type instance DetailsOfKind Unit = UnitDetails
type instance KeyOfKind Unit = UnitKey

data family Field (k :: CardKind) typ

data instance Field Unit typ where
  UnitController :: Field Unit PlayerKey
  UnitZone :: Field Unit Zone
  UnitPower :: Field Unit Int

class Entity (k :: CardKind) a where
  toDetails :: a -> DetailsOfKind k
  toKey :: a -> KeyOfKind k
  project :: HasGame m => Field k typ -> a -> m typ

getModifiers :: (HasGame m, Reference a) => a -> m [ModifierDetails]
getModifiers a = do
  g <- getGame
  pure $ fromMaybe [] $ Map.lookup (toRef a) g.modifiers

instance Entity Unit UnitDetails where
  toDetails = id
  toKey = (.key)
  project = \case
    UnitController -> pure . (.controller)
    UnitZone -> pure . (.zone)
    UnitPower -> \details -> do
      mods <- getModifiers details
      let additionalPower = sum [n | GainPower n <- mods]
      pure $ details.cardDef.power + additionalPower

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
    capital this.controller \controllerCapital -> when (any (.burning) controllerCapital.sections) (constantly $ gainPower this 2)

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

dwarfStarterDeck :: [CardCode]
dwarfStarterDeck =
  replicate 3 "core-001" -- Defender of the Hold
    <> replicate 1 "core-002" -- Zhufbar Engineers
    <> replicate 3 "core-003" -- Hammerer of Karak Azul
    <> replicate 3 "core-004" -- Troll Slayers
    <> replicate 3 "core-005" -- Runesmith
    <> replicate 1 "core-006" -- Durgnar the Bold
    <> replicate 1 "core-007" -- King Kazador
    <> replicate 1 "core-008" -- Dwarf Cannon Crew
    <> replicate 1 "core-009" -- Dwarf Masons
    <> replicate 2 "core-010" -- Dwarf Ranger
    <> replicate 2 "core-011" -- Mountain Brigade
    <> replicate 1 "core-012" -- Ironbreakers of Ankhor
    <> replicate 1 "core-013" -- Rune of Fortitude
    <> replicate 3 "core-014" -- Keystone Forge
    <> replicate 1 "core-015" -- Organ Gun
    <> replicate 1 "core-016" -- Master Rune of Dismay
    <> replicate 2 "core-017" -- A Glorious Death
    <> replicate 2 "core-018" -- Grudge Thrower
    <> replicate 1 "core-019" -- Burying the Grudge
    <> replicate 1 "core-020" -- Stubborn Refusal
    <> replicate 2 "core-021" -- Striking the Grudge
    <> replicate 1 "core-022" -- Grudge Thrower Assault
    <> replicate 1 "core-023" -- Demolition!
    <> replicate 1 "core-024" -- Wake the Mountain
    <> replicate 1 "core-025" -- Master Rune of Valaya

type DeckLoadError = String

loadDeck :: [CardCode] -> Either DeckLoadError [SomeCardDef]
loadDeck cs = for cs \c ->
  case Map.lookup c allCards of
    Nothing -> Left $ "Card not found: " <> show c
    Just cardDef -> Right cardDef

runGame :: GameT a -> Game -> IO a
runGame (GameT inner) = evalStateT inner

data PlayerKey = Player1 | Player2
  deriving stock (Show, Eq)

data DrawingKind = StartingHand | StandardDraw
  deriving stock Show

newtype Drawing = Drawing
  { kind :: DrawingKind
  }
  deriving stock Show

gameMain :: GameT ()
gameMain = do
  g <- get
  unless g.over do
    g' <- tick g
    put g'
    gameMain

class Tick a where
  tick :: a -> GameT a

overPlayer :: PlayerKey -> (Player -> Player) -> Game -> Game
overPlayer k f g =
  case k of
    Player1 -> g {player1 = f g.player1}
    Player2 -> g {player2 = f g.player2}

withPlayer :: PlayerKey -> (Player -> a) -> Game -> a
withPlayer k f g =
  case k of
    Player1 -> f g.player1
    Player2 -> f g.player2

class HasState a where
  type StateOf a
  updateState :: (StateOf a -> StateOf a) -> a -> a

instance HasState Player where
  type StateOf Player = PlayerState
  updateState f p = p {state = f p.state}

instance Tick Game where
  tick g = case g.state of
    SetupGame nextState -> do
      firstPlayer <- sample2 Player1 Player2
      let player1 = withPlayer Player1 (updateState (ShuffleDeck . Draw (Drawing StartingHand))) g
      let player2 = withPlayer Player2 (updateState (ShuffleDeck . Draw (Drawing StartingHand))) g
      pure
        $ g
          { firstPlayer
          , state = WaitOnPlayer Player1 (WaitOnPlayer Player2 (DoTurn firstPlayer nextState))
          , player1
          , player2
          }
    IdleGame -> do
      player1 <- tick g.player1
      player2 <- tick g.player2
      pure $ g {player1, player2}
    DoTurn currentPlayer nextState -> do
      let nextPlayer = if currentPlayer == Player1 then Player2 else Player1
      pure
        $ g
          { state =
              GamePhase KingdomPhase
                $ GamePhase QuestPhase
                $ GamePhase CapitalPhase
                $ GamePhase BattlefieldPhase
                $ DoTurn nextPlayer nextState
          }
    GamePhase KingdomPhase nextState -> do
      liftIO $ putStrLn $ "Begin Kingdom Phase for: " <> show g.currentPlayer
      let update = overPlayer g.currentPlayer (updateState $ PerformPhase KingdomPhase)
      pure $ update $ g {state = WaitOnPlayer g.currentPlayer nextState}
    GamePhase QuestPhase nextState -> do
      liftIO $ putStrLn $ "Begin Quest Phase for: " <> show g.currentPlayer
      pure $ g {state = WaitOnPlayer g.currentPlayer nextState}
    GamePhase CapitalPhase nextState -> do
      liftIO $ putStrLn $ "Begin Capital Phase for: " <> show g.currentPlayer
      pure $ g {state = WaitOnPlayer g.currentPlayer nextState}
    GamePhase BattlefieldPhase _nextState -> do
      liftIO $ putStrLn $ "Begin Battlefield Phase for: " <> show g.currentPlayer
      pure $ g {state = FinishedGame g.currentPlayer}
    WaitOnPlayer currentPlayer nextState -> do
      case currentPlayer of
        Player1 | g.player1.idle -> pure $ g {state = nextState}
        Player2 | g.player2.idle -> pure $ g {state = nextState}
        _ -> do
          p1 <- tick g.player1
          p2 <- tick g.player2
          let
            nextState' =
              if p1.eliminated || p2.eliminated
                then FinishedGame currentPlayer
                else g.state
          pure $ g {player1 = p1, player2 = p2, state = nextState'}
    FinishedGame _ -> pure g

instance Tick Player where
  tick p = case p.state of
    IdlePlayer -> pure p
    ShuffleDeck nextState -> do
      liftIO $ putStrLn $ "Shuffling deck " <> show p.key
      cards' <- liftIO $ shuffleM p.deck
      pure $ p {deck = cards', state = nextState}
    PerformPhase KingdomPhase nextState -> do
      liftIO $ putStrLn $ "Performing Kingdom Phase for: " <> show p.key
      pure $ p {state = nextState}
    PerformPhase QuestPhase nextState -> do
      liftIO $ putStrLn $ "Performing Quest Phase for: " <> show p.key
      pure $ p {state = nextState}
    PerformPhase CapitalPhase nextState -> do
      liftIO $ putStrLn $ "Performing Capital Phase for: " <> show p.key
      pure $ p {state = nextState}
    PerformPhase BattlefieldPhase nextState -> do
      liftIO $ putStrLn $ "Performing Battlefield Phase for: " <> show p.key
      pure $ p {state = nextState}
    Draw drawing nextState -> do
      liftIO $ putStrLn $ "Drawing " <> show drawing.kind <> " for " <> show p.key
      case drawing.kind of
        StartingHand -> do
          let (hand, deck') = splitAt 7 p.deck
          pure $ p {hand, deck = deck', state = nextState}
        StandardDraw -> do
          case p.deck of
            [] -> pure $ p {state = Eliminated}
            (card : deck') -> pure $ p {hand = card : p.hand, deck = deck', state = nextState}

newPlayer :: PlayerKey -> [CardCode] -> Either DeckLoadError Player
newPlayer k cs = Player k False IdlePlayer (Capital $ Battlefield 0 False) [] <$> loadDeck cs

newGame :: [CardCode] -> [CardCode] -> Either DeckLoadError Game
newGame deck1 deck2 = do
  player1 <- newPlayer Player1 deck1
  player2 <- newPlayer Player2 deck2
  pure $ Game player1 player2 Player1 Player1 mempty (SetupGame IdleGame)

main :: IO ()
main = do
  case newGame dwarfStarterDeck dwarfStarterDeck of
    Left err -> error err
    Right game -> runGame gameMain game
