{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.CardDef (module Invasion.CardDef) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Invasion.Player (Player)
import Invasion.Prelude
import Invasion.Types
import Queue (HasQueue)
import {-# SOURCE #-} Invasion.Engine (HasPromptIO)
import {-# SOURCE #-} Invasion.Game (Game, HasGame)
import {-# SOURCE #-} Invasion.Message (Message)

data Keyword
  = Toughness Number
  | BattlefieldOnly
  | KingdomOnly
  | QuestOnly
  | Scout
  | Limited
  | DamageCannotBeCancelled
  | Counterstrike Int
  | PlayInOpponentArea
    -- ^ Quest enters play in the opponent's play area while remaining
    -- under the playing player's control. Used by Dominion of Chaos.
  | Ambush
    -- ^ Triggerable only while the host development is facedown
    -- (FAQ 2.2 v2.0 keyword). If an effect has flipped the development
    -- face-up, its Ambush ability cannot fire.
  | OrderOnly
    -- ^ Neutral-card restriction: cannot be included in a Destruction
    -- (Chaos / Orc / Dark Elf) deck.
  | DestructionOnly
    -- ^ Neutral-card restriction: cannot be included in an Order
    -- (Empire / Dwarf / High Elf) deck.
  | LimitOneHeroPerZone
    -- ^ Hero restriction. While a player controls a Hero in a given
    -- zone, neither player may put, play, or move another Hero into
    -- that same zone (FAQ 2.2 clarification).
  deriving stock (Show, Eq)

data Cost = PayResources Number | NoCost

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
  | Daemon
  | Creature
  | Sorcerer
  | Knight
  | Cavalry
  | Mission
  | QuestTrait
  | Wasteland
  | CapitalCenter
  | Rift
  | Relic
  deriving stock (Show, Eq)

mconcat
  [ deriveToJSON defaultOptions ''Keyword
  , deriveToJSON defaultOptions ''Trait
  ]

-- | Open type family of in-play self-references, indexed by card kind.
-- Instances are declared next to each kind's in-play record in
-- 'Invasion.Entity'.
type family InPlay (k :: CardKind)

-- | Open type family of per-kind extras records. The engine queries
-- these fields when it would otherwise have to case on a card's
-- 'code'. A new kind's instance is declared next to its in-play
-- record in 'Invasion.Entity'.
type family Extras (k :: CardKind)

-- | Per-kind defaults. Each instance provides a record whose fields
-- are no-ops — cards override the slices they care about via the
-- builder helpers in 'Invasion.Card'.
class HasDefaultExtras (k :: CardKind) where
  defaultExtras :: Extras k

-- | Unit-specific tunables read by the engine each turn / each combat.
data UnitExtras = UnitExtras
  { selfPowerBonus :: Game -> InPlay Unit -> Int
    -- ^ Game-state-derived self power bonus (Troll Slayers,
    -- Durgnar the Bold, Korhil, Crone Hellebron, Skulltaker). Folded
    -- into the unit's cached @effectivePower@ each step.
  , combatPowerBonus :: Game -> InPlay Unit -> Int
    -- ^ Extra damage the unit deals during combat (Lord of Khorne
    -- per burning zone, Gorbad Ironclaw while attacking). The
    -- function inspects @g.combat@ to decide attacker vs defender
    -- vs out-of-combat behavior.
  , unitAuraPower :: Game -> InPlay Unit -> InPlay Unit -> Int
    -- ^ Power this unit grants to another unit while both are in
    -- play (Karl Franz buffs other Empire units; Templar of Sigmar
    -- buffs other Warriors in the battlefield). Args: game, source
    -- (this unit), target unit.
  , canAttackZone :: Game -> PlayerKey -> ZoneKind -> InPlay Unit -> Bool
    -- ^ Whether this unit may be declared as an attacker against
    -- the named defender zone (Sworn of Khorne requires a corrupted
    -- defender). Default: always True.
  , damageCap :: Maybe Int
    -- ^ Per-turn damage cap on this unit (Daemonettes of Slaanesh).
  , corruptsOnCombatDamage :: Bool
    -- ^ Corrupt any enemy this unit dealt non-zero combat damage to
    -- (Plaguebearers of Nurgle, Beasts of Nurgle).
  , extraTargetTax :: Game -> PlayerKey -> InPlay Unit -> Int
    -- ^ Extra resources the named (effect-firing) player must pay to
    -- target this unit (King Kazador: 3 for opponents). Args: game,
    -- the player firing the effect, this unit.
  , damageMultiplierWhileInPlay :: Int
    -- ^ Multiplier on every applied damage event while this unit is
    -- in play (Bloodletter: 2). Multipliers stack multiplicatively.
  }

-- | Support-specific tunables.
data SupportExtras = SupportExtras
  { attachmentPowerBonus :: Int
    -- ^ Static power contribution when this support is attached to
    -- a unit (Daemonsword, Hammer of Sigmar, etc.).
  , attachmentHPBonus :: Int
    -- ^ Static HP contribution when attached (Daemonsword).
  , grantsUncancellableDamage :: Bool
    -- ^ While attached, the host unit's combat damage is
    -- uncancellable (Hammer of Sigmar).
  , supportAuraPower :: Game -> InPlay Support -> InPlay Unit -> Int
    -- ^ Power this support grants to a unit (Iron Tower → Chaos
    -- units in battlefield; Cauldron of Blood → Witch Elves; Da Bad
    -- Moon static slice).
  , supportCombatBonus :: Game -> InPlay Support -> InPlay Unit -> Int
    -- ^ Extra combat damage this support grants to a unit (Rift of
    -- Battle gives every unit +1; Organ Gun adds +2 while the host
    -- defends; Da Bad Moon and Big Boss's Banner buff Orc
    -- attackers).
  , zonePowerBonus :: Game -> InPlay Support -> ZoneKind -> Int
    -- ^ Extra power this support contributes to a zone of its
    -- controller (Lighthouse of Lothern, Rift of Chaos).
  , globalCostAdjustment :: Game -> InPlay Support -> PlayerKey -> CardCodeFilter -> Int
    -- ^ Adjustment this support imposes on the printed cost of
    -- another card being played. Args: game, this support, playing
    -- player, a filter describing the target card.
    --
    -- Imperial Crown: -1 for the controller's Empire heroes while
    -- in their kingdom. Master Rune of Dismay: +1 for the opponent's
    -- units while in the opponent's kingdom.
  , runeOfFortitudeTax :: Bool
    -- ^ Marks the printed Rune of Fortitude effect: every attacker
    -- of the zone owes 1 resource to its controller or eats a
    -- @-1@ power penalty for the combat.
  }

-- | Static metadata about a card that's currently being played, used
-- by external cost-adjustment hooks. Decouples the @globalCostAdjustment@
-- callback from any one @CardDef k@ so it can be invoked uniformly
-- across all card kinds. Fields are prefixed with @cf@ so record-update
-- syntax doesn't become ambiguous with 'CardDef'\'s @races@/@traits@/…
data CardCodeFilter = CardCodeFilter
  { cfCode :: CardCode
  , cfKind :: CardKind
  , cfRaces :: [Race]
  , cfTraits :: [Trait]
  }
  deriving stock Show

-- | Quest-specific tunables. Empty for now — quest behavior is
-- entirely message-driven today.
data QuestExtras = QuestExtras

-- | Tactic-specific tunables. Empty for now.
data TacticExtras = TacticExtras

-- | Legend-specific tunables. Empty for now.
data LegendExtras = LegendExtras

type instance Extras Unit = UnitExtras
type instance Extras Support = SupportExtras
type instance Extras Quest = QuestExtras
type instance Extras Tactic = TacticExtras
type instance Extras Legend = LegendExtras

instance HasDefaultExtras Unit where
  defaultExtras = UnitExtras
    { selfPowerBonus = \_ _ -> 0
    , combatPowerBonus = \_ _ -> 0
    , unitAuraPower = \_ _ _ -> 0
    , canAttackZone = \_ _ _ _ -> True
    , damageCap = Nothing
    , corruptsOnCombatDamage = False
    , extraTargetTax = \_ _ _ -> 0
    , damageMultiplierWhileInPlay = 1
    }

instance HasDefaultExtras Support where
  defaultExtras = SupportExtras
    { attachmentPowerBonus = 0
    , attachmentHPBonus = 0
    , grantsUncancellableDamage = False
    , supportAuraPower = \_ _ _ -> 0
    , supportCombatBonus = \_ _ _ -> 0
    , zonePowerBonus = \_ _ _ -> 0
    , globalCostAdjustment = \_ _ _ _ -> 0
    , runeOfFortitudeTax = False
    }

instance HasDefaultExtras Quest where
  defaultExtras = QuestExtras

instance HasDefaultExtras Tactic where
  defaultExtras = TacticExtras

instance HasDefaultExtras Legend where
  defaultExtras = LegendExtras

-- | A card's reaction to engine events. Wrapped in a newtype because
-- record fields can't directly hold a polymorphic function. The
-- constraints (@HasGame@, @MonadIO@) describe the engine capabilities
-- card code is allowed to use; widen them when card behavior needs
-- more.
newtype Receive k = Receive
  { unReceive
      :: forall m
       . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
      => Message -> Player -> InPlay k -> m ()
  }

-- | No-op receiver: the default for cards without bespoke behavior.
noReceive :: Receive k
noReceive = Receive \_ _ _ -> pure ()

-- | What kind of target an action requires. The engine validates the
-- supplied 'ActionTarget' against this schema before invoking the
-- action's effect.
data TargetSchema
  = NoTargetSchema
    -- ^ The action takes no target.
  | AnyUnitTargetSchema
    -- ^ Any unit currently in play.
  | EnemyUnitTargetSchema
    -- ^ A unit controlled by the opponent.
  | FriendlyUnitTargetSchema
    -- ^ A unit controlled by the player triggering the action.
  | AnyZoneTargetSchema
    -- ^ A zone of any player.
  | EnemyZoneTargetSchema
    -- ^ A zone controlled by the opponent.
  | SupportTargetSchema
    -- ^ A free-standing support in play.
  deriving stock (Show, Eq)

-- | The concrete target supplied with a 'TriggerCardAction' message.
data ActionTarget
  = NoTarget
  | TargetUnit UnitKey
  | TargetZone PlayerKey ZoneKind
  | TargetSupport UnitKey
  deriving stock (Show, Eq)

mconcat
  [ deriveToJSON defaultOptions ''TargetSchema
  , deriveToJSON defaultOptions ''ActionTarget
  ]

-- | The bespoke effect a card's action runs once cost has been paid and
-- target has been validated. Same capability set as 'Receive'.
newtype ActionEffect k = ActionEffect
  { unEffect
      :: forall m
       . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
      => PlayerKey -> InPlay k -> ActionTarget -> m ()
  }

-- | A single Action ability printed on a card. Cards may declare zero
-- or more actions; the engine enumerates them during action windows.
-- Field names are prefixed with @action@ so they can't shadow
-- 'CardDef'\'s own @cost@/@target@ fields under
-- @DuplicateRecordFields@.
data ActionDef k = ActionDef
  { actionName :: Text
  , actionCost :: Int
  , actionTarget :: TargetSchema
  , actionEffect :: ActionEffect k
  }

data CardDef (k :: CardKind) = CardDef
  { code :: CardCode
  , title :: String
  , kind :: CardKind
  , races :: [Race]
  , cost :: Number
  , loyalty :: Int
  , power :: Int
  , hitPoints :: Maybe Number
  , traits :: [Trait]
  , text :: Maybe String
  , flavor :: Maybe String
  , keywords :: [Keyword]
  , unique :: Bool
  , actions :: [ActionDef k]
  , receive :: Receive k
  , extras :: Extras k
    -- ^ Per-kind tunables the engine reads instead of casing on a
    -- card's 'code'. Defaults come from 'defaultExtras'; cards
    -- override the slices they care about via the builder helpers
    -- in 'Invasion.Card'.
  , selfCostAdjustment :: Game -> PlayerKey -> Int
    -- ^ Adjustment to this card's printed play cost when played by
    -- @pk@ (e.g. Bloodcrusher: @-1@ per burning zone). Lives on the
    -- top-level 'CardDef' because any kind can be played and any
    -- kind might want a self adjustment in the future. May be
    -- negative; the final cost is clamped non-negative.
  }

-- | Convenience: build the 'CardCodeFilter' describing a card. Used by
-- the engine when invoking 'globalCostAdjustment'.
cardCodeFilter :: CardDef k -> CardCodeFilter
cardCodeFilter c = CardCodeFilter
  { cfCode = c.code
  , cfKind = c.kind
  , cfRaces = c.races
  , cfTraits = c.traits
  }

-- The 'receive' function field can't be 'Show'n, so we derive a manual
-- instance that prints just enough to identify the card in trace logs.
instance Show (CardDef k) where
  showsPrec d c =
    showParen (d > 10) $
      showString "CardDef "
        . shows c.code
        . showString " "
        . shows c.title

-- The 'receive' function field is not JSON-encodable; the frontend only
-- needs the static metadata anyway. Hand-roll the instance so the field
-- is silently dropped. Actions also serialize as just their static
-- metadata (name/cost/target schema) so the client can render the
-- available-actions list without seeing the effect closures.
instance ToJSON (CardDef k) where
  toJSON c =
    object
      [ "code" .= c.code
      , "title" .= c.title
      , "kind" .= c.kind
      , "races" .= c.races
      , "cost" .= c.cost
      , "loyalty" .= c.loyalty
      , "power" .= c.power
      , "hitPoints" .= c.hitPoints
      , "traits" .= c.traits
      , "text" .= c.text
      , "flavor" .= c.flavor
      , "keywords" .= c.keywords
      , "unique" .= c.unique
      , "actions" .= map actionDefMeta c.actions
      ]
    where
      actionDefMeta a =
        object
          [ "name" .= a.actionName
          , "cost" .= a.actionCost
          , "target" .= a.actionTarget
          ]
