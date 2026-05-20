{-# LANGUAGE TemplateHaskell #-}

module Invasion.Player (module Invasion.Player) where

import Data.Aeson (ToJSON)
import Data.Aeson.TH
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Invasion.Capital
import {-# SOURCE #-} Invasion.Card.Types (Card)
import Invasion.Prelude
import Invasion.Types

-- | A player's resource pool. Filled during the kingdom phase, spent on
-- card costs. Unspent resources are returned to the central pool at the
-- start of the next kingdom phase.
newtype Resources = Resources Int
  deriving stock Show
  deriving newtype (Eq, Ord, Num, ToJSON)

data Player = Player
  { key :: PlayerKey
  , state :: PlayerState
  , capital :: Capital
  , resources :: Resources
  , hand :: [Card]
  , deck :: [Card]
  , discard :: [Card]
  , developmentCards :: Map ZoneKind [Card]
    -- ^ The actual facedown cards placed as developments, keyed by
    -- the zone they sit in. The card identities are tracked here so
    -- that destroy-development effects (e.g. Demolition!) can route
    -- the card back to its owner's discard pile. The corresponding
    -- 'Zone.developments' count is the length of the list for that
    -- zone and is kept in sync by the engine.
  , race :: Race
  , handPlayability :: Map UnitKey PlayabilityIssue
    -- ^ Derived view-side field: keyed by the hand card's stable
    -- 'UnitKey', it lists the reason a card can't be played in the
    -- current game state. Absent = playable. Not maintained by the
    -- engine itself; rewritten in bulk just before the snapshot is
    -- published, so engine logic never reads from it.
  }
  deriving stock Show

-- | Why a hand card cannot be played right now. Surfaces to the client
-- so the hand can dim unplayable cards and explain why on tap. The
-- engine still re-checks server-side in 'withPaidPlay' — this is a
-- pure derivation off the same predicates.
data PlayabilityIssue
  = InsufficientResources Int Int
    -- ^ @InsufficientResources needed have@.
  | UniqueAlreadyInPlay
  | LimitedAlreadyPlayed
  | LegendAlreadyInPlay
  | NotYourTurn
    -- ^ Non-tactic plays require it to be the controller's turn.
  | NotInActionWindow
    -- ^ No open action window with priority for the would-be caster.
  | WrongActionWindow
    -- ^ A window is open, but a non-tactic needs the capital window
    -- specifically. (Tactics are happy with any window.)
  | NoValidTarget
    -- ^ The card's per-card 'canPlay' predicate refused — usually
    -- because a required target doesn't exist (Stubborn Refusal, …).
  deriving stock (Show, Eq)

-- | Initial empty per-zone development cards.
emptyDevelopmentCards :: Map ZoneKind [Card]
emptyDevelopmentCards =
  Map.fromList
    [ (KingdomZone, [])
    , (QuestZone, [])
    , (BattlefieldZone, [])
    ]

-- | Top-level lifecycle state of a player. 'Eliminated' is terminal: a
-- player loses if either two of their capital zones burn or their deck
-- runs out (decking out).
data PlayerState
  = IdlePlayer
  | Eliminated EliminationReason
  | PlayerDraw Drawing
  deriving stock Show

data EliminationReason
  = DeckedOut
  | CapitalBurned
  deriving stock Show

instance HasField "battlefield" Player Zone where
  getField p = p.capital.battlefield

instance HasField "idle" Player Bool where
  getField p = case p.state of
    IdlePlayer -> True
    _ -> False

instance HasField "eliminated" Player Bool where
  getField p = case p.state of
    Eliminated _ -> True
    _ -> False

data DrawingKind = StartingHand | StandardDraw
  deriving stock Show

data Drawing = Drawing
  { kind :: DrawingKind
  , player :: PlayerKey
  }
  deriving stock Show

mconcat
  [ deriveToJSON defaultOptions ''EliminationReason
  , -- Tagged-object on every constructor (no string-tag shortcut for
    -- the nullary cases) so the client only ever switches on
    -- 'PlayabilityIssue.tag' — never on whether the field is a string
    -- or an object.
    deriveToJSON
      defaultOptions {allNullaryToStringTag = False}
      ''PlayabilityIssue
  , deriveToJSON defaultOptions ''Player
  , deriveToJSON defaultOptions ''PlayerState
  , deriveToJSON defaultOptions ''Drawing
  , deriveToJSON defaultOptions ''DrawingKind
  ]
