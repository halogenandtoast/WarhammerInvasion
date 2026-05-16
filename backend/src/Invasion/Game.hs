{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Game (module Invasion.Game) where

import Control.Monad.State.Strict
import Data.Aeson (ToJSON)
import Data.Aeson.TH
import Data.Map.Strict (Map)
import Invasion.Capital
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types

class Monad m => HasGame m where
  getGame :: m Game

instance HasGame m => HasGame (StateT s m) where
  getGame = lift getGame

-- | 1-indexed counter of player-turns played so far. Turn 1 is the
-- first player's first turn (during which they skip the quest and
-- battlefield phases per the first-turn-penalty rule).
newtype Turn = Turn Int
  deriving stock Show
  deriving newtype (Eq, Ord, Num, ToJSON)

-- | Lifecycle of the game as a whole.
data GameState
  = GameSetup
    -- ^ Before play starts: decks shuffled, hands dealt, first player
    -- chosen, but no turn has begun.
  | GamePlaying
    -- ^ A turn is in progress.
  | GameFinished GameResult
  deriving stock Show

data GameResult = GameResult
  { winner :: PlayerKey
  , reason :: WinReason
  }
  deriving stock Show

data WinReason
  = OpponentDeckedOut
  | OpponentCapitalBurned
  deriving stock Show

-- | An action window is an explicit pause in the engine where both
-- players have an opportunity to take actions. The window closes when
-- both players pass consecutively without acting.
data ActionWindow = ActionWindow
  { trigger :: ActionWindowTrigger
  , awaiting :: PassState
  }
  deriving stock Show

-- | Context that opened the action window — useful for the client (to
-- know what's actionable here) and for restricting which card effects
-- can be played in this window.
data ActionWindowTrigger
  = KingdomActionWindow
    -- ^ Opened after resources are collected.
  | QuestActionWindow
    -- ^ Opened after quest-zone cards are drawn.
  | CapitalActionWindow
    -- ^ The capital phase IS one big action window: the active player
    -- may additionally play units/supports/quests/developments here.
  | BattlefieldActionWindow
    -- ^ Opened on entering the battlefield phase, before any attack is
    -- declared. Acts as the "do you want to attack?" pause; passing
    -- here ends the phase. Combat sub-steps emit their own windows.
  -- The 5 combat sub-step windows, emitted only when an attack is
  -- actually declared. Unused until combat is implemented.
  | AfterDeclareCombatTarget
  | AfterDeclareAttackers
  | AfterDeclareDefenders
  | AfterAssignCombatDamage
  | AfterApplyCombatDamage
  deriving stock Show

-- | The pass-bookkeeping needed to detect "both pass consecutively."
-- An action taken by either player resets the state to 'NoPasses', with
-- priority returning to the active player.
data PassState
  = NoPasses PlayerKey
    -- ^ The named player holds priority and has not passed.
  | OnePass PlayerKey
    -- ^ The named player holds priority; their opponent just passed.
    -- If this player passes (without acting), the window closes.
  deriving stock Show

priorityHolder :: PassState -> PlayerKey
priorityHolder = \case
  NoPasses p -> p
  OnePass p -> p

data Game = Game
  { player1 :: Player
  , player2 :: Player
  , firstPlayer :: PlayerKey
  , currentPlayer :: PlayerKey
  , turn :: Turn
  , phase :: Maybe Phase
    -- ^ 'Nothing' before the first 'BeginTurn'; otherwise the phase
    -- currently being processed.
  , actionWindow :: Maybe ActionWindow
    -- ^ 'Just' when the engine is paused awaiting player passes.
  , modifiers :: Map (Ref Target) [ModifierDetails]
  , lifecycle :: GameState
    -- ^ Named 'lifecycle' (not 'state') because 'Player' also has a
    -- 'state' field, and using the same name would force every record
    -- update site to annotate which type it's updating.
  }
  deriving stock Show

instance HasField "over" Game Bool where
  getField g = case g.lifecycle of
    GameFinished _ -> True
    _ -> False

getAllModifiers :: HasGame m => m (Map (Ref Target) [ModifierDetails])
getAllModifiers = do
  g <- getGame
  pure g.modifiers

getPlayer :: HasGame m => PlayerKey -> m Player
getPlayer pkey = do
  g <- getGame
  pure $ case pkey of
    Player1 -> g.player1
    Player2 -> g.player2

getBattleField :: HasGame m => PlayerKey -> m Zone
getBattleField pkey = do
  p <- getPlayer pkey
  pure $ p.battlefield

getKingdom :: HasGame m => PlayerKey -> m Zone
getKingdom pkey = do
  p <- getPlayer pkey
  pure $ p.capital.kingdom

getQuestZone :: HasGame m => PlayerKey -> m Zone
getQuestZone pkey = do
  p <- getPlayer pkey
  pure $ p.capital.quest

getCapital :: HasGame m => PlayerKey -> m Capital
getCapital pkey = do
  p <- getPlayer pkey
  pure $ p.capital

battlefield :: (HasGame m, HasField "controller" a PlayerKey) => a -> (Zone -> m ()) -> m ()
battlefield a f = getBattleField a.controller >>= f

capital :: HasGame m => PlayerKey -> (Capital -> m ()) -> m ()
capital pkey f = getCapital pkey >>= f

mconcat
  [ deriveToJSON defaultOptions ''PassState
  , deriveToJSON defaultOptions ''ActionWindowTrigger
  , deriveToJSON defaultOptions ''ActionWindow
  , deriveToJSON defaultOptions ''WinReason
  , deriveToJSON defaultOptions ''GameResult
  , deriveToJSON defaultOptions ''Game
  , deriveToJSON defaultOptions ''GameState
  ]
