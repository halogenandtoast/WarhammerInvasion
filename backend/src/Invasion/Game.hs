{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Game (module Invasion.Game) where

import Control.Monad.State.Strict
import Data.Aeson (ToJSON)
import Data.Aeson.TH
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import Invasion.Capital
import Invasion.Entity (LegendDetails, QuestDetails, SupportDetails, UnitDetails)
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
  = BeginningOfTurnActionWindow
    -- ^ FAQ 2.2 Phase 0. Opened after every "at the beginning of the
    -- turn" triggered Constant / Forced effect has resolved, before
    -- the Kingdom phase begins. Either player may take actions here.
  | KingdomActionWindow
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
  | EndOfTurnActionWindow
    -- ^ FAQ 2.2 Phase 5. Opened at end of turn before "at the end of
    -- the turn" triggers resolve and UntilEndOfTurn modifiers expire.
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

-- | A pending choice the engine is waiting for the client to resolve.
-- gameMain pauses while one is set; the client posts a 'ResolvePrompt'
-- message carrying a 'PromptResult', the engine clears the slot and
-- fires the callback Message constructed from the result.
data Prompt = Prompt
  { player :: PlayerKey
    -- ^ Which player is being asked. The wire layer should only
    -- accept a resolution from this seat.
  , kind :: PromptKind
    -- ^ Static metadata describing what the player must pick. The
    -- client renders this.
  , callback :: PromptCallback
    -- ^ Tag identifying which engine effect to fire once the player
    -- has chosen.
  }
  deriving stock Show

-- | What kind of choice the player has to make.
data PromptKind
  = ChooseUnits
      { filterSpec :: PromptFilter
      , minPick :: Int
      , maxPick :: Int
      , description :: Text
      }
    -- ^ Pick a list of units matching the filter, between min and max
    -- entries (inclusive). 'min == 0' means the player may pass.
  | ChooseSacrifice
      { zone :: ZoneKind
      , optional :: Bool
      , description :: Text
      }
    -- ^ Pick one of your own units in the named zone to sacrifice.
    -- 'optional' lets the player skip if no eligible target exists.
  | ChooseYesNo
      { description :: Text
      }
    -- ^ Simple boolean choice — for "you may pay X to do Y" gates.
  deriving stock Show

-- | Predicate describing which units a 'ChooseUnits' prompt accepts.
-- Kept as a tagged value (not a function) so it serializes onto the
-- wire and the client can do its own filtering.
data PromptFilter
  = AnyOwnUnit
    -- ^ Any unit the prompted player controls.
  | OwnUnitsFromHandByRace Race
    -- ^ Cards in the prompted player's hand whose CardDef carries the
    -- named race. Used for Iron Throneroom's summon-from-hand half.
  | OwnUnitsFromDiscardByRace Race
    -- ^ Same as above but from discard.
  | OwnUnitsFromHandOrDiscardByRace Race
    -- ^ Union of hand and discard, filtered by race.
  deriving stock Show

-- | A tag identifying the engine continuation to invoke once the
-- prompt resolves. Each constructor packages the source-card key (or
-- whatever extra context the continuation needs) so the engine can
-- locate the originating card.
data PromptCallback
  = CallbackIronThroneroomPayoff UnitKey
    -- ^ The Iron Throneroom's UnitKey. Result: up to 3 in-hand /
    -- in-discard CardKeys to summon for free into the kingdom.
  | CallbackBloodthirsterSacrifice PlayerKey UnitKey
    -- ^ (Sacrificing player, Bloodthirster's UnitKey). Result: one
    -- of the sacrificing player's units in the Bloodthirster's
    -- corresponding zone (or none if no eligible unit exists).
  | CallbackSkulltakerPayToAttach UnitKey CardCode
    -- ^ (Skulltaker's key, the departing enemy unit's CardCode).
    -- Result: PickBool — if True and the controller can afford 1
    -- resource, debit and attach as an experience.
  | CallbackHorrorOfTzeentchDiscard UnitKey
    -- ^ Horror of Tzeentch's UnitKey. Result: PickBool — if True,
    -- queue the follow-up CallbackHorrorOfTzeentchTarget prompt.
  | CallbackHorrorOfTzeentchTarget UnitKey
    -- ^ Same horror, after the player committed to discarding.
    -- Result: a single chosen enemy unit to take 2 damage.
  deriving stock Show

-- | A scheduled effect that fires at a specific trigger.
data PendingEffect
  = PEDealDamageToUnit UnitKey Int
    -- ^ Deal N damage to the named unit when the effect fires.
  | PESacrificeAttackersThisPhase
    -- ^ Destroy every unit currently recorded in
    -- 'Game.attackersThisPhase'. Used by Reckless Attack.
  deriving stock Show

-- | In-flight combat state. Set on 'BeginCombat', mutated through the
-- sub-steps, cleared on 'EndCombat'.
data CombatState = CombatState
  { attackingPlayer :: PlayerKey
  , defendingPlayer :: PlayerKey
  , targetZone :: ZoneKind
  , attackers :: [UnitKey]
  , defenders :: [UnitKey]
  , attackerPowerPenalty :: Int
    -- ^ Per-attacker power penalty for this combat. Currently set
    -- by Rune of Fortitude (core-013) when the attacker can't afford
    -- the 1-per-attacker tax.
  , pendingAssignments :: [PendingDamage]
    -- ^ Damage tokens placed during the Assign step (step 4) but not
    -- yet committed. Cancellation effects (Defenders of the Faith,
    -- Master Rune of Valaya) mutate this list during the
    -- AfterAssignCombatDamage window; AdvanceCombatToApply converts
    -- each entry into a real DealDamageToUnit / DealDamageToZone
    -- message.
  }
  deriving stock Show

-- | A single placed-but-not-applied damage assignment.
data PendingDamage = PendingDamage
  { target :: PendingTarget
  , cancellable :: Int
  , uncancellable :: Int
  }
  deriving stock Show

-- | Targets a 'PendingDamage' entry can name.
data PendingTarget
  = PDUnit UnitKey
  | PDZone PlayerKey ZoneKind
  deriving stock (Show, Eq)

-- | A single line in the game-event transcript. The engine appends
-- entries as it processes messages; the frontend renders them in the
-- side-panel above chat. The engine never produces user-visible text:
-- 'key' is an i18n key (resolved in @frontend/src/locales/@) and
-- 'params' are the interpolation arguments. Enum-shaped param values
-- (e.g. @"Player1"@, @"KingdomPhase"@) are themselves resolved via
-- nested i18n lookups on the client so player display names and phase
-- labels respect the active locale.
data LogEntry = LogEntry
  { at :: UTCTime
  , category :: LogCategory
  , key :: Text
  , params :: Map Text Text
  }
  deriving stock Show

-- | Tag for client-side styling. Add cases as new event groupings
-- become useful; the wire JSON uses the constructor name verbatim.
data LogCategory
  = LogSystem
    -- ^ Engine bookkeeping: setup, shuffles, draws, resources, action
    -- window open/close.
  | LogPhase
    -- ^ Phase boundaries.
  | LogTurn
    -- ^ Turn boundaries.
  | LogPlayerAction
    -- ^ Choices originating from a player (currently just
    -- 'PassPriority'; will grow as cards/abilities land).
  | LogResult
    -- ^ Eliminations and the final game-over line.
  deriving stock (Show, Eq)

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
    -- ^ Top of the action-window stack — the window 'PassPriority'
    -- is currently directed at. Always equal to @listToMaybe
    -- actionWindowStack@; kept denormalized so existing wire
    -- clients can keep reading a single window.
  , actionWindowStack :: [ActionWindow]
    -- ^ Stack of currently-open action windows. The head is the
    -- topmost window; OpenActionWindow pushes, CloseActionWindow
    -- pops. Combat sub-step windows live on top of the
    -- BattlefieldActionWindow that opened them.
  , modifiers :: Map (Ref Target) [Modifier]
  , lifecycle :: GameState
    -- ^ Named 'lifecycle' (not 'state') because 'Player' also has a
    -- 'state' field, and using the same name would force every record
    -- update site to annotate which type it's updating.
  , log :: [LogEntry]
    -- ^ Append-only transcript of engine events, oldest first. Capped
    -- at 500 entries (see 'Invasion.Engine.logIt').
  , units :: [UnitDetails]
    -- ^ All units in play across both capitals. Each carries its
    -- 'controller' and 'zone' so callers filter rather than indexing
    -- through 'Capital'. 'Zone' lives in 'Invasion.Capital', which is
    -- compiled below this module, so we can't hang the list off
    -- 'Zone' directly.
  , supports :: [SupportDetails]
    -- ^ Free-standing (non-attached) support cards across both
    -- capitals. Attached supports live inside their host unit's
    -- 'attachments' field.
  , quests :: [QuestDetails]
    -- ^ Quest cards currently in play (sit in the quest zone for the
    -- controller who played them, or — for Mission quests — in an
    -- opponent's zone).
  , legends :: [LegendDetails]
    -- ^ Legends currently in play. By rule each player may control at
    -- most one legend at a time; the engine enforces that gate on
    -- 'PlayLegend'. Legends live on their controller's capital board
    -- (not inside a zone) but contribute power to all three zones.
  , nextUnitKey :: UnitKey
    -- ^ Monotonic counter for minting fresh 'UnitKey's as units enter
    -- play.
  , pendingEndOfTurn :: [PendingEffect]
    -- ^ Effects scheduled to fire at the next 'EndTurn'. Cleared as
    -- they fire so they don't leak across turns.
  , combat :: Maybe CombatState
    -- ^ 'Just' while a combat is in progress between 'BeginCombat' and
    -- 'EndCombat'. Card receives consult this to know they're in the
    -- combat path.
  , attackersThisPhase :: [UnitKey]
    -- ^ Every unit that has attacked this battlefield phase. Reset on
    -- 'BeginPhase BattlefieldPhase'. Reckless Attack reads this to
    -- pick its sacrifice targets at end of phase.
  , pendingEndOfPhase :: [(Phase, PendingEffect)]
    -- ^ Effects scheduled to fire on 'EndPhase' for a specific phase.
    -- Entries matching the firing phase are extracted, run, and
    -- discarded.
  , limitedPlayedThisTurn :: Bool
    -- ^ One Limited tactic per turn. Set on 'PlayTactic' when the
    -- card carries the 'Limited' keyword; cleared on 'BeginTurn'.
  , unitsDiscardedThisTurn :: Int
    -- ^ Count of units that have entered a discard pile (destroyed,
    -- sacrificed, etc.) during the current turn. Reset on 'BeginTurn'.
    -- Read by 'Burying the Grudge' (core-019).
  , damageTakenThisTurn :: Map UnitKey Int
    -- ^ Damage successfully landed on each unit this turn. Reset on
    -- 'BeginTurn'. Read by Daemonettes of Slaanesh and any future
    -- per-turn cap mechanics.
  , damagedInCurrentCombat :: [UnitKey]
    -- ^ Units that had non-zero damage land on them during the
    -- currently-active combat. Reset on 'BeginCombat'; consulted at
    -- 'EndCombat' to fire "when this unit damages an enemy" effects
    -- (Plaguebearers, Beasts of Nurgle).
  , pendingPrompt :: Maybe Prompt
    -- ^ When 'Just', the engine is waiting for the named player to
    -- post a 'ResolvePrompt' carrying their choice. 'gameMain'
    -- returns early as long as this is set, leaving the queue
    -- partially-drained so the wire layer can push the state and
    -- wait for the client's response.
  , autoSkipActionWindows :: Bool
    -- ^ Host-controlled setting captured at game creation. When 'True'
    -- the engine auto-passes priority whenever the holder of a phase
    -- action window has neither a Tactic card in hand nor an in-play
    -- own card carrying an action ability. Combat sub-step windows
    -- already auto-pass regardless of this flag.
  }
  deriving stock Show

-- | Concrete answer the client posts back with 'ResolvePrompt'.
data PromptResult
  = PickUnits [UnitKey]
    -- ^ List of chosen unit keys (in-play, in-hand, or in-discard
    -- depending on the prompt). The engine validates against the
    -- prompt's filter / bounds before firing the callback.
  | PickBool Bool
    -- ^ Yes/No answer for 'ChooseYesNo'.
  | PickNone
    -- ^ Player declined / no eligible target.
  deriving stock Show

instance HasField "over" Game Bool where
  getField g = case g.lifecycle of
    GameFinished _ -> True
    _ -> False

getAllModifiers :: HasGame m => m (Map (Ref Target) [Modifier])
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
  , deriveToJSON
      defaultOptions {tagSingleConstructors = True, allNullaryToStringTag = True}
      ''LogCategory
  , deriveToJSON defaultOptions ''LogEntry
  , deriveToJSON defaultOptions ''PendingEffect
  , deriveToJSON defaultOptions ''PendingTarget
  , deriveToJSON defaultOptions ''PendingDamage
  , deriveToJSON defaultOptions ''CombatState
  , deriveToJSON defaultOptions ''PromptFilter
  , deriveToJSON defaultOptions ''PromptKind
  , deriveToJSON defaultOptions ''PromptCallback
  , deriveToJSON defaultOptions ''Prompt
  , deriveJSON defaultOptions ''PromptResult
  , deriveToJSON defaultOptions ''Game
  , deriveToJSON defaultOptions ''GameState
  ]
