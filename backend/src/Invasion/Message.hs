{-# LANGUAGE GADTs #-}

module Invasion.Message (module Invasion.Message) where

import Invasion.Game (ActionWindowTrigger)
import Invasion.Player (Drawing, EliminationReason)
import Invasion.Prelude
import Invasion.Types

-- | Engine events. Every state change goes through a constructor here;
-- card text plugs in by handling, intercepting, or emitting these. New
-- behavior generally means a new constructor, not inlining work in a
-- handler.
data Message where
  -- Setup / lifecycle
  Setup :: Message
  BeginGame :: Message
  -- Player upkeep
  ShuffleDeck :: PlayerKey -> Message
  Draw :: Drawing -> Message
  Eliminate :: PlayerKey -> EliminationReason -> Message
  -- Turn structure
  BeginTurn :: PlayerKey -> Message
  EndTurn :: PlayerKey -> Message
  BeginPhase :: Phase -> Message
  EndPhase :: Phase -> Message
  -- Phase steps
  ReturnResources :: PlayerKey -> Message
  CollectResources :: PlayerKey -> Message
  QuestDraw :: PlayerKey -> Message
  -- Action windows
  OpenActionWindow :: ActionWindowTrigger -> Message
  PassPriority :: PlayerKey -> Message
  CloseActionWindow :: Message
  -- Card play
  PlayUnit :: PlayerKey -> CardCode -> ZoneKind -> Message
  UnitEnteredPlay :: PlayerKey -> UnitKey -> Message
  -- Damage / destroy
  DealDamageToUnit :: UnitKey -> Int -> Message
    -- ^ Apply N damage to a target unit. If accumulated damage equals
    -- or exceeds the unit's HP, the engine queues 'DestroyUnit'.
  HealUnit :: UnitKey -> Int -> Message
    -- ^ Remove up to N damage from a target unit (clamped to 0).
  DestroyUnit :: UnitKey -> Message
    -- ^ Remove a unit from play and put its card into the controller's
    -- discard. Triggers 'UnitLeftPlay'.
  UnitLeftPlay :: PlayerKey -> UnitKey -> ZoneKind -> CardCode -> Message
    -- ^ Narration / hook point fired after a unit has been removed.
    -- Cards react by inspecting the previous controller and code.
  -- Corruption
  CorruptUnit :: UnitKey -> Message
    -- ^ Mark a unit corrupted. No-op if already corrupted.
  CleanseUnit :: UnitKey -> Message
    -- ^ Clear the corrupted flag (used by the kingdom-phase restoration
    -- step and by future cleanse effects).
  -- Attachments
  PlayAttachment :: PlayerKey -> CardCode -> UnitKey -> Message
    -- ^ Play a Support card from hand as an attachment to a target
    -- unit. Pays cost, removes the card from hand, and emits
    -- 'SupportEnteredPlay'.
  SupportEnteredPlay :: PlayerKey -> UnitKey -> Message
    -- ^ A support card (attached or free-standing) has just entered
    -- play. The support's 'attachedTo' field distinguishes the two
    -- cases.
  -- Free-standing supports + quests
  PlaySupport :: PlayerKey -> CardCode -> ZoneKind -> Message
    -- ^ Play a non-attachment Support card from hand into one of your
    -- zones. Pays cost, removes the card, emits 'SupportEnteredPlay'.
  PlayQuest :: PlayerKey -> CardCode -> Message
    -- ^ Play a Quest from hand. Emits 'QuestEnteredPlay'.
  QuestEnteredPlay :: PlayerKey -> UnitKey -> Message
    -- ^ A Quest has just entered play. The fresh key references the
    -- entry in 'Game.quests'.
  -- Token bookkeeping
  AdjustSupportTokens :: UnitKey -> Int -> Message
    -- ^ Add (positive) or remove (negative) tokens from a support's
    -- counter. Clamped to >= 0.
  AdjustQuestTokens :: UnitKey -> Int -> Message
    -- ^ Same for a quest card.
  -- Support destruction
  DestroySupport :: UnitKey -> Message
    -- ^ Remove a free-standing support from play. Card goes to its
    -- controller's discard. Triggers 'SupportLeftPlay'.
  SupportLeftPlay :: PlayerKey -> UnitKey -> CardCode -> Message
  -- Experiences
  AttachExperience :: UnitKey -> CardCode -> Message
    -- ^ Pin a card (by code) as an "experience" on a host unit. Used by
    -- Skulltaker; the host card text reads 'experiences' to scale.
  -- Tactics
  PlayTactic :: PlayerKey -> CardCode -> Message
    -- ^ Play a Tactic from hand: pay cost, send to discard, fire the
    -- tactic's 'receive' once with 'TacticResolved'.
  TacticResolved :: PlayerKey -> CardCode -> Message
    -- ^ Dispatch hook fired exactly once when a tactic resolves. The
    -- tactic's CardDef.receive is invoked with this message; cards
    -- like Berserk Fury and Blood for the Blood God react here.

deriving stock instance Show Message
