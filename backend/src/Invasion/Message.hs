{-# LANGUAGE GADTs #-}

module Invasion.Message (module Invasion.Message) where

import Invasion.Game (ActionWindowTrigger)
import Invasion.Modifier (Modifier, ModifierScope)
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
  -- Card play.
  --
  -- These reference a card-instance by its stable 'UnitKey' (the same
  -- key the card has carried since deck-init). The engine looks the
  -- card up in the named player's hand, verifies its kind matches the
  -- constructor, and reuses the key as the new in-play unit / support /
  -- quest / legend identity. No fresh key is minted on play.
  PlayUnit :: PlayerKey -> UnitKey -> ZoneKind -> Message
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
  PlayAttachment :: PlayerKey -> UnitKey -> UnitKey -> Message
    -- ^ Play a Support card from hand as an attachment to a target
    -- unit. First 'UnitKey' is the support card in hand; second is the
    -- target unit already in play. Pays cost, removes the card from
    -- hand, and emits 'SupportEnteredPlay'.
  SupportEnteredPlay :: PlayerKey -> UnitKey -> Message
    -- ^ A support card (attached or free-standing) has just entered
    -- play. The support's 'attachedTo' field distinguishes the two
    -- cases.
  -- Free-standing supports + quests
  PlaySupport :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Play a non-attachment Support card from hand into one of your
    -- zones. Pays cost, removes the card, emits 'SupportEnteredPlay'.
  PlayQuest :: PlayerKey -> UnitKey -> Message
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
  PlayTactic :: PlayerKey -> UnitKey -> Message
    -- ^ Play a Tactic from hand: pay cost, send to discard, fire the
    -- tactic's 'receive' once with 'TacticResolved'.
  TacticResolved :: PlayerKey -> CardCode -> Message
    -- ^ Dispatch hook fired exactly once when a tactic resolves. The
    -- tactic's CardDef.receive is invoked with this message; cards
    -- like Berserk Fury and Blood for the Blood God react here.
  -- Deferred effects
  DeferDamageToUnitUntilEoT :: UnitKey -> Int -> Message
    -- ^ Schedule N damage to land on the target at end of turn.
  -- Zone damage
  DealDamageToZone :: PlayerKey -> ZoneKind -> Int -> Message
    -- ^ Add N damage tokens to a capital zone. May burn the zone (and
    -- a second burn eliminates the player).
  -- Free unit summons (Iron Throneroom payoff, Reckless Attack, …).
  PutUnitIntoPlay :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Like 'PlayUnit' but skips the cost check / payment and pulls
    -- from hand. Used by effects that explicitly bypass the resource
    -- system. The 'UnitKey' is the in-hand card's stable key.
  PutUnitIntoPlayFromDiscard :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Same as 'PutUnitIntoPlay' but pulls the card from the
    -- player's discard pile instead of their hand.
  -- Scoped modifiers
  InstallModifier :: Ref Target -> Modifier -> Message
    -- ^ Add a 'Modifier' to the named target. Modifiers stack; multiple
    -- @GainPower n@ entries sum.
  ClearScopedModifiers :: ModifierScope -> Message
    -- ^ Drop every modifier matching the given scope (e.g. clear all
    -- 'UntilEndOfTurn' modifiers at end of turn).
  ScheduleAttackerSacrifice :: Message
    -- ^ Schedule a 'PESacrificeAttackersThisPhase' end-of-phase
    -- effect for the current battlefield phase. Used by Reckless
    -- Attack.
  -- Damage shuffling (Valkia)
  MoveAllDamage :: UnitKey -> UnitKey -> Message
    -- ^ Move all damage on 'fromKey' to 'toKey'. Source unit ends with
    -- 0 damage; destination accumulates.
  -- Legends
  PlayLegend :: PlayerKey -> UnitKey -> Message
    -- ^ Play a legend from hand directly onto the controller's capital
    -- board. Pays cost, removes the card from hand, emits
    -- 'LegendEnteredPlay'. Refused if the controller already has a
    -- legend in play.
  LegendEnteredPlay :: PlayerKey -> UnitKey -> Message
    -- ^ A legend has just entered play. Hook point for legend-side
    -- 'receive' bodies; Game itself just narrates.
  DealDamageToLegend :: UnitKey -> Int -> Message
    -- ^ Apply N damage to a target legend. If accumulated damage
    -- meets or exceeds the legend's printed HP, the engine queues
    -- 'DestroyLegend'.
  DestroyLegend :: UnitKey -> Message
    -- ^ Remove a legend from play; its card goes to the controller's
    -- discard. Triggers 'LegendLeftPlay'.
  LegendLeftPlay :: PlayerKey -> UnitKey -> CardCode -> Message
  -- Combat sequence
  BeginCombat :: PlayerKey -> ZoneKind -> [UnitKey] -> Message
    -- ^ Declare an attack: 'PlayerKey' is the attacker, 'ZoneKind' is
    -- the defending zone (of the opponent), and the list is the
    -- attacking unit keys.
  DeclareDefenders :: [UnitKey] -> Message
    -- ^ Defender locks in which of their units block. Auto-picks all
    -- eligible defenders if no list is supplied (we always supply
    -- one from the engine today).
  ResolveCombat :: Message
    -- ^ Compute and apply combat damage on both sides, then 'EndCombat'.
  EndCombat :: Message
    -- ^ Combat ends; clear 'Game.combat'.

deriving stock instance Show Message
