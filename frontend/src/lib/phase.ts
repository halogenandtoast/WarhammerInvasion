// Phase-bar mapping for the bottom strip of the in-game view.
//
// Every action-window trigger belongs to one of the 4 phases. The
// "Beginning of turn" / "End of turn" windows have no dedicated pip
// (they bracket the Kingdom and Battlefield phases respectively), so
// we map them to the nearest phase for visual placement.

import type { ActionWindowTrigger, Phase } from '../api/protocol'

export const PHASES: readonly Phase[] = [
  'KingdomPhase',
  'QuestPhase',
  'CapitalPhase',
  'BattlefieldPhase',
] as const

export const TRIGGER_TO_PHASE: Record<ActionWindowTrigger, Phase> = {
  BeginningOfTurnActionWindow: 'KingdomPhase',
  KingdomActionWindow: 'KingdomPhase',
  QuestActionWindow: 'QuestPhase',
  CapitalActionWindow: 'CapitalPhase',
  BattlefieldActionWindow: 'BattlefieldPhase',
  AfterDeclareCombatTarget: 'BattlefieldPhase',
  AfterDeclareAttackers: 'BattlefieldPhase',
  AfterDeclareDefenders: 'BattlefieldPhase',
  AfterAssignCombatDamage: 'BattlefieldPhase',
  AfterApplyCombatDamage: 'BattlefieldPhase',
  EndOfTurnActionWindow: 'BattlefieldPhase',
}
