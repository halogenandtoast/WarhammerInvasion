// Helpers for rendering the engine's structured log entries.
//
// The engine emits LogEntry records whose `key` is an i18n key and
// whose `params` map carries interpolation values. Some param values
// are themselves enum-shaped (player keys, phase names, trigger
// kinds, elimination reasons), and they have to be re-translated
// through nested i18n lookups before being substituted into the outer
// message — that's what keeps engine output decoupled from any
// locale's word order.

import type { LogEntry, SeatView } from '../api/protocol'

export type LogCategoryClass = 'cat-system' | 'cat-turn' | 'cat-phase' | 'cat-action' | 'cat-result'

export function logCategoryClass(cat: LogEntry['category']): LogCategoryClass {
  switch (cat) {
    case 'LogTurn': return 'cat-turn'
    case 'LogPhase': return 'cat-phase'
    case 'LogPlayerAction': return 'cat-action'
    case 'LogResult': return 'cat-result'
    default: return 'cat-system'
  }
}

// Params whose values are themselves i18n keys. Resolved through a
// nested t() lookup before substitution into the outer message.
const ENUM_PARAM_TABLE: Record<string, string> = {
  phase: 'log.phase_name',
  trigger: 'log.trigger_name',
  reason: 'log.elim_reason',
  // Game-over uses `reason` too, but with a different value space; the
  // engine emits 'OpponentDeckedOut' / 'OpponentCapitalBurned' there,
  // which only resolve under 'log.win_reason'. Try elim first, fall
  // back to win at render time.
}

export type TFn = (key: string, params?: Record<string, unknown>, opts?: { default?: string }) => string

export interface LogFormatter {
  format: (entry: LogEntry) => string
}

// Build a formatter bound to a translation function and the current
// seat list (used to resolve `player`/`winner` params into display
// names). Returns a `format(entry)` callable.
export function makeLogFormatter(t: TFn, seats: () => SeatView[], playerKeyLabel: (key: string) => string): LogFormatter {
  const resolvePlayer = (key: string): string => {
    const seat = seats().find((s) => s.seat === key)
    if (seat) return seat.user.displayName
    return playerKeyLabel(key)
  }

  const resolveParam = (name: string, value: string): string => {
    if (name === 'player' || name === 'winner') return resolvePlayer(value)
    const base = ENUM_PARAM_TABLE[name]
    if (!base) return value
    const elim = t(`${base}.${value}`, undefined, { default: value })
    if (elim !== value) return elim
    if (name === 'reason') return t(`log.win_reason.${value}`, undefined, { default: value })
    return value
  }

  return {
    format(entry: LogEntry): string {
      const resolved: Record<string, string> = {}
      for (const [k, v] of Object.entries(entry.params)) resolved[k] = resolveParam(k, v)
      return t(entry.key, resolved, { default: t('log.unknown', { key: entry.key }) })
    },
  }
}
