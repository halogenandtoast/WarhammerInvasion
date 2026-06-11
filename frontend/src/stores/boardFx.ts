// Board-effects bus: the game store diffs consecutive engine
// snapshots and publishes semantic events here; the board components
// subscribe and render transient effects (damage floats, destroy
// ghosts, burn flashes) with GSAP.
//
// This is presentation-only state. Nothing here feeds back into the
// game store, and dropping every event on the floor must always be
// safe (reduced-motion users, spectators joining mid-game, …).

import type { EngineGame, PlayerKey, ZoneKind } from '../api/protocol'

export type FxEvent =
  | { kind: 'unit-damaged'; unitKey: number; amount: number }
  | { kind: 'unit-healed'; unitKey: number; amount: number }
  | {
      kind: 'unit-destroyed'
      unitKey: number
      controller: PlayerKey
      code: string
      title: string
    }
  | { kind: 'unit-corrupted'; unitKey: number }
  | { kind: 'zone-damaged'; player: PlayerKey; zone: ZoneKind; amount: number }
  | { kind: 'zone-burned'; player: PlayerKey; zone: ZoneKind }
  | { kind: 'legend-damaged'; legendKey: number; amount: number }
  | { kind: 'cards-drawn'; player: PlayerKey; count: number }
  | { kind: 'resources-changed'; player: PlayerKey; delta: number }

type FxListener = (events: FxEvent[]) => void

const listeners = new Set<FxListener>()

export function onBoardFx(fn: FxListener): () => void {
  listeners.add(fn)
  return () => listeners.delete(fn)
}

function emit(events: FxEvent[]) {
  if (events.length === 0) return
  for (const fn of listeners) fn(events)
}

const ZONES: ZoneKind[] = ['KingdomZone', 'QuestZone', 'BattlefieldZone']

// Compare two consecutive snapshots and publish what changed. Runs
// just before the store swaps in the new snapshot, so subscribers can
// still measure the OLD DOM (e.g. the last on-screen position of a
// unit that is about to disappear).
export function diffSnapshots(prev: EngineGame | null, next: EngineGame | null) {
  if (!prev || !next) return
  const events: FxEvent[] = []

  const prevUnits = new Map(prev.units.map((u) => [u.key, u]))
  const nextUnits = new Map(next.units.map((u) => [u.key, u]))

  for (const [key, pu] of prevUnits) {
    const nu = nextUnits.get(key)
    if (!nu) {
      events.push({
        kind: 'unit-destroyed',
        unitKey: key,
        controller: pu.controller,
        code: pu.cardDef.code,
        title: pu.cardDef.title,
      })
      continue
    }
    if (nu.damage > pu.damage) {
      events.push({ kind: 'unit-damaged', unitKey: key, amount: nu.damage - pu.damage })
    } else if (nu.damage < pu.damage) {
      events.push({ kind: 'unit-healed', unitKey: key, amount: pu.damage - nu.damage })
    }
    if (nu.corrupted && !pu.corrupted) {
      events.push({ kind: 'unit-corrupted', unitKey: key })
    }
  }

  const prevLegends = new Map(prev.legends.map((l) => [l.key, l]))
  for (const nl of next.legends) {
    const pl = prevLegends.get(nl.key)
    if (pl && nl.damage > pl.damage) {
      events.push({
        kind: 'legend-damaged',
        legendKey: nl.key,
        amount: nl.damage - pl.damage,
      })
    }
  }

  for (const pk of ['Player1', 'Player2'] as PlayerKey[]) {
    const pp = pk === 'Player1' ? prev.player1 : prev.player2
    const np = pk === 'Player1' ? next.player1 : next.player2
    for (const z of ZONES) {
      const pz =
        z === 'KingdomZone' ? pp.capital.kingdom
        : z === 'QuestZone' ? pp.capital.quest
        : pp.capital.battlefield
      const nz =
        z === 'KingdomZone' ? np.capital.kingdom
        : z === 'QuestZone' ? np.capital.quest
        : np.capital.battlefield
      if (nz.burned && !pz.burned) {
        events.push({ kind: 'zone-burned', player: pk, zone: z })
      } else if (nz.damage > pz.damage) {
        events.push({ kind: 'zone-damaged', player: pk, zone: z, amount: nz.damage - pz.damage })
      }
    }
    if (np.hand.length > pp.hand.length) {
      events.push({ kind: 'cards-drawn', player: pk, count: np.hand.length - pp.hand.length })
    }
    if (np.resources !== pp.resources) {
      events.push({ kind: 'resources-changed', player: pk, delta: np.resources - pp.resources })
    }
  }

  emit(events)
}
