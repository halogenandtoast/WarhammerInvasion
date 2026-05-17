// Race + capital helpers shared by the catalog UI (Cards / Decks /
// DeckEdit / DeckView), the engine UI (Game / PlaySide), and the
// deck-list helpers.
//
// We have *two* `Race` types in the codebase that mean the same thing
// but spell themselves differently:
//
//   - Catalog race (types/card.ts) is the wire shape of /cards.json —
//     "High Elf", "Dark Elf" (with spaces, matching the printed names).
//   - Engine race (api/protocol.ts) is the wire shape of the game
//     socket — "HighElf", "DarkElf" (no spaces, matching the Haskell
//     constructors).
//
// Both representations canonicalize to the same kebab-case slug
// (`high-elf`, `dark-elf`, …), which is what every CSS class hook and
// asset filename in the app uses. `raceSlug` accepts either form so
// callers don't need to know which side they're on.

import type { Race as CatalogRace } from '../types/card'
import type { Race as EngineRace } from '../api/protocol'

export type RaceSlug =
  | 'empire'
  | 'dwarf'
  | 'high-elf'
  | 'chaos'
  | 'orc'
  | 'dark-elf'
  | 'neutral'

export type Capital = 'empire' | 'dwarf' | 'high_elf' | 'chaos' | 'orc' | 'dark_elf'

export type Faction = 'order' | 'destruction'

const CAPITAL_TO_SLUG: Record<Capital, Exclude<RaceSlug, 'neutral'>> = {
  empire: 'empire',
  dwarf: 'dwarf',
  high_elf: 'high-elf',
  chaos: 'chaos',
  orc: 'orc',
  dark_elf: 'dark-elf',
}

const CAPITAL_TO_FACTION: Record<Capital, Faction> = {
  empire: 'order',
  dwarf: 'order',
  high_elf: 'order',
  chaos: 'destruction',
  orc: 'destruction',
  dark_elf: 'destruction',
}

export const ALL_CAPITALS: readonly Capital[] = [
  'empire',
  'dwarf',
  'high_elf',
  'chaos',
  'orc',
  'dark_elf',
] as const

export function factionOfCapital(c: Capital): Faction {
  return CAPITAL_TO_FACTION[c]
}

export function slugOfCapital(c: Capital): Exclude<RaceSlug, 'neutral'> {
  return CAPITAL_TO_SLUG[c]
}

// Normalise either Race representation (or `null`) into a slug. Returns
// `null` for missing input so callers can spread `?? ''` cleanly.
export function raceSlug(race: CatalogRace | EngineRace | null | undefined): RaceSlug | null {
  if (!race) return null
  return race.toLowerCase().replace(/\s+/g, '-') as RaceSlug
}

// `race-empire` / `race-high-elf` / … — empty string for missing race.
export function raceClass(race: CatalogRace | EngineRace | null | undefined): string {
  const slug = raceSlug(race)
  return slug ? `race-${slug}` : ''
}

// Capital → catalog Race ("High Elf"). Used by deck-list parsers.
const CAPITAL_TO_CATALOG_RACE: Record<Capital, CatalogRace> = {
  empire: 'Empire',
  dwarf: 'Dwarf',
  high_elf: 'High Elf',
  chaos: 'Chaos',
  orc: 'Orc',
  dark_elf: 'Dark Elf',
}

export function raceOfCapital(c: Capital): CatalogRace {
  return CAPITAL_TO_CATALOG_RACE[c]
}

const CATALOG_RACE_TO_CAPITAL: Record<CatalogRace, Capital | null> = {
  Empire: 'empire',
  Dwarf: 'dwarf',
  'High Elf': 'high_elf',
  Chaos: 'chaos',
  Orc: 'orc',
  'Dark Elf': 'dark_elf',
  Neutral: null,
}

export function capitalOfRace(r: CatalogRace): Capital | null {
  return CATALOG_RACE_TO_CAPITAL[r]
}

export const ORDER_RACES: readonly CatalogRace[] = ['Empire', 'Dwarf', 'High Elf'] as const
export const DESTRUCTION_RACES: readonly CatalogRace[] = ['Chaos', 'Orc', 'Dark Elf'] as const

export type CardFaction = Faction | 'neutral' | 'unknown'

export function catalogRaceFaction(race: CatalogRace | null | undefined): CardFaction {
  if (race == null) return 'unknown'
  if (race === 'Neutral') return 'neutral'
  if ((ORDER_RACES as readonly CatalogRace[]).includes(race)) return 'order'
  if ((DESTRUCTION_RACES as readonly CatalogRace[]).includes(race)) return 'destruction'
  return 'unknown'
}

// ---------------------------------------------------------------------------
// Engine-race side: asset paths + printed labels.
//
// Assets live under frontend/public/capitals/ and are served verbatim
// by Vite at /capitals/*. Add new entries when the engine grows past
// the printed race set.

const ENGINE_RACE_LABEL: Record<EngineRace, string> = {
  Empire: 'Empire',
  Dwarf: 'Dwarf',
  HighElf: 'High Elf',
  Chaos: 'Chaos',
  Orc: 'Orc',
  DarkElf: 'Dark Elf',
}

const ENGINE_RACE_CAPITAL_ASSET: Record<EngineRace, string> = {
  Dwarf: '/capitals/dwarves.jpg',
  Empire: '/capitals/empire.jpg',
  HighElf: '/capitals/high-elves.jpg',
  Chaos: '/capitals/chaos.jpg',
  Orc: '/capitals/orcs.jpg',
  DarkElf: '/capitals/dark-elves.jpg',
}

export function capitalImageFor(race: EngineRace): string {
  return ENGINE_RACE_CAPITAL_ASSET[race]
}

export function raceLabel(race: EngineRace): string {
  return ENGINE_RACE_LABEL[race]
}

// Engine race → kebab-case slug (`high-elf`, `dark-elf`). Thin
// re-export so engine-side callers don't have to reach for `raceSlug`.
export function engineRaceSlug(race: EngineRace): string {
  return raceSlug(race) ?? ''
}
