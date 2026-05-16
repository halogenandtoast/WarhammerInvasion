// Pure deck model — validation, stats, faction logic. UI-agnostic.

import type { Card, CardStat, Race } from '../types/card'

export type Faction = 'order' | 'destruction'

// Capital values are the wire format (snake_case) of the six playable races.
// One capital = one race; the faction is derived from it.
export type Capital = 'empire' | 'dwarf' | 'high_elf' | 'chaos' | 'orc' | 'dark_elf'

export const ALL_CAPITALS: readonly Capital[] = [
  'empire',
  'dwarf',
  'high_elf',
  'chaos',
  'orc',
  'dark_elf',
] as const

export const ORDER_RACES: readonly Race[] = ['Empire', 'Dwarf', 'High Elf'] as const
export const DESTRUCTION_RACES: readonly Race[] = ['Chaos', 'Orc', 'Dark Elf'] as const

const CAPITAL_TO_RACE: Record<Capital, Race> = {
  empire: 'Empire',
  dwarf: 'Dwarf',
  high_elf: 'High Elf',
  chaos: 'Chaos',
  orc: 'Orc',
  dark_elf: 'Dark Elf',
}

const CAPITAL_TO_FACTION: Record<Capital, Faction> = {
  empire: 'order',
  dwarf: 'order',
  high_elf: 'order',
  chaos: 'destruction',
  orc: 'destruction',
  dark_elf: 'destruction',
}

export function raceOfCapital(c: Capital): Race {
  return CAPITAL_TO_RACE[c]
}

export function factionOfCapital(c: Capital): Faction {
  return CAPITAL_TO_FACTION[c]
}

export const MIN_DECK_SIZE = 50
export const MAX_DECK_SIZE = 100
export const MAX_COPIES_PER_TITLE = 3

export interface SavedDeck {
  id: string
  name: string
  capital: Capital | null
  cards: Record<string, number>
  createdAt: string
  updatedAt: string
}

export type CardFaction = Faction | 'neutral' | 'unknown'

export function cardFaction(card: Pick<Card, 'race'>): CardFaction {
  if (card.race == null) return 'unknown'
  if (card.race === 'Neutral') return 'neutral'
  if ((ORDER_RACES as readonly Race[]).includes(card.race)) return 'order'
  if ((DESTRUCTION_RACES as readonly Race[]).includes(card.race)) return 'destruction'
  return 'unknown'
}

/**
 * Whether a card may legally appear in a deck of a given faction.
 * 'unknown' (stub cards with no race) is rejected to avoid corrupting decks.
 */
export function isCardAllowedInFaction(card: Pick<Card, 'race'>, faction: Faction | null): boolean {
  const cf = cardFaction(card)
  if (cf === 'unknown') return false
  if (cf === 'neutral') return true
  if (faction == null) return true
  return cf === faction
}

export function isCardAllowedInDeck(card: Pick<Card, 'race'>, capital: Capital | null): boolean {
  return isCardAllowedInFaction(card, capital == null ? null : factionOfCapital(capital))
}

export interface DeckIssue {
  severity: 'error' | 'warning'
  code: string
  message: string
}

export interface DeckStats {
  total: number
  uniqueTitles: number
  byType: Record<string, number>
  byRace: Record<string, number>
  costCurve: { bucket: string; count: number }[]
}

export interface DeckSummary {
  cards: { card: Card; count: number }[]
  unknown: { id: string; count: number }[]
  capital: Capital | null
  faction: Faction | null
  stats: DeckStats
  issues: DeckIssue[]
}

const COST_BUCKETS: string[] = ['0', '1', '2', '3', '4', '5', '6+', 'X']

function bucketForCost(cost: CardStat): string {
  if (cost == null || cost === 'X') return 'X'
  if (typeof cost !== 'number' || Number.isNaN(cost)) return 'X'
  if (cost < 0) return 'X'
  if (cost >= 6) return '6+'
  return String(cost)
}

export function summarize(
  deck: Pick<SavedDeck, 'cards' | 'capital'>,
  cardIndex: Map<string, Card>,
): DeckSummary {
  const entries: { card: Card; count: number }[] = []
  const unknown: { id: string; count: number }[] = []
  let total = 0
  const byType: Record<string, number> = {}
  const byRace: Record<string, number> = {}
  const costMap = new Map<string, number>(COST_BUCKETS.map((b) => [b, 0]))

  for (const [id, count] of Object.entries(deck.cards)) {
    if (count <= 0) continue
    const card = cardIndex.get(id)
    if (!card) {
      unknown.push({ id, count })
      continue
    }
    entries.push({ card, count })
    total += count
    const t = card.type ?? 'Other'
    byType[t] = (byType[t] ?? 0) + count
    const r = card.race ?? 'Unknown'
    byRace[r] = (byRace[r] ?? 0) + count
    const b = bucketForCost(card.cost)
    costMap.set(b, (costMap.get(b) ?? 0) + count)
  }

  entries.sort((a, b) => a.card.name.localeCompare(b.card.name))

  const faction = deck.capital == null ? null : factionOfCapital(deck.capital)
  const issues: DeckIssue[] = []
  if (total < MIN_DECK_SIZE) {
    issues.push({
      severity: 'error',
      code: 'too_few',
      message: `Need ${MIN_DECK_SIZE - total} more cards (minimum ${MIN_DECK_SIZE}).`,
    })
  }
  if (total > MAX_DECK_SIZE) {
    issues.push({
      severity: 'error',
      code: 'too_many',
      message: `${total - MAX_DECK_SIZE} cards over the ${MAX_DECK_SIZE} limit.`,
    })
  }

  for (const { card, count } of entries) {
    if (count > MAX_COPIES_PER_TITLE) {
      issues.push({
        severity: 'error',
        code: 'too_many_copies',
        message: `${card.name}: ${count} copies (max ${MAX_COPIES_PER_TITLE}).`,
      })
    }
  }

  // Faction conflicts: any card whose faction disagrees with the deck's.
  const conflicting = entries.filter(({ card }) => !isCardAllowedInFaction(card, faction))
  if (conflicting.length > 0) {
    issues.push({
      severity: 'error',
      code: 'faction_mismatch',
      message: `${conflicting.length} card(s) don't match the deck's faction.`,
    })
  }

  if (unknown.length > 0) {
    issues.push({
      severity: 'warning',
      code: 'unknown_cards',
      message: `${unknown.length} unknown card id(s) in deck.`,
    })
  }

  const costCurve = COST_BUCKETS.map((b) => ({ bucket: b, count: costMap.get(b) ?? 0 }))

  return {
    cards: entries,
    unknown,
    capital: deck.capital,
    faction,
    stats: {
      total,
      uniqueTitles: entries.length,
      byType,
      byRace,
      costCurve,
    },
    issues,
  }
}

/**
 * Returns true if the deck contains any non-Neutral card. Used to gate the
 * "change capital" affordance — once a real faction card is in the deck,
 * swapping capitals can silently invalidate it.
 */
export function hasFactionCards(
  deck: Pick<SavedDeck, 'cards'>,
  cardIndex: Map<string, Card>,
): boolean {
  for (const [id, count] of Object.entries(deck.cards)) {
    if (count <= 0) continue
    const card = cardIndex.get(id)
    if (!card) continue
    const cf = cardFaction(card)
    if (cf === 'order' || cf === 'destruction') return true
  }
  return false
}

const RACE_TO_CAPITAL: Record<Race, Capital | null> = {
  Empire: 'empire',
  Dwarf: 'dwarf',
  'High Elf': 'high_elf',
  Chaos: 'chaos',
  Orc: 'orc',
  'Dark Elf': 'dark_elf',
  Neutral: null,
}

export interface ParsedDeckList {
  /** Card-id → count, ready to drop into a DeckInput. */
  counts: Record<string, number>
  /** Total number of cards (sum of counts), including matches only. */
  total: number
  /** Suggested capital based on the most-present non-Neutral race. */
  capital: Capital | null
  /** Per-race totals across matched cards. */
  raceCounts: Record<Race, number>
  /** Card names that didn't match any known card, in input order. */
  unknown: string[]
  /** Raw lines that did not parse as `N Name`. */
  parseErrors: string[]
}

/**
 * Parses a pasted deck list of the form `N Name` (one entry per line) into a
 * deck payload. Blank lines and lines beginning with `#` or `//` are ignored.
 * Card names are matched case-insensitively against the (non-stub) catalog.
 * The suggested capital is the one whose race has the highest total count;
 * ties break in `ALL_CAPITALS` order.
 */
export function parseDeckList(text: string, cards: Card[]): ParsedDeckList {
  const byName = new Map<string, Card>()
  for (const c of cards) {
    if (c.stub) continue
    byName.set(c.name.toLowerCase(), c)
  }

  const counts: Record<string, number> = {}
  const unknown: string[] = []
  const parseErrors: string[] = []
  const raceCounts: Record<Race, number> = {
    Empire: 0,
    Dwarf: 0,
    'High Elf': 0,
    Chaos: 0,
    Orc: 0,
    'Dark Elf': 0,
    Neutral: 0,
  }
  let total = 0

  // `3 Foo`, `3x Foo`, `3 x Foo`, `3× Foo`.
  const lineRe = /^\s*(\d+)\s*[xX×]?\s+(.+?)\s*$/
  // Section headers like `Unit (12)`, `Supports:`, `Tactic` — emitted by deck
  // export tools that group entries by card type. Skip silently.
  const sectionHeaderRe =
    /^(units?|supports?|tactics?|quests?|legends?|fulcrums?)\s*(\(\s*\d+\s*\))?\s*:?\s*$/i

  for (const rawLine of text.split(/\r?\n/)) {
    const trimmed = rawLine.trim()
    if (!trimmed) continue
    if (trimmed.startsWith('#') || trimmed.startsWith('//')) continue
    if (sectionHeaderRe.test(trimmed)) continue

    const m = lineRe.exec(rawLine)
    if (!m) {
      parseErrors.push(trimmed)
      continue
    }

    const count = parseInt(m[1], 10)
    const name = m[2].trim()
    if (count <= 0 || !name) {
      parseErrors.push(trimmed)
      continue
    }

    const card = byName.get(name.toLowerCase())
    if (!card) {
      unknown.push(name)
      continue
    }

    counts[card.id] = (counts[card.id] ?? 0) + count
    total += count
    if (card.race) raceCounts[card.race] += count
  }

  let bestRace: Race | null = null
  let bestCount = 0
  for (const cap of ALL_CAPITALS) {
    const race = raceOfCapital(cap)
    const n = raceCounts[race]
    if (n > bestCount) {
      bestCount = n
      bestRace = race
    }
  }
  const capital = bestRace ? RACE_TO_CAPITAL[bestRace] : null

  return { counts, total, capital, raceCounts, unknown, parseErrors }
}
