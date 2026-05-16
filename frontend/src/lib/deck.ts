// Pure deck model — validation, stats, faction logic. UI-agnostic.

import type { Card, Race } from '../types/card'

export type Faction = 'order' | 'destruction'

export const ORDER_RACES: readonly Race[] = ['Empire', 'Dwarf', 'High Elf'] as const
export const DESTRUCTION_RACES: readonly Race[] = ['Chaos', 'Orc', 'Dark Elf'] as const

export const MIN_DECK_SIZE = 50
export const MAX_DECK_SIZE = 100
export const MAX_COPIES_PER_TITLE = 3

export interface SavedDeck {
  id: string
  name: string
  faction: Faction | null
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
  faction: Faction | null
  stats: DeckStats
  issues: DeckIssue[]
}

const COST_BUCKETS: string[] = ['0', '1', '2', '3', '4', '5', '6+', 'X']

function bucketForCost(cost: string | null): string {
  if (cost == null) return 'X'
  if (cost === 'X') return 'X'
  const n = Number(cost)
  if (Number.isNaN(n)) return 'X'
  if (n >= 6) return '6+'
  return String(n)
}

export function summarize(
  deck: Pick<SavedDeck, 'cards' | 'faction'>,
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
  const conflicting = entries.filter(({ card }) => !isCardAllowedInFaction(card, deck.faction))
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
    faction: deck.faction,
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
 * Infer the deck's faction from its non-Neutral cards. Returns null if
 * the deck is empty or has only Neutral cards.
 */
export function inferFaction(
  deck: Pick<SavedDeck, 'cards'>,
  cardIndex: Map<string, Card>,
): Faction | null {
  for (const [id, count] of Object.entries(deck.cards)) {
    if (count <= 0) continue
    const card = cardIndex.get(id)
    if (!card) continue
    const cf = cardFaction(card)
    if (cf === 'order' || cf === 'destruction') return cf
  }
  return null
}
