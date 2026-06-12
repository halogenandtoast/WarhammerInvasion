export type CardType = 'Unit' | 'Support' | 'Tactic' | 'Quest' | 'Legend' | 'Fulcrum'
export type Race =
  | 'Empire'
  | 'Dwarf'
  | 'High Elf'
  | 'Chaos'
  | 'Orc'
  | 'Dark Elf'
  | 'Neutral'

/**
 * Numeric stats are usually a plain number. `"X"` stands in for variable
 * values; `null` means the field doesn't apply to this card.
 */
export type CardStat = number | 'X' | null

export interface Card {
  id: string
  name: string
  set: string
  cycle: string
  number: number | null
  type: CardType | null
  race: Race | null
  cost: CardStat
  loyalty: number | null
  power: CardStat
  health: CardStat
  traits: string | null
  text: string | null
  quantity: number | null
  illustrator: string | null
  image: string | null
  /** True for unique cards (heroes, relics) — at most one copy in play. */
  unique?: boolean
  /** True when card has only filename-derived data (no engine/text). */
  stub: boolean
}
