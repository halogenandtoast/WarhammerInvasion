export type CardType = 'Unit' | 'Support' | 'Tactic' | 'Quest' | 'Legend' | 'Fulcrum'
export type Race =
  | 'Empire'
  | 'Dwarf'
  | 'High Elf'
  | 'Chaos'
  | 'Orc'
  | 'Dark Elf'
  | 'Neutral'

export interface Card {
  id: string
  name: string
  set: string
  cycle: string
  number: number | null
  type: CardType | null
  race: Race | null
  cost: string | null
  loyalty: number | null
  power: string | null
  health: string | null
  traits: string | null
  text: string | null
  quantity: string | null
  illustrator: string | null
  image: string | null
  /** True when card has only filename-derived data (no engine/text). */
  stub: boolean
}
