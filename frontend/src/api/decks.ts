import { api } from './client'
import type { Capital } from '../lib/deck'

export interface DeckRecord {
  id: string
  name: string
  capital: Capital | null
  cards: Record<string, number>
  createdAt: string
  updatedAt: string
}

export interface DeckInput {
  name: string
  capital: Capital | null
  cards: Record<string, number>
}

export function listDecks(): Promise<DeckRecord[]> {
  return api<DeckRecord[]>('/api/decks')
}

export function getDeck(id: string): Promise<DeckRecord> {
  return api<DeckRecord>(`/api/decks/${id}`)
}

export function createDeck(input: DeckInput): Promise<DeckRecord> {
  return api<DeckRecord>('/api/decks', { method: 'POST', body: input })
}

export function updateDeck(id: string, input: DeckInput): Promise<DeckRecord> {
  return api<DeckRecord>(`/api/decks/${id}`, { method: 'PUT', body: input })
}

export function deleteDeck(id: string): Promise<void> {
  return api<void>(`/api/decks/${id}`, { method: 'DELETE' })
}
