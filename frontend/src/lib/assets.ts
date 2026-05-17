// Asset-URL helpers. The catalog views (Cards / DeckEdit / DeckView)
// resolve card art from the optional VITE_ASSETS_BASE_URL prefix; the
// in-game CardArt component hits the same `/cards/{code}.jpg` layout
// directly because it ships under the same origin as the SPA.

import type { Card } from '../types/card'

const ASSETS_BASE_URL = import.meta.env.VITE_ASSETS_BASE_URL ?? ''

// Returns null when the card row has no image filename in the catalog.
export function cardImageUrl(card: Pick<Card, 'image'>): string | null {
  return card.image ? `${ASSETS_BASE_URL}/cards/${card.image}` : null
}
