// Shared loader for the static card catalog (`/cards.json`). Five
// views fetch it independently — keep one in-module cache so a single
// SPA session never re-parses the file.

import { computed, ref, type ComputedRef, type Ref } from 'vue'
import type { Card } from '../types/card'

// Lazy module-level singleton. The first caller kicks off the fetch
// and every subsequent caller shares the same array / index by
// reference, so the JSON only crosses the wire (and `JSON.parse`)
// once per session.
const cardsRef = ref<Card[]>([])
const loaded = ref(false)
const loadError = ref<string | null>(null)
let inflight: Promise<Card[]> | null = null

async function fetchOnce(): Promise<Card[]> {
  if (loaded.value) return cardsRef.value
  if (inflight) return inflight
  inflight = (async () => {
    try {
      const res = await fetch('/cards.json')
      if (!res.ok) throw new Error(`cards.json: ${res.status}`)
      const data = (await res.json()) as Card[]
      cardsRef.value = data
      loaded.value = true
      loadError.value = null
      return data
    } catch (e) {
      loadError.value = e instanceof Error ? e.message : 'cards_load_failed'
      throw e
    } finally {
      inflight = null
    }
  })()
  return inflight
}

export interface CardCatalog {
  cards: Ref<Card[]>
  cardIndex: ComputedRef<Map<string, Card>>
  loading: Ref<boolean>
  loaded: Ref<boolean>
  error: Ref<string | null>
  ensureLoaded: () => Promise<Card[]>
}

// One Map index shared across all consumers — the catalog is ~hundreds
// of entries so a single derived map costs nothing.
const indexRef = computed<Map<string, Card>>(() => {
  const m = new Map<string, Card>()
  for (const c of cardsRef.value) m.set(c.id, c)
  return m
})

const loadingRef = computed(() => inflight != null && !loaded.value)

export function useCardCatalog(opts: { eager?: boolean } = {}): CardCatalog {
  if (opts.eager !== false) {
    // Kick the fetch off without awaiting so callers that only care
    // about the reactive refs still see the data populate.
    void fetchOnce().catch(() => {
      /* error is already exposed on `loadError` */
    })
  }
  return {
    cards: cardsRef,
    cardIndex: indexRef,
    loading: loadingRef as unknown as Ref<boolean>,
    loaded,
    error: loadError,
    ensureLoaded: fetchOnce,
  }
}
