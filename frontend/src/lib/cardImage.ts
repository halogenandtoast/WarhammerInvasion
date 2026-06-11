// Probes the natural dimensions of a card image so renderers can tell
// whether the art is portrait or landscape. Quests / Fulcrums ship as
// landscape JPGs; everything else is portrait — but rather than
// hard-code that by card type we just measure the image on first use.
//
// Results are cached forever (per session) on the URL: a card image is
// immutable once published, and there are only a few hundred of them.

export interface NaturalSize {
  width: number
  height: number
}

const cache = new Map<string, NaturalSize>()
const pending = new Map<string, Promise<NaturalSize>>()

// Card art is immutable once published, so persist measured sizes
// across sessions. Without this, the first hover of every card each
// session blocks the preview overlay on a fresh probe.
const LS_KEY = 'whi.cardSizes.v1'
try {
  const raw = localStorage.getItem(LS_KEY)
  if (raw) {
    for (const [src, dims] of Object.entries(
      JSON.parse(raw) as Record<string, NaturalSize>,
    )) {
      if (dims && dims.width > 0 && dims.height > 0) cache.set(src, dims)
    }
  }
} catch {
  /* private mode / quota — in-memory cache still works */
}

let persistTimer: ReturnType<typeof setTimeout> | null = null
function schedulePersist() {
  if (persistTimer) return
  persistTimer = setTimeout(() => {
    persistTimer = null
    try {
      localStorage.setItem(LS_KEY, JSON.stringify(Object.fromEntries(cache)))
    } catch {
      /* ignore */
    }
  }, 1000)
}

export function getCachedSize(src: string): NaturalSize | null {
  return cache.get(src) ?? null
}

export function loadImageSize(src: string): Promise<NaturalSize> {
  const cached = cache.get(src)
  if (cached) return Promise.resolve(cached)
  const inFlight = pending.get(src)
  if (inFlight) return inFlight

  const p = new Promise<NaturalSize>((resolve, reject) => {
    const img = new Image()
    img.onload = () => {
      const dims: NaturalSize = { width: img.naturalWidth, height: img.naturalHeight }
      cache.set(src, dims)
      schedulePersist()
      pending.delete(src)
      resolve(dims)
    }
    img.onerror = (e) => {
      pending.delete(src)
      reject(e)
    }
    img.src = src
  })
  pending.set(src, p)
  return p
}
