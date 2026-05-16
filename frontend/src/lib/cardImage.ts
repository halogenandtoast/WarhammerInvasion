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
