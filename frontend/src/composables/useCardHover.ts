// Wires a card tile's image (or its wrapper element) up to the
// floating preview overlay in CardOverlay.vue. Mirrors the hover
// behavior in CardArt.vue, but for plain <img> tiles used by the deck
// catalog views.
//
// Usage:
//   const hover = useCardHover()
//   hover.onEnter(card, event)   // call from @mouseenter
//   hover.onLeave(card)          // call from @mouseleave

import { onBeforeUnmount } from 'vue'
import { cardHover } from '../stores/cardHover'
import { cardImageUrl } from '../lib/assets'
import { getCachedSize, loadImageSize } from '../lib/cardImage'
import type { Card } from '../types/card'

export function useCardHover() {
  let currentSrc: string | null = null

  function onEnter(card: Pick<Card, 'image' | 'name'>, event: MouseEvent) {
    const src = cardImageUrl(card)
    if (!src) return
    const target = event.currentTarget as HTMLElement | null
    if (!target) return
    const r = target.getBoundingClientRect()
    if (r.width === 0 || r.height === 0) return

    currentSrc = src
    const cached = getCachedSize(src)
    cardHover.show({
      src,
      alt: card.name,
      anchor: { x: r.left, y: r.top, width: r.width, height: r.height },
      natural: cached,
    })

    if (!cached) {
      loadImageSize(src)
        .then((dims) => {
          if (cardHover.state.value?.src === src) {
            cardHover.show({
              ...cardHover.state.value,
              natural: dims,
            })
          }
        })
        .catch(() => {
          /* probe failure isn't fatal — overlay falls back to portrait aspect */
        })
    }
  }

  function onLeave(card: Pick<Card, 'image'>) {
    const src = cardImageUrl(card)
    if (src) cardHover.hide(src)
    if (currentSrc === src) currentSrc = null
  }

  onBeforeUnmount(() => {
    if (currentSrc) cardHover.hide(currentSrc)
  })

  return { onEnter, onLeave }
}
