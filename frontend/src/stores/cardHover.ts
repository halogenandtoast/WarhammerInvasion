// Hovered-card overlay state. Set by SvgCard on mouseenter and cleared
// on mouseleave; read by CardOverlay (mounted once over the play table).
// Lives outside Pinia for parity with the other simple module stores.

import { ref } from 'vue'

export interface CardHoverAnchor {
  x: number
  y: number
  width: number
  height: number
}

export interface CardHoverState {
  src: string
  alt: string
  anchor: CardHoverAnchor
}

const _hover = ref<CardHoverState | null>(null)

export const cardHover = {
  state: _hover,
  show(state: CardHoverState) {
    _hover.value = state
  },
  // Only clear if the current src still matches — guards against a stale
  // mouseleave from card A landing after the mouseenter for card B.
  hide(src: string) {
    if (_hover.value?.src === src) _hover.value = null
  },
  clear() {
    _hover.value = null
  },
}
