<script setup lang="ts">
// Floating enlarged-card preview. Mounted once over the play table;
// listens to the cardHover store and renders a fixed-position image
// next to the hovered source card, clamped inside the viewport.

import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { cardHover } from '../stores/cardHover'
import { CARD_W, CARD_H } from '../lib/cardSize'

// Preferred LONG-edge size in CSS pixels — the overlay's bigger
// dimension targets this and the other one follows from the source's
// natural aspect. Shrinks to fit if the viewport is too small.
const PREFERRED_LONG = 440
// Portrait fallback aspect (used until the image has been measured).
const PORTRAIT_ASPECT = CARD_W / CARD_H
const GAP = 14
const MARGIN = 8

const vw = ref(typeof window !== 'undefined' ? window.innerWidth : 1024)
const vh = ref(typeof window !== 'undefined' ? window.innerHeight : 768)

function onResize() {
  vw.value = window.innerWidth
  vh.value = window.innerHeight
}

onMounted(() => window.addEventListener('resize', onResize, { passive: true }))
onBeforeUnmount(() => window.removeEventListener('resize', onResize))

interface OverlayBox {
  left: number
  top: number
  width: number
  height: number
}

type Side = 'right' | 'left' | 'below' | 'above'

const placement = computed<OverlayBox | null>(() => {
  const s = cardHover.state.value
  if (!s) return null

  // Source aspect: width / height. Default to portrait until the image
  // has been measured. Quests / Fulcrums come back > 1 here, which
  // makes the overlay land as a landscape card instead of being
  // squashed into a portrait box.
  const aspect = s.natural ? s.natural.width / s.natural.height : PORTRAIT_ASPECT

  // Pick the long edge first, derive the short edge from the aspect.
  let width: number
  let height: number
  if (aspect >= 1) {
    width = PREFERRED_LONG
    height = width / aspect
  } else {
    height = PREFERRED_LONG
    width = height * aspect
  }

  // Clamp to viewport (with a margin) without distorting the aspect.
  const maxW = vw.value - MARGIN * 2
  const maxH = vh.value - MARGIN * 2
  if (width > maxW) {
    width = maxW
    height = width / aspect
  }
  if (height > maxH) {
    height = maxH
    width = height * aspect
  }

  const a = s.anchor
  // Sideways (rotated) cards land as landscape rects on screen — for
  // those, prefer vertical placement (above/below) so the preview isn't
  // squeezed into the narrow horizontal sliver next to the card.
  const isLandscape = a.width > a.height
  const order: Side[] = isLandscape
    ? ['below', 'above', 'right', 'left']
    : ['right', 'left', 'below', 'above']

  const fits: Record<Side, boolean> = {
    right: vw.value - (a.x + a.width) - MARGIN >= width + GAP,
    left: a.x - MARGIN >= width + GAP,
    below: vh.value - (a.y + a.height) - MARGIN >= height + GAP,
    above: a.y - MARGIN >= height + GAP,
  }

  const chosen: Side = order.find((side) => fits[side]) ?? order[0]

  let left: number
  let top: number
  switch (chosen) {
    case 'right':
      left = a.x + a.width + GAP
      top = a.y + a.height / 2 - height / 2
      break
    case 'left':
      left = a.x - GAP - width
      top = a.y + a.height / 2 - height / 2
      break
    case 'below':
      left = a.x + a.width / 2 - width / 2
      top = a.y + a.height + GAP
      break
    case 'above':
      left = a.x + a.width / 2 - width / 2
      top = a.y - GAP - height
      break
  }

  // Clamp to viewport — picks up the slack when no side had room.
  left = Math.max(MARGIN, Math.min(left, vw.value - width - MARGIN))
  top = Math.max(MARGIN, Math.min(top, vh.value - height - MARGIN))

  return { left, top, width, height }
})
</script>

<template>
  <Teleport to="body">
    <div
      v-if="cardHover.state.value && placement"
      class="card-overlay"
      :style="{
        left: `${placement.left}px`,
        top: `${placement.top}px`,
        width: `${placement.width}px`,
        height: `${placement.height}px`,
      }"
      aria-hidden="true"
    >
      <img
        :src="cardHover.state.value.src"
        :alt="cardHover.state.value.alt"
        draggable="false"
      />
    </div>
  </Teleport>
</template>

<style scoped>
.card-overlay {
  position: fixed;
  z-index: 1000;
  pointer-events: none;
  border-radius: 18px;
  overflow: hidden;
  background: rgba(10, 7, 4, 0.85);
  box-shadow:
    0 16px 40px rgba(0, 0, 0, 0.7),
    0 0 0 1px rgba(255, 255, 255, 0.12);
}

.card-overlay img {
  width: 100%;
  height: 100%;
  object-fit: contain;
  display: block;
  user-select: none;
  -webkit-user-drag: none;
}
</style>
