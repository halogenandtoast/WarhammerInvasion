<script setup lang="ts">
// Floating enlarged-card preview. Mounted once over the play table;
// listens to the cardHover store and renders a fixed-position image
// next to the hovered source card, clamped inside the viewport.

import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { cardHover } from '../stores/cardHover'
import { CARD_W, CARD_H } from '../lib/cardSize'

// Preferred display size in CSS pixels. Shrinks to fit if the viewport
// is too small to hold the preferred size.
const PREFERRED_H = 440
const ASPECT = CARD_W / CARD_H
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

const placement = computed<OverlayBox | null>(() => {
  const s = cardHover.state.value
  if (!s) return null

  // Fit the preferred size inside the viewport (with a small margin).
  let height = Math.min(PREFERRED_H, vh.value - MARGIN * 2)
  let width = height * ASPECT
  const maxW = vw.value - MARGIN * 2
  if (width > maxW) {
    width = maxW
    height = width / ASPECT
  }

  const a = s.anchor
  const rightSpace = vw.value - (a.x + a.width) - MARGIN
  const leftSpace = a.x - MARGIN

  let left: number
  if (rightSpace >= width + GAP) {
    left = a.x + a.width + GAP
  } else if (leftSpace >= width + GAP) {
    left = a.x - GAP - width
  } else {
    // Neither side has room — pick the wider side and clamp; we
    // accept that the preview may overlap the source card.
    left =
      rightSpace >= leftSpace
        ? Math.min(vw.value - width - MARGIN, a.x + a.width + GAP)
        : Math.max(MARGIN, a.x - GAP - width)
  }
  left = Math.max(MARGIN, Math.min(left, vw.value - width - MARGIN))

  // Vertical: center on the anchor, then clamp.
  let top = a.y + a.height / 2 - height / 2
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
  border-radius: 10px;
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
