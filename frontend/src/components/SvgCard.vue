<script setup lang="ts">
// SVG-native card renderer — emits a <g> with the card at (x, y) for
// an SVG playboard. Two modes:
//   - face-down: the card-back image
//   - face-up:   the card-front image at /cards/{code}.jpg (no
//                drawn overlay; the printed card art carries every
//                detail the player needs)
// Falls back to a labeled rect if the card has no code or the image
// fails to load.

import { computed, onBeforeUnmount, ref } from 'vue'
import { CARD_W, CARD_H } from '../lib/cardSize'
import { cardHover } from '../stores/cardHover'

interface CardMeta {
  code?: string
  title?: string
}

const props = defineProps<{
  x: number
  y: number
  width?: number
  height?: number
  card?: CardMeta | null
  faceDown?: boolean
}>()

const w = computed(() => props.width ?? CARD_W)
const h = computed(() => props.height ?? CARD_H)

const imageSrc = computed<string | null>(() => {
  if (!props.card?.code) return null
  return `/cards/${props.card.code}.jpg`
})

// If the <image> 404s we want to fall back gracefully. SVG <image>
// fires an onerror just like <img>; we toggle a ref.
const imageBroken = ref(false)

function onImgError() {
  imageBroken.value = true
}

// ---- hover preview ----
// On mouseenter over a face-up card image, publish the source rect to
// the shared cardHover store so <CardOverlay> can render an enlarged
// preview. mouseenter/mouseleave (not pointerenter) so the behavior
// is mouse-only — touch users still get the existing tap flow.
function onCardEnter(e: MouseEvent) {
  const src = imageSrc.value
  if (!src || imageBroken.value) return
  const el = e.currentTarget as SVGGraphicsElement | null
  if (!el) return
  const r = el.getBoundingClientRect()
  cardHover.show({
    src,
    alt: props.card?.title ?? '',
    anchor: { x: r.left, y: r.top, width: r.width, height: r.height },
  })
}

function onCardLeave() {
  const src = imageSrc.value
  if (src) cardHover.hide(src)
}

// If this card unmounts while it owns the overlay (DOM update during
// hover), make sure the preview disappears.
onBeforeUnmount(() => {
  const src = imageSrc.value
  if (src) cardHover.hide(src)
})
</script>

<template>
  <g :transform="`translate(${x}, ${y})`" class="svg-card">
    <template v-if="faceDown">
      <image
        x="0"
        y="0"
        :width="w"
        :height="h"
        href="/card-backs/basic.jpg"
        preserveAspectRatio="xMidYMid slice"
        class="back-img"
      />
      <rect
        x="0.5"
        y="0.5"
        :width="w - 1"
        :height="h - 1"
        rx="6"
        ry="6"
        fill="none"
        class="back-border"
      />
    </template>
    <template v-else-if="imageSrc && !imageBroken">
      <image
        x="0"
        y="0"
        :width="w"
        :height="h"
        :href="imageSrc"
        preserveAspectRatio="xMidYMid slice"
        class="art"
        @error="onImgError"
        @mouseenter="onCardEnter"
        @mouseleave="onCardLeave"
      />
    </template>
    <template v-else-if="card">
      <!-- Fallback when the card lacks a code (or its image is missing).
           Plain rect with the title centred — better than a blank slot. -->
      <rect x="0" y="0" :width="w" :height="h" rx="6" ry="6" class="bg" />
      <text
        :x="w / 2"
        :y="h / 2"
        text-anchor="middle"
        dominant-baseline="middle"
        class="title"
      >
        {{ card.title ?? '?' }}
      </text>
    </template>
  </g>
</template>

<style scoped>
.svg-card .bg {
  fill: var(--bg);
  stroke: var(--border);
  stroke-width: 1;
}

.svg-card .title {
  fill: var(--fg);
  font-size: 10px;
  font-weight: 600;
  font-family: var(--font-sans);
}

.svg-card .art {
  /* Rounded corners on the art image: use clip-path through a CSS
     border-radius — SVG image clips against its own bounding box, so
     a rect mask isn't needed for the basic case. */
  border-radius: 6px;
}

.svg-card .back-border {
  stroke: #4a2b1c;
  stroke-width: 1;
}
</style>
