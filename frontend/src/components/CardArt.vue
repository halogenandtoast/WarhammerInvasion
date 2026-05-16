<script setup lang="ts">
// Self-contained card art for the HTML card-slot wrappers in
// PlaySide.vue. The slot itself is a plain HTML <div> (so
// `view-transition-name` works reliably across renders); this
// component just draws the art at whatever size the slot has.
//
// We render an inline <svg> with a fixed viewBox in card units so the
// SVG transform machinery handles the front/back rotation cleanly —
// CSS rotation on an <img> would need pixel measurements of the
// containing slot (which we don't have at template time) to compute
// the inverted aspect.
//
// Modes:
//   - face-down: the card-back image
//   - face-up:   the front image at /cards/{code}.jpg
//   - fallback:  a labeled rect (no code or image 404'd)
//
// The optional `rotated` prop says the SLOT is laid out landscape (e.g.
// the deck/discard pile cell). Front and back rotate independently
// because the back is always portrait while the front may be either.

import { computed, onBeforeUnmount, ref, watchEffect } from 'vue'
import { cardHover } from '../stores/cardHover'
import { getCachedSize, loadImageSize, type NaturalSize } from '../lib/cardImage'

interface CardMeta {
  code?: string
  title?: string
}

const props = defineProps<{
  card?: CardMeta | null
  faceDown?: boolean
  rotated?: boolean
}>()

const rootEl = ref<HTMLDivElement | null>(null)

// Card unit dimensions. The actual on-screen size is set by the CSS
// of the card-slot wrapper; this just gives the SVG something to lay
// out in. Portrait by default; landscape when `rotated`.
const VB_PORTRAIT_W = 200
const VB_PORTRAIT_H = 280
const vbW = computed(() => (props.rotated ? VB_PORTRAIT_H : VB_PORTRAIT_W))
const vbH = computed(() => (props.rotated ? VB_PORTRAIT_W : VB_PORTRAIT_H))

const imageSrc = computed<string | null>(() => {
  if (!props.card?.code) return null
  return `/cards/${props.card.code}.jpg`
})

const imageBroken = ref(false)
function onImgError() {
  imageBroken.value = true
}

const naturalSize = ref<NaturalSize | null>(null)
watchEffect(() => {
  const src = imageSrc.value
  if (!src) {
    naturalSize.value = null
    return
  }
  const cached = getCachedSize(src)
  if (cached) {
    naturalSize.value = cached
    return
  }
  naturalSize.value = null
  loadImageSize(src)
    .then((dims) => {
      if (imageSrc.value === src) naturalSize.value = dims
    })
    .catch(() => {
      /* probe failure isn't fatal — the <image>'s onerror still fires */
    })
})

const displayLandscape = computed(() => !!props.rotated)
const sourceLandscape = computed(() => {
  const d = naturalSize.value
  return d ? d.width > d.height : false
})

const rotateFront = computed(() => sourceLandscape.value !== displayLandscape.value)
const rotateBack = computed(() => displayLandscape.value)

interface RotLayout {
  x: number
  y: number
  width: number
  height: number
  transform: string
}

function layoutFor(rotate: boolean): RotLayout {
  const w = vbW.value
  const h = vbH.value
  if (!rotate) {
    return { x: 0, y: 0, width: w, height: h, transform: '' }
  }
  const iw = h
  const ih = w
  return {
    x: (w - iw) / 2,
    y: (h - ih) / 2,
    width: iw,
    height: ih,
    transform: `rotate(90 ${w / 2} ${h / 2})`,
  }
}

const frontLayout = computed(() => layoutFor(rotateFront.value))
const backLayout = computed(() => layoutFor(rotateBack.value))

// ---- hover preview ----
// Mouse-only — touch users get the long-press path elsewhere.
function onCardEnter() {
  if (props.faceDown) return
  const src = imageSrc.value
  if (!src || imageBroken.value) return
  const el = rootEl.value
  if (!el) return
  const r = el.getBoundingClientRect()
  cardHover.show({
    src,
    alt: props.card?.title ?? '',
    anchor: { x: r.left, y: r.top, width: r.width, height: r.height },
    natural: naturalSize.value
      ? { width: naturalSize.value.width, height: naturalSize.value.height }
      : null,
  })
}

function onCardLeave() {
  const src = imageSrc.value
  if (src) cardHover.hide(src)
}

onBeforeUnmount(() => {
  const src = imageSrc.value
  if (src) cardHover.hide(src)
})
</script>

<template>
  <div
    ref="rootEl"
    class="card-art"
    @mouseenter="onCardEnter"
    @mouseleave="onCardLeave"
  >
    <svg
      class="card-svg"
      :viewBox="`0 0 ${vbW} ${vbH}`"
      preserveAspectRatio="xMidYMid slice"
    >
      <template v-if="faceDown">
        <g :transform="backLayout.transform">
          <image
            :x="backLayout.x"
            :y="backLayout.y"
            :width="backLayout.width"
            :height="backLayout.height"
            href="/card-backs/basic.jpg"
            preserveAspectRatio="xMidYMid slice"
            class="back-img"
          />
        </g>
        <rect
          x="0.5"
          y="0.5"
          :width="vbW - 1"
          :height="vbH - 1"
          fill="none"
          class="back-border"
        />
      </template>
      <template v-else-if="imageSrc && !imageBroken">
        <g :transform="frontLayout.transform">
          <image
            :x="frontLayout.x"
            :y="frontLayout.y"
            :width="frontLayout.width"
            :height="frontLayout.height"
            :href="imageSrc"
            preserveAspectRatio="xMidYMid slice"
            class="art"
            @error="onImgError"
          />
        </g>
      </template>
      <template v-else-if="card">
        <rect x="0" y="0" :width="vbW" :height="vbH" class="bg" />
        <text
          :x="vbW / 2"
          :y="vbH / 2"
          text-anchor="middle"
          dominant-baseline="middle"
          class="title"
        >
          {{ card.title ?? '?' }}
        </text>
      </template>
    </svg>
  </div>
</template>

<style scoped>
/* Round the corners on the HTML wrapper rather than via SVG clip-path:
   clip-path on an inline <svg> root isn't reliably honored across
   browsers, but border-radius + overflow:hidden on the wrapper div
   clips the contained SVG cleanly. */
.card-art {
  position: relative;
  width: 100%;
  height: 100%;
  border-radius: var(--card-radius);
  overflow: hidden;
  filter: drop-shadow(var(--shadow-card));
}

.card-svg {
  display: block;
  width: 100%;
  height: 100%;
}

.bg {
  fill: var(--bg);
  stroke: var(--border);
  stroke-width: 1;
}

.back-border {
  stroke: #4a2b1c;
  stroke-width: 1;
  fill: none;
}

.title {
  fill: var(--fg);
  font-size: 18px;
  font-weight: 600;
  font-family: var(--font-sans);
}
</style>
