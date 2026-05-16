<script setup lang="ts">
// SVG-native card renderer — emits a <g> with the card at (x, y) for
// an SVG playboard. Two modes:
//   - face-down: the card-back image
//   - face-up:   the card-front image at /cards/{code}.jpg (no
//                drawn overlay; the printed card art carries every
//                detail the player needs)
// Falls back to a labeled rect if the card has no code or the image
// fails to load.
//
// `rotated` controls how the card lands on screen — false = portrait
// outer rect, true = landscape outer rect. The source art itself may
// be portrait (most cards) or landscape (Quests / Fulcrums); we probe
// it via cardImage and rotate the inner <image> 90° only when the
// source orientation doesn't match the display orientation. That stops
// landscape art from being squashed into a portrait slot and vice
// versa. Front and back rotate independently because the back is
// always portrait.
//
// Hover events live on the wrapping <g> so the whole visible card
// silhouette catches mouseenter/leave even at the rounded corners.

import { computed, onBeforeUnmount, ref, useId, watchEffect } from 'vue'
import { CARD_W, CARD_H } from '../lib/cardSize'
import { cardHover } from '../stores/cardHover'
import { getCachedSize, loadImageSize, type NaturalSize } from '../lib/cardImage'

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
  rotated?: boolean
}>()

// Outer (visible) rect on screen. When rotated this is the landscape
// rect; when not, it's the portrait rect.
const w = computed(() => props.width ?? CARD_W)
const h = computed(() => props.height ?? CARD_H)

// Per-instance clipPath ID. SVG <image> ignores CSS border-radius in
// Firefox (and unreliably elsewhere), so we route the card's rounded
// corners through a real <clipPath> on the wrapper <g>. The rect inside
// pulls rx/ry from CSS — `--card-radius` stays the single source of
// truth. Clipping at the wrapper means inner rotated content is also
// trimmed to the same rounded silhouette.
const clipId = `card-clip-${useId()}`

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

// Natural dimensions of the front image. Null until measured — until
// then we assume portrait, which is correct for the vast majority of
// cards (only Quests / Fulcrums ship landscape). cardImage caches
// dimensions per URL, so subsequent renders of the same card never
// re-probe.
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
      // Guard against the card changing while the probe was in flight.
      if (imageSrc.value === src) naturalSize.value = dims
    })
    .catch(() => {
      /* probe failure isn't fatal — the <image>'s own onerror will fire */
    })
})

const displayLandscape = computed(() => !!props.rotated)
const sourceLandscape = computed(() => {
  const d = naturalSize.value
  return d ? d.width > d.height : false
})

// Rotate the front art iff its native orientation differs from the
// display orientation. The card back is always portrait, so it only
// rotates when the display rect itself is landscape.
const rotateFront = computed(() => sourceLandscape.value !== displayLandscape.value)
const rotateBack = computed(() => displayLandscape.value)

// Inner geometry for a sub-element that may need rotating 90° around
// the outer rect's center. When no rotation is needed we draw at
// (0, 0, w, h) with no transform.
interface RotLayout {
  x: number
  y: number
  width: number
  height: number
  transform: string
}

function layoutFor(rotate: boolean): RotLayout {
  if (!rotate) {
    return { x: 0, y: 0, width: w.value, height: h.value, transform: '' }
  }
  // Swap dims so the source is drawn in its native orientation, then
  // rotate the whole thing 90° around the outer rect's center, which
  // lands it back inside (0, 0, w, h).
  const iw = h.value
  const ih = w.value
  return {
    x: (w.value - iw) / 2,
    y: (h.value - ih) / 2,
    width: iw,
    height: ih,
    transform: `rotate(90 ${w.value / 2} ${h.value / 2})`,
  }
}

const frontLayout = computed(() => layoutFor(rotateFront.value))
const backLayout = computed(() => layoutFor(rotateBack.value))

// ---- hover preview ----
// Bound to the wrapping <g> so the whole visible silhouette (including
// the rotated AABB for sideways cards) catches mouseenter/leave even at
// the rounded corners. mouseenter/leave (not pointer*) so the behavior
// stays mouse-only and doesn't fire on touch.
function onCardEnter(e: MouseEvent) {
  if (props.faceDown) return
  const src = imageSrc.value
  if (!src || imageBroken.value) return
  const el = e.currentTarget as SVGGraphicsElement | null
  if (!el) return
  // <g>.getBoundingClientRect returns the union of children's screen
  // rects with all transforms applied — so for rotated cards we get the
  // landscape AABB on screen, exactly what we want to anchor against.
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

// If this card unmounts while it owns the overlay (DOM update during
// hover), make sure the preview disappears.
onBeforeUnmount(() => {
  const src = imageSrc.value
  if (src) cardHover.hide(src)
})
</script>

<template>
  <g
    :transform="`translate(${x}, ${y})`"
    class="svg-card"
    :clip-path="`url(#${clipId})`"
    @mouseenter="onCardEnter"
    @mouseleave="onCardLeave"
  >
    <!-- Clipping is applied at the wrapper, so anything inside — rotated
         or not — shares the same rounded silhouette. The clip rect uses
         the outer (w, h) since every branch ultimately fills it. -->
    <defs>
      <clipPath :id="clipId">
        <rect x="0" y="0" :width="w" :height="h" class="clip-rect" />
      </clipPath>
    </defs>
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
      <!-- Border traces the post-clip outer silhouette so the rounded
           corners read regardless of how the back is rotated. -->
      <rect
        x="0.5"
        y="0.5"
        :width="w - 1"
        :height="h - 1"
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
      <!-- Fallback when the card lacks a code (or its image is missing).
           Plain rect with the title centred — better than a blank slot. -->
      <rect x="0" y="0" :width="w" :height="h" class="bg" />
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
.svg-card {
  /* SVG can't take box-shadow on <g>/<image>; drop-shadow on the
     group filters by alpha, so the shadow follows the rounded card
     silhouette rather than its bounding box. */
  filter: drop-shadow(var(--shadow-card));
}

.svg-card .bg,
.svg-card .back-border,
.svg-card .clip-rect {
  /* CSS rx/ry on SVG <rect> works everywhere modern. The clip-rect
     drives the rounded silhouette for both face-up art and face-down
     back-image; the back-border and bg fallback share the same radius
     so the visible outline lines up with the image corners. */
  rx: var(--card-radius);
  ry: var(--card-radius);
}

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

.svg-card .back-border {
  stroke: #4a2b1c;
  stroke-width: 1;
}
</style>
