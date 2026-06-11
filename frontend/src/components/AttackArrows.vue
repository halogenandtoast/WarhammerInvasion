<script setup lang="ts">
// Combat arrows: curved strokes from each declared attacker to the
// attacked zone (or targeted legend) while a combat is in flight.
// Pure presentation — measures the live DOM each engine update and on
// resize, draws into a pointer-transparent SVG that covers the table.

import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import type { EngineGame } from '../api/protocol'

const props = defineProps<{
  engine: EngineGame
  // The element that hosts both PlaySides; arrows are positioned in
  // its coordinate space.
  root: HTMLElement | null
}>()

interface Arrow {
  id: string
  d: string
  tipX: number
  tipY: number
  angle: number
}

const arrows = ref<Arrow[]>([])
const size = ref({ w: 0, h: 0 })

let raf = 0
function scheduleMeasure() {
  if (raf) return
  raf = requestAnimationFrame(() => {
    raf = 0
    measure()
  })
}

function measure() {
  const host = props.root
  const combat = props.engine.combat
  if (!host || !combat) {
    arrows.value = []
    return
  }
  const hr = host.getBoundingClientRect()
  size.value = { w: hr.width, h: hr.height }

  // Target point: the legend card if the attack targets it, otherwise
  // the defending side's zone plate.
  let targetEl: Element | null = null
  if (combat.targetLegend != null) {
    targetEl = host.querySelector(
      `[data-side-player="${combat.defendingPlayer}"] [data-card-key="${combat.targetLegend}"]`,
    )
  }
  if (!targetEl) {
    targetEl = host.querySelector(
      `[data-side-player="${combat.defendingPlayer}"] [data-zone-plate="${combat.targetZone}"]`,
    )
  }
  if (!targetEl) {
    arrows.value = []
    return
  }
  const tr = targetEl.getBoundingClientRect()
  const tx = tr.left - hr.left + tr.width / 2
  const ty = tr.top - hr.top + tr.height / 2

  const next: Arrow[] = []
  for (const key of combat.attackers) {
    const el = host.querySelector(
      `[data-side-player="${combat.attackingPlayer}"] [data-card-key="${key}"]`,
    )
    if (!el) continue
    const ar = el.getBoundingClientRect()
    const ax = ar.left - hr.left + ar.width / 2
    const ay = ar.top - hr.top + ar.height / 2
    // Quadratic curve bowed sideways for a hand-drawn arc.
    const mx = (ax + tx) / 2
    const my = (ay + ty) / 2
    const dx = tx - ax
    const dy = ty - ay
    const len = Math.max(Math.hypot(dx, dy), 1)
    const bow = Math.min(60, len * 0.18)
    const cx = mx + (-dy / len) * bow
    const cy = my + (dx / len) * bow
    // Stop the line short of the target center so the arrowhead sits
    // on the plate edge rather than its middle.
    const tShort = 1 - Math.min(28 / len, 0.3)
    const qx = ax + 2 * (cx - ax) * tShort * (1 - tShort) + 0 // placeholder, replaced below
    void qx
    const endX = ax * (1 - tShort) * (1 - tShort) + 2 * cx * tShort * (1 - tShort) + tx * tShort * tShort
    const endY = ay * (1 - tShort) * (1 - tShort) + 2 * cy * tShort * (1 - tShort) + ty * tShort * tShort
    const angle = Math.atan2(ty - cy, tx - cx) * (180 / Math.PI)
    next.push({
      id: `atk-${key}`,
      d: `M ${ax} ${ay} Q ${cx} ${cy} ${endX} ${endY}`,
      tipX: endX,
      tipY: endY,
      angle,
    })
  }
  arrows.value = next
}

watch(
  () => [props.engine.combat, props.root] as const,
  () => scheduleMeasure(),
  { deep: true, immediate: true },
)

// Card slots animate for ~450ms after each update; re-measure once
// the dust settles so arrows track the FLIP'd positions.
watch(
  () => props.engine,
  () => {
    scheduleMeasure()
    setTimeout(scheduleMeasure, 480)
  },
)

function onResize() {
  scheduleMeasure()
}

onMounted(() => {
  window.addEventListener('resize', onResize)
  scheduleMeasure()
})

onBeforeUnmount(() => {
  window.removeEventListener('resize', onResize)
  if (raf) cancelAnimationFrame(raf)
})

const active = computed(() => props.engine.combat !== null && arrows.value.length > 0)
</script>

<template>
  <svg
    v-if="active"
    class="attack-arrows"
    :viewBox="`0 0 ${size.w} ${size.h}`"
    :width="size.w"
    :height="size.h"
    aria-hidden="true"
  >
    <g v-for="a in arrows" :key="a.id">
      <path :d="a.d" class="arrow-glow" />
      <path :d="a.d" class="arrow-line" />
      <polygon
        points="0,-6 12,0 0,6"
        class="arrow-head"
        :transform="`translate(${a.tipX}, ${a.tipY}) rotate(${a.angle})`"
      />
    </g>
  </svg>
</template>

<style scoped>
.attack-arrows {
  position: absolute;
  inset: 0;
  pointer-events: none;
  z-index: 40;
  overflow: visible;
}

.arrow-glow {
  fill: none;
  stroke: rgba(224, 90, 60, 0.25);
  stroke-width: 7;
  stroke-linecap: round;
}

.arrow-line {
  fill: none;
  stroke: #e05a3c;
  stroke-width: 2.5;
  stroke-linecap: round;
  stroke-dasharray: 10 7;
  animation: arrow-march 0.9s linear infinite;
}
@media (prefers-reduced-motion: reduce) {
  .arrow-line { animation: none; }
}
@keyframes arrow-march {
  to { stroke-dashoffset: -17; }
}

.arrow-head {
  fill: #e05a3c;
  stroke: rgba(0, 0, 0, 0.4);
  stroke-width: 0.5;
}
</style>
