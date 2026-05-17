<script setup lang="ts">
// Cost-curve + per-type / per-race breakdown for a deck summary.
// Used by both the read-only deck view and the editor's right rail.
//
// The optional `tone` / `i18nPrefix` props let each consumer use its
// own heading copy without forcing one site to learn the other's keys.

import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { DeckStats } from '../lib/deck'

const props = withDefaults(
  defineProps<{
    stats: DeckStats
    // Translation prefix for the section headings. The expected keys
    // are `<prefix>.cost_curve`, `<prefix>.by_type`, `<prefix>.by_race`.
    i18nPrefix?: string
  }>(),
  { i18nPrefix: 'deck_view' },
)

const { t } = useI18n({ useScope: 'global' })

const maxBucket = computed(() =>
  Math.max(1, ...props.stats.costCurve.map((b) => b.count)),
)

function curveWidth(count: number): string {
  return `${(count / maxBucket.value) * 100}%`
}
</script>

<template>
  <section class="curve">
    <h3>{{ t(`${i18nPrefix}.cost_curve`) }}</h3>
    <ul class="curve-list" role="list">
      <li v-for="b in stats.costCurve" :key="b.bucket" class="curve-row">
        <span class="curve-label">{{ b.bucket }}</span>
        <span class="curve-bar">
          <span class="curve-fill" :style="{ width: curveWidth(b.count) }" />
        </span>
        <span class="curve-count">{{ b.count }}</span>
      </li>
    </ul>
  </section>

  <section class="breakdown">
    <div>
      <h3>{{ t(`${i18nPrefix}.by_type`) }}</h3>
      <dl>
        <template v-for="(n, ty) in stats.byType" :key="ty">
          <dt>{{ ty }}</dt><dd>{{ n }}</dd>
        </template>
      </dl>
    </div>
    <div>
      <h3>{{ t(`${i18nPrefix}.by_race`) }}</h3>
      <dl>
        <template v-for="(n, r) in stats.byRace" :key="r">
          <dt>{{ r }}</dt><dd>{{ n }}</dd>
        </template>
      </dl>
    </div>
  </section>
</template>

<style scoped>
h3 {
  margin: 0 0 0.5rem;
  font-size: 0.72rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.curve-list {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
}

.curve-row {
  display: grid;
  grid-template-columns: 1.6em 1fr 2em;
  gap: 0.5rem;
  align-items: center;
  font-size: 0.82rem;
  color: var(--fg-dim);
}

.curve-bar {
  position: relative;
  background: var(--bg);
  height: 8px;
  border-radius: var(--radius-pill);
  overflow: hidden;
}

.curve-fill {
  position: absolute;
  inset: 0 auto 0 0;
  background: var(--accent);
  border-radius: inherit;
}

.curve-count {
  text-align: right;
  color: var(--fg);
}

.breakdown {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0.8rem;
}

.breakdown dl {
  margin: 0;
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 0.15rem 0.5rem;
  font-size: 0.85rem;
}

.breakdown dt {
  color: var(--fg-dim);
}

.breakdown dd {
  margin: 0;
  text-align: right;
  font-variant-numeric: tabular-nums;
}
</style>
