<script setup lang="ts">
// Right-rail summary panel inside the deck editor: capital chip,
// running count, change-capital affordance, validation issues, stats,
// and the linear deck list with inline +/− controls.

import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Card } from '../types/card'
import type { DeckSummary } from '../lib/deck'
import { MAX_COPIES_PER_TITLE, MIN_DECK_SIZE, MAX_DECK_SIZE, type Capital } from '../lib/deck'
import { raceClass } from '../lib/race'
import CapitalChip from './CapitalChip.vue'
import DeckStatsPanel from './DeckStatsPanel.vue'

const props = defineProps<{
  summary: DeckSummary
  capital: Capital | null
  // Whether the user can change capital right now (i.e. no
  // non-Neutral cards in the deck yet).
  canChangeCapital: boolean
  // Mobile drawer open/closed state — owned by the parent so it can
  // also drive the floating toggle.
  open: boolean
  counts: Record<string, number>
}>()

const emit = defineEmits<{
  (e: 'change-capital'): void
  (e: 'add', card: Card): void
  (e: 'remove', card: Card): void
}>()

const { t } = useI18n({ useScope: 'global' })

const tone = computed<'ok' | 'warn'>(() =>
  props.summary.issues.some((i) => i.severity === 'error') ? 'warn' : 'ok',
)
</script>

<template>
  <aside
    class="deck-panel"
    :class="{ open }"
    :aria-label="t('deck_edit.panel_label')"
  >
    <div class="panel-inner">
      <header class="panel-head">
        <CapitalChip :capital="capital" />
        <span class="count" :class="`tone-${tone}`">
          {{ summary.stats.total }} / {{ MIN_DECK_SIZE }}–{{ MAX_DECK_SIZE }}
        </span>
      </header>

      <button
        v-if="capital !== null"
        type="button"
        class="change-capital"
        :disabled="!canChangeCapital"
        :title="canChangeCapital ? '' : t('deck_edit.capital_picker.change_blocked')"
        @click="emit('change-capital')"
      >
        {{ t('deck_edit.capital_picker.change') }}
      </button>

      <ul v-if="summary.issues.length > 0" class="issues">
        <li
          v-for="(issue, idx) in summary.issues"
          :key="idx"
          :class="`severity-${issue.severity}`"
        >
          {{ issue.message }}
        </li>
      </ul>

      <DeckStatsPanel :stats="summary.stats" i18n-prefix="deck_edit" />

      <section class="card-list">
        <h3>{{ t('deck_edit.deck_list') }}</h3>
        <ul v-if="summary.cards.length > 0" role="list">
          <li v-for="entry in summary.cards" :key="entry.card.id" class="dl-row">
            <span class="dl-count">{{ entry.count }}×</span>
            <span class="dl-name" :class="raceClass(entry.card.race)">{{ entry.card.name }}</span>
            <span v-if="entry.card.cost !== null" class="dl-cost">{{ entry.card.cost }}</span>
            <button
              class="qty-btn ghost-btn"
              type="button"
              :aria-label="t('deck_edit.minus_aria', { name: entry.card.name })"
              @click="emit('remove', entry.card)"
            >−</button>
            <button
              class="qty-btn ghost-btn"
              type="button"
              :disabled="(counts[entry.card.id] ?? 0) >= MAX_COPIES_PER_TITLE"
              :aria-label="t('deck_edit.plus_aria', { name: entry.card.name })"
              @click="emit('add', entry.card)"
            >+</button>
          </li>
        </ul>
        <p v-else class="empty">{{ t('deck_edit.deck_empty') }}</p>
      </section>
    </div>
  </aside>
</template>

<style scoped>
.deck-panel {
  background: var(--bg-elev);
  border-left: 1px solid var(--border);
  height: calc(100dvh - 60px - 73px);
  position: sticky;
  top: 73px;
  overflow: hidden;
}

.panel-inner {
  height: 100%;
  overflow-y: auto;
  padding: 1rem 1.1rem 1.5rem;
  display: flex;
  flex-direction: column;
  gap: 0.95rem;
}

.panel-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.count {
  font-variant-numeric: tabular-nums;
  font-weight: 600;
  color: var(--fg);
}

.count.tone-warn {
  color: var(--accent-strong);
}

.change-capital {
  align-self: flex-start;
  background: transparent;
  border: 1px dashed var(--border);
  color: var(--fg-dim);
  border-radius: var(--radius-md);
  padding: 0.35rem 0.7rem;
  font-size: 0.8rem;
  cursor: pointer;
  min-height: var(--tap-target);
}

.change-capital:hover:not(:disabled) {
  color: var(--fg);
  border-color: var(--fg-dim);
}

.change-capital:disabled {
  opacity: 0.45;
  cursor: not-allowed;
}

.issues {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
}

.issues li {
  padding: 0.45rem 0.6rem;
  background: var(--bg);
  border-radius: var(--radius-sm);
  font-size: 0.85rem;
  color: var(--fg);
  border-left: 3px solid var(--border);
}

.issues li.severity-error {
  border-left-color: var(--accent-strong);
}

.card-list h3 {
  margin: 0 0 0.5rem;
  font-size: 0.72rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.card-list ul {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
}

.dl-row {
  display: grid;
  grid-template-columns: 2em 1fr auto 28px 28px;
  align-items: center;
  gap: 0.3rem;
  padding: 0.25rem 0.3rem;
  border-radius: var(--radius-sm);
  font-size: 0.85rem;
}

.dl-row:hover {
  background: var(--bg);
}

.dl-count {
  font-variant-numeric: tabular-nums;
  color: var(--fg-dim);
}

.dl-name {
  color: var(--fg);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  border-left: 3px solid transparent;
  padding-left: 0.4rem;
}

.dl-name.race-empire { border-left-color: var(--race-empire); }
.dl-name.race-dwarf { border-left-color: var(--race-dwarf); }
.dl-name.race-high-elf { border-left-color: var(--race-high-elf); }
.dl-name.race-chaos { border-left-color: var(--race-chaos); }
.dl-name.race-orc { border-left-color: var(--race-orc); }
.dl-name.race-dark-elf { border-left-color: var(--race-dark-elf); }
.dl-name.race-neutral { border-left-color: var(--race-neutral); }

.dl-cost {
  font-size: 0.78rem;
  color: var(--fg-faint);
  font-variant-numeric: tabular-nums;
}

.qty-btn {
  width: 32px;
  height: 32px;
  display: grid;
  place-items: center;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  color: var(--fg);
  font-size: 1rem;
  cursor: pointer;
}

.qty-btn:hover:not(:disabled) {
  border-color: var(--accent-strong);
  color: var(--accent-strong);
}

.qty-btn:disabled {
  opacity: 0.35;
  cursor: not-allowed;
}

.ghost-btn {
  width: 26px;
  height: 26px;
  font-size: 0.85rem;
}

.empty {
  margin: 0;
  font-size: 0.86rem;
  color: var(--fg-faint);
  text-align: center;
  padding: 0.5rem 0;
}

@media (max-width: 900px) {
  .deck-panel {
    position: fixed;
    inset: auto 0 0 0;
    top: auto;
    height: 70dvh;
    transform: translateY(100%);
    transition: transform var(--transition-base);
    border-top: 1px solid var(--border);
    border-left: none;
    z-index: 20;
    box-shadow: var(--shadow-drawer);
  }

  .deck-panel.open {
    transform: translateY(0);
  }
}
</style>
