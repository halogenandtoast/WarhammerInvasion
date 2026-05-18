<script setup lang="ts">
// Cycle/set tree on the left of the cards browser. Deluxe (single-set)
// cycles render as leaves with no nested list; everything else gets a
// parent + ordered child list.
//
// The parent owns the selected cycle/set state — this component only
// emits intent.

import { computed } from 'vue'
import { useI18n } from 'vue-i18n'

const props = defineProps<{
  open: boolean
  cycles: string[]
  setsByCycle: Record<string, string[]>
  cycleCounts: Record<string, number>
  setCounts: Record<string, number>
  totalCards: number
  selectedCycle: string | 'all'
  selectedSet: string | 'all'
}>()

const emit = defineEmits<{
  (e: 'select-cycle', cycle: string | 'all'): void
  (e: 'select-set', payload: { cycle: string; set: string }): void
}>()

const { t } = useI18n({ useScope: 'global' })

function isDeluxe(cycle: string): boolean {
  return (props.setsByCycle[cycle] ?? []).length <= 1
}

const isAllActive = computed(
  () => props.selectedCycle === 'all' && props.selectedSet === 'all',
)
</script>

<template>
  <aside
    class="sidebar"
    :class="{ open }"
    :aria-label="t('cards.sidebar.aria_label')"
  >
    <div class="sidebar-inner">
      <p class="sidebar-eyebrow">{{ t('cards.sidebar.eyebrow') }}</p>

      <h3 class="sidebar-heading">{{ t('cards.sidebar.cycles_heading') }}</h3>
      <ul class="cycle-tree" role="list">
        <li>
          <button
            type="button"
            class="sidebar-item all-cycles"
            :class="{ active: isAllActive }"
            @click="emit('select-cycle', 'all')"
          >
            <span class="item-label">{{ t('cards.sidebar.all_cycles') }}</span>
            <span class="count-pill">{{ totalCards }}</span>
          </button>
        </li>

        <template v-for="c in cycles" :key="c">
          <!-- Deluxe / single-set cycle: leaf with no nested list. -->
          <li v-if="isDeluxe(c)" class="cycle-row">
            <button
              type="button"
              class="sidebar-item cycle-name"
              :class="{ active: selectedCycle === c }"
              @click="emit('select-set', { cycle: c, set: c })"
            >
              <span class="item-label">{{ c }}</span>
              <span class="count-pill">{{ cycleCounts[c] ?? 0 }}</span>
            </button>
          </li>

          <!-- Cycle with multiple sets: parent + ordered child list. -->
          <li v-else class="cycle-row">
            <button
              type="button"
              class="sidebar-item cycle-name"
              :class="{ active: selectedCycle === c && selectedSet === 'all' }"
              @click="emit('select-cycle', c)"
            >
              <span class="item-label">{{ c }}</span>
              <span class="count-pill">{{ cycleCounts[c] ?? 0 }}</span>
            </button>
            <ol class="set-list">
              <li v-for="s in (setsByCycle[c] ?? [])" :key="s">
                <button
                  type="button"
                  class="sidebar-item set-item"
                  :class="{ active: selectedSet === s }"
                  @click="emit('select-set', { cycle: c, set: s })"
                >
                  <span class="item-label">{{ s }}</span>
                  <span class="count-pill">{{ setCounts[s] ?? 0 }}</span>
                </button>
              </li>
            </ol>
          </li>
        </template>
      </ul>
    </div>
  </aside>
</template>

<style scoped>
.sidebar {
  background: var(--bg-elev);
  border-right: 1px solid var(--border);
  position: sticky;
  align-self: start;
  height: calc(100dvh - 60px);
  overflow: hidden;
}

.sidebar-inner {
  padding: 1.5rem 1.1rem 2rem;
  height: 100%;
  overflow-y: auto;
}

.sidebar-eyebrow {
  margin: 0 0 1rem;
  font-size: 0.72rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.sidebar-heading {
  margin: 0 0 0.45rem;
  font-size: 0.78rem;
  letter-spacing: 0.14em;
  text-transform: uppercase;
  color: var(--fg-dim);
}

.cycle-tree {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.1rem;
}

.cycle-row + .cycle-row {
  margin-top: 0.05rem;
}

.set-list {
  margin: 0.2rem 0 0.5rem;
  padding-left: 2.1rem;
  display: flex;
  flex-direction: column;
  gap: 0.1rem;
  list-style: decimal;
  color: var(--fg-faint);
}

.set-list > li::marker {
  font-size: 0.78rem;
  font-variant-numeric: tabular-nums;
}

.set-list > li {
  padding-left: 0.2rem;
}

.sidebar-item {
  width: 100%;
  display: flex;
  align-items: center;
  gap: 0.5rem;
  min-height: 36px;
  padding: 0.4rem 0.55rem;
  background: transparent;
  color: var(--fg-dim);
  border: none;
  border-radius: 5px;
  text-align: left;
  font-size: 0.94rem;
  cursor: pointer;
  border-left: 2px solid transparent;
}

.sidebar-item:hover {
  background: var(--bg-elev-2);
  color: var(--fg);
}

.sidebar-item.active {
  color: var(--fg);
  background: var(--bg-elev-2);
  border-left-color: var(--accent);
}

.cycle-name {
  font-weight: 600;
  color: var(--fg);
  justify-content: space-between;
}

.set-item {
  font-size: 0.86rem;
  min-height: 30px;
  padding: 0.3rem 0.55rem;
  font-weight: 400;
  justify-content: space-between;
}

.all-cycles {
  justify-content: space-between;
  margin-bottom: 0.35rem;
}

.item-label {
  flex: 1;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.count-pill {
  display: inline-block;
  font-size: 0.7rem;
  color: var(--fg-faint);
  background: var(--bg);
  padding: 0.05rem 0.45rem;
  border-radius: var(--radius-pill);
  border: 1px solid var(--border);
  flex-shrink: 0;
}

.sidebar-item.active .count-pill {
  color: var(--accent-strong);
  border-color: var(--accent);
}

@media (max-width: 900px) {
  .sidebar {
    position: fixed;
    top: 0;
    left: 0;
    bottom: 0;
    width: min(85vw, 320px);
    max-height: 100dvh;
    transform: translateX(-100%);
    transition: transform var(--transition-base);
    z-index: var(--z-drawer);
    border-right: 1px solid var(--border);
  }

  .sidebar.open {
    transform: translateX(0);
    box-shadow: var(--shadow-drawer);
  }

  .sidebar-inner {
    padding-top: 4rem;
  }
}
</style>
