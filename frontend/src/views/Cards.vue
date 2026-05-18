<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Card, CardStat, CardType, Race } from '../types/card'
import { useCardCatalog } from '../composables/useCardCatalog'
import { raceClass } from '../lib/race'
import { cardImageUrl } from '../lib/assets'
import CardsSidebar from '../components/CardsSidebar.vue'
import CardDetailModal from '../components/CardDetailModal.vue'

const { t } = useI18n({ useScope: 'global' })

const catalog = useCardCatalog()
const allCards = catalog.cards
const loading = computed(() => catalog.loading.value && !catalog.loaded.value)
const loadError = catalog.error

const search = ref('')
const selectedCycle = ref<string | 'all'>('all')
const selectedSet = ref<string | 'all'>('all')
const selectedType = ref<CardType | 'all'>('all')
const selectedRace = ref<Race | 'all'>('all')
const selectedCost = ref<CardStat | 'all'>('all')
const includeStubs = ref(true)
const sidebarOpen = ref(false)

const focusedCard = ref<Card | null>(null)

// ----- derived filter sources -----
const cycles = computed(() => unique(allCards.value.map((c) => c.cycle)))
const setsByCycle = computed<Record<string, string[]>>(() => {
  const out: Record<string, string[]> = {}
  for (const c of allCards.value) {
    if (!includeStubs.value && c.stub) continue
    const seen = (out[c.cycle] ??= [])
    if (!seen.includes(c.set)) seen.push(c.set)
  }
  return out
})
const types = computed(() =>
  unique(allCards.value.map((c) => c.type).filter((t): t is CardType => Boolean(t))),
)
const races = computed(() =>
  unique(allCards.value.map((c) => c.race).filter((r): r is Race => Boolean(r))),
)
const costs = computed(() => {
  const all = unique(
    allCards.value
      .map((c) => c.cost)
      .filter((c): c is number | 'X' => c !== null && !(typeof c === 'number' && c < 0)),
  )
  return all.sort((a, b) => {
    if (typeof a === 'number' && typeof b === 'number') return a - b
    if (typeof a === 'number') return -1
    if (typeof b === 'number') return 1
    return a.localeCompare(b)
  })
})

const cycleCounts = computed(() => {
  const counts: Record<string, number> = {}
  for (const c of allCards.value) counts[c.cycle] = (counts[c.cycle] ?? 0) + 1
  return counts
})
const setCounts = computed(() => {
  const counts: Record<string, number> = {}
  for (const c of allCards.value) counts[c.set] = (counts[c.set] ?? 0) + 1
  return counts
})

const filtered = computed(() => {
  const q = search.value.trim().toLowerCase()
  return allCards.value.filter((c) => {
    if (!includeStubs.value && c.stub) return false
    if (selectedCycle.value !== 'all' && c.cycle !== selectedCycle.value) return false
    if (selectedSet.value !== 'all' && c.set !== selectedSet.value) return false
    if (selectedType.value !== 'all' && c.type !== selectedType.value) return false
    if (selectedRace.value !== 'all' && c.race !== selectedRace.value) return false
    if (selectedCost.value !== 'all' && c.cost !== selectedCost.value) return false
    if (q) {
      const hay = `${c.name} ${c.traits ?? ''} ${c.text ?? ''}`.toLowerCase()
      if (!hay.includes(q)) return false
    }
    return true
  })
})

// When the active cycle changes, reset the set selection if it no
// longer belongs to that cycle. (Sidebar may briefly hold a stale
// `selectedSet`.)
watch(selectedCycle, (cycle) => {
  if (cycle !== 'all') {
    const sets = setsByCycle.value[cycle] ?? []
    if (selectedSet.value !== 'all' && !sets.includes(selectedSet.value)) {
      selectedSet.value = 'all'
    }
  }
})

function clearFilters() {
  search.value = ''
  selectedCycle.value = 'all'
  selectedSet.value = 'all'
  selectedType.value = 'all'
  selectedRace.value = 'all'
  selectedCost.value = 'all'
  includeStubs.value = true
}

function pickCycle(cycle: string | 'all') {
  selectedCycle.value = cycle
  selectedSet.value = 'all'
  sidebarOpen.value = false
}

function pickSet(payload: { cycle: string; set: string }) {
  selectedCycle.value = payload.cycle
  selectedSet.value = payload.set
  sidebarOpen.value = false
}

function openCard(card: Card) {
  focusedCard.value = card
  document.body.style.overflow = 'hidden'
}

function closeCard() {
  focusedCard.value = null
  document.body.style.overflow = ''
}

function unique<T>(xs: T[]): T[] {
  return Array.from(new Set(xs))
}
</script>

<template>
  <div class="cards-page">
    <CardsSidebar
      :open="sidebarOpen"
      :cycles="cycles"
      :sets-by-cycle="setsByCycle"
      :cycle-counts="cycleCounts"
      :set-counts="setCounts"
      :total-cards="allCards.length"
      :selected-cycle="selectedCycle"
      :selected-set="selectedSet"
      @select-cycle="pickCycle"
      @select-set="pickSet"
    />

    <div class="main-wrapper">
    <div class="main">
      <section class="filters" :aria-label="t('cards.filters.aria_label')">
        <div class="filter-row">
          <button
            type="button"
            class="ghost sidebar-toggle"
            aria-controls="cards-sidebar"
            :aria-expanded="sidebarOpen"
            @click="sidebarOpen = !sidebarOpen"
          >
            {{ t('cards.sidebar.toggle') }}
          </button>
          <input
            v-model="search"
            class="search"
            type="search"
            :placeholder="t('cards.filters.search_placeholder')"
            :aria-label="t('cards.filters.search_aria')"
          />
        </div>
        <div class="filter-row pills">
          <label class="field">
            <span class="field-label">{{ t('cards.filters.type_label') }}</span>
            <select v-model="selectedType">
              <option value="all">{{ t('cards.filters.any_type') }}</option>
              <option v-for="ty in types" :key="ty" :value="ty">{{ ty }}</option>
            </select>
          </label>

          <label class="field">
            <span class="field-label">{{ t('cards.filters.race_label') }}</span>
            <select v-model="selectedRace">
              <option value="all">{{ t('cards.filters.any_race') }}</option>
              <option v-for="r in races" :key="r" :value="r">{{ r }}</option>
            </select>
          </label>

          <label class="field">
            <span class="field-label">{{ t('cards.filters.cost_label') }}</span>
            <select v-model="selectedCost">
              <option value="all">{{ t('cards.filters.any_cost') }}</option>
              <option v-for="c in costs" :key="c" :value="c">{{ c }}</option>
            </select>
          </label>

          <label class="toggle">
            <input v-model="includeStubs" type="checkbox" />
            <span>{{ t('cards.filters.include_stubs') }}</span>
          </label>

          <button type="button" class="ghost" @click="clearFilters">{{ t('cards.filters.clear') }}</button>
        </div>

        <div v-if="!loading" class="active-summary">
          <span class="count" v-html="t('cards.summary.total', { total: `<strong>${filtered.length}</strong>` })" />
          <span v-if="selectedCycle !== 'all'" class="chip">
            {{ selectedCycle }}
            <button type="button" :aria-label="t('cards.filters.clear_cycle')" @click="pickCycle('all')">×</button>
          </span>
          <span v-if="selectedSet !== 'all'" class="chip">
            {{ selectedSet }}
            <button type="button" :aria-label="t('cards.filters.clear_set')" @click="selectedSet = 'all'">×</button>
          </span>
        </div>
      </section>

      <div v-if="loading" class="status">{{ t('cards.status.loading') }}</div>
      <div v-else-if="loadError" class="status error">
        {{ t('cards.status.load_error', { message: loadError }) }}
      </div>
      <div v-else-if="filtered.length === 0" class="status">
        {{ t('cards.status.no_results') }}
      </div>

      <ul v-else class="grid" role="list">
        <li v-for="card in filtered" :key="card.id" class="card-tile">
          <button
            class="card-button"
            :class="[raceClass(card.race), { stub: card.stub }]"
            type="button"
            :aria-label="card.name"
            @click="openCard(card)"
          >
            <div class="img-wrap">
              <img
                v-if="cardImageUrl(card)"
                :src="cardImageUrl(card)!"
                :alt="card.name"
                loading="lazy"
                decoding="async"
              />
              <div v-else class="no-img">{{ t('cards.grid.no_image') }}</div>
              <span v-if="card.stub" class="stub-badge" :title="t('cards.grid.stub_title')">
                {{ t('cards.grid.stub_badge') }}
              </span>
            </div>
            <div class="meta">
              <span class="card-name">{{ card.name }}</span>
              <span class="card-sub">
                <span v-if="card.type">{{ card.type }}</span>
                <span v-if="card.set" class="muted"> · {{ card.set }}</span>
              </span>
            </div>
          </button>
        </li>
      </ul>
    </div>
    </div>

    <!-- Mobile sidebar backdrop -->
    <button
      v-if="sidebarOpen"
      class="sidebar-scrim"
      type="button"
      :aria-label="t('cards.sidebar.close_aria')"
      @click="sidebarOpen = false"
    />

    <CardDetailModal :card="focusedCard" @close="closeCard" />
  </div>
</template>

<style scoped>
.cards-page {
  display: grid;
  grid-template-columns: minmax(240px, 280px) minmax(0, 1fr);
  gap: 0;
  min-height: calc(100dvh - 60px);
  background: var(--bg);
}

.sidebar-scrim {
  display: none;
}

.sidebar-toggle {
  display: none;
}

.main-wrapper {
  container-type: inline-size;
  min-width: 0;
}

.main {
  padding: 2.5rem 1.5rem 5rem;
  width: 90cqi;
  margin: 0 auto;
}

.filters {
  margin-bottom: 1.4rem;
  padding: 1rem 1.2rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
}

.filter-row {
  display: flex;
  flex-wrap: wrap;
  gap: 0.6rem;
  align-items: center;
}

.filter-row + .filter-row {
  margin-top: 0.7rem;
}

.search {
  flex: 1 1 280px;
  min-height: var(--tap-target);
  padding: 0.55rem 0.9rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 1rem;
}

.search:focus-visible {
  outline: 2px solid var(--accent-strong);
  outline-offset: 1px;
  border-color: var(--accent-strong);
}

.field {
  display: flex;
  flex-direction: column;
  gap: 0.18rem;
  min-width: 130px;
}

.field-label {
  font-size: 0.7rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.field select {
  min-height: var(--tap-target);
  padding: 0.4rem 0.6rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.95rem;
}

.toggle {
  display: inline-flex;
  align-items: center;
  gap: 0.45rem;
  min-height: var(--tap-target);
  padding: 0 0.65rem;
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  background: var(--bg);
  color: var(--fg);
  font-size: 0.9rem;
  cursor: pointer;
  user-select: none;
}

.toggle input {
  width: 16px;
  height: 16px;
  accent-color: var(--accent);
}

button.ghost {
  min-height: var(--tap-target);
  padding: 0 1rem;
  border-radius: var(--radius-md);
  font-size: 0.92rem;
  cursor: pointer;
  border: 1px solid var(--border);
  background: var(--bg);
  color: var(--fg);
}

button.ghost:hover {
  background: var(--bg-elev-2);
}

.active-summary {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.5rem;
  margin-top: 0.85rem;
  color: var(--fg-dim);
  font-size: 0.92rem;
}

.chip {
  display: inline-flex;
  align-items: center;
  gap: 0.3rem;
  padding: 0.2rem 0.55rem;
  background: var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-pill);
  font-size: 0.8rem;
}

.chip button {
  background: transparent;
  border: none;
  color: var(--on-accent);
  cursor: pointer;
  padding: 0;
  margin: 0;
  font-size: 1rem;
  line-height: 1;
}

.muted {
  color: var(--fg-faint);
}

.status {
  padding: 3rem 1rem;
  text-align: center;
  color: var(--fg-dim);
}

.status.error {
  color: var(--accent-strong);
}

.grid {
  list-style: none;
  margin: 0;
  padding: 0;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(190px, 1fr));
  gap: 1rem;
}

.card-tile {
  display: contents;
}

.card-button {
  width: 100%;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  padding: 0.55rem;
  cursor: pointer;
  text-align: left;
  color: var(--fg);
  transition: transform var(--transition-fast), border-color var(--transition-fast);
}

.card-button:hover,
.card-button:focus-visible {
  border-color: var(--accent-strong);
  transform: translateY(-2px);
  outline: none;
}

.img-wrap {
  position: relative;
  aspect-ratio: 5 / 7;
  background: var(--bg);
  border-radius: var(--card-radius);
  box-shadow: var(--shadow-card);
  overflow: hidden;
  display: grid;
  place-items: center;
}

.img-wrap img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
}

.no-img {
  font-size: 0.78rem;
  color: var(--fg-faint);
  padding: 0.6rem;
  text-align: center;
}

.stub-badge {
  position: absolute;
  top: 6px;
  right: 6px;
  padding: 0.1rem 0.4rem;
  background: var(--overlay-medium);
  color: var(--on-accent);
  border-radius: var(--radius-xs);
  font-size: 0.65rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.meta {
  display: flex;
  flex-direction: column;
  gap: 0.1rem;
  min-height: 2.5em;
}

.card-name {
  font-size: 0.95rem;
  font-weight: 600;
  color: var(--fg);
  line-height: 1.25;
}

.card-sub {
  font-size: 0.78rem;
  color: var(--fg-dim);
  line-height: 1.3;
}

.card-button.race-empire { box-shadow: inset 3px 0 0 var(--race-empire); }
.card-button.race-dwarf { box-shadow: inset 3px 0 0 var(--race-dwarf); }
.card-button.race-high-elf { box-shadow: inset 3px 0 0 var(--race-high-elf); }
.card-button.race-chaos { box-shadow: inset 3px 0 0 var(--race-chaos); }
.card-button.race-orc { box-shadow: inset 3px 0 0 var(--race-orc); }
.card-button.race-dark-elf { box-shadow: inset 3px 0 0 var(--race-dark-elf); }
.card-button.race-neutral { box-shadow: inset 3px 0 0 var(--race-neutral); }

@media (max-width: 900px) {
  .cards-page {
    grid-template-columns: 1fr;
  }

  .sidebar-scrim {
    display: block;
    position: fixed;
    inset: 0;
    background: var(--overlay-medium);
    border: none;
    cursor: pointer;
    z-index: var(--z-drawer-scrim);
  }

  .sidebar-toggle {
    display: inline-flex;
    align-items: center;
  }

  .main {
    padding: 1.6rem 1rem 4rem;
  }
}

@media (max-width: 480px) {
  .grid {
    grid-template-columns: repeat(auto-fill, minmax(140px, 1fr));
    gap: 0.7rem;
  }

  .card-name {
    font-size: 0.88rem;
  }

  .field {
    min-width: 120px;
  }
}
</style>
