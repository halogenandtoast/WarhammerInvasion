<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Card, CardType, Race } from '../types/card'

const { t } = useI18n({ useScope: 'global' })

const allCards = ref<Card[]>([])
const loading = ref(true)
const loadError = ref<string | null>(null)

const search = ref('')
const selectedCycle = ref<string | 'all'>('all')
const selectedSet = ref<string | 'all'>('all')
const selectedType = ref<CardType | 'all'>('all')
const selectedRace = ref<Race | 'all'>('all')
const selectedCost = ref<string | 'all'>('all')
const includeStubs = ref(true)
const sidebarOpen = ref(false)

const focusedCard = ref<Card | null>(null)

onMounted(async () => {
  window.addEventListener('keydown', onKey)
  try {
    const res = await fetch('/cards.json')
    if (!res.ok) throw new Error(`status ${res.status}`)
    allCards.value = (await res.json()) as Card[]
  } catch (err) {
    loadError.value = (err as Error).message
  } finally {
    loading.value = false
  }
})

onUnmounted(() => {
  window.removeEventListener('keydown', onKey)
  document.body.style.overflow = ''
})

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
  const all = unique(allCards.value.map((c) => c.cost).filter(Boolean) as string[])
  return all.sort((a, b) => {
    const an = Number(a)
    const bn = Number(b)
    if (Number.isNaN(an) || Number.isNaN(bn)) return a.localeCompare(b)
    return an - bn
  })
})

const cycleCounts = computed(() => {
  const counts: Record<string, number> = {}
  for (const c of allCards.value) {
    if (!includeStubs.value && c.stub) continue
    counts[c.cycle] = (counts[c.cycle] ?? 0) + 1
  }
  return counts
})
const setCounts = computed(() => {
  const counts: Record<string, number> = {}
  for (const c of allCards.value) {
    if (!includeStubs.value && c.stub) continue
    if (selectedCycle.value !== 'all' && c.cycle !== selectedCycle.value) continue
    counts[c.set] = (counts[c.set] ?? 0) + 1
  }
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

function selectCycle(cycle: string | 'all') {
  selectedCycle.value = cycle
  selectedSet.value = 'all'
  sidebarOpen.value = false
}

function selectSet(cycle: string, set: string) {
  selectedCycle.value = cycle
  selectedSet.value = set
  sidebarOpen.value = false
}

function isDeluxe(cycle: string): boolean {
  // A cycle that contains a single set renders as a leaf with no nested list.
  return (setsByCycle.value[cycle] ?? []).length <= 1
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

function imageUrl(c: Card): string | null {
  return c.image ? `/cards/${c.image}` : null
}

function raceClass(race: Race | null): string {
  if (!race) return ''
  return `race-${race.toLowerCase().replace(/\s+/g, '-')}`
}

function renderText(text: string | null): string {
  if (!text) return ''
  return text.replace(/\[([^\]]+)\]/g, (_, sym: string) => {
    const cls = `icon icon-${sym.toLowerCase().replace(/\s+/g, '-')}`
    return `<span class="${cls}" aria-label="${sym}">${sym[0]}</span>`
  })
}

function onBackdropClick(e: MouseEvent) {
  if (e.target === e.currentTarget) closeCard()
}

function onKey(e: KeyboardEvent) {
  if (e.key === 'Escape' && focusedCard.value) closeCard()
}
</script>

<template>
  <div class="cards-page">
    <aside
      class="sidebar"
      :class="{ open: sidebarOpen }"
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
              :class="{ active: selectedCycle === 'all' && selectedSet === 'all' }"
              @click="selectCycle('all')"
            >
              <span class="item-label">{{ t('cards.sidebar.all_cycles') }}</span>
              <span class="count-pill">{{ allCards.length }}</span>
            </button>
          </li>

          <template v-for="c in cycles" :key="c">
            <!-- Deluxe / single-set cycle: render as a leaf with no nested list. -->
            <li v-if="isDeluxe(c)" class="cycle-row">
              <button
                type="button"
                class="sidebar-item cycle-name"
                :class="{ active: selectedCycle === c }"
                @click="selectSet(c, c)"
              >
                <span class="item-label">{{ c }}</span>
                <span class="count-pill">{{ cycleCounts[c] ?? 0 }}</span>
              </button>
            </li>

            <!-- Cycle with multiple distinct sets: parent + ordered list. -->
            <li v-else class="cycle-row">
              <button
                type="button"
                class="sidebar-item cycle-name"
                :class="{ active: selectedCycle === c && selectedSet === 'all' }"
                @click="selectCycle(c)"
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
                    @click="selectSet(c, s)"
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

        <div class="active-summary" v-if="!loading">
          <span class="count" v-html="t('cards.summary.total', { total: `<strong>${filtered.length}</strong>` })" />
          <span v-if="selectedCycle !== 'all'" class="chip">
            {{ selectedCycle }}
            <button type="button" @click="selectCycle('all')" :aria-label="t('cards.filters.clear_cycle')">
              ×
            </button>
          </span>
          <span v-if="selectedSet !== 'all'" class="chip">
            {{ selectedSet }}
            <button type="button" @click="selectedSet = 'all'" :aria-label="t('cards.filters.clear_set')">×</button>
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
            @click="openCard(card)"
            :aria-label="card.name"
          >
            <div class="img-wrap">
              <img
                v-if="imageUrl(card)"
                :src="imageUrl(card)!"
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
    ></button>

    <!-- Detail modal -->
    <div
      v-if="focusedCard"
      class="modal-backdrop"
      role="dialog"
      aria-modal="true"
      :aria-label="focusedCard.name"
      @click="onBackdropClick"
    >
      <div class="modal" :class="raceClass(focusedCard.race)">
        <button class="close" type="button" @click="closeCard" :aria-label="t('cards.modal.aria_close')">
          ×
        </button>
        <div class="modal-image">
          <img
            v-if="imageUrl(focusedCard)"
            :src="imageUrl(focusedCard)!"
            :alt="focusedCard.name"
          />
          <div v-else class="no-img big">{{ t('cards.modal.no_image') }}</div>
        </div>
        <div class="modal-detail">
          <header>
            <p class="modal-eyebrow">
              {{ focusedCard.set }}
              <span v-if="focusedCard.number" class="muted">
                · #{{ String(focusedCard.number).padStart(3, '0') }}
              </span>
            </p>
            <h2>{{ focusedCard.name }}</h2>
          </header>

          <div class="badges">
            <span v-if="focusedCard.race" class="badge">{{ focusedCard.race }}</span>
            <span v-if="focusedCard.type" class="badge">{{ focusedCard.type }}</span>
            <span v-if="focusedCard.cost !== null" class="badge cost">
              {{ focusedCard.cost }}{{ t('cards.modal.cost_suffix') }}
            </span>
            <span v-if="focusedCard.loyalty !== null" class="badge">
              {{ t('cards.modal.loyalty_prefix') }}{{ focusedCard.loyalty }}
            </span>
            <span v-if="focusedCard.power !== null" class="badge">
              {{ t('cards.modal.power_prefix') }}{{ focusedCard.power }}
            </span>
            <span v-if="focusedCard.health !== null" class="badge">
              {{ t('cards.modal.health_prefix') }}{{ focusedCard.health }}
            </span>
            <span v-if="focusedCard.quantity" class="badge">
              {{ t('cards.modal.quantity_prefix') }}{{ focusedCard.quantity }}
            </span>
          </div>

          <p v-if="focusedCard.traits" class="traits">
            <em>{{ focusedCard.traits }}</em>
          </p>

          <p
            v-if="focusedCard.text"
            class="card-text"
            v-html="renderText(focusedCard.text)"
          ></p>

          <p v-if="focusedCard.stub" class="stub-note">
            {{ t('cards.modal.stub_note') }}
          </p>

          <footer class="modal-footer">
            <span v-if="focusedCard.illustrator" class="muted">
              {{ t('cards.modal.illustrator', { name: focusedCard.illustrator }) }}
            </span>
            <span class="muted">{{ focusedCard.cycle }}{{ t('cards.modal.cycle_suffix') }}</span>
          </footer>
        </div>
      </div>
    </div>
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

/* ---------- Sidebar ---------- */
.sidebar {
  background: var(--bg-elev);
  border-right: 1px solid var(--border);
  position: sticky;
  top: 60px;
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

.sidebar-scrim {
  display: none;
}

.sidebar-toggle {
  display: none;
}

/* ---------- Main column ---------- */
.main-wrapper {
  container-type: inline-size;
  min-width: 0;
}

.main {
  padding: 2.5rem 1.5rem 5rem;
  width: 90cqi;
  margin: 0 auto;
}

.cards-header {
  margin-bottom: 1.6rem;
}

.eyebrow {
  margin: 0;
  font-size: 0.78rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--accent-strong);
}

.cards-header h1 {
  margin: 0.5rem 0 0.65rem;
  font-size: clamp(1.7rem, 3.6vw, 2.4rem);
  color: var(--fg);
}

.lead {
  margin: 0;
  color: var(--fg-dim);
  max-width: 60ch;
}

/* ---------- Filters ---------- */
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

button.ghost,
button.primary {
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

button.primary {
  background: var(--accent);
  color: var(--on-accent);
  border-color: var(--accent);
}

button.primary:hover {
  background: var(--accent-strong);
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

/* ---------- Grid ---------- */
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
  border-radius: var(--radius-sm);
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

/* ---------- Modal ---------- */
.modal-backdrop {
  position: fixed;
  inset: 0;
  background: var(--overlay-strong);
  display: grid;
  place-items: center;
  padding: 1rem;
  z-index: var(--z-modal);
  overflow-y: auto;
}

.modal {
  position: relative;
  width: 100%;
  max-width: 880px;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-xl);
  display: grid;
  grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
  gap: 0;
  overflow: hidden;
  max-height: calc(100dvh - 2rem);
}

.modal-image {
  background: var(--bg);
  display: grid;
  place-items: center;
  padding: 1rem;
  overflow: hidden;
}

.modal-image img {
  width: 100%;
  max-height: 70vh;
  object-fit: contain;
  border-radius: var(--radius-md);
}

.no-img.big {
  height: 60vh;
  display: grid;
  place-items: center;
  width: 100%;
}

.modal-detail {
  padding: 1.5rem 1.6rem 1.5rem;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 0.85rem;
}

.modal-detail header h2 {
  margin: 0.2rem 0 0;
  font-size: 1.5rem;
  color: var(--fg);
}

.modal-eyebrow {
  margin: 0;
  font-size: 0.72rem;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: var(--accent-strong);
}

.badges {
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
}

.badge {
  display: inline-flex;
  align-items: center;
  padding: 0.18rem 0.55rem;
  background: var(--bg-elev-2);
  border: 1px solid var(--border);
  color: var(--fg);
  border-radius: var(--radius-pill);
  font-size: 0.78rem;
  letter-spacing: 0.02em;
}

.badge.cost {
  background: var(--accent);
  border-color: var(--accent);
  color: var(--on-accent);
}

.traits {
  margin: 0;
  color: var(--fg-dim);
  font-size: 0.95rem;
}

.card-text {
  margin: 0;
  color: var(--fg);
  white-space: pre-line;
  line-height: 1.5;
}

.card-text :deep(.icon) {
  display: inline-grid;
  place-items: center;
  width: 1.3em;
  height: 1.3em;
  margin: 0 0.06em;
  font-size: 0.7em;
  font-weight: 700;
  color: var(--on-accent);
  background: var(--accent);
  border-radius: var(--radius-sm);
  vertical-align: -0.18em;
}

.stub-note {
  margin: 0;
  padding: 0.55rem 0.7rem;
  background: var(--bg);
  border-left: 3px solid var(--accent);
  border-radius: var(--radius-sm);
  color: var(--fg-dim);
  font-size: 0.88rem;
}

.modal-footer {
  margin-top: auto;
  padding-top: 0.6rem;
  border-top: 1px solid var(--border);
  display: flex;
  flex-wrap: wrap;
  gap: 0.6rem;
  justify-content: space-between;
  font-size: 0.84rem;
}

.close {
  position: absolute;
  top: 0.4rem;
  right: 0.5rem;
  width: 36px;
  height: 36px;
  display: grid;
  place-items: center;
  background: var(--overlay-soft);
  color: var(--on-accent);
  border: none;
  border-radius: var(--radius-circle);
  cursor: pointer;
  font-size: 1.4rem;
  line-height: 1;
  z-index: 1;
}

.close:hover {
  background: var(--overlay-strong);
}

/* ---------- Responsive ---------- */
@media (max-width: 900px) {
  .cards-page {
    grid-template-columns: 1fr;
  }

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

@media (max-width: 720px) {
  .modal {
    grid-template-columns: 1fr;
    max-height: calc(100dvh - 2rem);
  }

  .modal-image {
    padding: 0.8rem;
  }

  .modal-image img {
    max-height: 45vh;
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
