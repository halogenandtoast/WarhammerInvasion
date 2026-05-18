<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Card, CardStat, CardType, Race } from '../types/card'
import { ApiError } from '../api/client'
import { deleteDeck, getDeck, updateDeck, type DeckRecord } from '../api/decks'
import {
  hasFactionCards,
  isCardAllowedInDeck,
  isCardAllowedInFaction,
  summarize,
  MAX_COPIES_PER_TITLE,
  factionOfCapital,
  type Capital,
} from '../lib/deck'
import { useCardCatalog } from '../composables/useCardCatalog'
import { navigate } from '../router'
import CapitalPicker from '../components/CapitalPicker.vue'
import CardTile from '../components/CardTile.vue'
import DeckBuilderPanel from '../components/DeckBuilderPanel.vue'

const props = defineProps<{ deckId: string }>()
const { t } = useI18n({ useScope: 'global' })

// ----- data ----------------------------------------------------------------

const catalog = useCardCatalog()

const deck = ref<DeckRecord | null>(null)
const counts = ref<Record<string, number>>({})
const editingName = ref('')
const editingCapital = ref<Capital | null>(null)
const isChangingCapital = ref(false)

const loading = ref(true)
const loadError = ref<string | null>(null)
const saveState = ref<'clean' | 'pending' | 'saving' | 'saved' | 'error'>('clean')
const saveError = ref<string | null>(null)
let saveTimer: number | null = null

onMounted(async () => {
  try {
    const [, deckRes] = await Promise.all([catalog.ensureLoaded(), getDeck(props.deckId)])
    deck.value = deckRes
    counts.value = { ...deckRes.cards }
    editingName.value = deckRes.name
    editingCapital.value = deckRes.capital
  } catch (e) {
    loadError.value = e instanceof ApiError ? e.code : e instanceof Error ? e.message : 'load_failed'
  } finally {
    loading.value = false
  }
})

onUnmounted(() => {
  if (saveTimer != null) {
    clearTimeout(saveTimer)
    if (saveState.value === 'pending') {
      void persist()
    }
  }
})

// ----- save ----------------------------------------------------------------

function scheduleSave() {
  saveState.value = 'pending'
  if (saveTimer != null) clearTimeout(saveTimer)
  saveTimer = window.setTimeout(() => { void persist() }, 800)
}

async function persist() {
  if (!deck.value) return
  saveState.value = 'saving'
  try {
    const updated = await updateDeck(deck.value.id, {
      name: editingName.value.trim() || t('deck_edit.untitled'),
      capital: editingCapital.value,
      cards: counts.value,
    })
    deck.value = updated
    saveState.value = 'saved'
    saveError.value = null
  } catch (e) {
    saveState.value = 'error'
    saveError.value = e instanceof ApiError ? e.code : 'save_failed'
  }
}

// ----- deck mutation -------------------------------------------------------

function add(card: Card) {
  if (editingCapital.value === null) return
  if (!isCardAllowedInDeck(card, editingCapital.value)) return
  const current = counts.value[card.id] ?? 0
  if (current >= MAX_COPIES_PER_TITLE) return
  counts.value = { ...counts.value, [card.id]: current + 1 }
  scheduleSave()
}

function remove(card: Card | { id: string }) {
  const current = counts.value[card.id] ?? 0
  if (current <= 0) return
  const next = { ...counts.value }
  if (current === 1) delete next[card.id]
  else next[card.id] = current - 1
  counts.value = next
  scheduleSave()
}

function pickCapital(c: Capital) {
  editingCapital.value = c
  isChangingCapital.value = false
  // The watcher on editingCapital below catches this via scheduleSave().
}

function renameDeck(event: Event) {
  editingName.value = (event.target as HTMLInputElement).value
  scheduleSave()
}

async function destroyDeck() {
  if (!deck.value) return
  if (!confirm(t('deck_edit.confirm_delete', { name: deck.value.name }))) return
  try {
    await deleteDeck(deck.value.id)
    navigate('#/decks')
  } catch (e) {
    saveError.value = e instanceof ApiError ? e.code : 'delete_failed'
  }
}

// ----- filters -------------------------------------------------------------

const search = ref('')
const selectedType = ref<CardType | 'all'>('all')
const selectedRace = ref<Race | 'all'>('all')
const selectedCost = ref<CardStat | 'all'>('all')
const showOtherFaction = ref(false)
const showDeckOnMobile = ref(false)

const editingFaction = computed(() =>
  editingCapital.value === null ? null : factionOfCapital(editingCapital.value),
)

const types = computed(() =>
  Array.from(new Set(catalog.cards.value.map((c) => c.type).filter((t): t is CardType => Boolean(t)))),
)
const races = computed(() =>
  Array.from(new Set(catalog.cards.value.map((c) => c.race).filter((r): r is Race => Boolean(r)))),
)
const costs = computed(() => {
  const seen = new Set<number | 'X'>()
  for (const c of catalog.cards.value) {
    if (c.cost === null) continue
    if (typeof c.cost === 'number' && c.cost < 0) continue
    seen.add(c.cost)
  }
  return Array.from(seen).sort((a, b) => {
    if (typeof a === 'number' && typeof b === 'number') return a - b
    if (typeof a === 'number') return -1
    if (typeof b === 'number') return 1
    return a.localeCompare(b)
  })
})

const filtered = computed(() => {
  const q = search.value.trim().toLowerCase()
  return catalog.cards.value.filter((c) => {
    if (c.stub) return false
    if (!showOtherFaction.value && !isCardAllowedInFaction(c, editingFaction.value)) return false
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

const addedFiltered = computed(() =>
  filtered.value
    .filter((c) => (counts.value[c.id] ?? 0) > 0)
    .sort((a, b) => a.name.localeCompare(b.name)),
)

const availableFiltered = computed(() =>
  filtered.value.filter((c) => (counts.value[c.id] ?? 0) === 0),
)

// ----- derived summary -----------------------------------------------------

const summary = computed(() =>
  summarize({ cards: counts.value, capital: editingCapital.value }, catalog.cardIndex.value),
)
const canChangeCapital = computed(
  () => !hasFactionCards({ cards: counts.value }, catalog.cardIndex.value),
)

function saveLabel(): string {
  switch (saveState.value) {
    case 'saving': return t('deck_edit.save.saving')
    case 'saved': return t('deck_edit.save.saved')
    case 'pending': return t('deck_edit.save.pending')
    case 'error': return t('deck_edit.save.error')
    default: return ''
  }
}

watch(editingCapital, () => scheduleSave())
</script>

<template>
  <main class="edit-page" v-if="!loadError">
    <div v-if="loading" class="status">{{ t('deck_edit.loading') }}</div>

    <template v-else-if="deck">
      <header class="page-head">
        <button class="back" type="button" @click="navigate(`#/decks/${deck.id}`)">
          ← {{ t('deck_edit.back') }}
        </button>
        <div class="name-wrap">
          <input
            class="name"
            type="text"
            :value="editingName"
            maxlength="80"
            :aria-label="t('deck_edit.name_aria')"
            @input="renameDeck"
          />
          <span class="save-state" :class="`state-${saveState}`" v-if="saveLabel()">
            {{ saveLabel() }}
          </span>
        </div>
        <div class="actions">
          <button class="ghost danger" type="button" @click="destroyDeck">
            {{ t('deck_edit.delete') }}
          </button>
        </div>
      </header>

      <p v-if="saveError" class="error">{{ saveError }}</p>

      <CapitalPicker
        v-if="editingCapital === null || isChangingCapital"
        :current="editingCapital"
        :changing="isChangingCapital"
        @pick="pickCapital"
        @cancel="isChangingCapital = false"
      />

      <div v-else class="layout">
        <!-- Card browser -->
        <section class="browser" :aria-label="t('deck_edit.browser_label')">
          <div class="filters">
            <input
              v-model="search"
              class="search"
              type="search"
              :placeholder="t('deck_edit.search_placeholder')"
              :aria-label="t('deck_edit.search_aria')"
            />
            <div class="filter-row">
              <select v-model="selectedType" :aria-label="t('deck_edit.type_label')">
                <option value="all">{{ t('deck_edit.any_type') }}</option>
                <option v-for="ty in types" :key="ty" :value="ty">{{ ty }}</option>
              </select>
              <select v-model="selectedRace" :aria-label="t('deck_edit.race_label')">
                <option value="all">{{ t('deck_edit.any_race') }}</option>
                <option v-for="r in races" :key="r" :value="r">{{ r }}</option>
              </select>
              <select v-model="selectedCost" :aria-label="t('deck_edit.cost_label')">
                <option value="all">{{ t('deck_edit.any_cost') }}</option>
                <option v-for="c in costs" :key="c" :value="c">{{ c }}</option>
              </select>
              <label class="toggle">
                <input v-model="showOtherFaction" type="checkbox" />
                <span>{{ t('deck_edit.show_other_faction') }}</span>
              </label>
            </div>
            <p class="result-count">
              {{ t('deck_edit.result_count', { count: filtered.length }) }}
            </p>
          </div>

          <section v-if="addedFiltered.length > 0" class="card-section added-section">
            <h3 class="section-heading">
              {{ t('deck_edit.in_deck_heading', { count: addedFiltered.length }) }}
            </h3>
            <ul class="grid" role="list">
              <CardTile
                v-for="card in addedFiltered"
                :key="`a-${card.id}`"
                :card="card"
                :count="counts[card.id] ?? 0"
                :can-add="isCardAllowedInDeck(card, editingCapital)"
                @add="add"
                @remove="remove"
              />
            </ul>
          </section>

          <section class="card-section available-section">
            <h3 v-if="addedFiltered.length > 0" class="section-heading">
              {{ t('deck_edit.available_heading', { count: availableFiltered.length }) }}
            </h3>
            <ul class="grid" role="list">
              <CardTile
                v-for="card in availableFiltered"
                :key="card.id"
                :card="card"
                :count="0"
                :can-add="isCardAllowedInDeck(card, editingCapital)"
                @add="add"
                @remove="remove"
              />
            </ul>
          </section>
        </section>

        <DeckBuilderPanel
          :summary="summary"
          :capital="editingCapital"
          :can-change-capital="canChangeCapital"
          :counts="counts"
          :open="showDeckOnMobile"
          @change-capital="isChangingCapital = true"
          @add="add"
          @remove="remove"
        />
      </div>

      <button
        class="mobile-toggle"
        type="button"
        :aria-expanded="showDeckOnMobile"
        @click="showDeckOnMobile = !showDeckOnMobile"
      >
        {{
          showDeckOnMobile
            ? t('deck_edit.hide_deck')
            : t('deck_edit.show_deck', { count: summary.stats.total })
        }}
      </button>
    </template>
  </main>

  <main v-else class="edit-page">
    <p class="error">{{ loadError }}</p>
    <button class="ghost" type="button" @click="navigate('#/decks')">{{ t('deck_edit.back') }}</button>
  </main>
</template>

<style scoped>
.edit-page {
  background: var(--bg);
  min-height: calc(100dvh - 60px);
}

.status {
  padding: 3rem;
  text-align: center;
  color: var(--fg-dim);
}

.error {
  margin: 1rem 1.5rem;
  padding: 0.6rem 0.9rem;
  background: var(--bg-elev);
  border-left: 3px solid var(--accent-strong);
  border-radius: var(--radius-sm);
  color: var(--fg);
}

.page-head {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 0.9rem 1.5rem;
  background: var(--bg-elev);
  border-bottom: 1px solid var(--border);
  position: sticky;
  top: 0;
  z-index: 5;
}

.back {
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  padding: 0.4rem 0.8rem;
  border-radius: var(--radius-md);
  cursor: pointer;
  min-height: var(--tap-target);
}

.back:hover {
  color: var(--fg);
  border-color: var(--fg-dim);
}

.name-wrap {
  flex: 1;
  display: flex;
  align-items: center;
  gap: 0.6rem;
}

.name {
  flex: 1;
  min-width: 0;
  background: transparent;
  border: 1px solid transparent;
  border-radius: var(--radius-md);
  color: var(--fg);
  font-size: 1.1rem;
  font-weight: 600;
  padding: 0.45rem 0.6rem;
  min-height: var(--tap-target);
}

.name:hover,
.name:focus-visible {
  border-color: var(--border);
  outline: none;
}

.save-state {
  font-size: 0.78rem;
  color: var(--fg-faint);
  white-space: nowrap;
}

.save-state.state-saving,
.save-state.state-pending {
  color: var(--fg-dim);
}

.save-state.state-error {
  color: var(--accent-strong);
}

.actions {
  display: flex;
  gap: 0.5rem;
}

.layout {
  display: grid;
  grid-template-columns: minmax(0, 1fr) minmax(300px, 360px);
  gap: 0;
  align-items: start;
}

.browser {
  padding: 1rem 1.25rem 4rem;
  min-width: 0;
}

.filters {
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
  padding: 0.9rem 1rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  margin-bottom: 1rem;
  position: sticky;
  top: 73px;
  z-index: 4;
}

.search {
  min-height: var(--tap-target);
  padding: 0.5rem 0.85rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.95rem;
}

.search:focus-visible {
  outline: 2px solid var(--accent-strong);
  border-color: var(--accent-strong);
}

.filter-row {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.filter-row select {
  min-height: var(--tap-target);
  padding: 0.4rem 0.6rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.9rem;
}

.toggle {
  display: inline-flex;
  align-items: center;
  gap: 0.4rem;
  padding: 0.25rem 0.6rem;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  color: var(--fg-dim);
  font-size: 0.85rem;
  cursor: pointer;
  user-select: none;
  min-height: var(--tap-target);
}

.toggle input {
  width: 16px;
  height: 16px;
  accent-color: var(--accent);
}

.result-count {
  margin: 0;
  font-size: 0.82rem;
  color: var(--fg-faint);
}

.card-section + .card-section {
  margin-top: 1.25rem;
}

.section-heading {
  margin: 0 0 0.55rem;
  font-size: 0.72rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.added-section .section-heading {
  color: var(--accent-strong);
}

.grid {
  list-style: none;
  margin: 0;
  padding: 0;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
  gap: 0.7rem;
}

.mobile-toggle {
  display: none;
}

@media (max-width: 900px) {
  .layout {
    grid-template-columns: 1fr;
  }

  .mobile-toggle {
    display: block;
    position: fixed;
    bottom: 1rem;
    right: 1rem;
    background: var(--accent);
    color: var(--on-accent);
    border: none;
    border-radius: var(--radius-pill);
    padding: 0.7rem 1.2rem;
    font-size: 0.9rem;
    box-shadow: var(--shadow-drawer);
    cursor: pointer;
    z-index: 25;
  }
}

@media (max-width: 480px) {
  .grid {
    grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
    gap: 0.5rem;
  }

  .name {
    font-size: 0.95rem;
  }
}
</style>
