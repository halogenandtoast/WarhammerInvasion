<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Card, CardStat, CardType, Race } from '../types/card'
import { ApiError } from '../api/client'
import { deleteDeck, getDeck, updateDeck, type DeckRecord } from '../api/decks'
import {
  ALL_CAPITALS,
  factionOfCapital,
  hasFactionCards,
  isCardAllowedInDeck,
  isCardAllowedInFaction,
  raceOfCapital,
  summarize,
  MAX_COPIES_PER_TITLE,
  MIN_DECK_SIZE,
  MAX_DECK_SIZE,
  type Capital,
} from '../lib/deck'

const props = defineProps<{ deckId: string }>()
const emit = defineEmits<{ (e: 'navigate', target: string): void }>()
const { t } = useI18n({ useScope: 'global' })

const assetsBaseUrl = import.meta.env.VITE_ASSETS_BASE_URL ?? ''

// ----- data ----------------------------------------------------------------

const allCards = ref<Card[]>([])
const cardIndex = computed<Map<string, Card>>(() => {
  const m = new Map<string, Card>()
  for (const c of allCards.value) m.set(c.id, c)
  return m
})

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
    const [cardsRes, deckRes] = await Promise.all([
      fetch('/cards.json').then((r) => {
        if (!r.ok) throw new Error(`cards.json: ${r.status}`)
        return r.json() as Promise<Card[]>
      }),
      getDeck(props.deckId),
    ])
    allCards.value = cardsRes
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
  saveTimer = window.setTimeout(() => {
    void persist()
  }, 800)
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
  if (current === 1) {
    delete next[card.id]
  } else {
    next[card.id] = current - 1
  }
  counts.value = next
  scheduleSave()
}

function pickCapital(c: Capital) {
  editingCapital.value = c
  isChangingCapital.value = false
  scheduleSave()
}

function requestChangeCapital() {
  isChangingCapital.value = true
}

function cancelChangeCapital() {
  isChangingCapital.value = false
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
    emit('navigate', '#/decks')
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
  Array.from(new Set(allCards.value.map((c) => c.type).filter((t): t is CardType => Boolean(t)))),
)
const races = computed(() =>
  Array.from(new Set(allCards.value.map((c) => c.race).filter((r): r is Race => Boolean(r)))),
)
const costs = computed(() => {
  const seen = new Set<number | 'X'>()
  for (const c of allCards.value) {
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
  return allCards.value.filter((c) => {
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
  summarize({ cards: counts.value, capital: editingCapital.value }, cardIndex.value),
)
const canChangeCapital = computed(
  () => !hasFactionCards({ cards: counts.value }, cardIndex.value),
)

const maxCostBucket = computed(() =>
  Math.max(1, ...summary.value.stats.costCurve.map((b) => b.count)),
)
function curveWidth(count: number): string {
  return `${(count / maxCostBucket.value) * 100}%`
}

// ----- UI helpers ----------------------------------------------------------

function imageUrl(c: Card): string | null {
  return c.image ? `${assetsBaseUrl}/cards/${c.image}` : null
}

function raceClass(race: Race | null): string {
  if (!race) return ''
  return `race-${race.toLowerCase().replace(/\s+/g, '-')}`
}

function capitalLabel(c: Capital | null): string {
  if (c === null) return t('decks.capital.unset')
  return t(`decks.capital.${c}`)
}

function capitalRaceClass(c: Capital): string {
  return `race-${raceOfCapital(c).toLowerCase().replace(/\s+/g, '-')}`
}

function saveLabel(): string {
  switch (saveState.value) {
    case 'saving':
      return t('deck_edit.save.saving')
    case 'saved':
      return t('deck_edit.save.saved')
    case 'pending':
      return t('deck_edit.save.pending')
    case 'error':
      return t('deck_edit.save.error')
    default:
      return ''
  }
}

const validationTone = computed<'ok' | 'warn'>(() =>
  summary.value.issues.some((i) => i.severity === 'error') ? 'warn' : 'ok',
)

// Persist capital changes through the UI.
watch(editingCapital, () => scheduleSave())
</script>

<template>
  <main class="edit-page" v-if="!loadError">
    <div v-if="loading" class="status">{{ t('deck_edit.loading') }}</div>

    <template v-else-if="deck">
      <header class="page-head">
        <button class="back" type="button" @click="emit('navigate', `#/decks/${deck.id}`)">
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

      <section
        v-if="editingCapital === null || isChangingCapital"
        class="capital-gate"
        :aria-label="t('deck_edit.capital_picker.aria')"
      >
        <header class="gate-head">
          <h2>{{ t('deck_edit.capital_picker.heading') }}</h2>
          <p>{{ t('deck_edit.capital_picker.lead') }}</p>
        </header>
        <ul class="capital-grid" role="list">
          <li v-for="c in ALL_CAPITALS" :key="c">
            <button
              type="button"
              class="capital-tile"
              :class="[capitalRaceClass(c), { selected: editingCapital === c }]"
              @click="pickCapital(c)"
            >
              <span class="tile-race">{{ t(`decks.capital.${c}`) }}</span>
              <span class="tile-faction">{{ t(`decks.faction.${factionOfCapital(c)}`) }}</span>
            </button>
          </li>
        </ul>
        <button
          v-if="isChangingCapital && editingCapital !== null"
          class="ghost"
          type="button"
          @click="cancelChangeCapital"
        >
          {{ t('deck_edit.capital_picker.cancel') }}
        </button>
      </section>

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
              <li
                v-for="card in addedFiltered"
                :key="`a-${card.id}`"
                class="tile added"
                :class="raceClass(card.race)"
              >
                <div class="img-wrap">
                  <img v-if="imageUrl(card)" :src="imageUrl(card)!" :alt="card.name" loading="lazy" decoding="async" />
                  <div v-else class="no-img">{{ t('deck_edit.no_image') }}</div>
                  <span class="badge">
                    {{ counts[card.id] }} / {{ MAX_COPIES_PER_TITLE }}
                  </span>
                </div>
                <div class="tile-foot">
                  <span class="name" :title="card.name">{{ card.name }}</span>
                  <div class="qty">
                    <button
                      class="qty-btn"
                      type="button"
                      @click="remove(card)"
                      :aria-label="t('deck_edit.minus_aria', { name: card.name })"
                    >−</button>
                    <span class="qty-num">{{ counts[card.id] ?? 0 }}</span>
                    <button
                      class="qty-btn"
                      type="button"
                      :disabled="(counts[card.id] ?? 0) >= MAX_COPIES_PER_TITLE"
                      @click="add(card)"
                      :aria-label="t('deck_edit.plus_aria', { name: card.name })"
                    >+</button>
                  </div>
                </div>
              </li>
            </ul>
          </section>

          <section class="card-section available-section">
            <h3 v-if="addedFiltered.length > 0" class="section-heading">
              {{ t('deck_edit.available_heading', { count: availableFiltered.length }) }}
            </h3>
            <ul class="grid" role="list">
              <li
                v-for="card in availableFiltered"
                :key="card.id"
                class="tile"
                :class="[raceClass(card.race), { blocked: !isCardAllowedInDeck(card, editingCapital) }]"
              >
                <div class="img-wrap">
                  <img v-if="imageUrl(card)" :src="imageUrl(card)!" :alt="card.name" loading="lazy" decoding="async" />
                  <div v-else class="no-img">{{ t('deck_edit.no_image') }}</div>
                </div>
                <div class="tile-foot">
                  <span class="name" :title="card.name">{{ card.name }}</span>
                  <div class="qty">
                    <button
                      class="qty-btn"
                      type="button"
                      disabled
                      :aria-label="t('deck_edit.minus_aria', { name: card.name })"
                    >−</button>
                    <span class="qty-num">0</span>
                    <button
                      class="qty-btn"
                      type="button"
                      :disabled="!isCardAllowedInDeck(card, editingCapital)"
                      @click="add(card)"
                      :aria-label="t('deck_edit.plus_aria', { name: card.name })"
                    >+</button>
                  </div>
                </div>
              </li>
            </ul>
          </section>
        </section>

        <!-- Deck panel -->
        <aside class="deck-panel" :class="{ open: showDeckOnMobile }" :aria-label="t('deck_edit.panel_label')">
          <div class="panel-inner">
            <header class="panel-head">
              <span
                class="chip capital-chip"
                :class="editingCapital ? capitalRaceClass(editingCapital) : 'capital-unset'"
              >
                {{ capitalLabel(editingCapital) }}
              </span>
              <span class="count" :class="`tone-${validationTone}`">
                {{ summary.stats.total }} / {{ MIN_DECK_SIZE }}–{{ MAX_DECK_SIZE }}
              </span>
            </header>

            <button
              v-if="editingCapital !== null"
              type="button"
              class="change-capital"
              :disabled="!canChangeCapital"
              :title="canChangeCapital ? '' : t('deck_edit.capital_picker.change_blocked')"
              @click="requestChangeCapital"
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

            <section class="curve">
              <h3>{{ t('deck_edit.cost_curve') }}</h3>
              <ul class="curve-list" role="list">
                <li v-for="b in summary.stats.costCurve" :key="b.bucket" class="curve-row">
                  <span class="curve-label">{{ b.bucket }}</span>
                  <span class="curve-bar"><span class="curve-fill" :style="{ width: curveWidth(b.count) }"></span></span>
                  <span class="curve-count">{{ b.count }}</span>
                </li>
              </ul>
            </section>

            <section class="breakdown">
              <div>
                <h3>{{ t('deck_edit.by_type') }}</h3>
                <dl>
                  <template v-for="(n, ty) in summary.stats.byType" :key="ty">
                    <dt>{{ ty }}</dt><dd>{{ n }}</dd>
                  </template>
                </dl>
              </div>
              <div>
                <h3>{{ t('deck_edit.by_race') }}</h3>
                <dl>
                  <template v-for="(n, r) in summary.stats.byRace" :key="r">
                    <dt>{{ r }}</dt><dd>{{ n }}</dd>
                  </template>
                </dl>
              </div>
            </section>

            <section class="card-list">
              <h3>{{ t('deck_edit.deck_list') }}</h3>
              <ul v-if="summary.cards.length > 0" role="list">
                <li v-for="entry in summary.cards" :key="entry.card.id" class="dl-row">
                  <span class="dl-count">{{ entry.count }}×</span>
                  <span class="dl-name" :class="raceClass(entry.card.race)">{{ entry.card.name }}</span>
                  <span class="dl-cost" v-if="entry.card.cost !== null">{{ entry.card.cost }}</span>
                  <button class="qty-btn ghost-btn" type="button" @click="remove(entry.card)" :aria-label="t('deck_edit.minus_aria', { name: entry.card.name })">−</button>
                  <button
                    class="qty-btn ghost-btn"
                    type="button"
                    :disabled="(counts[entry.card.id] ?? 0) >= MAX_COPIES_PER_TITLE"
                    @click="add(entry.card)"
                    :aria-label="t('deck_edit.plus_aria', { name: entry.card.name })"
                  >+</button>
                </li>
              </ul>
              <p v-else class="empty">{{ t('deck_edit.deck_empty') }}</p>
            </section>
          </div>
        </aside>
      </div>

      <button
        class="mobile-toggle"
        type="button"
        :aria-expanded="showDeckOnMobile"
        @click="showDeckOnMobile = !showDeckOnMobile"
      >
        {{ showDeckOnMobile ? t('deck_edit.hide_deck') : t('deck_edit.show_deck', { count: summary.stats.total }) }}
      </button>
    </template>
  </main>

  <main v-else class="edit-page">
    <p class="error">{{ loadError }}</p>
    <button class="ghost" type="button" @click="emit('navigate', '#/decks')">{{ t('deck_edit.back') }}</button>
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

.ghost {
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  padding: 0.4rem 0.85rem;
  border-radius: var(--radius-md);
  font-size: 0.88rem;
  cursor: pointer;
  min-height: var(--tap-target);
}

.ghost:hover {
  color: var(--fg);
  border-color: var(--fg-dim);
}

.ghost.danger:hover {
  color: var(--accent-strong);
  border-color: var(--accent-strong);
}

.layout {
  display: grid;
  grid-template-columns: minmax(0, 1fr) minmax(300px, 360px);
  gap: 0;
  align-items: start;
}

/* ----- Browser ----- */
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

.tile.added {
  border-color: var(--accent);
  outline: 1px solid var(--accent-strong);
  outline-offset: -2px;
}

.tile {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  padding: 0.45rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
}

.tile.blocked {
  opacity: 0.45;
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
  font-size: 0.7rem;
  color: var(--fg-faint);
  padding: 0.5rem;
  text-align: center;
}

.badge {
  position: absolute;
  top: 4px;
  right: 4px;
  padding: 0.1rem 0.4rem;
  background: var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-pill);
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.04em;
}

.tile-foot {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
}

.tile-foot .name {
  font-size: 0.82rem;
  color: var(--fg);
  line-height: 1.2;
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
}

.qty {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 0.3rem;
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

.qty-num {
  font-variant-numeric: tabular-nums;
  min-width: 1.2em;
  text-align: center;
  color: var(--fg);
  font-size: 0.9rem;
}

.tile.race-empire { box-shadow: inset 3px 0 0 var(--race-empire); }
.tile.race-dwarf { box-shadow: inset 3px 0 0 var(--race-dwarf); }
.tile.race-high-elf { box-shadow: inset 3px 0 0 var(--race-high-elf); }
.tile.race-chaos { box-shadow: inset 3px 0 0 var(--race-chaos); }
.tile.race-orc { box-shadow: inset 3px 0 0 var(--race-orc); }
.tile.race-dark-elf { box-shadow: inset 3px 0 0 var(--race-dark-elf); }
.tile.race-neutral { box-shadow: inset 3px 0 0 var(--race-neutral); }

/* ----- Deck panel ----- */
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

.chip {
  display: inline-block;
  font-size: 0.7rem;
  letter-spacing: 0.05em;
  padding: 0.18rem 0.65rem;
  border-radius: var(--radius-pill);
  background: var(--bg);
  border: 1px solid var(--border);
  color: var(--fg-dim);
  text-transform: uppercase;
}

.chip.faction-order {
  background: var(--faction-order);
  color: var(--bg);
  border-color: transparent;
}

.chip.faction-destruction {
  background: var(--faction-destruction);
  color: var(--on-accent);
  border-color: transparent;
}

.capital-chip {
  color: var(--bg);
  border-color: transparent;
  font-weight: 600;
}
.capital-chip.race-empire { background: var(--race-empire); }
.capital-chip.race-dwarf { background: var(--race-dwarf); }
.capital-chip.race-high-elf { background: var(--race-high-elf); }
.capital-chip.race-chaos { background: var(--race-chaos); color: var(--on-accent); }
.capital-chip.race-orc { background: var(--race-orc); }
.capital-chip.race-dark-elf { background: var(--race-dark-elf); color: var(--on-accent); }
.capital-chip.capital-unset {
  background: var(--bg);
  color: var(--fg-dim);
  border-color: var(--border);
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

.capital-gate {
  max-width: 720px;
  margin: 1.5rem auto;
  padding: 1.5rem 1.25rem 2rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.gate-head h2 {
  margin: 0 0 0.3rem;
  font-size: 1.3rem;
  color: var(--fg);
}

.gate-head p {
  margin: 0;
  color: var(--fg-dim);
  font-size: 0.92rem;
}

.capital-grid {
  list-style: none;
  margin: 0;
  padding: 0;
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
  gap: 0.7rem;
}

.capital-tile {
  width: 100%;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 0.25rem;
  padding: 0.9rem 1rem;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  color: var(--fg);
  cursor: pointer;
  min-height: var(--tap-target);
  text-align: left;
  transition:
    transform var(--transition-fast),
    border-color var(--transition-fast);
}

.capital-tile:hover {
  transform: translateY(-1px);
}

.capital-tile.selected {
  border-color: var(--accent);
  box-shadow: inset 0 0 0 1px var(--accent);
}

.capital-tile.race-empire { border-left: 4px solid var(--race-empire); }
.capital-tile.race-dwarf { border-left: 4px solid var(--race-dwarf); }
.capital-tile.race-high-elf { border-left: 4px solid var(--race-high-elf); }
.capital-tile.race-chaos { border-left: 4px solid var(--race-chaos); }
.capital-tile.race-orc { border-left: 4px solid var(--race-orc); }
.capital-tile.race-dark-elf { border-left: 4px solid var(--race-dark-elf); }

.tile-race {
  font-size: 1.05rem;
  font-weight: 600;
}

.tile-faction {
  font-size: 0.75rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.count {
  font-variant-numeric: tabular-nums;
  font-weight: 600;
  color: var(--fg);
}

.count.tone-warn {
  color: var(--accent-strong);
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

.curve h3,
.breakdown h3,
.card-list h3 {
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
  top: 0;
  left: 0;
  bottom: 0;
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

.mobile-toggle {
  display: none;
}

/* Responsive */
@media (max-width: 900px) {
  .layout {
    grid-template-columns: 1fr;
  }

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
