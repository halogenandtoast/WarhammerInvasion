<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { createDeck, deleteDeck, listDecks, type DeckRecord } from '../api/decks'
import { MIN_DECK_SIZE, MAX_DECK_SIZE, raceOfCapital, type Capital } from '../lib/deck'
import { ApiError } from '../api/client'

const { t } = useI18n({ useScope: 'global' })

const emit = defineEmits<{ (e: 'navigate', target: string): void }>()

const decks = ref<DeckRecord[]>([])
const loading = ref(true)
const loadError = ref<string | null>(null)

const creating = ref(false)
const newName = ref('')
const showNew = ref(false)

onMounted(async () => {
  try {
    decks.value = await listDecks()
  } catch (e) {
    loadError.value = e instanceof Error ? e.message : 'load_failed'
  } finally {
    loading.value = false
  }
})

async function submitNew() {
  if (creating.value) return
  const name = newName.value.trim()
  if (name.length === 0) return
  creating.value = true
  try {
    const deck = await createDeck({
      name,
      capital: null,
      cards: {},
    })
    emit('navigate', `#/decks/${deck.id}`)
  } catch (e) {
    loadError.value = e instanceof ApiError ? e.code : 'create_failed'
  } finally {
    creating.value = false
  }
}

async function remove(deck: DeckRecord) {
  if (!confirm(t('decks.list.confirm_delete', { name: deck.name }))) return
  try {
    await deleteDeck(deck.id)
    decks.value = decks.value.filter((d) => d.id !== deck.id)
  } catch (e) {
    loadError.value = e instanceof ApiError ? e.code : 'delete_failed'
  }
}

function deckSize(deck: DeckRecord): number {
  return Object.values(deck.cards).reduce((a, b) => a + b, 0)
}

function validityLabel(size: number): { label: string; tone: 'ok' | 'warn' } {
  if (size < MIN_DECK_SIZE) {
    return { label: t('decks.list.size_under', { size, min: MIN_DECK_SIZE }), tone: 'warn' }
  }
  if (size > MAX_DECK_SIZE) {
    return { label: t('decks.list.size_over', { size, max: MAX_DECK_SIZE }), tone: 'warn' }
  }
  return { label: t('decks.list.size_ok', { size }), tone: 'ok' }
}

function capitalLabel(c: Capital | null): string {
  if (c === null) return t('decks.capital.unset')
  return t(`decks.capital.${c}`)
}

function capitalRaceClass(c: Capital | null): string {
  if (c === null) return 'capital-unset'
  return `race-${raceOfCapital(c).toLowerCase().replace(/\s+/g, '-')}`
}

const empty = computed(() => !loading.value && decks.value.length === 0)
</script>

<template>
  <main class="decks-page">
    <header class="page-head">
      <div>
        <p class="eyebrow">{{ t('decks.list.eyebrow') }}</p>
        <h1>{{ t('decks.list.heading') }}</h1>
        <p class="lead">{{ t('decks.list.lead') }}</p>
      </div>
      <button class="primary" type="button" @click="showNew = !showNew">
        {{ showNew ? t('decks.list.cancel_new') : t('decks.list.new') }}
      </button>
    </header>

    <form v-if="showNew" class="new-form" @submit.prevent="submitNew">
      <div class="row">
        <label class="field grow">
          <span class="field-label">{{ t('decks.list.name_label') }}</span>
          <input v-model="newName" type="text" required maxlength="80" :placeholder="t('decks.list.name_placeholder')" />
        </label>
      </div>
      <p class="hint">{{ t('decks.list.capital_hint') }}</p>
      <button class="primary" type="submit" :disabled="creating || newName.trim().length === 0">
        {{ creating ? t('decks.list.creating') : t('decks.list.create') }}
      </button>
    </form>

    <p v-if="loadError" class="error">{{ loadError }}</p>

    <div v-if="loading" class="status">{{ t('decks.list.loading') }}</div>
    <div v-else-if="empty" class="status">{{ t('decks.list.empty') }}</div>

    <ul v-else class="deck-grid" role="list">
      <li v-for="deck in decks" :key="deck.id" class="deck-card">
        <a class="deck-link" :href="`#/decks/${deck.id}`" @click.prevent="emit('navigate', `#/decks/${deck.id}`)">
          <header>
            <h2>{{ deck.name }}</h2>
            <span class="chip capital-chip" :class="capitalRaceClass(deck.capital)">
              {{ capitalLabel(deck.capital) }}
            </span>
          </header>
          <p class="size" :class="`tone-${validityLabel(deckSize(deck)).tone}`">
            {{ validityLabel(deckSize(deck)).label }}
          </p>
          <p class="updated">
            {{ t('decks.list.updated_at', { date: new Date(deck.updatedAt).toLocaleString() }) }}
          </p>
        </a>
        <button class="ghost danger" type="button" @click="remove(deck)" :aria-label="t('decks.list.delete_aria', { name: deck.name })">
          {{ t('decks.list.delete') }}
        </button>
      </li>
    </ul>
  </main>
</template>

<style scoped>
.decks-page {
  max-width: 1080px;
  margin: 0 auto;
  padding: 2.5rem 1.5rem 5rem;
}

.page-head {
  display: flex;
  align-items: flex-end;
  justify-content: space-between;
  flex-wrap: wrap;
  gap: 1rem;
  margin-bottom: 1.5rem;
}

.eyebrow {
  margin: 0;
  font-size: 0.78rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--accent-strong);
}

h1 {
  margin: 0.4rem 0 0.3rem;
  font-size: clamp(1.6rem, 3.2vw, 2.2rem);
  color: var(--fg);
}

.lead {
  margin: 0;
  color: var(--fg-dim);
  max-width: 60ch;
}

.primary {
  min-height: var(--tap-target);
  padding: 0 1.1rem;
  background: var(--accent);
  border: 1px solid var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-md);
  font-size: 0.95rem;
  cursor: pointer;
}

.primary:hover:not(:disabled) {
  background: var(--accent-strong);
  border-color: var(--accent-strong);
}

.primary:disabled {
  opacity: 0.6;
  cursor: progress;
}

.ghost {
  min-height: var(--tap-target);
  padding: 0 0.85rem;
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  border-radius: var(--radius-md);
  cursor: pointer;
  font-size: 0.85rem;
}

.ghost:hover {
  color: var(--fg);
  border-color: var(--fg-dim);
}

.ghost.danger:hover {
  color: var(--accent-strong);
  border-color: var(--accent-strong);
}

.new-form {
  margin-bottom: 1.5rem;
  padding: 1rem 1.2rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  display: flex;
  flex-direction: column;
  gap: 0.8rem;
}

.row {
  display: flex;
  gap: 0.75rem;
  flex-wrap: wrap;
  align-items: flex-end;
}

.field {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  min-width: 160px;
}

.field.grow {
  flex: 1 1 240px;
}

.field-label {
  font-size: 0.7rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.field input,
.field select {
  min-height: var(--tap-target);
  padding: 0.5rem 0.75rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.95rem;
}

.status {
  padding: 3rem 1rem;
  text-align: center;
  color: var(--fg-dim);
}

.error {
  margin: 0 0 1rem;
  padding: 0.6rem 0.9rem;
  background: var(--bg-elev);
  border-left: 3px solid var(--accent-strong);
  border-radius: var(--radius-sm);
  color: var(--fg);
}

.deck-grid {
  list-style: none;
  margin: 0;
  padding: 0;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
  gap: 0.85rem;
}

.deck-card {
  position: relative;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  padding: 1rem 1.05rem 0.9rem;
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
  transition: border-color var(--transition-fast);
}

.deck-card:hover {
  border-color: var(--accent-strong);
}

.deck-link {
  display: block;
  color: var(--fg);
  text-decoration: none;
}

.deck-link header {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  gap: 0.6rem;
}

.deck-link h2 {
  margin: 0;
  font-size: 1.05rem;
  font-weight: 600;
}

.chip {
  display: inline-block;
  font-size: 0.7rem;
  letter-spacing: 0.05em;
  padding: 0.15rem 0.6rem;
  border-radius: var(--radius-pill);
  background: var(--bg);
  border: 1px solid var(--border);
  color: var(--fg-dim);
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

.hint {
  margin: 0;
  font-size: 0.82rem;
  color: var(--fg-faint);
}

.size {
  margin: 0.4rem 0 0;
  font-size: 0.88rem;
  color: var(--fg-dim);
}

.size.tone-warn {
  color: var(--accent-strong);
}

.updated {
  margin: 0.1rem 0 0;
  font-size: 0.78rem;
  color: var(--fg-faint);
}

.ghost.danger {
  margin-top: 0.6rem;
  align-self: flex-end;
}
</style>
