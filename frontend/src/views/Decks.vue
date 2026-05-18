<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { createDeck, deleteDeck, listDecks, type DeckRecord } from '../api/decks'
import {
  MIN_DECK_SIZE,
  MAX_DECK_SIZE,
  parseDeckList,
  type ParsedDeckList,
} from '../lib/deck'
import { ApiError } from '../api/client'
import { useCardCatalog } from '../composables/useCardCatalog'
import { navigate } from '../router'
import CapitalChip from '../components/CapitalChip.vue'

const { t } = useI18n({ useScope: 'global' })

const decks = ref<DeckRecord[]>([])
const loading = ref(true)
const loadError = ref<string | null>(null)

const creating = ref(false)
const newName = ref('')
const showNew = ref(false)

const showImport = ref(false)
const importing = ref(false)
const importName = ref('')
const importText = ref('')

// Card catalog is shared across views via the composable; we only
// need to kick the fetch when the import form opens.
const catalog = useCardCatalog({ eager: false })

onMounted(async () => {
  try {
    decks.value = await listDecks()
  } catch (e) {
    loadError.value = e instanceof Error ? e.message : 'load_failed'
  } finally {
    loading.value = false
  }
})

async function loadCardsIfNeeded() {
  if (catalog.loaded.value || catalog.error.value) return
  try {
    await catalog.ensureLoaded()
  } catch {
    /* error surfaced via catalog.error */
  }
}

function openImport() {
  showNew.value = false
  showImport.value = !showImport.value
  if (showImport.value) void loadCardsIfNeeded()
}

function openNew() {
  showImport.value = false
  showNew.value = !showNew.value
}

const parsed = computed<ParsedDeckList | null>(() => {
  if (!catalog.loaded.value) return null
  if (importText.value.trim().length === 0) return null
  return parseDeckList(importText.value, catalog.cards.value)
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
    navigate(`#/decks/${deck.id}/edit`)
  } catch (e) {
    loadError.value = e instanceof ApiError ? e.code : 'create_failed'
  } finally {
    creating.value = false
  }
}

async function submitImport() {
  if (importing.value) return
  const p = parsed.value
  if (!p || p.total === 0) return
  importing.value = true
  try {
    const deck = await createDeck({
      name: importName.value.trim() || t('decks.list.import_form.name_default'),
      capital: p.capital,
      cards: p.counts,
    })
    navigate(`#/decks/${deck.id}/edit`)
  } catch (e) {
    loadError.value = e instanceof ApiError ? e.code : 'create_failed'
  } finally {
    importing.value = false
  }
}

watch(showImport, (open) => {
  if (!open) {
    importText.value = ''
    importName.value = ''
  }
})

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

const empty = computed(() => !loading.value && decks.value.length === 0)
</script>

<template>
  <main class="decks-page">
    <header class="page-head">
      <div>
        <h1>{{ t('decks.list.heading') }}</h1>
        <p class="lead">{{ t('decks.list.lead') }}</p>
      </div>
      <div class="head-actions">
        <button class="ghost" type="button" @click="openImport">
          {{ showImport ? t('decks.list.cancel_import') : t('decks.list.import') }}
        </button>
        <button class="primary" type="button" @click="openNew">
          {{ showNew ? t('decks.list.cancel_new') : t('decks.list.new') }}
        </button>
      </div>
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

    <form v-if="showImport" class="new-form import-form" @submit.prevent="submitImport">
      <div class="row">
        <label class="field grow">
          <span class="field-label">{{ t('decks.list.name_label') }}</span>
          <input
            v-model="importName"
            type="text"
            maxlength="80"
            :placeholder="t('decks.list.import_form.name_default')"
          />
        </label>
      </div>
      <label class="field grow">
        <span class="field-label">{{ t('decks.list.import_form.paste_label') }}</span>
        <textarea
          v-model="importText"
          rows="10"
          spellcheck="false"
          :placeholder="t('decks.list.import_form.paste_placeholder')"
        ></textarea>
      </label>
      <p class="hint">{{ t('decks.list.import_form.paste_hint') }}</p>

      <p v-if="catalog.error.value" class="error">{{ catalog.error.value }}</p>

      <section v-if="parsed" class="import-preview" aria-live="polite">
        <h3>{{ t('decks.list.import_form.preview_heading') }}</h3>
        <p class="preview-total">
          {{ t('decks.list.import_form.preview_total', { count: parsed.total }) }}
        </p>
        <p class="preview-capital">
          <template v-if="parsed.capital">
            {{ t('decks.list.import_form.preview_capital', { capital: t(`decks.capital.${parsed.capital}`) }) }}
            <CapitalChip :capital="parsed.capital" />
          </template>
          <template v-else>{{ t('decks.list.import_form.preview_capital_none') }}</template>
        </p>
        <div v-if="parsed.unknown.length > 0" class="preview-warnings">
          <p class="warn-heading">
            {{ t('decks.list.import_form.preview_unknown_heading', { count: parsed.unknown.length }) }}
          </p>
          <ul>
            <li v-for="(n, i) in parsed.unknown" :key="`u-${i}`">{{ n }}</li>
          </ul>
        </div>
        <div v-if="parsed.parseErrors.length > 0" class="preview-warnings">
          <p class="warn-heading">
            {{ t('decks.list.import_form.preview_parse_errors_heading', { count: parsed.parseErrors.length }) }}
          </p>
          <ul>
            <li v-for="(line, i) in parsed.parseErrors" :key="`p-${i}`">{{ line }}</li>
          </ul>
        </div>
      </section>
      <p v-else-if="catalog.loaded.value" class="hint">{{ t('decks.list.import_form.empty') }}</p>

      <button
        class="primary"
        type="submit"
        :disabled="importing || !parsed || parsed.total === 0"
      >
        {{ importing ? t('decks.list.import_form.submitting') : t('decks.list.import_form.submit') }}
      </button>
    </form>

    <p v-if="loadError" class="error">{{ loadError }}</p>

    <div v-if="loading" class="status">{{ t('decks.list.loading') }}</div>
    <div v-else-if="empty" class="status">{{ t('decks.list.empty') }}</div>

    <ul v-else class="deck-grid" role="list">
      <li v-for="deck in decks" :key="deck.id" class="deck-card">
        <a class="deck-link" :href="`#/decks/${deck.id}`" @click.prevent="navigate(`#/decks/${deck.id}`)">
          <header>
            <h2>{{ deck.name }}</h2>
            <CapitalChip :capital="deck.capital" />
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

.head-actions {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
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

.import-form textarea {
  min-height: 220px;
  padding: 0.6rem 0.75rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-family:
    ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono",
    "Courier New", monospace;
  font-size: 0.85rem;
  line-height: 1.45;
  resize: vertical;
}

.import-form textarea:focus-visible {
  outline: 2px solid var(--accent-strong);
  border-color: var(--accent-strong);
}

.import-preview {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
  padding: 0.75rem 0.9rem;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
}

.import-preview h3 {
  margin: 0;
  font-size: 0.72rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.preview-total {
  margin: 0;
  color: var(--fg);
  font-weight: 600;
}

.preview-capital {
  margin: 0;
  color: var(--fg-dim);
  display: flex;
  align-items: center;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.preview-warnings {
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
  border-left: 3px solid var(--accent-strong);
  padding: 0.25rem 0 0.25rem 0.6rem;
}

.preview-warnings .warn-heading {
  margin: 0;
  font-size: 0.78rem;
  color: var(--accent-strong);
  text-transform: uppercase;
  letter-spacing: 0.06em;
}

.preview-warnings ul {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.1rem;
  font-size: 0.85rem;
  color: var(--fg);
  font-family:
    ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono",
    "Courier New", monospace;
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
