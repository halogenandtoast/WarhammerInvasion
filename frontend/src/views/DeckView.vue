<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Card, CardType } from '../types/card'
import { ApiError } from '../api/client'
import { deleteDeck, getDeck, type DeckRecord } from '../api/decks'
import {
  factionOfCapital,
  summarize,
  MIN_DECK_SIZE,
  MAX_DECK_SIZE,
} from '../lib/deck'
import { raceClass } from '../lib/race'
import { cardImageUrl } from '../lib/assets'
import { useCardCatalog } from '../composables/useCardCatalog'
import { navigate } from '../router'
import CapitalChip from '../components/CapitalChip.vue'
import DeckStatsPanel from '../components/DeckStatsPanel.vue'

const props = defineProps<{ deckId: string }>()
const { t } = useI18n({ useScope: 'global' })

const catalog = useCardCatalog()
const deck = ref<DeckRecord | null>(null)
const loading = ref(true)
const loadError = ref<string | null>(null)
const deleting = ref(false)

onMounted(async () => {
  try {
    const [, deckRes] = await Promise.all([catalog.ensureLoaded(), getDeck(props.deckId)])
    deck.value = deckRes
  } catch (e) {
    loadError.value = e instanceof ApiError ? e.code : e instanceof Error ? e.message : 'load_failed'
  } finally {
    loading.value = false
  }
})

const summary = computed(() => {
  if (!deck.value) return null
  return summarize({ cards: deck.value.cards, capital: deck.value.capital }, catalog.cardIndex.value)
})

const TYPE_ORDER: CardType[] = ['Unit', 'Support', 'Tactic', 'Quest', 'Legend', 'Fulcrum']

const groupedByType = computed(() => {
  const s = summary.value
  if (!s) return []
  const groups = new Map<string, { card: Card; count: number }[]>()
  for (const entry of s.cards) {
    const key = entry.card.type ?? 'Other'
    if (!groups.has(key)) groups.set(key, [])
    groups.get(key)!.push(entry)
  }
  const sortedKeys = [...groups.keys()].sort((a, b) => {
    const ai = TYPE_ORDER.indexOf(a as CardType)
    const bi = TYPE_ORDER.indexOf(b as CardType)
    if (ai === -1 && bi === -1) return a.localeCompare(b)
    if (ai === -1) return 1
    if (bi === -1) return -1
    return ai - bi
  })
  return sortedKeys.map((type) => {
    const entries = groups.get(type)!
    const count = entries.reduce((n, e) => n + e.count, 0)
    return { type, count, entries }
  })
})

async function destroyDeck() {
  if (!deck.value || deleting.value) return
  if (!confirm(t('deck_view.confirm_delete', { name: deck.value.name }))) return
  deleting.value = true
  try {
    await deleteDeck(deck.value.id)
    navigate('#/decks')
  } catch (e) {
    loadError.value = e instanceof ApiError ? e.code : 'delete_failed'
  } finally {
    deleting.value = false
  }
}

const validationTone = computed<'ok' | 'warn'>(() => {
  const s = summary.value
  if (!s) return 'ok'
  return s.issues.some((i) => i.severity === 'error') ? 'warn' : 'ok'
})
</script>

<template>
  <main class="view-page">
    <div v-if="loading" class="status">{{ t('deck_view.loading') }}</div>

    <template v-else-if="deck && summary">
      <header class="page-head">
        <button class="back" type="button" @click="navigate('#/decks')">
          ← {{ t('deck_view.back') }}
        </button>
        <div class="title-wrap">
          <h1>{{ deck.name }}</h1>
          <div class="meta">
            <CapitalChip :capital="deck.capital" />
            <span
              v-if="deck.capital"
              class="chip faction-chip"
              :class="`faction-${factionOfCapital(deck.capital)}`"
            >
              {{ t(`decks.faction.${factionOfCapital(deck.capital)}`) }}
            </span>
            <span class="count" :class="`tone-${validationTone}`">
              {{ summary.stats.total }} / {{ MIN_DECK_SIZE }}–{{ MAX_DECK_SIZE }}
            </span>
          </div>
        </div>
        <div class="actions">
          <button class="primary" type="button" @click="navigate(`#/decks/${deck.id}/edit`)">
            {{ t('deck_view.edit') }}
          </button>
          <button class="ghost danger" type="button" :disabled="deleting" @click="destroyDeck">
            {{ t('deck_view.delete') }}
          </button>
        </div>
      </header>

      <p v-if="!deck.capital" class="hint">{{ t('deck_view.capital_unset') }}</p>

      <ul v-if="summary.issues.length > 0" class="issues">
        <li
          v-for="(issue, idx) in summary.issues"
          :key="idx"
          :class="`severity-${issue.severity}`"
        >
          {{ issue.message }}
        </li>
      </ul>

      <div class="layout">
        <aside class="stats" :aria-label="t('deck_edit.panel_label')">
          <DeckStatsPanel :stats="summary.stats" i18n-prefix="deck_view" />
        </aside>

        <section class="groups">
          <p v-if="summary.cards.length === 0" class="empty">{{ t('deck_view.empty') }}</p>
          <section v-for="group in groupedByType" :key="group.type" class="type-group">
            <h2 class="group-heading">
              {{ t('deck_view.group_heading', { type: group.type, count: group.count }) }}
            </h2>
            <ul class="card-grid" role="list">
              <li
                v-for="entry in group.entries"
                :key="entry.card.id"
                class="card-tile"
                :class="raceClass(entry.card.race)"
              >
                <div class="img-wrap">
                  <img
                    v-if="cardImageUrl(entry.card)"
                    :src="cardImageUrl(entry.card)!"
                    :alt="entry.card.name"
                    loading="lazy"
                    decoding="async"
                  />
                  <div v-else class="no-img">{{ entry.card.name }}</div>
                  <span class="copies">{{ t('deck_view.copies', { count: entry.count }) }}</span>
                </div>
                <span class="card-name" :title="entry.card.name">{{ entry.card.name }}</span>
              </li>
            </ul>
          </section>
        </section>
      </div>
    </template>

    <template v-else-if="loadError">
      <p class="error">{{ loadError }}</p>
      <button class="ghost" type="button" @click="navigate('#/decks')">
        ← {{ t('deck_view.back') }}
      </button>
    </template>
  </main>
</template>

<style scoped>
.view-page {
  background: var(--bg);
  min-height: calc(100dvh - 60px);
  padding: 0 0 4rem;
}

.status,
.empty {
  padding: 2.5rem 1rem;
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
  display: grid;
  grid-template-columns: auto 1fr auto;
  align-items: center;
  gap: 1rem;
  padding: 1rem 1.5rem;
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

.title-wrap {
  min-width: 0;
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
}

h1 {
  margin: 0;
  font-size: clamp(1.2rem, 2.6vw, 1.6rem);
  color: var(--fg);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.meta {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  flex-wrap: wrap;
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

.faction-chip.faction-order {
  background: var(--faction-order);
  color: var(--bg);
  border-color: transparent;
}

.faction-chip.faction-destruction {
  background: var(--faction-destruction);
  color: var(--on-accent);
  border-color: transparent;
}

.count {
  font-variant-numeric: tabular-nums;
  font-weight: 600;
  color: var(--fg);
}

.count.tone-warn {
  color: var(--accent-strong);
}

.actions {
  display: flex;
  gap: 0.5rem;
}

.hint {
  margin: 0.8rem 1.5rem 0;
  padding: 0.55rem 0.85rem;
  background: var(--bg-elev);
  border-left: 3px solid var(--accent-strong);
  border-radius: var(--radius-sm);
  color: var(--fg);
  font-size: 0.88rem;
}

.issues {
  list-style: none;
  margin: 0.8rem 1.5rem 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
}

.issues li {
  padding: 0.45rem 0.7rem;
  background: var(--bg-elev);
  border-radius: var(--radius-sm);
  font-size: 0.88rem;
  color: var(--fg);
  border-left: 3px solid var(--border);
}

.issues li.severity-error {
  border-left-color: var(--accent-strong);
}

.layout {
  margin: 1rem 1.5rem 0;
  display: grid;
  grid-template-columns: minmax(240px, 280px) minmax(0, 1fr);
  gap: 1.25rem;
  align-items: start;
}

.stats {
  position: sticky;
  top: 88px;
  display: flex;
  flex-direction: column;
  gap: 1rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  padding: 1rem 1.1rem;
}

.groups {
  display: flex;
  flex-direction: column;
  gap: 1.25rem;
  min-width: 0;
}

.group-heading {
  margin: 0 0 0.5rem;
  font-size: 0.75rem;
  letter-spacing: 0.14em;
  text-transform: uppercase;
  color: var(--fg-faint);
  border-bottom: 1px solid var(--border);
  padding-bottom: 0.35rem;
}

.card-grid {
  list-style: none;
  margin: 0;
  padding: 0;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
  gap: 0.7rem;
}

.card-tile {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  padding: 0.45rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
}

.card-tile.race-empire { box-shadow: inset 3px 0 0 var(--race-empire); }
.card-tile.race-dwarf { box-shadow: inset 3px 0 0 var(--race-dwarf); }
.card-tile.race-high-elf { box-shadow: inset 3px 0 0 var(--race-high-elf); }
.card-tile.race-chaos { box-shadow: inset 3px 0 0 var(--race-chaos); }
.card-tile.race-orc { box-shadow: inset 3px 0 0 var(--race-orc); }
.card-tile.race-dark-elf { box-shadow: inset 3px 0 0 var(--race-dark-elf); }
.card-tile.race-neutral { box-shadow: inset 3px 0 0 var(--race-neutral); }

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
  text-align: center;
  padding: 0.5rem;
}

.copies {
  position: absolute;
  bottom: 4px;
  right: 4px;
  padding: 0.15rem 0.5rem;
  background: var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-pill);
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.04em;
}

.card-name {
  font-size: 0.82rem;
  color: var(--fg);
  line-height: 1.2;
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
}

@media (max-width: 900px) {
  .layout {
    grid-template-columns: 1fr;
  }
  .stats {
    position: static;
  }
}

@media (max-width: 480px) {
  .card-grid {
    grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
    gap: 0.5rem;
  }
}
</style>
