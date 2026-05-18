<script setup lang="ts">
// Capital-gate UI shown before any cards have been added to a deck
// (and re-opened via "change capital" once the deck has only Neutral
// cards). Six big tiles, one per playable race, each painted in the
// race's accent colour.

import { useI18n } from 'vue-i18n'
import {
  ALL_CAPITALS,
  factionOfCapital,
  slugOfCapital,
  type Capital,
} from '../lib/race'

const props = defineProps<{
  current: Capital | null
  // Whether the picker is currently in "change capital" mode (vs. the
  // first-time pick before any cards exist). Controls whether a
  // cancel button is shown.
  changing: boolean
}>()

const emit = defineEmits<{
  (e: 'pick', c: Capital): void
  (e: 'cancel'): void
}>()

const { t } = useI18n({ useScope: 'global' })
</script>

<template>
  <section class="capital-gate" :aria-label="t('deck_edit.capital_picker.aria')">
    <header class="gate-head">
      <h2>{{ t('deck_edit.capital_picker.heading') }}</h2>
      <p>{{ t('deck_edit.capital_picker.lead') }}</p>
    </header>
    <ul class="capital-grid" role="list">
      <li v-for="c in ALL_CAPITALS" :key="c">
        <button
          type="button"
          class="capital-tile"
          :class="[`race-${slugOfCapital(c)}`, { selected: props.current === c }]"
          @click="emit('pick', c)"
        >
          <span class="tile-race">{{ t(`decks.capital.${c}`) }}</span>
          <span class="tile-faction">{{ t(`decks.faction.${factionOfCapital(c)}`) }}</span>
        </button>
      </li>
    </ul>
    <button
      v-if="changing && current !== null"
      class="ghost"
      type="button"
      @click="emit('cancel')"
    >
      {{ t('deck_edit.capital_picker.cancel') }}
    </button>
  </section>
</template>

<style scoped>
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

/* "Cancel" button bottoms-left of the picker. */
.ghost { align-self: flex-start; }
</style>
