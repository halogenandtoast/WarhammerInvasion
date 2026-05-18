<script setup lang="ts">
// One card tile in the deck-builder browser. Two states:
//
//   - `count > 0` (in deck): shows the count badge, both +/- buttons,
//     and an accent outline ("added" treatment).
//   - `count == 0` (available): minus is disabled; plus is gated by
//     `canAdd`. Renders dimmed when `blocked`.
//
// All wire-up is via the parent — this component just emits +/− events.

import { useI18n } from 'vue-i18n'
import type { Card } from '../types/card'
import { MAX_COPIES_PER_TITLE } from '../lib/deck'
import { raceClass } from '../lib/race'
import { cardImageUrl } from '../lib/assets'

defineProps<{
  card: Card
  count: number
  // Whether the card may legally be added to the current deck (capital
  // / faction check). When false, the plus button is disabled and the
  // tile is dimmed even at count = 0.
  canAdd: boolean
}>()

const emit = defineEmits<{
  (e: 'add', card: Card): void
  (e: 'remove', card: Card): void
}>()

const { t } = useI18n({ useScope: 'global' })
</script>

<template>
  <li
    class="tile"
    :class="[
      raceClass(card.race),
      { added: count > 0, blocked: !canAdd && count === 0 },
    ]"
  >
    <div class="img-wrap">
      <img
        v-if="cardImageUrl(card)"
        :src="cardImageUrl(card)!"
        :alt="card.name"
        loading="lazy"
        decoding="async"
      />
      <div v-else class="no-img">{{ t('deck_edit.no_image') }}</div>
      <span v-if="count > 0" class="badge">
        {{ count }} / {{ MAX_COPIES_PER_TITLE }}
      </span>
    </div>
    <div class="tile-foot">
      <span class="name" :title="card.name">{{ card.name }}</span>
      <div class="qty">
        <button
          class="qty-btn"
          type="button"
          :disabled="count === 0"
          :aria-label="t('deck_edit.minus_aria', { name: card.name })"
          @click="emit('remove', card)"
        >−</button>
        <span class="qty-num">{{ count }}</span>
        <button
          class="qty-btn"
          type="button"
          :disabled="!canAdd || count >= MAX_COPIES_PER_TITLE"
          :aria-label="t('deck_edit.plus_aria', { name: card.name })"
          @click="emit('add', card)"
        >+</button>
      </div>
    </div>
  </li>
</template>

<style scoped>
.tile {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  padding: 0.45rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
}

.tile.added {
  border-color: var(--accent);
  outline: 1px solid var(--accent-strong);
  outline-offset: -2px;
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
</style>
