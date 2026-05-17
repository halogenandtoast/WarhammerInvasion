<script setup lang="ts">
// Body of a single seat slot inside the game's waiting room. Shows:
//
//   - nothing useful when the seat is empty (placeholder text)
//   - the deck summary (name / size / capital) if a deck is selected
//   - a "pick a deck" affordance only for the seated player while the
//     game hasn't started (otherwise just a "waiting" line)
//
// The parent owns deck loading + the actual `selectDeck` / `clearDeck`
// calls — this component only renders and emits intent.

import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { GameStatus, SeatView, UserInfo } from '../api/protocol'
import type { DeckRecord } from '../api/decks'
import { MIN_DECK_SIZE } from '../lib/deck'

const props = withDefaults(
  defineProps<{
    seat: SeatView | null
    you: UserInfo | null
    decks?: DeckRecord[]
    decksLoading?: boolean
    decksError?: string | null
    gameStatus?: GameStatus
  }>(),
  {
    decks: () => [],
    decksLoading: false,
    decksError: null,
    gameStatus: 'StatusWaiting',
  },
)

const emit = defineEmits<{
  (e: 'select', ev: Event): void
  (e: 'clear'): void
}>()

const { t } = useI18n({ useScope: 'global' })

const isMe = computed(
  () => props.you != null && props.seat?.user.userId === props.you.userId,
)
const canEdit = computed(() => isMe.value && props.gameStatus === 'StatusWaiting')

function deckSize(d: DeckRecord): number {
  return Object.values(d.cards).reduce((a, b) => a + b, 0)
}
</script>

<template>
  <div v-if="!seat" class="seat-empty">{{ t('game.seat.empty') }}</div>

  <div v-else class="seat-body">
    <div class="seat-user">
      <span class="seat-user-name">{{ seat.user.displayName }}</span>
    </div>

    <!-- Deck already chosen — show summary. -->
    <template v-if="seat.deck">
      <div class="deck-summary">
        <p class="deck-name">{{ seat.deck.name }}</p>
        <p class="deck-line">
          {{ t('game.seat.deck_summary.size', { count: seat.deck.size }) }}
        </p>
        <p class="deck-line">
          {{
            seat.deck.capital
              ? t('game.seat.deck_summary.capital', { name: seat.deck.capital })
              : t('game.seat.deck_summary.no_capital')
          }}
        </p>
      </div>
      <button v-if="canEdit" type="button" class="ghost" @click="emit('clear')">
        {{ t('game.seat.deck_clear') }}
      </button>
    </template>

    <!-- Opponent — we can't pick for them. -->
    <p v-else-if="!isMe" class="deck-pending">{{ t('game.seat.deck_pending') }}</p>

    <!-- This is me, no deck selected yet — show the picker. -->
    <p v-else-if="decksLoading" class="deck-pending">{{ t('decks.list.loading') }}</p>
    <p v-else-if="decksError" class="deck-pending">{{ decksError }}</p>
    <p v-else-if="!decks.length" class="deck-pending">{{ t('game.seat.deck_no_decks') }}</p>
    <select
      v-else
      class="deck-select"
      :aria-label="t('game.seat.deck_picker_label')"
      @change="(e) => emit('select', e)"
    >
      <option value="" disabled selected>{{ t('game.seat.deck_picker_placeholder') }}</option>
      <option
        v-for="d in decks"
        :key="d.id"
        :value="d.id"
        :disabled="deckSize(d) < MIN_DECK_SIZE"
      >
        {{ d.name }}{{ d.capital ? ` · ${d.capital}` : '' }} ({{ deckSize(d) }})
      </option>
    </select>
  </div>
</template>
