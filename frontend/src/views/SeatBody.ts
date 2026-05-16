// Render-function helper for a single seat slot inside Game.vue. Lives
// in its own file so Game.vue's <script setup> can import it cleanly.

import { defineComponent, h } from 'vue'
import type { PropType } from 'vue'
import { useI18n } from 'vue-i18n'
import type { GameStatus, SeatView, UserInfo } from '../api/protocol'
import type { DeckRecord } from '../api/decks'

export default defineComponent({
  name: 'SeatBody',
  props: {
    seat: { type: Object as PropType<SeatView | null>, default: null },
    you: { type: Object as PropType<UserInfo | null>, default: null },
    decks: { type: Array as PropType<DeckRecord[]>, default: () => [] },
    decksLoading: { type: Boolean, default: false },
    decksError: { type: String as PropType<string | null>, default: null },
    gameStatus: { type: String as PropType<GameStatus>, default: 'StatusWaiting' },
  },
  emits: ['select', 'clear'],
  setup(props, { emit }) {
    const { t } = useI18n({ useScope: 'global' })

    const deckSize = (deck: DeckRecord): number =>
      Object.values(deck.cards).reduce((a, b) => a + b, 0)

    return () => {
      const seat = props.seat
      if (!seat) {
        return h('div', { class: 'seat-empty' }, t('game.seat.empty'))
      }
      const isMe = props.you?.userId === seat.user.userId
      const canEdit = isMe && props.gameStatus === 'StatusWaiting'

      const headline = h('div', { class: 'seat-user' }, [
        h('span', { class: 'seat-user-name' }, seat.user.displayName),
      ])

      if (seat.deck) {
        const cap = seat.deck.capital
        const summary = h('div', { class: 'deck-summary' }, [
          h('p', { class: 'deck-name' }, seat.deck.name),
          h(
            'p',
            { class: 'deck-line' },
            t('game.seat.deck_summary.size', { count: seat.deck.size }),
          ),
          h(
            'p',
            { class: 'deck-line' },
            cap
              ? t('game.seat.deck_summary.capital', { name: cap })
              : t('game.seat.deck_summary.no_capital'),
          ),
        ])
        const children = [headline, summary]
        if (canEdit) {
          children.push(
            h(
              'button',
              {
                type: 'button',
                class: 'ghost',
                onClick: () => emit('clear'),
              },
              t('game.seat.deck_clear'),
            ),
          )
        }
        return h('div', { class: 'seat-body' }, children)
      }

      if (!isMe) {
        return h('div', { class: 'seat-body' }, [
          headline,
          h('p', { class: 'deck-pending' }, t('game.seat.deck_pending')),
        ])
      }

      if (props.decksLoading) {
        return h('div', { class: 'seat-body' }, [
          headline,
          h('p', { class: 'deck-pending' }, t('decks.list.loading')),
        ])
      }
      if (props.decksError) {
        return h('div', { class: 'seat-body' }, [
          headline,
          h('p', { class: 'deck-pending' }, props.decksError),
        ])
      }
      if (!props.decks.length) {
        return h('div', { class: 'seat-body' }, [
          headline,
          h('p', { class: 'deck-pending' }, t('game.seat.deck_no_decks')),
        ])
      }

      const options = [
        h(
          'option',
          { value: '', disabled: true, selected: true },
          t('game.seat.deck_picker_placeholder'),
        ),
        ...props.decks.map((d) =>
          h(
            'option',
            { value: d.id, disabled: deckSize(d) < 50 },
            `${d.name}${d.capital ? ` · ${d.capital}` : ''} (${deckSize(d)})`,
          ),
        ),
      ]
      const select = h(
        'select',
        {
          class: 'deck-select',
          'aria-label': t('game.seat.deck_picker_label'),
          onChange: (e: Event) => emit('select', e),
        },
        options,
      )
      return h('div', { class: 'seat-body' }, [headline, select])
    }
  },
})
