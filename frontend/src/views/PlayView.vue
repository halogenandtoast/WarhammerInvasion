<script setup lang="ts">
// In-game table. Pure renderer of the engine snapshot held by the
// game store.
//
// Layout: the available vertical space is split exactly 50/50 between
// the opponent (top) and self (bottom). The action window pill floats
// over the seam between the two halves so it sits adjacent to both
// players' battlefields without consuming layout space.

import { computed, onBeforeUnmount } from 'vue'
import { useI18n } from 'vue-i18n'
import { game } from '../stores/game'
import type {
  ActionWindowTrigger,
  EngineGame,
  EnginePlayer,
  PlayerKey,
  SeatView,
} from '../api/protocol'
import { priorityHolder } from '../api/protocol'
import PlaySide from '../components/PlaySide.vue'
import CardOverlay from '../components/CardOverlay.vue'
import { cardHover } from '../stores/cardHover'

onBeforeUnmount(() => cardHover.clear())

const props = defineProps<{
  engine: EngineGame
  seats: SeatView[]
}>()

const { t } = useI18n({ useScope: 'global' })

const mySeatKey = computed<PlayerKey | null>(() => {
  const me = game.you.value
  if (!me) return null
  const row = props.seats.find((s) => s.user.userId === me.userId)
  if (!row) return null
  return row.seat === 'Player1' ? 'Player1' : 'Player2'
})

const opponentSeatKey = computed<PlayerKey | null>(() =>
  mySeatKey.value === 'Player1' ? 'Player2' : mySeatKey.value === 'Player2' ? 'Player1' : null,
)

function playerFor(k: PlayerKey | null): EnginePlayer | null {
  if (!k) return null
  return k === 'Player1' ? props.engine.player1 : props.engine.player2
}

const me = computed(() => playerFor(mySeatKey.value))
const opponent = computed(() => playerFor(opponentSeatKey.value))

function seatName(k: PlayerKey): string {
  return props.seats.find((s) => s.seat === k)?.user.displayName ?? k
}

const aw = computed(() => props.engine.actionWindow)
const windowTriggerLabel = computed(() =>
  aw.value ? t(`game.play.window.trigger.${aw.value.trigger satisfies ActionWindowTrigger}`) : null,
)
const priorityIsMe = computed(
  () => aw.value != null && mySeatKey.value != null && priorityHolder(aw.value.awaiting) === mySeatKey.value,
)
const waitingMessage = computed(() => {
  if (!aw.value) return null
  if (priorityIsMe.value) return t('game.play.window.waiting_you')
  return t('game.play.window.waiting_them', { name: seatName(priorityHolder(aw.value.awaiting)) })
})

function pass() {
  if (priorityIsMe.value) game.passPriority()
}

const finished = computed(() => {
  const lc = props.engine.lifecycle
  if (lc.tag !== 'GameFinished') return null
  const youWon = mySeatKey.value === lc.contents.winner
  const reasonKey =
    lc.contents.reason === 'OpponentDeckedOut' ? 'reason_decked' : 'reason_burned'
  return {
    youWon,
    headline: youWon
      ? t('game.play.finished.you_win')
      : t('game.play.finished.you_lose', { name: seatName(lc.contents.winner) }),
    reason: t(`game.play.finished.${reasonKey}`),
  }
})
</script>

<template>
  <div class="play-table">
    <div class="half top">
      <PlaySide
        v-if="opponent && opponentSeatKey"
        :player="opponent"
        perspective="opponent"
        :seat-name="seatName(opponentSeatKey)"
        :is-active="engine.currentPlayer === opponentSeatKey"
        :is-first-player="engine.firstPlayer === opponentSeatKey"
      />
    </div>

    <div class="half bottom">
      <PlaySide
        v-if="me && mySeatKey"
        :player="me"
        perspective="self"
        :seat-name="seatName(mySeatKey)"
        :is-active="engine.currentPlayer === mySeatKey"
        :is-first-player="engine.firstPlayer === mySeatKey"
      />
    </div>

    <!-- Action window floats at the seam between the two halves so it
         sits adjacent to both battlefields without eating layout. -->
    <div v-if="aw" class="window-overlay">
      <div class="window-pill" :class="{ mine: priorityIsMe }" role="status">
        <span class="window-trigger">{{ windowTriggerLabel }}</span>
        <span class="window-waiting">{{ waitingMessage }}</span>
        <button
          class="pass-btn"
          type="button"
          :disabled="!priorityIsMe"
          :title="!priorityIsMe ? t('game.play.window.pass_disabled') : undefined"
          @click="pass"
        >
          {{ t('game.play.window.pass') }}
        </button>
      </div>
    </div>

    <!-- Hover-zoom preview for face-up cards. Teleports to <body> so
         the enlarged image isn't clipped by the play-table container. -->
    <CardOverlay />

    <!-- Game-over banner (overlays the table) -->
    <div v-if="finished" class="finished-overlay">
      <div class="finished-card" :class="{ win: finished.youWon }">
        <p class="finished-heading">{{ t('game.play.finished.heading') }}</p>
        <p class="finished-headline">{{ finished.headline }}</p>
        <p class="finished-reason">{{ finished.reason }}</p>
      </div>
    </div>
  </div>
</template>

<style scoped>
.play-table {
  position: relative;
  flex: 1;
  min-height: 0;
  display: grid;
  /* Exact 50/50 split. The action window strip is overlaid so it
     doesn't steal layout from either half. */
  grid-template-rows: 1fr 1fr;
  gap: 0;
  padding: 0.4rem 0.5rem;
}

.half {
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 0;
  /* Allow the SVG to fill the cell vertically. */
  overflow: hidden;
}

.half.top { align-items: flex-start; padding-bottom: 0.3rem; }
.half.bottom { align-items: flex-end; padding-top: 0.3rem; }

/* ───────── action window overlay ───────── */

.window-overlay {
  position: absolute;
  top: 50%;
  left: 0;
  right: 0;
  transform: translateY(-50%);
  display: flex;
  justify-content: center;
  pointer-events: none; /* let half-rows below take pointer events; pill re-enables */
  z-index: 5;
}

.window-pill {
  pointer-events: auto;
  display: inline-flex;
  align-items: center;
  gap: 0.85rem;
  padding: 0.45rem 0.85rem;
  background: rgba(20, 14, 8, 0.92);
  border: 1px solid rgba(255, 255, 255, 0.15);
  border-radius: var(--radius-pill);
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.55);
  color: rgba(255, 255, 255, 0.92);
  max-width: 90%;
  flex-wrap: wrap;
  justify-content: center;
}

.window-pill.mine {
  border-color: var(--accent);
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.55), 0 0 0 3px rgba(196, 99, 74, 0.22);
}

.window-trigger {
  font-size: 0.7rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.55);
}

.window-waiting {
  font-size: 0.88rem;
  white-space: nowrap;
}

.pass-btn {
  min-height: 32px;
  padding: 0 0.95rem;
  background: var(--accent);
  border: 1px solid var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-pill);
  font-size: 0.85rem;
  cursor: pointer;
}
.pass-btn:hover:not(:disabled) {
  background: var(--accent-strong);
  border-color: var(--accent-strong);
}
.pass-btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

/* ───────── game-over overlay ───────── */

.finished-overlay {
  position: absolute;
  inset: 0;
  background: rgba(0, 0, 0, 0.65);
  display: grid;
  place-items: center;
  z-index: 10;
}
.finished-card {
  padding: 1.3rem 1.6rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  border-left: 4px solid var(--accent-strong);
  text-align: center;
  min-width: min(360px, 80%);
}
.finished-card.win { border-left-color: #5da46a; }
.finished-heading { margin: 0 0 0.25rem; font-size: 0.72rem; letter-spacing: 0.18em; text-transform: uppercase; color: var(--fg-faint); }
.finished-headline { margin: 0 0 0.25rem; font-size: 1.4rem; font-weight: 600; }
.finished-reason { margin: 0; color: var(--fg-dim); font-size: 0.88rem; }
</style>
