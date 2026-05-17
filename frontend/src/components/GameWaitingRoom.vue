<script setup lang="ts">
// Waiting-room state inside the in-game page: invite strip, two seat
// cards with deck pickers, and the start/leave controls.
//
// The store actions for selecting / clearing / starting / leaving live
// in the parent — this component is just layout + event wiring.

import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import type { GameView, PlayerKey, SeatView, UserInfo } from '../api/protocol'
import type { DeckRecord } from '../api/decks'
import SeatBody from './SeatBody.vue'

const props = defineProps<{
  view: GameView
  you: UserInfo | null
  decks: DeckRecord[]
  decksLoading: boolean
  decksError: string | null
  isSpectator: boolean
  startDisabledReason: string | null
}>()

const emit = defineEmits<{
  (e: 'select-deck', ev: Event): void
  (e: 'clear-deck'): void
  (e: 'start'): void
  (e: 'leave'): void
}>()

const { t } = useI18n({ useScope: 'global' })

const copied = ref(false)

function seatFor(key: PlayerKey): SeatView | null {
  return props.view.seats.find((s) => s.seat === key) ?? null
}

const inviteUrl = computed<string | null>(() => {
  if (!props.view.inviteToken) return null
  const path = `${window.location.pathname}${window.location.search}#/games/${props.view.gameId}?t=${encodeURIComponent(props.view.inviteToken)}`
  return `${window.location.origin}${path}`
})

function seatTitle(key: PlayerKey): string {
  return key === 'Player1' ? t('game.seat.heading_player1') : t('game.seat.heading_player2')
}

function isYouSeat(seat: SeatView | null): boolean {
  return !!seat && !!props.you && seat.user.userId === props.you.userId
}

async function copyInvite() {
  if (!inviteUrl.value) return
  try {
    await navigator.clipboard.writeText(inviteUrl.value)
    copied.value = true
    setTimeout(() => { copied.value = false }, 1500)
  } catch {
    // older browsers / permission-denied
  }
}
</script>

<template>
  <section v-if="view.inviteToken" class="invite-strip">
    <p class="invite-title">{{ t('game.invite.heading') }}</p>
    <div class="invite-row">
      <input
        v-if="inviteUrl"
        class="invite-input"
        readonly
        :value="inviteUrl"
        :aria-label="t('game.invite.link_label')"
      />
      <button class="ghost" type="button" @click="copyInvite">
        {{ copied ? t('game.invite.copied') : t('game.invite.copy') }}
      </button>
    </div>
    <p v-if="view.hasPassword" class="invite-pw">
      {{ t('game.invite.password_label') }}: <span>•••••••</span>
    </p>
    <p v-else class="invite-pw faint">{{ t('game.invite.no_password') }}</p>
  </section>

  <section class="seats">
    <article
      v-for="key in (['Player1', 'Player2'] as const)"
      :key="key"
      class="seat"
      :class="{
        filled: !!seatFor(key),
        you: isYouSeat(seatFor(key)),
      }"
    >
      <header>
        <h2>{{ seatTitle(key) }}</h2>
        <span v-if="seatFor(key)?.isHost" class="seat-tag tag-host">
          {{ t('game.seat.host_tag') }}
        </span>
        <span v-if="isYouSeat(seatFor(key))" class="seat-tag tag-you">
          {{ t('game.seat.you_tag') }}
        </span>
      </header>
      <SeatBody
        :seat="seatFor(key)"
        :you="you"
        :decks="decks"
        :decks-loading="decksLoading"
        :decks-error="decksError"
        :game-status="view.status"
        @select="(e) => emit('select-deck', e)"
        @clear="emit('clear-deck')"
      />
    </article>
  </section>

  <section class="controls">
    <template v-if="!isSpectator">
      <button
        class="primary big"
        type="button"
        :disabled="startDisabledReason !== null"
        :title="startDisabledReason ?? undefined"
        @click="emit('start')"
      >
        {{ t('game.controls.start') }}
      </button>
      <p v-if="startDisabledReason" class="hint">{{ startDisabledReason }}</p>
    </template>
    <p v-else class="hint">{{ t('game.spectators.waiting_note') }}</p>

    <button class="danger" type="button" @click="emit('leave')">
      {{ isSpectator ? t('game.spectators.leave') : t('game.controls.leave') }}
    </button>
  </section>
</template>

<style scoped>
.invite-strip {
  margin: 0.75rem 0.75rem 0;
  padding: 0.7rem 0.9rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  display: flex;
  flex-direction: column;
  gap: 0.45rem;
}
.invite-title {
  margin: 0;
  font-size: 0.72rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--fg-faint);
}
.invite-row { display: flex; gap: 0.5rem; flex-wrap: wrap; }
.invite-input {
  flex: 1 1 300px;
  min-height: var(--tap-target);
  padding: 0.45rem 0.7rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-family: var(--font-mono);
  font-size: 0.82rem;
  user-select: all;
}
.invite-pw { margin: 0; font-size: 0.82rem; color: var(--fg-dim); }
.invite-pw.faint { color: var(--fg-faint); }

.seats {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0.9rem;
  padding: 0.75rem;
}
@media (max-width: 720px) {
  .seats { grid-template-columns: 1fr; }
}
.seat {
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  padding: 0.85rem 1rem;
  display: flex;
  flex-direction: column;
  gap: 0.55rem;
  min-height: 200px;
}
.seat.you { border-color: var(--accent); }
.seat header { display: flex; align-items: center; gap: 0.45rem; }
.seat header h2 {
  margin: 0;
  font-size: 0.82rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--fg-faint);
}
.seat-tag {
  font-size: 0.68rem;
  padding: 0.05rem 0.45rem;
  border-radius: var(--radius-pill);
  border: 1px solid var(--border);
  color: var(--fg-dim);
  letter-spacing: 0.04em;
}
.tag-host {
  background: rgba(212, 179, 87, 0.16);
  border-color: rgba(212, 179, 87, 0.4);
  color: var(--race-empire);
}
.tag-you {
  background: rgba(196, 99, 74, 0.16);
  border-color: rgba(196, 99, 74, 0.4);
  color: var(--accent-strong);
}

.controls {
  display: flex;
  align-items: center;
  gap: 0.7rem;
  flex-wrap: wrap;
  padding: 0.75rem;
}
.primary {
  min-height: var(--tap-target);
  padding: 0 1.1rem;
  background: var(--accent);
  border: 1px solid var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-md);
  font-size: 0.9rem;
  cursor: pointer;
}
.primary:hover:not(:disabled) {
  background: var(--accent-strong);
  border-color: var(--accent-strong);
}
.primary:disabled {
  opacity: 0.55;
  cursor: not-allowed;
}
.primary.big {
  font-size: 1rem;
  padding: 0 1.4rem;
  min-height: 48px;
}
.danger {
  min-height: var(--tap-target);
  padding: 0 1.1rem;
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  border-radius: var(--radius-md);
  font-size: 0.85rem;
  cursor: pointer;
}
.danger:hover { color: var(--accent-strong); border-color: var(--accent-strong); }
.ghost {
  min-height: 32px;
  padding: 0 0.65rem;
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  border-radius: var(--radius-md);
  font-size: 0.78rem;
  cursor: pointer;
  align-self: flex-start;
}
.ghost:hover { color: var(--fg); border-color: var(--fg-dim); }
.hint { margin: 0; font-size: 0.78rem; color: var(--fg-faint); }
</style>
