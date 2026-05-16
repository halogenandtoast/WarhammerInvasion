<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { auth } from '../stores/auth'
import { game } from '../stores/game'
import { listDecks, type DeckRecord } from '../api/decks'
import { ApiError } from '../api/client'
import type { SeatView } from '../api/protocol'
import SeatBody from './SeatBody'

const props = defineProps<{
  gameId: string
  inviteToken: string | null
  password: string | null
}>()

const emit = defineEmits<{ (e: 'navigate', target: string): void }>()

const { t } = useI18n({ useScope: 'global' })

const decks = ref<DeckRecord[]>([])
const decksLoading = ref(true)
const decksError = ref<string | null>(null)

const chatText = ref('')
const chatBox = ref<HTMLElement | null>(null)
const copied = ref(false)

const errorBanner = ref<string | null>(null)

const view = computed(() => game.view.value)
const you = computed(() => game.you.value)

const seat1 = computed<SeatView | null>(() => {
  const v = view.value
  if (!v) return null
  return v.seats.find((s) => s.seat === 'Player1') ?? null
})

const seat2 = computed<SeatView | null>(() => {
  const v = view.value
  if (!v) return null
  return v.seats.find((s) => s.seat === 'Player2') ?? null
})

const isHost = computed<boolean>(() => {
  const v = view.value
  const me = you.value
  return Boolean(v && me && v.host.userId === me.userId)
})

const bothReady = computed<boolean>(() => {
  const v = view.value
  if (!v) return false
  return v.seats.length === 2 && v.seats.every((s) => s.deck !== null)
})

const startDisabledReason = computed<string | null>(() => {
  const v = view.value
  if (!v) return null
  if (v.status !== 'StatusWaiting') return t('game.controls.start_disabled_started')
  if (!isHost.value) return t('game.controls.start_disabled_not_host')
  if (!bothReady.value) return t('game.controls.start_disabled_not_ready')
  return null
})

const phaseHeading = computed<string>(() => {
  const v = view.value
  if (!v) return t('game.waiting_heading')
  switch (v.status) {
    case 'StatusPlaying':
      return t('game.playing_heading')
    case 'StatusEnded':
      return t('game.ended_heading')
    default:
      return t('game.waiting_heading')
  }
})

const inviteUrl = computed<string | null>(() => {
  const v = view.value
  if (!v || !v.inviteToken) return null
  const path = `${window.location.pathname}${window.location.search}#/games/${v.gameId}?t=${encodeURIComponent(v.inviteToken)}`
  return `${window.location.origin}${path}`
})

onMounted(async () => {
  // Connect the per-game socket. The lobby socket stays alive in the
  // background so we can broadcast updates back.
  game.connect({
    gameId: props.gameId,
    inviteToken: props.inviteToken,
    password: props.password,
  })

  decksLoading.value = true
  try {
    decks.value = await listDecks()
  } catch (e) {
    decksError.value = e instanceof ApiError ? e.code : 'load_failed'
  } finally {
    decksLoading.value = false
  }
})

onBeforeUnmount(() => {
  game.disconnect()
})

watch(
  () => props.gameId,
  (id) => {
    game.connect({
      gameId: id,
      inviteToken: props.inviteToken,
      password: props.password,
    })
  },
)

watch(
  () => auth.isAuthenticated.value,
  (v) => {
    if (!v) {
      game.disconnect()
      emit('navigate', '#/login')
    }
  },
)

watch(
  () => game.lastError.value?.at,
  () => {
    const code = game.lastError.value?.code
    if (!code) return
    errorBanner.value = mapError(code)
  },
)

watch(
  () => game.closed.value?.at,
  () => {
    const c = game.closed.value
    if (!c) return
    emit('navigate', '#/lobby')
  },
)

watch(
  () => view.value?.chat.length,
  async () => {
    await nextTick()
    const el = chatBox.value
    if (el) el.scrollTop = el.scrollHeight
  },
)

function mapError(code: string): string {
  const known = [
    'not_seated',
    'deck_not_found',
    'deck_not_owned',
    'not_host',
    'not_ready',
    'already_started',
    'forbidden',
    'game_started',
  ]
  if (known.includes(code)) return t(`game.errors.${code}`, code)
  return code
}

function seatTitle(seat: 'Player1' | 'Player2'): string {
  return seat === 'Player1' ? t('game.seat.heading_player1') : t('game.seat.heading_player2')
}

function selectDeck(e: Event) {
  const v = (e.target as HTMLSelectElement).value
  if (v) game.selectDeck(v)
}

function clearDeck() {
  game.clearDeck()
}

function start() {
  game.startGame()
}

function leave() {
  if (!confirm(t('game.controls.confirm_leave'))) return
  game.leaveGame()
}

function backToLobby() {
  emit('navigate', '#/lobby')
}

function sendChat() {
  const text = chatText.value.trim()
  if (!text) return
  game.sendChat(text)
  chatText.value = ''
}

async function copyInvite() {
  if (!inviteUrl.value) return
  try {
    await navigator.clipboard.writeText(inviteUrl.value)
    copied.value = true
    setTimeout(() => {
      copied.value = false
    }, 1500)
  } catch {
    // ignore — older browsers / permission-denied
  }
}

function formatTime(at: string): string {
  try {
    const d = new Date(at)
    return d.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
  } catch {
    return ''
  }
}
</script>

<template>
  <main class="game-page">
    <header class="page-head">
      <div>
        <button class="ghost-link" type="button" @click="backToLobby">
          ← {{ t('game.back_to_lobby') }}
        </button>
        <p class="eyebrow">{{ t('game.eyebrow') }}</p>
        <h1 v-if="view">{{ view.name }}</h1>
        <h1 v-else>…</h1>
        <p v-if="view" class="lead">{{ t('game.hosted_by', { name: view.host.displayName }) }}</p>
      </div>
      <div class="head-right">
        <div class="status-pill" :data-status="game.status.value">
          <span class="status-dot" aria-hidden="true" />
          {{ t(`lobby.status.${game.status.value}`) }}
        </div>
        <p class="phase-heading">{{ phaseHeading }}</p>
      </div>
    </header>

    <p v-if="errorBanner" class="error" role="alert">{{ errorBanner }}</p>

    <section v-if="view && view.status === 'StatusWaiting' && view.inviteToken" class="invite-strip">
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
      <article class="seat" :class="{ filled: !!seat1, you: seat1 && you && seat1.user.userId === you.userId }">
        <header>
          <h2>{{ seatTitle('Player1') }}</h2>
          <span v-if="seat1?.isHost" class="seat-tag tag-host">{{ t('game.seat.host_tag') }}</span>
          <span v-if="seat1 && you && seat1.user.userId === you.userId" class="seat-tag tag-you">
            {{ t('game.seat.you_tag') }}
          </span>
        </header>
        <SeatBody
          :seat="seat1"
          :you="you"
          :decks="decks"
          :decks-loading="decksLoading"
          :decks-error="decksError"
          :game-status="view?.status ?? 'StatusWaiting'"
          @select="selectDeck"
          @clear="clearDeck"
        />
      </article>
      <article class="seat" :class="{ filled: !!seat2, you: seat2 && you && seat2.user.userId === you.userId }">
        <header>
          <h2>{{ seatTitle('Player2') }}</h2>
          <span v-if="seat2?.isHost" class="seat-tag tag-host">{{ t('game.seat.host_tag') }}</span>
          <span v-if="seat2 && you && seat2.user.userId === you.userId" class="seat-tag tag-you">
            {{ t('game.seat.you_tag') }}
          </span>
        </header>
        <SeatBody
          :seat="seat2"
          :you="you"
          :decks="decks"
          :decks-loading="decksLoading"
          :decks-error="decksError"
          :game-status="view?.status ?? 'StatusWaiting'"
          @select="selectDeck"
          @clear="clearDeck"
        />
      </article>
    </section>

    <section class="controls">
      <button
        class="primary big"
        type="button"
        :disabled="startDisabledReason !== null"
        :title="startDisabledReason ?? undefined"
        @click="start"
      >
        {{ t('game.controls.start') }}
      </button>
      <p v-if="startDisabledReason" class="hint">{{ startDisabledReason }}</p>

      <button class="danger" type="button" @click="leave">
        {{ t('game.controls.leave') }}
      </button>
    </section>

    <section class="chat-panel" :aria-label="t('game.chat.heading')">
      <header class="panel-head">
        <h2>{{ t('game.chat.heading') }}</h2>
      </header>
      <div ref="chatBox" class="chat-scroll">
        <div v-if="!view || view.chat.length === 0" class="chat-empty">
          {{ t('game.chat.empty') }}
        </div>
        <ul v-else class="chat-list" role="log" aria-live="polite">
          <li v-for="(line, i) in view.chat" :key="`${line.at}-${i}`" class="chat-line">
            <div class="chat-meta">
              <span class="chat-author">{{ line.from.displayName }}</span>
              <time class="chat-time">{{ formatTime(line.at) }}</time>
            </div>
            <p class="chat-text">{{ line.text }}</p>
          </li>
        </ul>
      </div>
      <form class="chat-input" @submit.prevent="sendChat">
        <input
          v-model="chatText"
          type="text"
          maxlength="1000"
          :placeholder="t('game.chat.placeholder')"
          :aria-label="t('game.chat.placeholder')"
        />
        <button class="primary" type="submit" :disabled="!chatText.trim()">
          {{ t('game.chat.send') }}
        </button>
      </form>
    </section>
  </main>
</template>

<style scoped>
.game-page {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem 1.25rem 4rem;
  display: grid;
  gap: 1.1rem;
}

.page-head {
  display: flex;
  align-items: flex-end;
  justify-content: space-between;
  gap: 1rem;
  flex-wrap: wrap;
}

.eyebrow {
  margin: 0;
  font-size: 0.78rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--accent-strong);
}

.ghost-link {
  background: transparent;
  border: none;
  color: var(--fg-dim);
  cursor: pointer;
  padding: 0 0 0.3rem;
  font-size: 0.85rem;
}

.ghost-link:hover {
  color: var(--fg);
}

h1 {
  margin: 0.35rem 0 0.2rem;
  font-size: clamp(1.4rem, 3vw, 1.9rem);
  word-break: break-word;
}

.lead {
  margin: 0;
  color: var(--fg-dim);
}

.head-right {
  display: flex;
  flex-direction: column;
  align-items: flex-end;
  gap: 0.4rem;
}

.status-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.4rem;
  height: 26px;
  padding: 0 0.65rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-pill);
  color: var(--fg-dim);
  font-size: 0.78rem;
}

.status-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--fg-faint);
}

.status-pill[data-status='open'] .status-dot { background: #5da46a; }
.status-pill[data-status='connecting'] .status-dot {
  background: #d8b66c;
  animation: pulse 1.2s infinite;
}
.status-pill[data-status='closed'] .status-dot { background: var(--accent-strong); }

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.35; }
}

.phase-heading {
  margin: 0;
  font-size: 0.78rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.error {
  margin: 0;
  padding: 0.6rem 0.9rem;
  background: var(--bg-elev);
  border-left: 3px solid var(--accent-strong);
  border-radius: var(--radius-sm);
}

.invite-strip {
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

.invite-row {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

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

.invite-pw {
  margin: 0;
  font-size: 0.82rem;
  color: var(--fg-dim);
}

.invite-pw.faint {
  color: var(--fg-faint);
}

.seats {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0.9rem;
}

@media (max-width: 720px) {
  .seats {
    grid-template-columns: 1fr;
  }
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

.seat header {
  display: flex;
  align-items: center;
  gap: 0.45rem;
}

.seat header h2 {
  margin: 0;
  font-size: 0.82rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.seat.you {
  border-color: var(--accent);
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

.seat-empty {
  margin: auto;
  color: var(--fg-faint);
  font-style: italic;
  padding: 1.2rem 0;
}

.seat-user {
  display: flex;
  align-items: center;
  gap: 0.45rem;
}

.seat-user-name {
  font-size: 1.05rem;
  font-weight: 600;
}

.deck-summary {
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
}

.deck-name {
  margin: 0;
  font-weight: 600;
}

.deck-line {
  margin: 0;
  font-size: 0.85rem;
  color: var(--fg-dim);
}

.deck-pending {
  margin: 0;
  color: var(--fg-faint);
  font-style: italic;
}

.deck-select {
  min-height: var(--tap-target);
  padding: 0.4rem 0.6rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.9rem;
}

.controls {
  display: flex;
  align-items: center;
  gap: 0.7rem;
  flex-wrap: wrap;
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

.danger:hover {
  color: var(--accent-strong);
  border-color: var(--accent-strong);
}

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

.ghost:hover {
  color: var(--fg);
  border-color: var(--fg-dim);
}

@media (pointer: coarse) {
  .ghost {
    min-height: var(--tap-target);
  }
}

.hint {
  margin: 0;
  font-size: 0.78rem;
  color: var(--fg-faint);
}

.panel-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.65rem 0.95rem;
  border-bottom: 1px solid var(--border);
}

.panel-head h2 {
  margin: 0;
  font-size: 0.9rem;
}

.chat-panel {
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  display: flex;
  flex-direction: column;
  min-height: 320px;
}

.chat-scroll {
  flex: 1;
  overflow-y: auto;
  padding: 0.75rem 0.95rem;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.chat-empty {
  margin: auto;
  color: var(--fg-faint);
  font-style: italic;
}

.chat-list {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.55rem;
}

.chat-line {
  display: flex;
  flex-direction: column;
  gap: 0.1rem;
}

.chat-meta {
  display: flex;
  align-items: baseline;
  gap: 0.4rem;
}

.chat-author {
  font-weight: 600;
  font-size: 0.88rem;
}

.chat-time {
  color: var(--fg-faint);
  font-size: 0.72rem;
}

.chat-text {
  margin: 0;
  font-size: 0.92rem;
  line-height: 1.4;
  word-break: break-word;
}

.chat-input {
  display: flex;
  gap: 0.5rem;
  padding: 0.6rem 0.75rem;
  border-top: 1px solid var(--border);
  background: var(--bg);
  border-radius: 0 0 var(--radius-lg) var(--radius-lg);
}

.chat-input input {
  flex: 1;
  min-height: var(--tap-target);
  padding: 0.4rem 0.7rem;
  background: var(--bg-elev);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.92rem;
}

.chat-input input:focus-visible {
  outline: 2px solid var(--accent-strong);
  border-color: var(--accent-strong);
}
</style>
