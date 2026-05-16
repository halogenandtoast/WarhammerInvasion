<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { auth } from '../stores/auth'
import { game } from '../stores/game'
import { listDecks, type DeckRecord } from '../api/decks'
import { ApiError } from '../api/client'
import type { LogEntry, Phase, PlayerKey, SeatView } from '../api/protocol'
import SeatBody from './SeatBody'
import PlayView from './PlayView.vue'

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
const logBox = ref<HTMLElement | null>(null)
const copied = ref(false)

const errorBanner = ref<string | null>(null)

const view = computed(() => game.view.value)
const you = computed(() => game.you.value)
const engine = computed(() => game.engine.value)

const isPlaying = computed(() => view.value?.status === 'StatusPlaying' && !!engine.value)
const isWaiting = computed(() => view.value?.status === 'StatusWaiting')

const seat1 = computed<SeatView | null>(() =>
  view.value?.seats.find((s) => s.seat === 'Player1') ?? null,
)

const seat2 = computed<SeatView | null>(() =>
  view.value?.seats.find((s) => s.seat === 'Player2') ?? null,
)

const isHost = computed<boolean>(
  () => !!view.value && !!you.value && view.value.host.userId === you.value.userId,
)

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

// ---- bottom phase bar derivation ----

const PHASES: Phase[] = ['KingdomPhase', 'QuestPhase', 'CapitalPhase', 'BattlefieldPhase']

const mySeatKey = computed<PlayerKey | null>(() => {
  const v = view.value
  const me = you.value
  if (!v || !me) return null
  const row = v.seats.find((s) => s.user.userId === me.userId)
  if (!row) return null
  return row.seat === 'Player1' ? 'Player1' : 'Player2'
})

const turnLabel = computed(() => engine.value ? t('game.play.turn_label', { n: engine.value.turn }) : null)

const activeLabel = computed(() => {
  const e = engine.value
  if (!e) return null
  if (mySeatKey.value === e.currentPlayer) return t('game.play.your_turn')
  const row = view.value?.seats.find((s) => s.seat === e.currentPlayer)
  return t('game.play.opponent_turn', { name: row?.user.displayName ?? e.currentPlayer })
})

const currentPhaseLabel = (p: Phase): string => t(`game.play.phase.${p.replace('Phase', '')}`)

// ---- lifecycle ----

onMounted(async () => {
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

onBeforeUnmount(() => game.disconnect())

watch(
  () => props.gameId,
  (id) => game.connect({ gameId: id, inviteToken: props.inviteToken, password: props.password }),
)

watch(
  () => auth.isAuthenticated.value,
  (v) => {
    if (!v) {
      game.disconnect()
      emit('navigate', '#/login')
    } else {
      // Auth bootstrap completed after this view mounted (refresh on a
      // game URL). The initial connect call bailed because the access
      // token wasn't ready yet — try again now.
      game.connect({
        gameId: props.gameId,
        inviteToken: props.inviteToken,
        password: props.password,
      })
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
    if (game.closed.value) emit('navigate', '#/lobby')
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

watch(
  () => engine.value?.log.length,
  async () => {
    await nextTick()
    const el = logBox.value
    if (el) el.scrollTop = el.scrollHeight
  },
)

const logEntries = computed<LogEntry[]>(() => engine.value?.log ?? [])

// Resolve a player key in a log param to that player's display name,
// pulled from the seat list. Falls back to a generic "Player 1/2"
// label when seats haven't been populated yet (shouldn't happen once
// the engine is live, but it's harmless).
function playerDisplayName(playerKey: string): string {
  const seat = view.value?.seats.find((s) => s.seat === playerKey)
  if (seat) return seat.user.displayName
  if (playerKey === 'Player1') return t('game.seat.heading_player1')
  if (playerKey === 'Player2') return t('game.seat.heading_player2')
  return playerKey
}

// Param keys whose values are themselves i18n keys (enum-shaped). We
// resolve them through a nested t() lookup before passing the params
// to the outer message — that's what keeps engine output decoupled
// from any locale's word order.
const ENUM_PARAM_TABLE: Record<string, string> = {
  phase: 'log.phase_name',
  trigger: 'log.trigger_name',
  reason: 'log.elim_reason',
  // Game-over uses `reason` too, but with a different value space; the
  // engine emits 'OpponentDeckedOut' / 'OpponentCapitalBurned' there,
  // which only resolve under 'log.win_reason'. Try elim first, fall
  // back to win at render time.
}

function resolveParam(name: string, value: string): string {
  if (name === 'player' || name === 'winner') {
    return playerDisplayName(value)
  }
  const base = ENUM_PARAM_TABLE[name]
  if (base) {
    const elim = t(`${base}.${value}`, value)
    if (elim !== value) return elim
    if (name === 'reason') return t(`log.win_reason.${value}`, value)
    return value
  }
  return value
}

function formatLogEntry(entry: LogEntry): string {
  const resolved: Record<string, string> = {}
  for (const [k, v] of Object.entries(entry.params)) {
    resolved[k] = resolveParam(k, v)
  }
  return t(entry.key, resolved, { default: t('log.unknown', { key: entry.key }) })
}

function logEntryKey(entry: LogEntry, i: number): string {
  return `${entry.at}-${i}`
}

function logCategoryClass(cat: LogEntry['category']): string {
  switch (cat) {
    case 'LogTurn': return 'cat-turn'
    case 'LogPhase': return 'cat-phase'
    case 'LogPlayerAction': return 'cat-action'
    case 'LogResult': return 'cat-result'
    default: return 'cat-system'
  }
}

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
    'game_not_started',
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
function clearDeck() { game.clearDeck() }
function start() { game.startGame() }
function leave() {
  if (!confirm(t('game.controls.confirm_leave'))) return
  game.leaveGame()
}
function backToLobby() { emit('navigate', '#/lobby') }
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
    setTimeout(() => { copied.value = false }, 1500)
  } catch {
    // older browsers / permission-denied
  }
}
function formatTime(at: string): string {
  try {
    return new Date(at).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
  } catch { return '' }
}
</script>

<template>
  <main class="game-page" :class="{ playing: isPlaying }">
    <!-- ────────────────────────────────────────────────────────────
         Top bar: slim, sticky. Holds back link, game name, host,
         connection status, and a high-level "what's happening" phrase
         ──────────────────────────────────────────────────────────── -->
    <header class="game-bar">
      <button class="back-link" type="button" @click="backToLobby">
        ← {{ t('game.back_to_lobby') }}
      </button>
      <div class="game-bar-title">
        <strong v-if="view">{{ view.name }}</strong>
        <span v-if="view" class="dim">·</span>
        <span v-if="view" class="dim">{{ t('game.hosted_by', { name: view.host.displayName }) }}</span>
      </div>
      <div class="game-bar-right">
        <span class="status-pill" :data-status="game.status.value">
          <span class="status-dot" aria-hidden="true" />
          {{ t(`lobby.status.${game.status.value}`) }}
        </span>
        <span class="phase-heading">{{ phaseHeading }}</span>
      </div>
    </header>

    <p v-if="errorBanner" class="error" role="alert">{{ errorBanner }}</p>

    <!-- ────────────────────────────────────────────────────────────
         Main area + chat sidebar (two columns on desktop, stacked
         on narrow viewports).
         ──────────────────────────────────────────────────────────── -->
    <section class="game-grid">
      <section class="main-col">
        <!-- Waiting room: seat/deck pickers + start controls -->
        <template v-if="isWaiting">
          <section v-if="view && view.inviteToken" class="invite-strip">
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
        </template>

        <!-- Playing: full-bleed table surface, PlayView splits 50/50 -->
        <template v-else-if="isPlaying && view && view.engine">
          <section class="table-surface">
            <PlayView :engine="view.engine" :seats="view.seats" />
          </section>
        </template>

        <!-- Ended: same surface but no further actions allowed -->
        <template v-else-if="view && view.engine">
          <section class="table-surface ended">
            <PlayView :engine="view.engine" :seats="view.seats" />
          </section>
        </template>
      </section>

      <!-- Side rail: game log on top, chat on bottom. They share the
           same column so wide layouts get both at once, narrow layouts
           stack them under the table surface. -->
      <aside class="side-panel">
        <section class="log-panel" :aria-label="t('game.log.heading')">
          <header class="panel-head">
            <h2>{{ t('game.log.heading') }}</h2>
          </header>
          <div ref="logBox" class="log-scroll">
            <div v-if="logEntries.length === 0" class="log-empty">
              {{ t('game.log.empty') }}
            </div>
            <ol v-else class="log-list" role="log" aria-live="polite">
              <li
                v-for="(entry, i) in logEntries"
                :key="logEntryKey(entry, i)"
                class="log-line"
                :class="logCategoryClass(entry.category)"
              >
                <time class="log-time">{{ formatTime(entry.at) }}</time>
                <p class="log-text">{{ formatLogEntry(entry) }}</p>
              </li>
            </ol>
          </div>
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
      </aside>
    </section>

    <!-- ────────────────────────────────────────────────────────────
         Bottom phase bar (sticky, only while a game is running). Holds
         the turn counter, whose turn it is, and the 4-phase tracker.
         ──────────────────────────────────────────────────────────── -->
    <footer v-if="isPlaying && engine" class="phase-bar">
      <div class="phase-bar-left">
        <span class="turn-label">{{ turnLabel }}</span>
        <span class="active-label">{{ activeLabel }}</span>
      </div>
      <ol class="phase-track">
        <li
          v-for="p in PHASES"
          :key="p"
          class="phase-pip"
          :class="{ active: engine.phase === p }"
        >
          {{ currentPhaseLabel(p) }}
        </li>
      </ol>
      <button class="leave-btn" type="button" @click="leave">
        {{ t('game.controls.leave') }}
      </button>
    </footer>
  </main>
</template>

<style scoped>
.game-page {
  display: grid;
  grid-template-columns: 1fr 320px;
  grid-template-rows: auto auto 1fr auto;
  grid-template-areas:
    "bar bar"
    "err err"
    "main chat"
    "foot foot";
  /* Strict viewport fit — the table doesn't scroll the page. The
     global top nav is 48px tall (see App.vue). */
  height: calc(100dvh - 48px);
  max-height: calc(100dvh - 48px);
  overflow: hidden;
  gap: 0;
}

.game-page > .game-bar { grid-area: bar; }
.game-page > .error { grid-area: err; margin: 0.5rem 0.75rem 0; }
.game-page > .game-grid {
  grid-area: main / main / main / chat;
  display: grid;
  grid-template-columns: 1fr 320px;
  min-height: 0;
  /* Allow the inner main+chat columns to scroll independently. */
  overflow: hidden;
}
.game-page > .phase-bar { grid-area: foot; }

@media (max-width: 880px) {
  .game-page {
    grid-template-columns: 1fr;
    grid-template-areas:
      "bar"
      "err"
      "main"
      "foot";
  }
  .game-page > .game-grid {
    grid-template-columns: 1fr;
    grid-area: main;
  }
}

/* ───────── top game bar ───────── */

.game-bar {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.45rem 0.9rem;
  background: var(--bg-elev);
  border-bottom: 1px solid var(--border);
  min-height: 44px;
  flex-wrap: nowrap;
}

.back-link {
  background: transparent;
  border: none;
  color: var(--fg-dim);
  cursor: pointer;
  padding: 0.2rem 0.3rem;
  font-size: 0.86rem;
}
.back-link:hover { color: var(--fg); }

.game-bar-title {
  display: flex;
  align-items: baseline;
  gap: 0.45rem;
  min-width: 0;
  flex: 1;
}
.game-bar-title strong {
  font-size: 0.98rem;
  font-weight: 600;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.game-bar-title .dim { color: var(--fg-faint); font-size: 0.82rem; }

.game-bar-right {
  display: flex;
  align-items: center;
  gap: 0.6rem;
  flex-shrink: 0;
}

.status-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.35rem;
  height: 24px;
  padding: 0 0.55rem;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-pill);
  color: var(--fg-dim);
  font-size: 0.74rem;
}
.status-dot {
  width: 7px;
  height: 7px;
  border-radius: 50%;
  background: var(--fg-faint);
}
.status-pill[data-status='open'] .status-dot { background: #5da46a; }
.status-pill[data-status='connecting'] .status-dot { background: #d8b66c; animation: pulse 1.2s infinite; }
.status-pill[data-status='closed'] .status-dot { background: var(--accent-strong); }

.phase-heading {
  font-size: 0.78rem;
  color: var(--fg-faint);
  letter-spacing: 0.06em;
  text-transform: uppercase;
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.35; }
}

.error {
  padding: 0.55rem 0.9rem;
  background: var(--bg-elev);
  border-left: 3px solid var(--accent-strong);
  border-radius: var(--radius-sm);
}

/* ───────── grid layout ───────── */

.main-col {
  min-width: 0;
  min-height: 0;
  display: flex;
  flex-direction: column;
  /* Waiting-state content (invite + seats + controls) may exceed the
     viewport on short screens — let it scroll. The playing-state
     '.table-surface' below is overflow:hidden so the table itself
     never produces a scrollbar. */
  overflow-y: auto;
}

/* ───────── playing state ───────── */

.table-surface {
  flex: 1;
  min-height: 0;
  background:
    radial-gradient(ellipse at center, rgba(60, 40, 20, 0.6) 0%, rgba(20, 14, 8, 0.85) 80%),
    repeating-linear-gradient(
      45deg,
      rgba(40, 28, 16, 0.25) 0 4px,
      rgba(30, 22, 12, 0.25) 4px 8px
    );
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

/* ───────── waiting state ───────── */

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

/* ───────── side rail (log + chat) ───────── */

.side-panel {
  background: var(--bg-elev);
  border-left: 1px solid var(--border);
  display: grid;
  /* Log gets a generous share at the top; chat keeps a tighter slice
     since most exchanges are short. minmax(0, …) keeps both children
     scrollable rather than letting one push the other off-screen. */
  grid-template-rows: minmax(0, 1.4fr) minmax(0, 1fr);
  min-height: 0;
  min-width: 0;
}
@media (max-width: 880px) {
  .side-panel {
    border-left: none;
    border-top: 1px solid var(--border);
    min-height: 320px;
  }
}

.log-panel,
.chat-panel {
  display: flex;
  flex-direction: column;
  min-height: 0;
  min-width: 0;
}
.chat-panel { border-top: 1px solid var(--border); }

.panel-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.55rem 0.85rem;
  border-bottom: 1px solid var(--border);
  flex-shrink: 0;
}
.panel-head h2 { margin: 0; font-size: 0.88rem; font-weight: 600; }

/* ─── game log ─── */
.log-scroll {
  flex: 1;
  overflow-y: auto;
  padding: 0.5rem 0.75rem;
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  min-height: 0;
  font-size: 0.82rem;
  line-height: 1.35;
}
.log-empty { margin: auto; color: var(--fg-faint); font-style: italic; }
.log-list { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.2rem; }
.log-line {
  display: grid;
  grid-template-columns: 3rem 1fr;
  column-gap: 0.45rem;
  align-items: baseline;
  border-left: 2px solid transparent;
  padding-left: 0.4rem;
  color: var(--fg-dim);
}
.log-time { color: var(--fg-faint); font-size: 0.68rem; font-variant-numeric: tabular-nums; }
.log-text { margin: 0; word-break: break-word; }
.log-line.cat-turn   { border-left-color: var(--accent); color: var(--fg); font-weight: 600; }
.log-line.cat-phase  { border-left-color: var(--race-empire, #d4b357); color: var(--fg); }
.log-line.cat-action { border-left-color: var(--accent-strong, #c4634a); color: var(--fg); }
.log-line.cat-result { border-left-color: var(--accent-strong, #c4634a); color: var(--fg); font-weight: 600; }
.log-line.cat-system { /* default colors */ }

/* ─── chat ─── */
.chat-scroll {
  flex: 1;
  overflow-y: auto;
  padding: 0.65rem 0.85rem;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  min-height: 0;
}
.chat-empty { margin: auto; color: var(--fg-faint); font-style: italic; }
.chat-list { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.55rem; }
.chat-line { display: flex; flex-direction: column; gap: 0.1rem; }
.chat-meta { display: flex; align-items: baseline; gap: 0.4rem; }
.chat-author { font-weight: 600; font-size: 0.86rem; }
.chat-time { color: var(--fg-faint); font-size: 0.7rem; }
.chat-text { margin: 0; font-size: 0.9rem; line-height: 1.4; word-break: break-word; }
.chat-input {
  display: flex;
  gap: 0.5rem;
  padding: 0.55rem 0.7rem;
  border-top: 1px solid var(--border);
  background: var(--bg);
  flex-shrink: 0;
}
.chat-input input {
  flex: 1;
  min-height: var(--tap-target);
  padding: 0.4rem 0.7rem;
  background: var(--bg-elev);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.9rem;
}
.chat-input input:focus-visible {
  outline: 2px solid var(--accent-strong);
  border-color: var(--accent-strong);
}

/* ───────── bottom phase bar ───────── */

.phase-bar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 0.8rem;
  padding: 0.45rem 0.9rem;
  background: var(--bg-elev);
  border-top: 1px solid var(--border);
  min-height: 48px;
  flex-wrap: wrap;
}
.phase-bar-left {
  display: flex;
  align-items: baseline;
  gap: 0.65rem;
}
.turn-label {
  font-size: 0.72rem;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: var(--fg-faint);
}
.active-label {
  font-size: 0.95rem;
  font-weight: 600;
  color: var(--fg);
}
.phase-track {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  gap: 0.3rem;
  flex-wrap: wrap;
}
.phase-pip {
  font-size: 0.72rem;
  letter-spacing: 0.06em;
  padding: 0.3rem 0.7rem;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-pill);
  color: var(--fg-faint);
}
.phase-pip.active {
  color: var(--on-accent);
  background: var(--accent);
  border-color: var(--accent);
}
.leave-btn {
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  padding: 0.35rem 0.7rem;
  border-radius: var(--radius-md);
  font-size: 0.8rem;
  cursor: pointer;
}
.leave-btn:hover { color: var(--accent-strong); border-color: var(--accent-strong); }
</style>
