<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { auth } from '../stores/auth'
import { game } from '../stores/game'
import { listDecks, type DeckRecord } from '../api/decks'
import { ApiError } from '../api/client'
import type {
  ActionWindowTrigger,
  ChatLine,
  LogEntry,
  Phase,
  PlayerKey,
  Race,
  SeatView,
} from '../api/protocol'
import { priorityHolder } from '../api/protocol'
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
const messagesBox = ref<HTMLElement | null>(null)
const copied = ref(false)

const errorBanner = ref<string | null>(null)

const view = computed(() => game.view.value)
const you = computed(() => game.you.value)
const engine = computed(() => game.engine.value)
// Guest = signed-out viewer. They land here via the lobby's
// "Spectate" button on a public spectator-friendly game. The server
// rejects all `GameIn` actions from guests, so the UI hides those
// affordances rather than presenting a button that silently fails.
const isGuest = computed(() => auth.ready.value && you.value === null)

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

// True when you're not occupying either seat. Includes guests, who can
// never be seated. Host watching their own (unstarted) game still
// counts as seated only if they're in the seat list — see `mySeatKey`.
const isSpectator = computed<boolean>(() => {
  const v = view.value
  if (!v) return false
  const me = you.value
  if (!me) return true
  return !v.seats.some((s) => s.user.userId === me.userId)
})

const spectatorCount = computed<number>(() => view.value?.spectatorCount ?? 0)

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

// ---- action window state (woven into the bottom phase-pip strip) ----

const aw = computed(() => engine.value?.actionWindow ?? null)

const windowTriggerLabel = computed(() =>
  aw.value ? t(`game.play.window.trigger.${aw.value.trigger satisfies ActionWindowTrigger}`) : null,
)

// Which phase pip the open window belongs to. Every combat sub-step
// trigger lives inside the Battlefield phase.
const TRIGGER_TO_PHASE: Record<ActionWindowTrigger, Phase> = {
  KingdomActionWindow: 'KingdomPhase',
  QuestActionWindow: 'QuestPhase',
  CapitalActionWindow: 'CapitalPhase',
  BattlefieldActionWindow: 'BattlefieldPhase',
  AfterDeclareCombatTarget: 'BattlefieldPhase',
  AfterDeclareAttackers: 'BattlefieldPhase',
  AfterDeclareDefenders: 'BattlefieldPhase',
  AfterAssignCombatDamage: 'BattlefieldPhase',
  AfterApplyCombatDamage: 'BattlefieldPhase',
}
const awPhase = computed<Phase | null>(() =>
  aw.value ? TRIGGER_TO_PHASE[aw.value.trigger] : null,
)

const priorityIsMe = computed(
  () => aw.value != null && mySeatKey.value != null && priorityHolder(aw.value.awaiting) === mySeatKey.value,
)

// Pip text: when the open window belongs to this pip, swap the phase
// name for the more specific trigger label so combat sub-steps surface.
function pipLabel(p: Phase): string {
  if (awPhase.value === p && windowTriggerLabel.value) return windowTriggerLabel.value
  return currentPhaseLabel(p)
}

function pass() {
  if (priorityIsMe.value) game.passPriority()
}

// ---- lifecycle ----

onMounted(async () => {
  game.connect({
    gameId: props.gameId,
    inviteToken: props.inviteToken,
    password: props.password,
  })

  // Guests can't load decks (the API is auth-gated) and can't take a
  // seat anyway, so don't bother fetching. Drop the loading flag so
  // the SeatBody doesn't get stuck on its "Loading decks…" line.
  if (auth.isAuthenticated.value) {
    decksLoading.value = true
    try {
      decks.value = await listDecks()
    } catch (e) {
      decksError.value = e instanceof ApiError ? e.code : 'load_failed'
    } finally {
      decksLoading.value = false
    }
  } else {
    decksLoading.value = false
  }
})

onBeforeUnmount(() => game.disconnect())

watch(
  () => props.gameId,
  (id) => game.connect({ gameId: id, inviteToken: props.inviteToken, password: props.password }),
)

// Reconnect whenever the auth identity changes. The first firing
// covers the cold bootstrap (ready flips false→true) so the initial
// `game.connect` in onMounted bails harmlessly and this watcher opens
// the real socket once we know whether we're authed or a guest.
watch(
  () => [auth.ready.value, auth.accessToken.value] as const,
  () => {
    game.disconnect()
    game.connect({
      gameId: props.gameId,
      inviteToken: props.inviteToken,
      password: props.password,
    })
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

const logEntries = computed<LogEntry[]>(() => engine.value?.log ?? [])
const chatLines = computed<ChatLine[]>(() => view.value?.chat ?? [])

// Merge chat + log into a single chronologically-ordered stream so the
// side rail reads as one conversation. ISO-8601 timestamps sort
// lexicographically, so a stable string sort is sufficient.
type Message =
  | { kind: 'log'; at: string; entry: LogEntry }
  | { kind: 'chat'; at: string; line: ChatLine }

const messages = computed<Message[]>(() => {
  const out: Message[] = []
  for (const entry of logEntries.value) out.push({ kind: 'log', at: entry.at, entry })
  for (const line of chatLines.value) out.push({ kind: 'chat', at: line.at, line })
  out.sort((a, b) => (a.at < b.at ? -1 : a.at > b.at ? 1 : 0))
  return out
})

watch(
  () => messages.value.length,
  async () => {
    await nextTick()
    const el = messagesBox.value
    if (el) el.scrollTop = el.scrollHeight
  },
)

// Map a chat author's userId to their seat so the renderer can tint
// each player's messages distinctly. Returns null for system/unknown
// authors (e.g. spectators down the line) so they fall through to the
// neutral chat styling.
function seatOf(userId: string): PlayerKey | null {
  const seat = view.value?.seats.find((s) => s.user.userId === userId)
  if (!seat) return null
  return seat.seat === 'Player1' ? 'Player1' : 'Player2'
}

const RACE_SLUG: Record<Race, string> = {
  Empire: 'empire',
  Dwarf: 'dwarf',
  HighElf: 'high-elf',
  Chaos: 'chaos',
  Orc: 'orc',
  DarkElf: 'dark-elf',
}

// CSS class for chat/seat tinting derived from the player's actual
// race. Null when the speaker isn't a seated player or the engine
// hasn't sent a snapshot yet — caller falls back to neutral styling.
function raceClassOf(userId: string): string | null {
  const seat = seatOf(userId)
  if (!seat || !engine.value) return null
  const player = seat === 'Player1' ? engine.value.player1 : engine.value.player2
  return `race-${RACE_SLUG[player.race]}`
}

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
    'card_unknown',
    'zone_required',
    'target_required',
    'unauthorized',
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
  // Spectators aren't seated, so leaving doesn't end the game — just
  // navigate back. Confirming would be misleading ("ends the game for
  // both players").
  if (isSpectator.value) {
    emit('navigate', '#/lobby')
    return
  }
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
        <span
          v-if="spectatorCount > 0"
          class="spec-pill"
          :title="t('game.spectators.tooltip', { n: spectatorCount })"
        >
          <span class="spec-eye" aria-hidden="true">👁</span>
          {{ t('game.spectators.count', { n: spectatorCount }) }}
        </span>
        <span v-if="isSpectator" class="spec-tag">{{ t('game.spectators.you_tag') }}</span>
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
            <template v-if="!isSpectator">
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
            </template>
            <p v-else class="hint">{{ t('game.spectators.waiting_note') }}</p>

            <button class="danger" type="button" @click="leave">
              {{ isSpectator ? t('game.spectators.leave') : t('game.controls.leave') }}
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

      <!-- Side rail: one merged stream of game-log entries and player
           chat, sorted chronologically. Chat lines are highlighted so
           they stand out against the engine's transcript. -->
      <aside class="side-panel">
        <section class="messages-panel" :aria-label="t('game.messages.heading')">
          <header class="panel-head">
            <h2>{{ t('game.messages.heading') }}</h2>
          </header>
          <div ref="messagesBox" class="messages-scroll">
            <div v-if="messages.length === 0" class="messages-empty">
              {{ t('game.messages.empty') }}
            </div>
            <ol v-else class="messages-list" role="log" aria-live="polite">
              <template v-for="(msg, i) in messages" :key="`${msg.at}-${i}`">
                <li
                  v-if="msg.kind === 'log'"
                  class="log-line"
                  :class="logCategoryClass(msg.entry.category)"
                >
                  <time class="log-time">{{ formatTime(msg.entry.at) }}</time>
                  <p class="log-text">{{ formatLogEntry(msg.entry) }}</p>
                </li>
                <li
                  v-else
                  class="chat-line"
                  :class="raceClassOf(msg.line.from.userId)"
                >
                  <div class="chat-meta">
                    <span class="chat-author">{{ msg.line.from.displayName }}</span>
                    <time class="chat-time">{{ formatTime(msg.line.at) }}</time>
                  </div>
                  <p class="chat-text">{{ msg.line.text }}</p>
                </li>
              </template>
            </ol>
          </div>
          <form v-if="!isGuest" class="chat-input" @submit.prevent="sendChat">
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
          <div v-else class="chat-input chat-input-guest">
            <p class="guest-cta-text">{{ t('game.chat.guest_cta') }}</p>
            <button class="primary" type="button" @click="emit('navigate', '#/login')">
              {{ t('app.nav.login') }}
            </button>
          </div>
        </section>
      </aside>
    </section>

    <!-- ────────────────────────────────────────────────────────────
         Bottom phase bar (sticky, only while a game is running). Holds
         the turn counter, whose turn it is, the 4-phase tracker, and
         the active action-window pill with the pass button.
         ──────────────────────────────────────────────────────────── -->
    <footer v-if="isPlaying && engine" class="phase-bar">
      <div class="phase-bar-left">
        <span class="turn-label" :title="turnLabel ?? undefined">
          {{ t('game.play.turn_prefix') }}
          <span class="turn-number-slot">
            <Transition name="turn-flip">
              <span class="turn-number" :key="engine.turn">{{ engine.turn }}</span>
            </Transition>
          </span>
        </span>
        <span class="active-label">{{ activeLabel }}</span>
      </div>
      <ol class="phase-track">
        <li
          v-for="p in PHASES"
          :key="p"
          class="phase-pip"
          :class="{
            active: engine.phase === p,
            'window-open': awPhase === p,
            mine: awPhase === p && priorityIsMe,
          }"
        >
          <span class="pip-label">{{ pipLabel(p) }}</span>
          <button
            v-if="awPhase === p"
            class="pip-pass"
            type="button"
            :disabled="!priorityIsMe"
            :title="!priorityIsMe ? t('game.play.window.pass_disabled') : undefined"
            @click.stop="pass"
          >
            {{ t('game.play.window.pass') }}
          </button>
        </li>
      </ol>
      <button class="leave-btn" type="button" @click="leave">
        {{ isSpectator ? t('game.spectators.leave') : t('game.controls.leave') }}
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
    /* Give the table surface the bulk of the height and let the
       messages rail take only as much as it needs (capped below). */
    grid-template-rows: minmax(0, 1fr) auto;
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

.spec-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.3rem;
  height: 24px;
  padding: 0 0.55rem;
  background: rgba(95, 160, 214, 0.10);
  border: 1px solid rgba(95, 160, 214, 0.30);
  border-radius: var(--radius-pill);
  color: var(--race-high-elf, #5fa0d6);
  font-size: 0.74rem;
  font-variant-numeric: tabular-nums;
}
.spec-eye { font-size: 0.85rem; line-height: 1; }

.spec-tag {
  display: inline-flex;
  align-items: center;
  height: 24px;
  padding: 0 0.55rem;
  background: rgba(212, 179, 87, 0.16);
  border: 1px solid rgba(212, 179, 87, 0.4);
  border-radius: var(--radius-pill);
  color: var(--race-empire, #d4b357);
  font-size: 0.72rem;
  letter-spacing: 0.06em;
  text-transform: uppercase;
}

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

/* ───────── side rail (merged messages stream) ───────── */

.side-panel {
  background: var(--bg-elev);
  border-left: 1px solid var(--border);
  display: flex;
  flex-direction: column;
  min-height: 0;
  min-width: 0;
}
@media (max-width: 880px) {
  .side-panel {
    border-left: none;
    border-top: 1px solid var(--border);
    /* Keep the messages rail compact on narrow screens — the table is
       the focus; the rail is just enough to see recent activity and
       reach the chat input. */
    min-height: 140px;
    max-height: 28dvh;
  }
}

.messages-panel {
  display: flex;
  flex-direction: column;
  flex: 1;
  min-height: 0;
  min-width: 0;
}

.panel-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.55rem 0.85rem;
  border-bottom: 1px solid var(--border);
  flex-shrink: 0;
}
.panel-head h2 { margin: 0; font-size: 0.88rem; font-weight: 600; }

.messages-scroll {
  flex: 1;
  overflow-y: auto;
  padding: 0.5rem 0.75rem;
  min-height: 0;
}
.messages-empty {
  margin: auto;
  color: var(--fg-faint);
  font-style: italic;
  text-align: center;
  padding: 1rem 0;
}
.messages-list {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  font-size: 0.82rem;
  line-height: 1.35;
}

/* ─── log entries (engine transcript) ─── */
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

/* ─── chat lines (player messages) ─── */
/* Player chat needs to read as "someone is talking" against the
   engine's terse transcript, so it gets its own card-style block with
   a tinted background, accent rule, and slightly larger body text.
   The race-* overrides below paint the line in the speaker's faction
   palette — hue + tint + ring + a brighter author shade — so the two
   players are visually separable at a glance and tied to their race. */
.chat-line {
  --chat-hue: var(--accent);
  --chat-author: var(--accent-strong);
  --chat-bg: rgba(196, 99, 74, 0.10);
  --chat-ring: rgba(196, 99, 74, 0.18);
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
  padding: 0.45rem 0.6rem;
  margin: 0.25rem 0;
  background: var(--chat-bg);
  border-left: 3px solid var(--chat-hue);
  border-radius: var(--radius-sm);
  box-shadow: 0 0 0 1px var(--chat-ring);
}
.chat-line.race-empire {
  --chat-hue: var(--race-empire);
  --chat-author: var(--race-empire-strong);
  --chat-bg: var(--race-empire-tint);
  --chat-ring: var(--race-empire-ring);
}
.chat-line.race-dwarf {
  --chat-hue: var(--race-dwarf);
  --chat-author: var(--race-dwarf-strong);
  --chat-bg: var(--race-dwarf-tint);
  --chat-ring: var(--race-dwarf-ring);
}
.chat-line.race-high-elf {
  --chat-hue: var(--race-high-elf);
  --chat-author: var(--race-high-elf-strong);
  --chat-bg: var(--race-high-elf-tint);
  --chat-ring: var(--race-high-elf-ring);
}
.chat-line.race-chaos {
  --chat-hue: var(--race-chaos);
  --chat-author: var(--race-chaos-strong);
  --chat-bg: var(--race-chaos-tint);
  --chat-ring: var(--race-chaos-ring);
}
.chat-line.race-orc {
  --chat-hue: var(--race-orc);
  --chat-author: var(--race-orc-strong);
  --chat-bg: var(--race-orc-tint);
  --chat-ring: var(--race-orc-ring);
}
.chat-line.race-dark-elf {
  --chat-hue: var(--race-dark-elf);
  --chat-author: var(--race-dark-elf-strong);
  --chat-bg: var(--race-dark-elf-tint);
  --chat-ring: var(--race-dark-elf-ring);
}
.chat-line .chat-author { color: var(--chat-author); }
.chat-meta { display: flex; align-items: baseline; gap: 0.4rem; }
.chat-author {
  font-weight: 600;
  font-size: 0.86rem;
  color: var(--fg);
}
.chat-time { color: var(--fg-faint); font-size: 0.7rem; font-variant-numeric: tabular-nums; }
.chat-text {
  margin: 0;
  font-size: 0.9rem;
  line-height: 1.4;
  word-break: break-word;
  color: var(--fg);
}
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

.chat-input-guest {
  align-items: center;
  justify-content: space-between;
  gap: 0.55rem;
}
.guest-cta-text {
  margin: 0;
  color: var(--fg-dim);
  font-size: 0.82rem;
}

/* ───────── bottom phase bar ───────── */

.phase-bar {
  display: flex;
  align-items: center;
  /* Left-anchor the turn indicator + phase track; the leave button
     pushes itself to the right edge via `margin-left: auto`. */
  justify-content: flex-start;
  gap: 0.6rem;
  padding: 0.2rem 0.9rem;
  background: var(--bg-elev);
  border-top: 1px solid var(--border);
  min-height: 34px;
  flex-wrap: wrap;
}
.phase-bar-left {
  display: flex;
  align-items: baseline;
  gap: 0.55rem;
}
.turn-label {
  font-size: 0.68rem;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: var(--fg-faint);
  display: inline-flex;
  align-items: baseline;
  gap: 0.35em;
}
.turn-number-slot {
  position: relative;
  display: inline-block;
  min-width: 1ch;
  text-align: left;
  line-height: 1;
}
.turn-number {
  display: inline-block;
  will-change: transform, opacity;
}
.turn-flip-enter-active {
  transition: transform 320ms cubic-bezier(0.4, 0, 0.2, 1), opacity 320ms ease;
}
.turn-flip-enter-from {
  opacity: 0;
  transform: translateY(60%);
}
.turn-flip-leave-active {
  position: absolute;
  top: 0;
  left: 0;
  transition: transform 420ms cubic-bezier(0.4, 0, 0.2, 1), opacity 420ms ease;
}
.turn-flip-leave-to {
  transform: translateY(-120%);
  opacity: 0;
}
@media (prefers-reduced-motion: reduce) {
  .turn-flip-enter-active,
  .turn-flip-leave-active { transition: opacity 120ms ease; }
  .turn-flip-enter-from { transform: none; }
  .turn-flip-leave-to { transform: none; }
}
.active-label {
  font-size: 0.86rem;
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
  display: inline-flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.68rem;
  letter-spacing: 0.06em;
  padding: 0.18rem 0.6rem;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-pill);
  color: var(--fg-faint);
  /* Smooth the expansion when an action window opens on this pip. */
  transition:
    padding 180ms ease,
    background 180ms ease,
    color 180ms ease,
    border-color 180ms ease,
    box-shadow 180ms ease;
}
.phase-pip.active {
  color: var(--on-accent);
  background: var(--accent);
  border-color: var(--accent);
}
.pip-label { white-space: nowrap; }

/* When an action window is open on this pip, it expands and recolors
   to read like the old standalone action-window pill. The "mine"
   variant pulses to signal that the player holds priority. */
.phase-pip.window-open {
  padding: 0.18rem 0.35rem 0.18rem 0.75rem;
  background: rgba(20, 14, 8, 0.92);
  border-color: rgba(255, 255, 255, 0.25);
  color: rgba(255, 255, 255, 0.92);
  text-transform: uppercase;
  font-size: 0.66rem;
  letter-spacing: 0.1em;
}
.phase-pip.window-open.mine {
  border-color: var(--accent);
  animation: pip-pulse 1.6s ease-in-out infinite;
}
@keyframes pip-pulse {
  0%, 100% { box-shadow: 0 0 0 2px rgba(196, 99, 74, 0.35); }
  50%      { box-shadow: 0 0 0 6px rgba(196, 99, 74, 0.10); }
}

.pip-pass {
  min-height: 22px;
  padding: 0 0.6rem;
  background: var(--accent);
  border: 1px solid var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-pill);
  font-size: 0.68rem;
  letter-spacing: 0.04em;
  cursor: pointer;
}
.pip-pass:hover:not(:disabled) {
  background: var(--accent-strong);
  border-color: var(--accent-strong);
}
.pip-pass:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.leave-btn {
  /* Pushes itself to the right edge of the phase bar. */
  margin-left: auto;
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  padding: 0.18rem 0.6rem;
  border-radius: var(--radius-md);
  font-size: 0.74rem;
  cursor: pointer;
}
.leave-btn:hover { color: var(--accent-strong); border-color: var(--accent-strong); }
</style>
