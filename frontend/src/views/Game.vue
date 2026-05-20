<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { auth } from '../stores/auth'
import { game } from '../stores/game'
import { listDecks, type DeckRecord } from '../api/decks'
import { ApiError } from '../api/client'
import type { ChatLine, LogEntry, PlayerKey, Race } from '../api/protocol'
import PlayView from './PlayView.vue'
import PromptPanel from '../components/PromptPanel.vue'
import MaintenanceBanner from '../components/MaintenanceBanner.vue'
import ChatInput from '../components/ChatInput.vue'
import GameTopBar from '../components/GameTopBar.vue'
import GameWaitingRoom from '../components/GameWaitingRoom.vue'
import PhaseBar from '../components/PhaseBar.vue'
import MessagesStream, { type Message } from '../components/MessagesStream.vue'
import { makeLogFormatter } from '../lib/gameLog'
import { GAME_ERROR_CODES, mapKnown } from '../lib/errors'
import { engineRaceSlug } from '../lib/race'
import { navigate } from '../router'
import { useGameSocket } from '../composables/useGameSocket'

const props = defineProps<{
  gameId: string
  inviteToken: string | null
  password: string | null
}>()

const { t } = useI18n({ useScope: 'global' })

const decks = ref<DeckRecord[]>([])
const decksLoading = ref(true)
const decksError = ref<string | null>(null)
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

// True when you're not occupying either seat. Includes guests, who can
// never be seated.
const isSpectator = computed<boolean>(() => {
  const v = view.value
  if (!v) return false
  const me = you.value
  if (!me) return true
  return !v.seats.some((s) => s.user.userId === me.userId)
})

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
  switch (view.value?.status) {
    case 'StatusPlaying': return t('game.playing_heading')
    case 'StatusEnded': return t('game.ended_heading')
    default: return t('game.waiting_heading')
  }
})

const mySeatKey = computed<PlayerKey | null>(() => {
  const v = view.value
  const me = you.value
  if (!v || !me) return null
  return v.seats.find((s) => s.user.userId === me.userId)?.seat ?? null
})

const activePlayerName = computed(() => {
  const e = engine.value
  if (!e) return ''
  return view.value?.seats.find((s) => s.seat === e.currentPlayer)?.user.displayName ?? e.currentPlayer
})

// ---- lifecycle ----

useGameSocket(props)

onMounted(async () => {
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

watch(
  () => game.lastError.value?.at,
  () => {
    const code = game.lastError.value?.code
    if (code) errorBanner.value = mapError(code)
  },
)

watch(
  () => game.closed.value?.at,
  () => {
    if (game.closed.value) navigate('#/lobby')
  },
)

// ---- merged log + chat stream for the side rail ----

const logEntries = computed<LogEntry[]>(() => engine.value?.log ?? [])
const chatLines = computed<ChatLine[]>(() => view.value?.chat ?? [])

// ISO-8601 timestamps sort lexicographically — stable string sort is fine.
const messages = computed<Message[]>(() => {
  const out: Message[] = []
  for (const entry of logEntries.value) out.push({ kind: 'log', at: entry.at, entry })
  for (const line of chatLines.value) out.push({ kind: 'chat', at: line.at, line })
  out.sort((a, b) => (a.at < b.at ? -1 : a.at > b.at ? 1 : 0))
  return out
})

function seatOf(userId: string): PlayerKey | null {
  return view.value?.seats.find((s) => s.user.userId === userId)?.seat ?? null
}

// CSS class for chat tinting derived from the speaker's actual race.
// Null when the speaker isn't a seated player or the engine hasn't
// sent a snapshot yet — caller falls back to neutral styling.
function raceClassOf(userId: string): string | null {
  const seat = seatOf(userId)
  if (!seat || !engine.value) return null
  const player = seat === 'Player1' ? engine.value.player1 : engine.value.player2
  return `race-${engineRaceSlug(player.race)}`
}

function playerKeyLabel(key: string): string {
  if (key === 'Player1') return t('game.seat.heading_player1')
  if (key === 'Player2') return t('game.seat.heading_player2')
  return key
}

const logFormatter = makeLogFormatter(
  t as unknown as Parameters<typeof makeLogFormatter>[0],
  () => view.value?.seats ?? [],
  playerKeyLabel,
)
const formatLogEntry = (entry: LogEntry) => logFormatter.format(entry)

const mapError = (code: string) =>
  mapKnown(code, GAME_ERROR_CODES, (k) => t(`game.errors.${k}`))

// ---- waiting-room handlers ----

function selectDeck(e: Event) {
  const v = (e.target as HTMLSelectElement).value
  if (v) game.selectDeck(v)
}
function selectStarter(race: Race) { game.selectStarter(race) }
function clearDeck() { game.clearDeck() }
function start() { game.startGame() }
function leave() {
  // Spectators aren't seated, so leaving doesn't end the game — just
  // navigate back. Confirming would be misleading ("ends the game for
  // both players").
  if (isSpectator.value) {
    navigate('#/lobby')
    return
  }
  if (!confirm(t('game.controls.confirm_leave'))) return
  game.leaveGame()
}
function backToLobby() { navigate('#/lobby') }
function pass() { game.passPriority() }
</script>

<template>
  <main class="game-page" :class="{ playing: isPlaying }">
    <GameTopBar
      :view="view"
      :socket-status="game.status.value"
      :spectator-count="view?.spectatorCount ?? 0"
      :is-spectator="isSpectator"
      :phase-heading="phaseHeading"
      @back="backToLobby"
    />

    <MaintenanceBanner :state="game.maintenance.value" />

    <p v-if="errorBanner" class="error" role="alert">{{ errorBanner }}</p>

    <!-- ────────────────────────────────────────────────────────────
         Main area + chat sidebar (two columns on desktop, stacked
         on narrow viewports).
         ──────────────────────────────────────────────────────────── -->
    <section class="game-grid">
      <section class="main-col">
        <!-- Waiting room: seat/deck pickers + start controls -->
        <GameWaitingRoom
          v-if="isWaiting && view"
          :view="view"
          :you="you"
          :decks="decks"
          :decks-loading="decksLoading"
          :decks-error="decksError"
          :is-spectator="isSpectator"
          :start-disabled-reason="startDisabledReason"
          @select-deck="selectDeck"
          @select-starter="selectStarter"
          @clear-deck="clearDeck"
          @start="start"
          @leave="leave"
        />

        <!-- Playing (or ended) — full-bleed table surface. -->
        <section
          v-else-if="view && view.engine"
          class="table-surface"
          :class="{ ended: !isPlaying }"
        >
          <PlayView :engine="view.engine" :seats="view.seats" />
        </section>
      </section>

      <!-- Side rail: prompt panel (when present) + merged log/chat. -->
      <aside class="side-panel">
        <PromptPanel
          v-if="engine && engine.pendingPrompt"
          :engine="engine"
          :seat="mySeatKey"
        />
        <section class="messages-panel" :aria-label="t('game.messages.heading')">
          <header class="panel-head">
            <h2>{{ t('game.messages.heading') }}</h2>
          </header>
          <MessagesStream
            :messages="messages"
            :race-class-of="raceClassOf"
            :format-log-entry="formatLogEntry"
          />
          <ChatInput
            :guest="isGuest"
            i18n-prefix="game.chat"
            @send="game.sendChat"
            @sign-in="navigate('#/login')"
          />
        </section>
      </aside>
    </section>

    <PhaseBar
      v-if="isPlaying && engine"
      :engine="engine"
      :my-seat="mySeatKey"
      :active-player-name="activePlayerName"
      :is-spectator="isSpectator"
      @pass="pass"
      @leave="leave"
    />
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

/* ───────── side rail ───────── */

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
</style>
