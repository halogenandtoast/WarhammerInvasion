<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { auth } from '../stores/auth'
import { lobby } from '../stores/lobby'
import MaintenanceBanner from '../components/MaintenanceBanner.vue'
import StatusPill from '../components/StatusPill.vue'
import LobbyHostForm from '../components/LobbyHostForm.vue'
import LobbyGameCard from '../components/LobbyGameCard.vue'
import PasswordPrompt from '../components/PasswordPrompt.vue'
import ChatInput from '../components/ChatInput.vue'
import type { GameSummary, Visibility } from '../api/protocol'
import { formatTime } from '../lib/format'
import { LOBBY_ERROR_CODES, mapKnown } from '../lib/errors'
import { useAutoScroll } from '../composables/useAutoScroll'
import { navigate } from '../router'

const { t } = useI18n({ useScope: 'global' })

const showHost = ref(false)
const creating = ref(false)
const promptForPassword = ref<GameSummary | null>(null)
const errorBanner = ref<string | null>(null)

const chatBox = useAutoScroll(() => lobby.chat.value.length)

const me = computed(() => lobby.you.value)
// Guests (anonymous viewers) see the lobby in read-only mode: chat
// and game list visible, but the chat input, host button, and seated
// join button are gated behind sign-in.
const isGuest = computed(() => auth.ready.value && me.value === null)
const inMaintenance = computed(() => lobby.maintenance.value !== null)

function isMine(g: GameSummary): boolean {
  return me.value != null && g.host.userId === me.value.userId
}

const publicGames = computed(() =>
  lobby.games.value.filter((g) => g.visibility === 'Public' || isMine(g)),
)

const errorMessage = (code: string) =>
  mapKnown(code, LOBBY_ERROR_CODES, (k) => t(`lobby.errors.${k}`))

function gameUrl(gameId: string, inviteToken: string | null): string {
  return inviteToken
    ? `#/games/${gameId}?t=${encodeURIComponent(inviteToken)}`
    : `#/games/${gameId}`
}

onMounted(() => {
  lobby.connect()
})

// Reconnect whenever the auth identity changes — cold bootstrap, sign-in,
// or sign-out. `lobby.connect()` is identity-aware: it no-ops if the live
// socket already matches the current user.
watch(
  () => [auth.ready.value, auth.user.value?.id ?? null] as const,
  () => lobby.connect(),
)

watch(
  () => lobby.lastError.value?.at,
  () => {
    const code = lobby.lastError.value?.code
    if (code) errorBanner.value = errorMessage(code)
  },
)

// Server confirms our game was created — navigate into it.
watch(
  () => lobby.lastCreated.value?.at,
  () => {
    const c = lobby.lastCreated.value
    if (!c) return
    creating.value = false
    showHost.value = false
    navigate(gameUrl(c.gameId, c.inviteToken))
  },
)

// Server confirms we may join — navigate.
watch(
  () => lobby.lastJoined.value?.at,
  () => {
    const j = lobby.lastJoined.value
    if (!j) return
    promptForPassword.value = null
    navigate(gameUrl(j.gameId, j.inviteToken))
  },
)

function goSignIn() {
  navigate('#/login')
}

function openHost() {
  errorBanner.value = null
  showHost.value = !showHost.value
}

function submitHost(payload: {
  name: string
  visibility: Visibility
  password: string | null
  allowSpectators: boolean
  autoSkipActionWindows: boolean
}) {
  creating.value = true
  errorBanner.value = null
  lobby.createGame(payload)
}

function clickJoin(g: GameSummary) {
  errorBanner.value = null
  // Guests can never claim a seat — route them straight to the game
  // socket as a spectator; the server enforces allowSpectators.
  // Resuming our own game also bypasses the join round-trip.
  if (isGuest.value || isMine(g)) {
    navigate(`#/games/${g.gameId}`)
    return
  }
  if (g.hasPassword && g.visibility === 'Private') {
    promptForPassword.value = g
    return
  }
  if (g.visibility === 'Public') {
    lobby.joinPublic(g.gameId)
  }
}

function submitPasswordJoin(payload: { game: GameSummary; password: string }) {
  lobby.joinWithPassword(payload.game.gameId, payload.password)
}

// Label for the join button. "Resume" if it's our own game, "Spectate"
// once the game has actually started (or for any guest viewer), otherwise
// plain "Join" — seats can free up in the waiting room.
function joinLabel(g: GameSummary): string {
  if (isMine(g)) return t('lobby.games.card.resume')
  if (isGuest.value) return t('lobby.games.card.spectate')
  if (g.status !== 'StatusWaiting' && g.allowSpectators) return t('lobby.games.card.spectate')
  return t('lobby.games.card.join')
}

// Guests can only enter games that explicitly welcome spectators.
function canClickJoin(g: GameSummary): boolean {
  if (!isGuest.value) return true
  return g.visibility === 'Public' && g.allowSpectators
}

function blockedTooltip(g: GameSummary): string | null {
  return canClickJoin(g) ? null : t('lobby.games.card.guest_blocked')
}

function statusLabel(s: GameSummary['status']): string {
  switch (s) {
    case 'StatusPlaying': return t('lobby.games.card.status_playing')
    case 'StatusEnded': return t('lobby.games.card.status_ended')
    default: return t('lobby.games.card.status_waiting')
  }
}
</script>

<template>
  <main class="lobby-page">
    <header class="page-head">
      <div><h1>{{ t('lobby.heading') }}</h1></div>
      <StatusPill
        :status="lobby.status.value"
        :label="t(`lobby.status.${lobby.status.value}`)"
      />
    </header>

    <MaintenanceBanner :state="lobby.maintenance.value" />

    <p v-if="isGuest" class="guest-banner">
      {{ t('lobby.guest_banner.lead') }}
      <button class="link-button" type="button" @click="goSignIn">
        {{ t('lobby.guest_banner.cta') }}
      </button>
    </p>

    <p v-if="errorBanner" class="error" role="alert">{{ errorBanner }}</p>

    <section class="lobby-grid">
      <!-- Chat -->
      <section class="chat-panel" :aria-label="t('lobby.chat.heading')">
        <header class="panel-head">
          <h2>{{ t('lobby.chat.heading') }}</h2>
        </header>
        <div ref="chatBox" class="chat-scroll">
          <div v-if="lobby.chat.value.length === 0" class="chat-empty">
            {{ t('lobby.chat.empty') }}
          </div>
          <ul v-else class="chat-list" role="log" aria-live="polite">
            <li v-for="(line, i) in lobby.chat.value" :key="`${line.at}-${i}`" class="chat-line">
              <div class="chat-meta">
                <span class="chat-author">{{ line.from.displayName }}</span>
                <time class="chat-time">{{ formatTime(line.at) }}</time>
              </div>
              <p class="chat-text">{{ line.text }}</p>
            </li>
          </ul>
        </div>
        <ChatInput
          :guest="isGuest"
          i18n-prefix="lobby.chat"
          @send="lobby.sendChat"
          @sign-in="goSignIn"
        />
      </section>

      <!-- Sidebar -->
      <aside class="side-rail">
        <section class="users-panel">
          <header class="panel-head">
            <h2>{{ t('lobby.users.heading', { count: lobby.users.value.length }) }}</h2>
          </header>
          <ul class="user-list">
            <li v-for="u in lobby.users.value" :key="u.userId">
              <span class="user-dot" aria-hidden="true" />
              <span class="user-name">{{ u.displayName }}</span>
              <span v-if="me && u.userId === me.userId" class="user-you">·</span>
            </li>
          </ul>
        </section>

        <section class="games-panel">
          <header class="panel-head with-action">
            <h2>{{ t('lobby.games.heading') }}</h2>
            <button
              v-if="!isGuest"
              class="ghost"
              type="button"
              :disabled="inMaintenance"
              :title="inMaintenance ? t('lobby.errors.maintenance_in_progress') : undefined"
              @click="openHost"
            >
              {{ showHost ? t('lobby.games.cancel_host') : t('lobby.games.host_button') }}
            </button>
            <button v-else class="ghost" type="button" @click="goSignIn">
              {{ t('lobby.games.guest_host_cta') }}
            </button>
          </header>

          <LobbyHostForm
            v-if="showHost && !inMaintenance"
            :creating="creating"
            @submit="submitHost"
          />

          <div v-if="publicGames.length === 0 && !showHost" class="status">
            {{ t('lobby.games.none') }}
          </div>
          <ul v-else-if="publicGames.length > 0" class="games-list">
            <LobbyGameCard
              v-for="g in publicGames"
              :key="g.gameId"
              :game="g"
              :can-join="canClickJoin(g)"
              :join-label="joinLabel(g)"
              :blocked-tooltip="blockedTooltip(g)"
              :status-label="statusLabel(g.status)"
              @join="clickJoin"
            />
          </ul>

          <PasswordPrompt
            :game="promptForPassword"
            @submit="submitPasswordJoin"
            @cancel="promptForPassword = null"
          />
        </section>
      </aside>
    </section>
  </main>
</template>

<style scoped>
.lobby-page {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem 1.25rem 4rem;
  min-height: calc(100dvh - 48px);
}

.page-head {
  display: flex;
  align-items: flex-end;
  justify-content: space-between;
  gap: 1rem;
  margin-bottom: 1.5rem;
  flex-wrap: wrap;
}

h1 {
  margin: 0.35rem 0 0.3rem;
  font-size: clamp(1.6rem, 3.2vw, 2.1rem);
}

.error {
  margin: 0 0 1rem;
  padding: 0.6rem 0.9rem;
  background: var(--bg-elev);
  border-left: 3px solid var(--accent-strong);
  border-radius: var(--radius-sm);
}

.guest-banner {
  margin: 0 0 1rem;
  padding: 0.55rem 0.9rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  color: var(--fg-dim);
  font-size: 0.88rem;
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.35rem;
}

.link-button {
  background: transparent;
  border: none;
  padding: 0;
  color: var(--accent-strong);
  cursor: pointer;
  font: inherit;
  text-decoration: underline;
}
.link-button:hover { color: var(--accent); }

.lobby-grid {
  display: grid;
  gap: 1.1rem;
  grid-template-columns: minmax(0, 1.6fr) minmax(280px, 1fr);
}

@media (max-width: 880px) {
  .lobby-grid {
    grid-template-columns: 1fr;
  }
}

.panel-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.7rem 1rem;
  border-bottom: 1px solid var(--border);
}

.panel-head h2 {
  margin: 0;
  font-size: 0.95rem;
  font-weight: 600;
  color: var(--fg);
}

.panel-head.with-action {
  gap: 0.5rem;
}

.chat-panel,
.users-panel,
.games-panel {
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  display: flex;
  flex-direction: column;
}

.chat-panel {
  min-height: 540px;
}

.chat-scroll {
  flex: 1;
  overflow-y: auto;
  padding: 0.8rem 1rem;
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
}

.chat-empty {
  margin: auto;
  color: var(--fg-faint);
  font-size: 0.9rem;
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
  color: var(--fg);
  font-size: 0.92rem;
  line-height: 1.4;
  word-break: break-word;
}

.side-rail {
  display: flex;
  flex-direction: column;
  gap: 1.1rem;
}

.user-list {
  list-style: none;
  margin: 0;
  padding: 0.4rem 1rem 0.7rem;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
}

.user-list li {
  display: flex;
  align-items: center;
  gap: 0.45rem;
  color: var(--fg-dim);
  font-size: 0.9rem;
}

.user-dot {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: #5da46a;
}

.user-you {
  color: var(--accent-strong);
  margin-left: 0.15rem;
}

.games-list {
  list-style: none;
  margin: 0;
  padding: 0.65rem;
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
}

.status {
  padding: 1.6rem 1rem;
  text-align: center;
  color: var(--fg-faint);
}

.primary {
  min-height: var(--tap-target);
  padding: 0 1rem;
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

.ghost {
  min-height: 32px;
  padding: 0 0.75rem;
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  border-radius: var(--radius-md);
  cursor: pointer;
  font-size: 0.82rem;
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
</style>
