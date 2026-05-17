<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { auth } from '../stores/auth'
import { lobby } from '../stores/lobby'
import MaintenanceBanner from '../components/MaintenanceBanner.vue'
import type { GameSummary } from '../api/protocol'

const { t } = useI18n({ useScope: 'global' })

const emit = defineEmits<{ (e: 'navigate', target: string): void }>()

const showHost = ref(false)
const newName = ref('')
const newVisibility = ref<'Public' | 'Private'>('Public')
const newPassword = ref('')
// Spectator preference. Resets to the visibility-driven default each
// time the user flips visibility (see watcher below), so the form
// matches the new default when they change their mind. They can still
// override it.
const newAllowSpectators = ref(true)
// Off by default — every action window waits for a manual pass unless
// the host opts in. Turning this on makes the engine auto-skip windows
// where the priority holder can't play anything; the speed-up costs a
// (mild) information tell to the opponent.
const newAutoSkipActionWindows = ref(false)
const creating = ref(false)

const promptForPassword = ref<GameSummary | null>(null)
const passwordInput = ref('')

const errorBanner = ref<string | null>(null)

const chatText = ref('')
const chatBox = ref<HTMLElement | null>(null)

onMounted(() => {
  lobby.connect()
})

onBeforeUnmount(() => {
  // Keep the connection alive when navigating to a game (the game view
  // is responsible for its own socket). Only fully disconnect when
  // logging out, which the watcher below handles.
})

// Reconnect whenever the auth identity changes — including the cold
// bootstrap (ready flips false→true), sign-in, and sign-out. The
// server hands back a fresh welcome that reflects whichever identity
// we open the socket with (signed-in user or anonymous guest).
watch(
  () => [auth.ready.value, auth.accessToken.value] as const,
  () => {
    lobby.disconnect()
    lobby.connect()
  },
)

// Errors from the lobby socket: surface the latest one as a banner.
// Default the spectator toggle to match the chosen visibility: public
// games invite spectators by default, private ones don't. The user can
// override after flipping if they want.
watch(
  () => newVisibility.value,
  (v) => {
    newAllowSpectators.value = v === 'Public'
  },
)

watch(
  () => lobby.lastError.value?.at,
  () => {
    const code = lobby.lastError.value?.code
    if (!code) return
    errorBanner.value = errorMessage(code)
  },
)

// When the server confirms our game was created, navigate into it.
watch(
  () => lobby.lastCreated.value?.at,
  () => {
    const c = lobby.lastCreated.value
    if (!c) return
    creating.value = false
    showHost.value = false
    newName.value = ''
    newPassword.value = ''
    const params = new URLSearchParams()
    if (c.inviteToken) params.set('t', c.inviteToken)
    const qs = params.toString()
    emit('navigate', `#/games/${c.gameId}${qs ? `?${qs}` : ''}`)
  },
)

// When the server confirms we may join, navigate.
watch(
  () => lobby.lastJoined.value?.at,
  () => {
    const j = lobby.lastJoined.value
    if (!j) return
    promptForPassword.value = null
    passwordInput.value = ''
    const params = new URLSearchParams()
    if (j.inviteToken) params.set('t', j.inviteToken)
    const qs = params.toString()
    emit('navigate', `#/games/${j.gameId}${qs ? `?${qs}` : ''}`)
  },
)

watch(
  () => lobby.chat.value.length,
  async () => {
    await nextTick()
    const el = chatBox.value
    if (el) el.scrollTop = el.scrollHeight
  },
)

const publicGames = computed(() =>
  lobby.games.value.filter((g) => g.visibility === 'Public' || isMine(g)),
)

const me = computed(() => lobby.you.value)
// Guests (anonymous viewers) see the lobby in read-only mode: chat
// and game list visible, but the chat input, host button, and seated
// join button are gated behind sign-in.
const isGuest = computed(() => auth.ready.value && me.value === null)

function isMine(g: GameSummary): boolean {
  return me.value != null && g.host.userId === me.value.userId
}

function errorMessage(code: string): string {
  const known = [
    'name_too_short',
    'name_too_long',
    'invalid_password',
    'game_not_found',
    'game_is_private',
    'game_full',
    'wrong_password',
    'unauthorized',
    'maintenance_in_progress',
  ]
  if (known.includes(code)) return t(`lobby.errors.${code}`)
  return code
}

const inMaintenance = computed(() => lobby.maintenance.value !== null)

function sendChat() {
  if (isGuest.value) return
  const text = chatText.value.trim()
  if (!text) return
  lobby.sendChat(text)
  chatText.value = ''
}

function goSignIn() {
  emit('navigate', '#/login')
}

function openHost() {
  errorBanner.value = null
  showHost.value = !showHost.value
}

function submitHost() {
  const name = newName.value.trim()
  if (!name) return
  creating.value = true
  errorBanner.value = null
  const pw = newPassword.value.trim()
  lobby.createGame({
    name,
    visibility: newVisibility.value,
    password: newVisibility.value === 'Private' && pw.length > 0 ? pw : null,
    allowSpectators: newAllowSpectators.value,
    autoSkipActionWindows: newAutoSkipActionWindows.value,
  })
}

function clickJoin(g: GameSummary) {
  errorBanner.value = null
  if (isGuest.value) {
    // Guests skip the `LobbyJoinPublic` round trip — they can't claim
    // a seat anyway, so we route them straight to the game socket as
    // a spectator. The game socket itself enforces allowSpectators.
    emit('navigate', `#/games/${g.gameId}`)
    return
  }
  if (isMine(g)) {
    emit('navigate', `#/games/${g.gameId}`)
    return
  }
  if (g.hasPassword && g.visibility === 'Private') {
    promptForPassword.value = g
    passwordInput.value = ''
    return
  }
  if (g.visibility === 'Public') {
    lobby.joinPublic(g.gameId)
  }
}

function submitPasswordJoin() {
  const g = promptForPassword.value
  if (!g) return
  const pw = passwordInput.value.trim()
  if (!pw) return
  lobby.joinWithPassword(g.gameId, pw)
}

function cancelPasswordJoin() {
  promptForPassword.value = null
  passwordInput.value = ''
}

// Label for the join button. "Resume" if it's our own game, "Spectate"
// when both seats are taken on a spectator-friendly game (or for any
// guest viewer, who can never claim a seat), otherwise plain "Join".
// The button still fires `clickJoin` either way; the server attaches
// us as the right kind of viewer.
function joinLabel(g: GameSummary): string {
  if (isMine(g)) return t('lobby.games.card.resume')
  if (isGuest.value) return t('lobby.games.card.spectate')
  if (g.filledSeats >= 2 && g.allowSpectators) return t('lobby.games.card.spectate')
  return t('lobby.games.card.join')
}

// Guests can only enter games that explicitly welcome spectators.
function canClickJoin(g: GameSummary): boolean {
  if (!isGuest.value) return true
  return g.visibility === 'Public' && g.allowSpectators
}

function statusLabel(s: GameSummary['status']): string {
  switch (s) {
    case 'StatusPlaying':
      return t('lobby.games.card.status_playing')
    case 'StatusEnded':
      return t('lobby.games.card.status_ended')
    default:
      return t('lobby.games.card.status_waiting')
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
  <main class="lobby-page">
    <header class="page-head">
      <div><h1>{{ t('lobby.heading') }}</h1></div>
      <div class="status-pill" :data-status="lobby.status.value">
        <span class="status-dot" aria-hidden="true" />
        {{ t(`lobby.status.${lobby.status.value}`) }}
      </div>
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
        <form v-if="!isGuest" class="chat-input" @submit.prevent="sendChat">
          <input
            v-model="chatText"
            type="text"
            maxlength="1000"
            :placeholder="t('lobby.chat.placeholder')"
            :aria-label="t('lobby.chat.placeholder')"
          />
          <button class="primary" type="submit" :disabled="!chatText.trim()">
            {{ t('lobby.chat.send') }}
          </button>
        </form>
        <div v-else class="chat-input chat-input-guest">
          <p class="guest-cta-text">{{ t('lobby.chat.guest_cta') }}</p>
          <button class="primary" type="button" @click="goSignIn">
            {{ t('app.nav.login') }}
          </button>
        </div>
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

          <form v-if="showHost && !inMaintenance" class="new-form" @submit.prevent="submitHost">
            <label class="field">
              <span class="field-label">{{ t('lobby.games.new_form.name_label') }}</span>
              <input
                v-model="newName"
                type="text"
                required
                maxlength="80"
                :placeholder="t('lobby.games.new_form.name_placeholder')"
              />
            </label>

            <fieldset class="vis-fieldset">
              <legend>{{ t('lobby.games.new_form.visibility_label') }}</legend>
              <label class="vis-option">
                <input v-model="newVisibility" type="radio" value="Public" />
                <span>
                  <strong>{{ t('lobby.games.new_form.public') }}</strong>
                  <small>{{ t('lobby.games.new_form.public_help') }}</small>
                </span>
              </label>
              <label class="vis-option">
                <input v-model="newVisibility" type="radio" value="Private" />
                <span>
                  <strong>{{ t('lobby.games.new_form.private') }}</strong>
                  <small>{{ t('lobby.games.new_form.private_help') }}</small>
                </span>
              </label>
            </fieldset>

            <label v-if="newVisibility === 'Private'" class="field">
              <span class="field-label">{{ t('lobby.games.new_form.password_label') }}</span>
              <input
                v-model="newPassword"
                type="text"
                maxlength="60"
                :placeholder="t('lobby.games.new_form.password_placeholder')"
              />
              <small class="hint">{{ t('lobby.games.new_form.password_help') }}</small>
            </label>

            <label class="spec-option">
              <input v-model="newAllowSpectators" type="checkbox" />
              <span>
                <strong>{{ t('lobby.games.new_form.spectators_label') }}</strong>
                <small>{{ t('lobby.games.new_form.spectators_help') }}</small>
              </span>
            </label>

            <label class="spec-option">
              <input v-model="newAutoSkipActionWindows" type="checkbox" />
              <span>
                <strong>{{ t('lobby.games.new_form.auto_skip_label') }}</strong>
                <small>{{ t('lobby.games.new_form.auto_skip_help') }}</small>
              </span>
            </label>

            <button class="primary" type="submit" :disabled="creating || !newName.trim()">
              {{ creating ? t('lobby.games.new_form.submitting') : t('lobby.games.new_form.submit') }}
            </button>
          </form>

          <div v-if="publicGames.length === 0 && !showHost" class="status">
            {{ t('lobby.games.none') }}
          </div>
          <ul v-else-if="publicGames.length > 0" class="games-list">
            <li v-for="g in publicGames" :key="g.gameId" class="game-card">
              <div class="game-card-main">
                <div class="game-row">
                  <h3>{{ g.name }}</h3>
                  <div class="badges">
                    <span
                      class="badge"
                      :class="g.visibility === 'Public' ? 'badge-public' : 'badge-private'"
                    >
                      {{
                        g.visibility === 'Public'
                          ? t('lobby.games.card.public_badge')
                          : t('lobby.games.card.private_badge')
                      }}
                    </span>
                    <span v-if="g.hasPassword" class="badge badge-pw">
                      {{ t('lobby.games.card.password_badge') }}
                    </span>
                    <span v-if="g.allowSpectators" class="badge badge-spec">
                      {{ t('lobby.games.card.spectators_badge') }}
                    </span>
                  </div>
                </div>
                <p class="game-meta">
                  {{ t('lobby.games.card.host_by', { name: g.host.displayName }) }}
                  · {{ t('lobby.games.card.seats_label', { filled: g.filledSeats }) }}
                  · <span class="status-tag" :data-status="g.status">{{ statusLabel(g.status) }}</span>
                  <template v-if="g.spectatorCount > 0">
                    · {{ t('lobby.games.card.spectators_count', { n: g.spectatorCount }) }}
                  </template>
                </p>
              </div>
              <button
                class="primary"
                type="button"
                :disabled="!canClickJoin(g)"
                :title="!canClickJoin(g) ? t('lobby.games.card.guest_blocked') : undefined"
                @click="clickJoin(g)"
              >
                {{ joinLabel(g) }}
              </button>
            </li>
          </ul>

          <div v-if="promptForPassword" class="pw-overlay" @click.self="cancelPasswordJoin">
            <form class="pw-dialog" @submit.prevent="submitPasswordJoin">
              <h3>{{ t('lobby.games.card.password_prompt', { name: promptForPassword.name }) }}</h3>
              <input
                v-model="passwordInput"
                type="password"
                maxlength="60"
                autofocus
                :aria-label="t('lobby.games.new_form.password_label')"
              />
              <div class="pw-buttons">
                <button class="ghost" type="button" @click="cancelPasswordJoin">
                  {{ t('lobby.games.card.cancel') }}
                </button>
                <button class="primary" type="submit" :disabled="!passwordInput.trim()">
                  {{ t('lobby.games.card.join_password') }}
                </button>
              </div>
            </form>
          </div>
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

.eyebrow {
  margin: 0;
  font-size: 0.78rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--accent-strong);
}

h1 {
  margin: 0.35rem 0 0.3rem;
  font-size: clamp(1.6rem, 3.2vw, 2.1rem);
}

.lead {
  margin: 0;
  color: var(--fg-dim);
  max-width: 60ch;
}

.status-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.45rem;
  height: 28px;
  padding: 0 0.7rem;
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

.status-pill[data-status='open'] .status-dot {
  background: #5da46a;
  box-shadow: 0 0 0 3px rgba(93, 164, 106, 0.18);
}

.status-pill[data-status='connecting'] .status-dot {
  background: #d8b66c;
  animation: pulse 1.2s infinite;
}

.status-pill[data-status='closed'] .status-dot {
  background: var(--accent-strong);
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.35; }
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

.chat-input-guest {
  flex-direction: row;
  align-items: center;
  justify-content: space-between;
  gap: 0.6rem;
}
.guest-cta-text {
  margin: 0;
  color: var(--fg-dim);
  font-size: 0.85rem;
}

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

.chat-input {
  display: flex;
  gap: 0.5rem;
  padding: 0.65rem 0.8rem;
  border-top: 1px solid var(--border);
  background: var(--bg);
  border-radius: 0 0 var(--radius-lg) var(--radius-lg);
}

.chat-input input {
  flex: 1;
  min-height: var(--tap-target);
  padding: 0.4rem 0.75rem;
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

.game-card {
  display: flex;
  gap: 0.8rem;
  align-items: center;
  justify-content: space-between;
  padding: 0.7rem 0.85rem;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
}

.game-card-main {
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
  min-width: 0;
}

.game-row {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.game-card h3 {
  margin: 0;
  font-size: 0.98rem;
  font-weight: 600;
  word-break: break-word;
}

.badges {
  display: inline-flex;
  gap: 0.25rem;
}

.badge {
  font-size: 0.7rem;
  padding: 0.1rem 0.55rem;
  border-radius: var(--radius-pill);
  border: 1px solid var(--border);
  color: var(--fg-dim);
  letter-spacing: 0.05em;
}

.badge-public {
  background: rgba(110, 155, 209, 0.18);
  border-color: rgba(110, 155, 209, 0.35);
  color: var(--faction-order);
}

.badge-private {
  background: rgba(196, 99, 74, 0.16);
  border-color: rgba(196, 99, 74, 0.36);
  color: var(--accent-strong);
}

.badge-pw {
  background: rgba(212, 179, 87, 0.12);
  border-color: rgba(212, 179, 87, 0.35);
  color: var(--race-empire);
}

.badge-spec {
  background: rgba(95, 160, 214, 0.10);
  border-color: rgba(95, 160, 214, 0.30);
  color: var(--race-high-elf, #5fa0d6);
}

.game-meta {
  margin: 0;
  font-size: 0.79rem;
  color: var(--fg-faint);
}

.status-tag[data-status='StatusPlaying'] {
  color: var(--race-orc);
}

.status-tag[data-status='StatusEnded'] {
  color: var(--fg-faint);
}

.status {
  padding: 1.6rem 1rem;
  text-align: center;
  color: var(--fg-faint);
}

.new-form {
  padding: 0.9rem 1rem;
  display: flex;
  flex-direction: column;
  gap: 0.7rem;
  border-bottom: 1px solid var(--border);
}

.field {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.field-label {
  font-size: 0.7rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.field input {
  min-height: var(--tap-target);
  padding: 0.5rem 0.75rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.95rem;
}

.field input:focus-visible {
  outline: 2px solid var(--accent-strong);
  border-color: var(--accent-strong);
}

.vis-fieldset {
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  padding: 0.55rem 0.7rem;
  display: flex;
  flex-direction: column;
  gap: 0.45rem;
  background: var(--bg);
}

.vis-fieldset legend {
  font-size: 0.7rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint);
  padding: 0 0.35rem;
}

.vis-option {
  display: flex;
  gap: 0.55rem;
  align-items: flex-start;
  cursor: pointer;
}

.vis-option input[type='radio'] {
  margin-top: 0.32rem;
}

.vis-option strong {
  font-size: 0.9rem;
  color: var(--fg);
}

.vis-option small {
  display: block;
  font-size: 0.78rem;
  color: var(--fg-faint);
  margin-top: 0.05rem;
}

.spec-option {
  display: flex;
  gap: 0.55rem;
  align-items: flex-start;
  cursor: pointer;
  padding: 0.45rem 0.7rem;
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  background: var(--bg);
}

.spec-option input[type='checkbox'] {
  margin-top: 0.32rem;
}

.spec-option strong {
  font-size: 0.9rem;
  color: var(--fg);
}

.spec-option small {
  display: block;
  font-size: 0.78rem;
  color: var(--fg-faint);
  margin-top: 0.05rem;
}

.hint {
  font-size: 0.78rem;
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

.pw-overlay {
  position: fixed;
  inset: 0;
  background: var(--overlay-medium);
  display: grid;
  place-items: center;
  z-index: var(--z-modal);
  padding: 1rem;
}

.pw-dialog {
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  padding: 1rem 1.1rem;
  display: flex;
  flex-direction: column;
  gap: 0.8rem;
  min-width: min(360px, 100%);
}

.pw-dialog h3 {
  margin: 0;
  font-size: 0.95rem;
}

.pw-dialog input {
  min-height: var(--tap-target);
  padding: 0.5rem 0.75rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.95rem;
}

.pw-dialog input:focus-visible {
  outline: 2px solid var(--accent-strong);
  border-color: var(--accent-strong);
}

.pw-buttons {
  display: flex;
  justify-content: flex-end;
  gap: 0.45rem;
}

@media (pointer: coarse) {
  .ghost {
    min-height: var(--tap-target);
  }
}
</style>
