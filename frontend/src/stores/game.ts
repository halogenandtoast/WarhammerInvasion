// Per-game store: holds the latest GameView pushed by the server.
//
// The store is *assigned to* by server frames. The view layer never
// mutates GameView directly — every change is a server round-trip.

import { computed, ref } from 'vue'
import { openSocket, type SocketStatus, type TypedSocket } from '../api/socket'
import type {
  ChatLine,
  GameIn,
  GameOut,
  GameView,
  UserInfo,
  ZoneKind,
} from '../api/protocol'
import { auth } from './auth'

const _status = ref<SocketStatus>('idle')
const _gameId = ref<string | null>(null)
const _you = ref<UserInfo | null>(null)
const _view = ref<GameView | null>(null)
const _lastError = ref<{ code: string; at: number } | null>(null)
const _closed = ref<{ reason: string; at: number } | null>(null)

let socket: TypedSocket<GameIn> | null = null

function reset() {
  _status.value = 'idle'
  _gameId.value = null
  _you.value = null
  _view.value = null
  _lastError.value = null
  _closed.value = null
}

// View Transitions: when the engine snapshot changes, run the mutation
// inside `document.startViewTransition` so the browser captures the
// before/after DOM and morphs elements that share a
// `view-transition-name` (see SvgCard's `transitionName` prop). Falls
// back to a plain assignment on browsers without the API (Firefox).
type ViewTransitionDoc = Document & {
  startViewTransition?: (cb: () => void) => unknown
}
function withViewTransition(update: () => void) {
  const d = document as ViewTransitionDoc
  if (typeof d.startViewTransition === 'function') {
    d.startViewTransition(update)
  } else {
    update()
  }
}

function handle(msg: GameOut) {
  switch (msg.tag) {
    case 'GameWelcome':
      _you.value = msg.you
      // First frame — no prior state to animate from, so skip the
      // transition wrapper.
      _view.value = msg.game
      break
    case 'GameUpdate':
      withViewTransition(() => {
        _view.value = msg.game
      })
      break
    case 'GameChatNew':
      if (_view.value) {
        const next: ChatLine[] = [..._view.value.chat, msg.line].slice(-200)
        _view.value = { ..._view.value, chat: next }
      }
      break
    case 'GameError':
      _lastError.value = { code: msg.code, at: Date.now() }
      break
    case 'GameClosed':
      _closed.value = { reason: msg.reason, at: Date.now() }
      break
  }
}

interface ConnectOpts {
  gameId: string
  inviteToken?: string | null
  password?: string | null
}

function connect(opts: ConnectOpts) {
  if (socket && _gameId.value === opts.gameId) return
  if (socket) {
    socket.close()
    socket = null
  }
  reset()
  const token = auth.accessToken.value
  if (!token) return
  _gameId.value = opts.gameId

  const params = new URLSearchParams()
  if (opts.inviteToken) params.set('t', opts.inviteToken)
  if (opts.password) params.set('password', opts.password)
  const qs = params.toString()
  const url = `/ws/games/${opts.gameId}${qs ? `?${qs}` : ''}`

  socket = openSocket<GameOut, GameIn>(
    { url, token },
    {
      onMessage: handle,
      onStatusChange: (s) => {
        _status.value = s
      },
    },
  )
}

function disconnect() {
  if (socket) {
    socket.close()
    socket = null
  }
  reset()
}

function sendChat(text: string) {
  socket?.send({ tag: 'GameChatSend', text })
}

function selectDeck(deckId: string) {
  socket?.send({ tag: 'GameSelectDeck', deckId })
}

function clearDeck() {
  socket?.send({ tag: 'GameClearDeck' })
}

function startGame() {
  socket?.send({ tag: 'GameStart' })
}

function leaveGame() {
  socket?.send({ tag: 'GameLeave' })
}

function passPriority() {
  socket?.send({ tag: 'GamePassPriority' })
}

function playCard(card: string, zone: ZoneKind | null = null, target: number | null = null) {
  socket?.send({ tag: 'GamePlayCard', card, zone, target })
}

export const game = {
  status: computed(() => _status.value),
  gameId: computed(() => _gameId.value),
  you: computed(() => _you.value),
  view: computed(() => _view.value),
  // Convenience: pulls the engine snapshot off the latest view. Null
  // until the game is started.
  engine: computed(() => _view.value?.engine ?? null),
  lastError: computed(() => _lastError.value),
  closed: computed(() => _closed.value),
  connect,
  disconnect,
  sendChat,
  selectDeck,
  clearDeck,
  startGame,
  passPriority,
  playCard,
  leaveGame,
}
