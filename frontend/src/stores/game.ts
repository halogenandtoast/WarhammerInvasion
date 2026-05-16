// Per-game store: holds the latest GameView pushed by the server.
//
// The store is *assigned to* by server frames. The view layer never
// mutates GameView directly — every change is a server round-trip.

import { computed, ref } from 'vue'
import { openSocket, type SocketStatus, type TypedSocket } from '../api/socket'
import type { ChatLine, GameIn, GameOut, GameView, UserInfo } from '../api/protocol'
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

function handle(msg: GameOut) {
  switch (msg.tag) {
    case 'GameWelcome':
      _you.value = msg.you
      _view.value = msg.game
      break
    case 'GameUpdate':
      _view.value = msg.game
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

export const game = {
  status: computed(() => _status.value),
  gameId: computed(() => _gameId.value),
  you: computed(() => _you.value),
  view: computed(() => _view.value),
  lastError: computed(() => _lastError.value),
  closed: computed(() => _closed.value),
  connect,
  disconnect,
  sendChat,
  selectDeck,
  clearDeck,
  startGame,
  leaveGame,
}
