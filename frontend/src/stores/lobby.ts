// Lobby store: a single global slot of state owned by the lobby
// WebSocket. The store is *assigned to* by inbound messages and read by
// the Lobby view. Outbound actions go through this module's `send` helper.

import { computed, ref } from 'vue'
import { openSocket, type SocketStatus, type TypedSocket } from '../api/socket'
import type {
  ChatLine,
  GameSummary,
  LobbyIn,
  LobbyOut,
  UserInfo,
} from '../api/protocol'
import { auth } from './auth'

const _status = ref<SocketStatus>('idle')
const _you = ref<UserInfo | null>(null)
const _users = ref<UserInfo[]>([])
const _games = ref<GameSummary[]>([])
const _chat = ref<ChatLine[]>([])

// One-off events: keyed by an ever-increasing counter so listeners can
// be re-set up across re-renders without missing late deliveries.
const _lastError = ref<{ code: string; at: number } | null>(null)
const _lastCreated = ref<{
  gameId: string
  inviteToken: string | null
  at: number
} | null>(null)
const _lastJoined = ref<{
  gameId: string
  inviteToken: string | null
  at: number
} | null>(null)

let socket: TypedSocket<LobbyIn> | null = null

function reset() {
  _you.value = null
  _users.value = []
  _games.value = []
  _chat.value = []
  _status.value = 'idle'
}

function handle(msg: LobbyOut) {
  switch (msg.tag) {
    case 'LobbyWelcome':
      _you.value = msg.you
      _users.value = msg.users
      _games.value = msg.games
      _chat.value = msg.chat
      break
    case 'LobbyChatNew':
      _chat.value = [..._chat.value, msg.line].slice(-200)
      break
    case 'LobbyUsersUpdate':
      _users.value = msg.users
      break
    case 'LobbyGamesUpdate':
      _games.value = msg.games
      break
    case 'LobbyGameCreated':
      _lastCreated.value = {
        gameId: msg.gameId,
        inviteToken: msg.inviteToken,
        at: Date.now(),
      }
      break
    case 'LobbyGameJoinOk':
      _lastJoined.value = {
        gameId: msg.gameId,
        inviteToken: msg.inviteToken,
        at: Date.now(),
      }
      break
    case 'LobbyError':
      _lastError.value = { code: msg.code, at: Date.now() }
      break
  }
}

function connect() {
  if (socket) return
  // Wait for the auth bootstrap to settle so we don't open a guest
  // socket only to immediately replace it once the access token
  // arrives. Once `ready`, we open either authed (token present) or
  // guest (token null); the server handles both.
  if (!auth.ready.value) return
  const token = auth.accessToken.value
  socket = openSocket<LobbyOut, LobbyIn>(
    { url: '/ws/lobby', token },
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
  socket?.send({ tag: 'LobbyChatSend', text })
}

function createGame(input: {
  name: string
  visibility: 'Public' | 'Private'
  password: string | null
  allowSpectators: boolean | null
}) {
  socket?.send({ tag: 'LobbyCreateGame', ...input })
}

function joinPublic(gameId: string) {
  socket?.send({ tag: 'LobbyJoinPublic', gameId })
}

function joinWithPassword(gameId: string, password: string) {
  socket?.send({ tag: 'LobbyJoinWithPassword', gameId, password })
}

export const lobby = {
  status: computed(() => _status.value),
  you: computed(() => _you.value),
  users: computed(() => _users.value),
  games: computed(() => _games.value),
  chat: computed(() => _chat.value),
  lastError: computed(() => _lastError.value),
  lastCreated: computed(() => _lastCreated.value),
  lastJoined: computed(() => _lastJoined.value),
  connect,
  disconnect,
  sendChat,
  createGame,
  joinPublic,
  joinWithPassword,
}
