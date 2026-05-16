// Wire types for the lobby and per-game WebSockets.
// Mirror of backend/src/Invasion/Server/Protocol.hs. When that file
// changes, this one changes in the same PR.

export interface UserInfo {
  userId: string
  displayName: string
}

export interface ChatLine {
  from: UserInfo
  text: string
  at: string // ISO timestamp from the server
}

export type Visibility = 'Public' | 'Private'

export type GameStatus = 'StatusWaiting' | 'StatusPlaying' | 'StatusEnded'

export interface DeckView {
  deckId: string
  name: string
  capital: string | null
  size: number
}

export interface SeatView {
  seat: string // "Player1" | "Player2"
  user: UserInfo
  isHost: boolean
  deck: DeckView | null
}

export interface GameSummary {
  gameId: string
  name: string
  host: UserInfo
  visibility: Visibility
  hasPassword: boolean
  filledSeats: number
  status: GameStatus
}

export interface GameView {
  gameId: string
  name: string
  host: UserInfo
  visibility: Visibility
  hasPassword: boolean
  inviteToken: string | null
  seats: SeatView[]
  status: GameStatus
  chat: ChatLine[]
}

// ---------------------------------------------------------------------------
// Lobby socket

export type LobbyIn =
  | { tag: 'LobbyChatSend'; text: string }
  | {
      tag: 'LobbyCreateGame'
      name: string
      visibility: Visibility
      password: string | null
    }
  | { tag: 'LobbyJoinPublic'; gameId: string }
  | { tag: 'LobbyJoinWithPassword'; gameId: string; password: string | null }

export type LobbyOut =
  | {
      tag: 'LobbyWelcome'
      you: UserInfo
      users: UserInfo[]
      games: GameSummary[]
      chat: ChatLine[]
    }
  | { tag: 'LobbyChatNew'; line: ChatLine }
  | { tag: 'LobbyUsersUpdate'; users: UserInfo[] }
  | { tag: 'LobbyGamesUpdate'; games: GameSummary[] }
  | { tag: 'LobbyGameCreated'; gameId: string; inviteToken: string | null }
  | { tag: 'LobbyGameJoinOk'; gameId: string; inviteToken: string | null }
  | { tag: 'LobbyError'; code: string }

// ---------------------------------------------------------------------------
// Game socket

export type GameIn =
  | { tag: 'GameChatSend'; text: string }
  | { tag: 'GameSelectDeck'; deckId: string }
  | { tag: 'GameClearDeck' }
  | { tag: 'GameStart' }
  | { tag: 'GameLeave' }

export type GameOut =
  | { tag: 'GameWelcome'; you: UserInfo; game: GameView }
  | { tag: 'GameUpdate'; game: GameView }
  | { tag: 'GameChatNew'; line: ChatLine }
  | { tag: 'GameError'; code: string }
  | { tag: 'GameClosed'; reason: string }
