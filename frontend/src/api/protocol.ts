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
  // The engine snapshot. `null` while the game is waiting; populated
  // once Setup + BeginGame have run. Kept here as a single payload so
  // the renderer can derive everything from one source of truth.
  engine: EngineGame | null
}

// ---------------------------------------------------------------------------
// Engine snapshot
// Mirror of the JSON produced by Invasion.Game's ToJSON instance.

export type PlayerKey = 'Player1' | 'Player2'

export type Phase = 'KingdomPhase' | 'QuestPhase' | 'CapitalPhase' | 'BattlefieldPhase'

export type ZoneKind = 'KingdomZone' | 'QuestZone' | 'BattlefieldZone'

export interface EngineZone {
  kind: ZoneKind
  developments: number
  damage: number
  burned: boolean
  // hitPoints is derived (8 + developments); not on the wire.
}

export interface EngineCapital {
  kingdom: EngineZone
  quest: EngineZone
  battlefield: EngineZone
}

export type ActionWindowTrigger =
  | 'KingdomActionWindow'
  | 'QuestActionWindow'
  | 'CapitalActionWindow'
  | 'BattlefieldActionWindow'
  | 'AfterDeclareCombatTarget'
  | 'AfterDeclareAttackers'
  | 'AfterDeclareDefenders'
  | 'AfterAssignCombatDamage'
  | 'AfterApplyCombatDamage'

export type PassState =
  | { tag: 'NoPasses'; contents: PlayerKey }
  | { tag: 'OnePass'; contents: PlayerKey }

export interface EngineActionWindow {
  trigger: ActionWindowTrigger
  awaiting: PassState
}

export type EliminationReason = 'DeckedOut' | 'CapitalBurned'

export type PlayerLifecycle =
  | { tag: 'IdlePlayer' }
  | { tag: 'Eliminated'; contents: EliminationReason }
  | { tag: 'PlayerDraw'; contents: unknown }

// Currently only Dwarf exists engine-side, but the asset set covers the
// full Warhammer: Invasion race list. Add to this union as we add races
// to backend/src/Invasion/Types.hs.
export type Race =
  | 'Dwarf'
  | 'Empire'
  | 'HighElf'
  | 'Chaos'
  | 'Orc'
  | 'DarkElf'

export interface EnginePlayer {
  key: PlayerKey
  state: PlayerLifecycle
  capital: EngineCapital
  resources: number
  // hand / deck / discard are arrays of card-def JSON; we only need
  // their .length for the basic in-game UI, so leave them unknown.
  hand: unknown[]
  deck: unknown[]
  discard: unknown[]
  race: Race
}

export type GameLifecycle =
  | { tag: 'GameSetup' }
  | { tag: 'GamePlaying' }
  | {
      tag: 'GameFinished'
      contents: {
        winner: PlayerKey
        reason: 'OpponentDeckedOut' | 'OpponentCapitalBurned'
      }
    }

export type LogCategory =
  | 'LogSystem'
  | 'LogPhase'
  | 'LogTurn'
  | 'LogPlayerAction'
  | 'LogResult'

// Engine-emitted transcript entry. `key` is an i18n key (resolved in
// frontend/src/locales/). `params` carries interpolation values; enum-
// shaped values (player keys, phases, triggers, reasons) are written
// raw and resolved through nested i18n lookups before substitution —
// see `formatLogEntry` in the game view.
export interface LogEntry {
  at: string
  category: LogCategory
  key: string
  params: Record<string, string>
}

export interface EngineGame {
  player1: EnginePlayer
  player2: EnginePlayer
  firstPlayer: PlayerKey
  currentPlayer: PlayerKey
  turn: number
  phase: Phase | null
  actionWindow: EngineActionWindow | null
  modifiers: unknown
  lifecycle: GameLifecycle
  log: LogEntry[]
  units: unknown[]
  nextUnitKey: unknown
}

// Derived helpers — keep alongside the wire types so they stay in sync.
export function zoneHitPoints(z: EngineZone): number {
  return 8 + z.developments
}

export function zoneBurning(z: EngineZone): boolean {
  return z.damage >= zoneHitPoints(z)
}

export function priorityHolder(s: PassState): PlayerKey {
  return s.contents
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
  | { tag: 'GamePassPriority' }
  | { tag: 'GameLeave' }

export type GameOut =
  | { tag: 'GameWelcome'; you: UserInfo; game: GameView }
  | { tag: 'GameUpdate'; game: GameView }
  | { tag: 'GameChatNew'; line: ChatLine }
  | { tag: 'GameError'; code: string }
  | { tag: 'GameClosed'; reason: string }
