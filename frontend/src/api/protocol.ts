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
  allowSpectators: boolean
  spectatorCount: number
}

export interface GameView {
  gameId: string
  name: string
  host: UserInfo
  visibility: Visibility
  hasPassword: boolean
  allowSpectators: boolean
  spectatorCount: number
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

export type CardKind = 'Unit' | 'Support' | 'Quest' | 'Tactic' | 'Legend' | 'DraftFormat'

// Mirror of Invasion.Types.Number: either Fixed n or Variable.
export type EngineNumber =
  | { tag: 'Fixed'; contents: number }
  | { tag: 'Variable' }

// Mirror of Invasion.CardDef.Trait. Enum constructors serialize as their
// bare names (allNullaryToStringTag = True by default).
export type Trait =
  | 'Warrior' | 'Spell' | 'Engineer' | 'Elite' | 'Slayer' | 'Priest' | 'Hero'
  | 'Ranger' | 'Rune' | 'Building' | 'Attachment' | 'Weapon' | 'Siege'
  | 'Daemon' | 'Creature' | 'Sorcerer' | 'Knight' | 'Cavalry' | 'Mission'
  | 'QuestTrait' | 'Wasteland' | 'CapitalCenter' | 'Rift' | 'Relic'

// Card definition as serialized by Invasion.CardDef.ToJSON. The 'receive'
// function field is dropped on the wire — see CardDef.hs.
export interface EngineCardDef {
  code: string
  title: string
  kind: CardKind
  races: Race[]
  cost: EngineNumber
  loyalty: number
  power: number
  hitPoints: EngineNumber | null
  traits: Trait[]
  text: string | null
  flavor: string | null
  keywords: unknown[]
  unique: boolean
}

// A specific card instance (Invasion.Card.Card). Every card in deck,
// hand, discard, or play carries the same stable `key` from setup
// through the end of the game. The frontend uses this key as its CSS
// view-transition name so a card visually morphs as it moves between
// surfaces (hand → zone → discard).
//
// On the wire, the backend flattens the card definition's fields onto
// the same object as `key`, so EngineCard extends EngineCardDef.
export interface EngineCard extends EngineCardDef {
  key: number
}

// In-play unit (Invasion.Entity.UnitDetails). 'key' is the engine's
// 'UnitKey', written as a bare integer on the wire.
export interface EngineUnit {
  key: number
  controller: PlayerKey
  zone: ZoneKind
  cardDef: EngineCardDef
  damage: number
  corrupted: boolean
  attachments: EngineSupport[]
  experiences: string[]
}

export interface EngineSupport {
  key: number
  controller: PlayerKey
  zone: ZoneKind
  cardDef: EngineCardDef
  attachedTo: number | null
  tokens: number
}

export interface EngineQuest {
  key: number
  controller: PlayerKey
  cardDef: EngineCardDef
  tokens: number
}

// A legend in play. Legends live on their controller's capital board
// (not inside a zone) but the engine still carries a `zone` tag for
// rendering / attack-routing parity with units. Each player may have at
// most one legend in play at a time.
export interface EngineLegend {
  key: number
  controller: PlayerKey
  zone: ZoneKind
  cardDef: EngineCardDef
  damage: number
}

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
  // Hand cards carry the full card-def payload so the UI can render the
  // art and decide what kind of "play" action to send. Deck/discard only
  // need their lengths for piles, but the same payload arrives there too.
  // Each card carries the stable `key` that the engine uses to identify
  // this specific copy — pass it back in `GamePlayCard` to disambiguate
  // duplicates in hand.
  hand: EngineCard[]
  deck: EngineCard[]
  discard: EngineCard[]
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
  units: EngineUnit[]
  supports: EngineSupport[]
  quests: EngineQuest[]
  legends: EngineLegend[]
  nextUnitKey: number
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
      // Optional: when null, server defaults to true for public games
      // and false for private ones.
      allowSpectators: boolean | null
    }
  | { tag: 'LobbyJoinPublic'; gameId: string }
  | { tag: 'LobbyJoinWithPassword'; gameId: string; password: string | null }

export type LobbyOut =
  | {
      tag: 'LobbyWelcome'
      // Null for guest connections — they see chat + games but can't
      // post or host.
      you: UserInfo | null
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
  | {
      // Play a specific card instance from this seat's hand. `cardKey`
      // is the engine's stable identity for that card (the same `key`
      // value carried on each EngineCard).
      tag: 'GamePlayCard'
      cardKey: number
      zone: ZoneKind | null
      target: number | null
    }
  | { tag: 'GameLeave' }

export type GameOut =
  // Null `you` indicates a guest spectator (no signed-in account).
  | { tag: 'GameWelcome'; you: UserInfo | null; game: GameView }
  | { tag: 'GameUpdate'; game: GameView }
  | { tag: 'GameChatNew'; line: ChatLine }
  | { tag: 'GameError'; code: string }
  | { tag: 'GameClosed'; reason: string }
