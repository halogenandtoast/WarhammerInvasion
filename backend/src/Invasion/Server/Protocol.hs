{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Wire types for the lobby and per-game WebSockets.
--
-- The frontend mirror lives in @frontend/src/api/protocol.ts@; keep the
-- two in lockstep. Aeson's default 'TaggedObject' encoding produces
-- @{ "tag": "ConstructorName", ... }@ which the TS side discriminates on.
module Invasion.Server.Protocol
  ( -- * Common
    UserInfo (..)
  , ChatLine (..)
  , Visibility (..)
  , GameStatus (..)
  , GameSummary (..)
  , SeatView (..)
  , DeckView (..)
  , GameView (..)
    -- * Lobby socket
  , LobbyIn (..)
  , LobbyOut (..)
    -- * Game socket
  , GameIn (..)
  , GameOut (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Invasion.Prelude

-- ----------------------------------------------------------------------------
-- Common

data UserInfo = UserInfo
  { userId :: UUID
  , displayName :: Text
  }
  deriving stock (Show, Eq, Generic)

data ChatLine = ChatLine
  { from :: UserInfo
  , text :: Text
  , at :: UTCTime
  }
  deriving stock (Show, Generic)

data Visibility = Public | Private
  deriving stock (Show, Eq, Generic)

data GameStatus
  = StatusWaiting
  | StatusPlaying
  | StatusEnded
  deriving stock (Show, Eq, Generic)

data DeckView = DeckView
  { deckId :: UUID
  , name :: Text
  , capital :: Maybe Text
  , size :: Int
  }
  deriving stock (Show, Generic)

data SeatView = SeatView
  { seat :: Text -- "Player1" | "Player2"
  , user :: UserInfo
  , isHost :: Bool
  , deck :: Maybe DeckView
  }
  deriving stock (Show, Generic)

-- | What lobby clients see in the game list. Private games are not
-- broadcast at all, so this only ever describes public ones.
data GameSummary = GameSummary
  { gameId :: UUID
  , name :: Text
  , host :: UserInfo
  , visibility :: Visibility
  , hasPassword :: Bool
  , filledSeats :: Int -- 0..2
  , status :: GameStatus
  }
  deriving stock (Show, Generic)

-- | The full server-side view of a game, sent to clients connected to
-- its game socket. 'inviteToken' is only populated for the host.
data GameView = GameView
  { gameId :: UUID
  , name :: Text
  , host :: UserInfo
  , visibility :: Visibility
  , hasPassword :: Bool
  , inviteToken :: Maybe Text
  , seats :: [SeatView]
  , status :: GameStatus
  , chat :: [ChatLine]
  }
  deriving stock (Show, Generic)

-- ----------------------------------------------------------------------------
-- Lobby socket

data LobbyIn
  = -- | Send a chat line to the global lobby.
    LobbyChatSend { text :: Text }
  | -- | Create a new game slot. Server replies with 'LobbyGameCreated'.
    LobbyCreateGame
      { name :: Text
      , visibility :: Visibility
      , password :: Maybe Text
      }
  | -- | Join a public game (no password needed). Reply: 'LobbyGameJoinOk'.
    LobbyJoinPublic { gameId :: UUID }
  | -- | Join a password-gated game. Reply: 'LobbyGameJoinOk' or 'LobbyError'.
    LobbyJoinWithPassword { gameId :: UUID, password :: Maybe Text }
  deriving stock (Show, Generic)

data LobbyOut
  = LobbyWelcome
      { you :: UserInfo
      , users :: [UserInfo]
      , games :: [GameSummary]
      , chat :: [ChatLine]
      }
  | LobbyChatNew { line :: ChatLine }
  | LobbyUsersUpdate { users :: [UserInfo] }
  | LobbyGamesUpdate { games :: [GameSummary] }
  | LobbyGameCreated
      { gameId :: UUID
      , inviteToken :: Maybe Text
      }
  | LobbyGameJoinOk
      { gameId :: UUID
      , inviteToken :: Maybe Text
      }
  | LobbyError { code :: Text }
  deriving stock (Show, Generic)

-- ----------------------------------------------------------------------------
-- Per-game socket

data GameIn
  = GameChatSend { text :: Text }
  | -- | Replace this seat's loaded deck with the deck identified by id.
    -- The deck must belong to the seated user.
    GameSelectDeck { deckId :: UUID }
  | -- | Clear the loaded deck for this seat.
    GameClearDeck
  | -- | Host-only. Transition Waiting -> Playing if both seats have decks.
    GameStart
  | -- | Drop this user from the seat, broadcast to the other seat.
    GameLeave
  deriving stock (Show, Generic)

data GameOut
  = GameWelcome
      { you :: UserInfo
      , game :: GameView
      }
  | GameUpdate { game :: GameView }
  | GameChatNew { line :: ChatLine }
  | GameError { code :: Text }
  | -- | Sent when the slot is being torn down. Frontend should redirect
    -- back to the lobby.
    GameClosed { reason :: Text }
  deriving stock (Show, Generic)

mconcat
  [ deriveJSON defaultOptions ''UserInfo
  , deriveJSON defaultOptions ''ChatLine
  , deriveJSON defaultOptions ''Visibility
  , deriveJSON defaultOptions ''GameStatus
  , deriveJSON defaultOptions ''DeckView
  , deriveJSON defaultOptions ''SeatView
  , deriveJSON defaultOptions ''GameSummary
  , deriveJSON defaultOptions ''GameView
  , deriveJSON defaultOptions ''LobbyIn
  , deriveJSON defaultOptions ''LobbyOut
  , deriveJSON defaultOptions ''GameIn
  , deriveJSON defaultOptions ''GameOut
  ]

-- Pacify -Wunused-top-binds on the generic-derived hooks above.
_unused :: (FromJSON UserInfo, ToJSON UserInfo) => ()
_unused = ()
