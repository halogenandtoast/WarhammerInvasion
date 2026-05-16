{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | In-memory state for the global lobby and active game slots.
--
-- One 'LobbyState' value lives in the 'Invasion.Server.App' and is shared
-- across HTTP and WebSocket handlers. All mutations go through STM so
-- the runtime can hand the same view to every connected client.
module Invasion.Server.Lobby
  ( -- * State
    LobbyState (..)
  , LobbyConn (..)
  , GameSlot (..)
  , SeatRow (..)
  , GameConn (..)
  , ConnId
  , newLobbyState
    -- * Lobby ops
  , addLobbyConn
  , removeLobbyConn
  , pushLobbyChat
  , uniqueUsersSTM
  , broadcastLobby
  , sendTo
  , summariesSTM
  , chatHistorySTM
  , gameLookup
    -- * Game ops
  , createGame
  , attachGameConn
  , detachGameConn
  , pushGameChat
  , broadcastGame
  , setSeatDeck
  , clearSeatDeck
  , gameViewSTM
  , reserveSeat
  , removeSeat
  , trySetStatus
  , markGameEmptyIfIdle
  , sweepIdle
    -- * Constants
  , chatHistoryLimit
  , idleTtl
    -- * IDs
  , freshConnId
  ) where

import Control.Concurrent.STM
import Control.Monad (forM, unless, when)
import Data.Aeson qualified as Aeson
import Data.Aeson (toJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Data.UUID (UUID)
import Invasion.Game (Game)
import Invasion.Prelude
import Invasion.Server.Protocol

-- ----------------------------------------------------------------------------
-- Identifiers

type ConnId = Int

-- ----------------------------------------------------------------------------
-- Per-connection record

data LobbyConn = LobbyConn
  { connId :: ConnId
  , user :: UserInfo
  , outbox :: TQueue LobbyOut
  }

data GameConn = GameConn
  { connId :: ConnId
  , user :: UserInfo
  , outbox :: TQueue GameOut
  }

-- ----------------------------------------------------------------------------
-- Game slot

data SeatRow = SeatRow
  { user :: UserInfo
  , deck :: Maybe DeckView
  }

data GameSlot = GameSlot
  { gameId :: UUID
  , name :: Text
  , host :: UserInfo
  , visibility :: Visibility
  , password :: Maybe Text
  , inviteToken :: Text
  , seats :: TVar (Map Text SeatRow) -- key = "Player1" | "Player2"
  , chat :: TVar (Seq ChatLine)
  , status :: TVar GameStatus
  , connections :: TVar (Map ConnId GameConn)
  , lastEmptyAt :: TVar (Maybe UTCTime)
  , engine :: TVar (Maybe Game)
    -- ^ The authoritative engine state. 'Nothing' while the slot is in
    -- 'StatusWaiting'; 'Just' once 'GameStart' kicks off Setup +
    -- BeginGame. Mutated by the WebSocket handler thread, never by the
    -- engine itself (see ARCHITECTURE.md §3.3).
  }

-- ----------------------------------------------------------------------------
-- Lobby state

data LobbyState = LobbyState
  { connections :: TVar (Map ConnId LobbyConn)
  , chat :: TVar (Seq ChatLine)
  , games :: TVar (Map UUID GameSlot)
  , nextConnId :: TVar ConnId
  }

newLobbyState :: IO LobbyState
newLobbyState = atomically do
  connections <- newTVar Map.empty
  chat <- newTVar Seq.empty
  games <- newTVar Map.empty
  nextConnId <- newTVar 1
  pure LobbyState {..}

freshConnId :: LobbyState -> STM ConnId
freshConnId st = do
  n <- readTVar st.nextConnId
  writeTVar st.nextConnId (n + 1)
  pure n

-- ----------------------------------------------------------------------------
-- Lobby connection book-keeping

addLobbyConn :: LobbyState -> UserInfo -> TQueue LobbyOut -> STM ConnId
addLobbyConn st user outbox = do
  cid <- freshConnId st
  modifyTVar' st.connections (Map.insert cid LobbyConn {connId = cid, user, outbox})
  pure cid

removeLobbyConn :: LobbyState -> ConnId -> STM ()
removeLobbyConn st cid = modifyTVar' st.connections (Map.delete cid)

pushLobbyChat :: LobbyState -> ChatLine -> STM ()
pushLobbyChat st line = modifyTVar' st.chat (capChat . (|> line))

capChat :: Seq ChatLine -> Seq ChatLine
capChat s
  | Seq.length s > chatHistoryLimit = Seq.drop (Seq.length s - chatHistoryLimit) s
  | otherwise = s

chatHistorySTM :: TVar (Seq ChatLine) -> STM [ChatLine]
chatHistorySTM v = foldr (:) [] <$> readTVar v

uniqueUsersSTM :: LobbyState -> STM [UserInfo]
uniqueUsersSTM st = do
  conns <- readTVar st.connections
  let users :: Set (UUID, Text)
      users = Set.fromList [(u.userId, u.displayName) | c <- Map.elems conns, let u = c.user]
  pure [UserInfo i n | (i, n) <- Set.toList users]

broadcastLobby :: LobbyState -> LobbyOut -> STM ()
broadcastLobby st msg = do
  conns <- readTVar st.connections
  traverse_ (\c -> writeTQueue c.outbox msg) (Map.elems conns)

sendTo :: TQueue a -> a -> STM ()
sendTo q msg = writeTQueue q msg

-- ----------------------------------------------------------------------------
-- Game registry

createGame
  :: LobbyState
  -> UUID
  -> Text
  -> UserInfo
  -> Visibility
  -> Maybe Text
  -> Text
  -> STM GameSlot
createGame st gid name host vis pw token = do
  seats <- newTVar Map.empty
  chat <- newTVar Seq.empty
  status <- newTVar StatusWaiting
  connections <- newTVar Map.empty
  lastEmptyAt <- newTVar Nothing
  engine <- newTVar Nothing
  let slot = GameSlot
        { gameId = gid
        , name
        , host
        , visibility = vis
        , password = pw
        , inviteToken = token
        , seats
        , chat
        , status
        , connections
        , lastEmptyAt
        , engine
        }
  modifyTVar' st.games (Map.insert gid slot)
  pure slot

gameLookup :: LobbyState -> UUID -> STM (Maybe GameSlot)
gameLookup st gid = Map.lookup gid <$> readTVar st.games

summariesSTM :: LobbyState -> STM [GameSummary]
summariesSTM st = do
  slots <- readTVar st.games
  fmap (sortOn (.gameId)) $ forM (Map.elems slots) \s -> do
    sts <- readTVar s.status
    sm <- readTVar s.seats
    pure GameSummary
      { gameId = s.gameId
      , name = s.name
      , host = s.host
      , visibility = s.visibility
      , hasPassword = isNothing s.password == False
      , filledSeats = Map.size sm
      , status = sts
      }

-- | Attach a fresh WS connection to a game slot. Bumps the slot out of
-- "everybody left" idle countdown.
attachGameConn :: GameSlot -> UserInfo -> TQueue GameOut -> STM ConnId
attachGameConn slot user outbox = do
  -- Use the slot's own connId space — we never collide with lobby ids
  -- because the conn id is only ever compared within a slot.
  cs <- readTVar slot.connections
  let cid = case Map.keys cs of
        [] -> 1
        ks -> maximum ks + 1
  modifyTVar' slot.connections (Map.insert cid GameConn {connId = cid, user, outbox})
  writeTVar slot.lastEmptyAt Nothing
  pure cid

detachGameConn :: GameSlot -> ConnId -> UTCTime -> STM ()
detachGameConn slot cid now = do
  modifyTVar' slot.connections (Map.delete cid)
  cs <- readTVar slot.connections
  when (Map.null cs) (writeTVar slot.lastEmptyAt (Just now))

pushGameChat :: GameSlot -> ChatLine -> STM ()
pushGameChat slot line = modifyTVar' slot.chat (capChat . (|> line))

broadcastGame :: GameSlot -> GameOut -> STM ()
broadcastGame slot msg = do
  conns <- readTVar slot.connections
  traverse_ (\c -> writeTQueue c.outbox msg) (Map.elems conns)

setSeatDeck :: GameSlot -> Text -> DeckView -> STM ()
setSeatDeck slot seatKey dv = modifyTVar' slot.seats \m ->
  Map.adjust (\row -> SeatRow {user = row.user, deck = Just dv}) seatKey m

clearSeatDeck :: GameSlot -> Text -> STM ()
clearSeatDeck slot seatKey = modifyTVar' slot.seats \m ->
  Map.adjust (\row -> SeatRow {user = row.user, deck = Nothing}) seatKey m

-- | Try to seat the user. Returns the seat key they ended up in (existing
-- seat if they already had one, or the next vacant seat). Returns
-- @Nothing@ if both seats are taken by other users.
reserveSeat :: GameSlot -> UserInfo -> STM (Maybe Text)
reserveSeat slot user = do
  sm <- readTVar slot.seats
  case find (\(_, r) -> r.user.userId == user.userId) (Map.toList sm) of
    Just (k, _) -> pure (Just k)
    Nothing -> do
      let vacant = case (Map.lookup "Player1" sm, Map.lookup "Player2" sm) of
            (Nothing, _) -> Just "Player1"
            (_, Nothing) -> Just "Player2"
            _ -> Nothing
      case vacant of
        Nothing -> pure Nothing
        Just k -> do
          writeTVar slot.seats (Map.insert k (SeatRow user Nothing) sm)
          pure (Just k)

-- | Remove a user from their seat, if any. Returns whether anything was
-- actually removed.
removeSeat :: GameSlot -> UUID -> STM Bool
removeSeat slot uid = do
  sm <- readTVar slot.seats
  case find (\(_, r) -> r.user.userId == uid) (Map.toList sm) of
    Just (k, _) -> do
      writeTVar slot.seats (Map.delete k sm)
      pure True
    Nothing -> pure False

trySetStatus :: GameSlot -> GameStatus -> STM ()
trySetStatus slot s = writeTVar slot.status s

gameViewSTM :: GameSlot -> UserInfo -> STM GameView
gameViewSTM slot viewer = do
  sm <- readTVar slot.seats
  sts <- readTVar slot.status
  chatLines <- foldr (:) [] <$> readTVar slot.chat
  mEngine <- readTVar slot.engine
  let seatList =
        [ SeatView
            { seat = k
            , user = r.user
            , isHost = r.user.userId == slot.host.userId
            , deck = r.deck
            }
        | (k, r) <- sortOn fst (Map.toList sm)
        ]
      tokenForViewer
        | viewer.userId == slot.host.userId = Just slot.inviteToken
        | otherwise = Nothing
  pure GameView
    { gameId = slot.gameId
    , name = slot.name
    , host = slot.host
    , visibility = slot.visibility
    , hasPassword = case slot.password of Just _ -> True; Nothing -> False
    , inviteToken = tokenForViewer
    , seats = seatList
    , status = sts
    , chat = chatLines
    , engine = fmap toJSON mEngine
    }

-- | If the slot has no connections, write 'now' to the empty timestamp.
-- Idempotent.
markGameEmptyIfIdle :: GameSlot -> UTCTime -> STM ()
markGameEmptyIfIdle slot now = do
  cs <- readTVar slot.connections
  when (Map.null cs) do
    last' <- readTVar slot.lastEmptyAt
    when (isNothing last') (writeTVar slot.lastEmptyAt (Just now))

-- | Idle slots whose @lastEmptyAt@ is older than 'idleTtl'. Removed from
-- the registry, returned so the caller can close stragglers and announce
-- the new list.
sweepIdle :: LobbyState -> UTCTime -> STM [GameSlot]
sweepIdle st now = do
  slots <- readTVar st.games
  ageOuts <- forM (Map.toList slots) \(gid, slot) -> do
    cs <- readTVar slot.connections
    if not (Map.null cs)
      then pure Nothing
      else do
        mLast <- readTVar slot.lastEmptyAt
        case mLast of
          Just t | diffUTCTime now t > idleTtl -> pure (Just (gid, slot))
          _ -> pure Nothing
  let dead = [s | Just s <- ageOuts]
  unless (null dead) do
    writeTVar st.games (foldr Map.delete slots (map fst dead))
  pure (map snd dead)

-- ----------------------------------------------------------------------------
-- Constants

chatHistoryLimit :: Int
chatHistoryLimit = 100

idleTtl :: NominalDiffTime
idleTtl = 300 -- 5 minutes
