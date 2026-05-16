{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | WAI integration for the lobby and per-game WebSockets.
--
-- The entry point 'wsMiddleware' wraps a WAI app and intercepts upgrade
-- requests for @/ws/lobby@ and @/ws/games/:id@. Anything else falls
-- through to the underlying Yesod app.
module Invasion.Server.WebSocket
  ( WsEnv (..)
  , wsMiddleware
  , idleSweeperLoop
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, unless, when)
import Crypto.Random (getRandomBytes)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist qualified as P
import Database.Persist.Sql (Entity (..))
import Invasion.Auth.Jwt
  ( JwtClaims (..)
  , JwtSecret
  , verifyJwt
  )
import Invasion.DB (DbPool, runDB)
import Invasion.Engine
  ( Message (BeginGame, PassPriority, Setup)
  , applyMessage
  , dwarfStarterDeck
  , newGame
  )
import Invasion.Model
import Invasion.Prelude
import Invasion.Server.Lobby
import Invasion.Server.Protocol
import Invasion.Types (PlayerKey (..))
import Network.HTTP.Types (Query, parseQuery)
import Network.Wai (Application, rawPathInfo, rawQueryString)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

-- ----------------------------------------------------------------------------
-- Wiring

data WsEnv = WsEnv
  { lobby :: LobbyState
  , dbPool :: DbPool
  , jwtSecret :: JwtSecret
  }

-- | Wrap a WAI app so WS upgrades for our routes are intercepted.
wsMiddleware :: WsEnv -> Application -> Application
wsMiddleware env = websocketsOr WS.defaultConnectionOptions (dispatch env)

-- | Decide which handler to run based on the upgrade request URI.
dispatch :: WsEnv -> WS.ServerApp
dispatch env pending = do
  let req = WS.pendingRequest pending
      path = WS.requestPath req
      (pathOnly, qsBs) = BS.break (== 0x3F) path -- '?'
      qs = parseQuery (BS.drop 1 qsBs)
  case BS.split 0x2F (BS.dropWhile (== 0x2F) pathOnly) of
    ["ws", "lobby"] -> handleLobby env pending qs
    ["ws", "games", gidBs] -> handleGame env pending qs gidBs
    _ -> WS.rejectRequest pending "not found"

-- ----------------------------------------------------------------------------
-- Authentication

queryText :: ByteString -> Query -> Maybe Text
queryText key qs = case lookup key qs of
  Just (Just v) -> Just (decodeUtf8 v)
  _ -> Nothing

resolveUser :: WsEnv -> Query -> IO (Maybe UserInfo)
resolveUser env qs = case queryText "token" qs of
  Nothing -> pure Nothing
  Just tok -> do
    now <- getCurrentTime
    case verifyJwt env.jwtSecret now tok of
      Left _ -> pure Nothing
      Right claims -> do
        mu <- runDB env.dbPool (P.get (UserKey claims.sub))
        pure $ case mu of
          Just u -> Just UserInfo
            { userId = claims.sub
            , displayName = userDisplayName u
            }
          Nothing -> Nothing

-- ----------------------------------------------------------------------------
-- Lobby handler

handleLobby :: WsEnv -> WS.PendingConnection -> Query -> IO ()
handleLobby env pending qs = do
  muser <- resolveUser env qs
  case muser of
    Nothing -> WS.rejectRequest pending "unauthorized"
    Just user -> do
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 25 (pure ()) (runLobbyConn env user conn)

runLobbyConn :: WsEnv -> UserInfo -> WS.Connection -> IO ()
runLobbyConn env user conn = do
  outbox <- atomically newTQueue
  cid <- atomically (addLobbyConn env.lobby user outbox)
  -- Build the welcome inside one transaction so the snapshot is consistent.
  (welcome, usersNow) <- atomically do
    hist <- chatHistorySTM env.lobby.chat
    games <- summariesSTM env.lobby
    users <- uniqueUsersSTM env.lobby
    pure
      ( LobbyWelcome {you = user, users, games, chat = hist}
      , users
      )
  -- Send welcome only to this connection.
  atomically (sendTo outbox welcome)
  -- Notify everyone (including us) of the new user list.
  atomically (broadcastLobby env.lobby LobbyUsersUpdate {users = usersNow})

  let cleanup = do
        now <- getCurrentTime
        users' <- atomically do
          removeLobbyConn env.lobby cid
          us <- uniqueUsersSTM env.lobby
          broadcastLobby env.lobby LobbyUsersUpdate {users = us}
          pure us
        -- Suppress warning
        pure (now, users')
  _ <- try @SomeException (race_ (lobbyWriter conn outbox) (lobbyReader env user conn))
  _ <- cleanup
  pure ()

lobbyWriter :: WS.Connection -> TQueue LobbyOut -> IO ()
lobbyWriter conn outbox = forever do
  msg <- atomically (readTQueue outbox)
  WS.sendTextData conn (Aeson.encode msg)

lobbyReader :: WsEnv -> UserInfo -> WS.Connection -> IO ()
lobbyReader env user conn = forever do
  raw <- WS.receiveData conn :: IO BSL.ByteString
  case Aeson.eitherDecode raw of
    Left _ -> pure ()
    Right msg -> handleLobbyIn env user msg

handleLobbyIn :: WsEnv -> UserInfo -> LobbyIn -> IO ()
handleLobbyIn env user = \case
  LobbyChatSend {text}
    | T.null (T.strip text) -> pure ()
    | T.length text > 1000 -> pure ()
    | otherwise -> do
        now <- getCurrentTime
        let line = ChatLine {from = user, text, at = now}
        atomically do
          pushLobbyChat env.lobby line
          broadcastLobby env.lobby LobbyChatNew {line}
  LobbyCreateGame {name = gname, visibility, password} -> do
    let trimmedName = T.strip gname
    case validateGameName trimmedName of
      Just code -> notifyError env user code
      Nothing -> case visibility of
        Private | maybe False badPw password -> notifyError env user "invalid_password"
        _ -> do
          gid <- nextRandom
          token <- randomTokenText 12
          slot <- atomically do
            slot <- createGame env.lobby gid trimmedName user visibility password token
            sm <- summariesSTM env.lobby
            broadcastLobby env.lobby LobbyGamesUpdate {games = sm}
            pure slot
          -- Reply directly to the creator with the created game + token.
          notifyMe env user
            LobbyGameCreated {gameId = slot.gameId, inviteToken = Just token}
  LobbyJoinPublic {gameId = gid} -> do
    mslot <- atomically (gameLookup env.lobby gid)
    case mslot of
      Nothing -> notifyError env user "game_not_found"
      Just slot -> case slot.visibility of
        Private -> notifyError env user "game_is_private"
        Public -> do
          ok <- atomically do
            sm <- readTVar slot.seats
            let alreadySeated = any (\r -> r.user.userId == user.userId) (Map.elems sm)
            if alreadySeated || Map.size sm < 2
              then pure True
              else pure False
          if ok
            then notifyMe env user LobbyGameJoinOk {gameId = gid, inviteToken = Nothing}
            else notifyError env user "game_full"
  LobbyJoinWithPassword {gameId = gid, password = mpw} -> do
    mslot <- atomically (gameLookup env.lobby gid)
    case mslot of
      Nothing -> notifyError env user "game_not_found"
      Just slot -> case (slot.password, mpw) of
        (Just expected, Just pw) | expected == pw -> do
          ok <- atomically do
            sm <- readTVar slot.seats
            let alreadySeated = any (\r -> r.user.userId == user.userId) (Map.elems sm)
            if alreadySeated || Map.size sm < 2
              then pure True
              else pure False
          if ok
            then notifyMe env user LobbyGameJoinOk {gameId = gid, inviteToken = Nothing}
            else notifyError env user "game_full"
        _ -> notifyError env user "wrong_password"
  where
    badPw t = T.length t < 1 || T.length t > 60

notifyMe :: WsEnv -> UserInfo -> LobbyOut -> IO ()
notifyMe env user msg = atomically do
  cs <- readTVar env.lobby.connections
  traverse_ (\c -> when (c.user.userId == user.userId) (sendTo c.outbox msg)) (Map.elems cs)

notifyError :: WsEnv -> UserInfo -> Text -> IO ()
notifyError env user code = notifyMe env user LobbyError {code}

validateGameName :: Text -> Maybe Text
validateGameName n
  | T.length n < 1 = Just "name_too_short"
  | T.length n > 80 = Just "name_too_long"
  | otherwise = Nothing

-- ----------------------------------------------------------------------------
-- Game handler

handleGame :: WsEnv -> WS.PendingConnection -> Query -> ByteString -> IO ()
handleGame env pending qs gidBs = case UUID.fromASCIIBytes gidBs of
  Nothing -> WS.rejectRequest pending "bad game id"
  Just gid -> do
    muser <- resolveUser env qs
    case muser of
      Nothing -> WS.rejectRequest pending "unauthorized"
      Just user -> do
        mslot <- atomically (gameLookup env.lobby gid)
        case mslot of
          Nothing -> WS.rejectRequest pending "game not found"
          Just slot -> do
            let mPw = queryText "password" qs
                mInvite = queryText "t" qs
            authed <- canEnter slot user mPw mInvite
            if not authed
              then WS.rejectRequest pending "forbidden"
              else do
                conn <- WS.acceptRequest pending
                WS.withPingThread conn 25 (pure ()) (runGameConn env slot user conn)

canEnter :: GameSlot -> UserInfo -> Maybe Text -> Maybe Text -> IO Bool
canEnter slot user mPw mInvite = atomically do
  sm <- readTVar slot.seats
  let alreadySeated = any (\r -> r.user.userId == user.userId) (Map.elems sm)
      isHost = user.userId == slot.host.userId
      hasSlot = Map.size sm < 2
  if alreadySeated || isHost
    then pure True
    else case slot.visibility of
      Public -> pure hasSlot
      Private -> case (mInvite, mPw, slot.password) of
        (Just t, _, _) | t == slot.inviteToken -> pure hasSlot
        (_, Just pw, Just expected) | pw == expected -> pure hasSlot
        _ -> pure False

runGameConn :: WsEnv -> GameSlot -> UserInfo -> WS.Connection -> IO ()
runGameConn env slot user conn = do
  outbox <- atomically newTQueue
  -- Reserve a seat (or reclaim existing one). Refuse if both seats taken.
  mSeat <- atomically (reserveSeat slot user)
  case mSeat of
    Nothing -> do
      WS.sendTextData conn (Aeson.encode GameError {code = "no_seat"})
      WS.sendClose conn ("full" :: ByteString)
    Just _seatKey -> do
      cid <- atomically (attachGameConn slot user outbox)
      welcomeView <- atomically (gameViewSTM slot user)
      atomically (sendTo outbox GameWelcome {you = user, game = welcomeView})
      -- Inform other connections + lobby listings about the new seat.
      atomically do
        v <- gameViewSTM slot user
        broadcastGameWithView slot v
        sm <- summariesSTM env.lobby
        broadcastLobby env.lobby LobbyGamesUpdate {games = sm}
      _ <- try @SomeException
        (race_ (gameWriter conn outbox) (gameReader env slot user conn))
      now <- getCurrentTime
      atomically do
        detachGameConn slot cid now
        -- Slot view changes when conns drop (live count). Re-broadcast.
        v <- gameViewSTM slot user
        broadcastGameWithView slot v
        sm <- summariesSTM env.lobby
        broadcastLobby env.lobby LobbyGamesUpdate {games = sm}

-- | The game view depends on the viewer (host gets the invite token).
-- For broadcasts, build a per-recipient view; the host token only leaks
-- to the host's outbox.
broadcastGameWithView :: GameSlot -> GameView -> STM ()
broadcastGameWithView slot _ = do
  conns <- readTVar slot.connections
  traverse_ sendForConn (Map.elems conns)
  where
    sendForConn c = do
      v <- gameViewSTM slot c.user
      sendTo c.outbox GameUpdate {game = v}

gameWriter :: WS.Connection -> TQueue GameOut -> IO ()
gameWriter conn outbox = forever do
  msg <- atomically (readTQueue outbox)
  WS.sendTextData conn (Aeson.encode msg)

gameReader :: WsEnv -> GameSlot -> UserInfo -> WS.Connection -> IO ()
gameReader env slot user conn = forever do
  raw <- WS.receiveData conn :: IO BSL.ByteString
  case Aeson.eitherDecode raw of
    Left _ -> pure ()
    Right msg -> handleGameIn env slot user msg

handleGameIn :: WsEnv -> GameSlot -> UserInfo -> GameIn -> IO ()
handleGameIn env slot user = \case
  GameChatSend {text}
    | T.null (T.strip text) -> pure ()
    | T.length text > 1000 -> pure ()
    | otherwise -> do
        now <- getCurrentTime
        let line = ChatLine {from = user, text, at = now}
        atomically do
          pushGameChat slot line
          broadcastGame slot GameChatNew {line}
  GameSelectDeck {deckId = did} -> do
    mDeck <- runDB env.dbPool (P.get (DeckKey did))
    case mDeck of
      Nothing -> sendGameError slot user "deck_not_found"
      Just deck | deckUserId deck /= UserKey user.userId ->
        sendGameError slot user "deck_not_owned"
      Just deck -> do
        sts <- readTVarIO slot.status
        if sts /= StatusWaiting
          then sendGameError slot user "game_started"
          else do
            seatKey <- atomically do
              sm <- readTVar slot.seats
              pure $ findSeatFor user sm
            case seatKey of
              Nothing -> sendGameError slot user "not_seated"
              Just k -> do
                let dv = DeckView
                      { deckId = did
                      , name = deckName deck
                      , capital = deckCapital deck
                      , size = countCards (deckCards deck)
                      }
                atomically do
                  setSeatDeck slot k dv
                  v <- gameViewSTM slot user
                  broadcastGameWithView slot v
  GameClearDeck -> do
    sts <- readTVarIO slot.status
    when (sts == StatusWaiting) do
      seatKey <- atomically do
        sm <- readTVar slot.seats
        pure $ findSeatFor user sm
      case seatKey of
        Nothing -> pure ()
        Just k -> atomically do
          clearSeatDeck slot k
          v <- gameViewSTM slot user
          broadcastGameWithView slot v
  GameStart
    | user.userId /= slot.host.userId -> sendGameError slot user "not_host"
    | otherwise -> do
        sts <- readTVarIO slot.status
        if sts /= StatusWaiting
          then sendGameError slot user "already_started"
          else do
            sm <- readTVarIO slot.seats
            let ready = Map.size sm == 2
                  && all (\r -> case r.deck of Just _ -> True; Nothing -> False)
                       (Map.elems sm)
            if not ready
              then sendGameError slot user "not_ready"
              else do
                -- Build the engine game and pump it through Setup +
                -- BeginGame so we land on the first action window. We
                -- still use the dwarf starter for both seats — actual
                -- deck loading from the chosen deckIds is the next
                -- step, but the engine has nothing to do with the
                -- card pool yet so this is enough to drive the basic
                -- phase machinery.
                case newGame dwarfStarterDeck dwarfStarterDeck of
                  Left err -> sendGameError slot user (T.pack err)
                  Right g0 -> do
                    g1 <- applyMessage g0 Setup
                    g2 <- applyMessage g1 BeginGame
                    atomically do
                      writeTVar slot.engine (Just g2)
                      trySetStatus slot StatusPlaying
                      v <- gameViewSTM slot user
                      broadcastGameWithView slot v
                      summaries <- summariesSTM env.lobby
                      broadcastLobby env.lobby LobbyGamesUpdate {games = summaries}
  GamePassPriority -> do
    mGame <- readTVarIO slot.engine
    case mGame of
      Nothing -> sendGameError slot user "game_not_started"
      Just g -> do
        seatKey <- atomically do
          sm <- readTVar slot.seats
          pure $ findSeatFor user sm
        case seatKey >>= seatKeyToPlayerKey of
          Nothing -> sendGameError slot user "not_seated"
          Just pk -> do
            g' <- applyMessage g (PassPriority pk)
            atomically do
              writeTVar slot.engine (Just g')
              v <- gameViewSTM slot user
              broadcastGameWithView slot v
  GameLeave -> do
    sts <- readTVarIO slot.status
    -- A "leave" while playing ends the game.
    atomically do
      _ <- removeSeat slot user.userId
      when (sts == StatusPlaying) (trySetStatus slot StatusEnded)
      v <- gameViewSTM slot user
      broadcastGameWithView slot v
      summaries <- summariesSTM env.lobby
      broadcastLobby env.lobby LobbyGamesUpdate {games = summaries}
      -- Push a Closed notice to this user's connections so the UI can
      -- redirect them back to the lobby cleanly.
      conns <- readTVar slot.connections
      traverse_
        (\c -> when (c.user.userId == user.userId)
          (sendTo c.outbox GameClosed {reason = "left"}))
        (Map.elems conns)

findSeatFor :: UserInfo -> Map.Map Text SeatRow -> Maybe Text
findSeatFor user m =
  let matches = [k | (k, r) <- Map.toList m, r.user.userId == user.userId]
   in case matches of
        (k : _) -> Just k
        [] -> Nothing

seatKeyToPlayerKey :: Text -> Maybe PlayerKey
seatKeyToPlayerKey = \case
  "Player1" -> Just Player1
  "Player2" -> Just Player2
  _ -> Nothing

sendGameError :: GameSlot -> UserInfo -> Text -> IO ()
sendGameError slot user code = atomically do
  cs <- readTVar slot.connections
  traverse_
    (\c -> when (c.user.userId == user.userId) (sendTo c.outbox GameError {code}))
    (Map.elems cs)

countCards :: Aeson.Value -> Int
countCards (Aeson.Object o) =
  sum [countOne v | (_, v) <- KM.toList o]
  where
    countOne (Aeson.Number n) = truncate (realToFrac n :: Double)
    countOne _ = 0
countCards _ = 0

-- ----------------------------------------------------------------------------
-- Idle sweeper

idleSweeperLoop :: WsEnv -> IO ()
idleSweeperLoop env = forever do
  threadDelay (60 * 1_000_000)
  now <- getCurrentTime
  dead <- atomically do
    ds <- sweepIdle env.lobby now
    unless (null ds) do
      sm <- summariesSTM env.lobby
      broadcastLobby env.lobby LobbyGamesUpdate {games = sm}
    pure ds
  -- Force any straggler connections (none expected, since sweep requires empty)
  traverse_
    (\_ -> pure ())
    dead

-- ----------------------------------------------------------------------------
-- Random tokens

randomTokenText :: Int -> IO Text
randomTokenText n = do
  bs <- getRandomBytes n :: IO BS.ByteString
  pure (decodeUtf8 (BAE.convertToBase BAE.Base64URLUnpadded bs))
