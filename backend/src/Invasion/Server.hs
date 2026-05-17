{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | HTTP routes: game, auth, decks.
--
-- All handlers live in this module so Yesod's 'mkYesod' template can see
-- them. Split into sub-modules if/when the file grows past a few hundred
-- lines.
--
-- 'mkYesod' generates @resourcesApp@ and a @Widget@ alias that this app
-- doesn't consume directly, so unused-top-binds is silenced.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Invasion.Server (App (..), runServer) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import Data.Aeson ((.:?))
import Data.Aeson qualified as Aeson
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isDigit)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.Esqueleto.Experimental qualified as E
import Database.Persist qualified as P
import Database.Persist.Sql (Entity (..), SqlPersistT)
import Invasion.Auth.Jwt
  ( JwtClaims (..)
  , JwtError (..)
  , JwtSecret (..)
  , issueJwt
  , verifyJwt
  )
import Invasion.Auth.Password (hashPassword, verifyPassword)
import Invasion.DB (DbPool, runDB)
import Invasion.Engine (runSetup)
import Invasion.Model
import Invasion.Prelude
import Invasion.Server.Lobby
  ( LobbyState
  , broadcastMaintenance
  , readMaintenanceSTM
  , setMaintenanceSTM
  )
import Invasion.Server.Protocol (MaintenanceState (..))
import Invasion.Server.WebSocket (WsEnv (..), idleSweeperLoop, wsMiddleware)
import Network.HTTP.Types.Status
  ( Status
  , status200
  , status201
  , status204
  , status400
  , status401
  , status404
  , status409
  )
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, sameSiteLax)
import Yesod.Core

-- ----------------------------------------------------------------------------
-- App

data App = App
  { dbPool :: DbPool
  , jwtSecret :: JwtSecret
  , accessTtl :: NominalDiffTime
  , refreshTtl :: NominalDiffTime
  , cookieSecure :: Bool
  , lobby :: LobbyState
  , adminToken :: Maybe Text
    -- ^ Shared secret for @/api/admin/*@. 'Nothing' (WHI_ADMIN_TOKEN
    -- unset) disables the admin endpoints entirely.
  }

mkYesod "App" [parseRoutes|
/api/health                HealthR        GET
/api/game                  GameR          POST
/api/auth/register         RegisterR      POST
/api/auth/login            LoginR         POST
/api/auth/refresh          RefreshR       POST
/api/auth/logout           LogoutR        POST
/api/auth/me               MeR            GET
/api/decks                 DecksR         GET POST
/api/decks/#UUID           DeckR          GET PUT DELETE
/api/admin/maintenance     MaintenanceR   GET POST DELETE
|]

instance Yesod App where
  yesodMiddleware = defaultYesodMiddleware

-- ----------------------------------------------------------------------------
-- DB helper

runHandlerDB :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> Handler a
runHandlerDB action = do
  pool <- (.dbPool) <$> getYesod
  liftIO $ runDB pool action

-- | Send a bodyless 204. 'sendStatusJSON status204' would attach a JSON
-- body, which RFC 9110 forbids and which the Vite dev proxy refuses to
-- forward.
sendNoContent :: Handler a
sendNoContent = sendWaiResponse $ responseLBS status204 [] mempty

-- ----------------------------------------------------------------------------
-- Error helpers

-- | Single JSON-error envelope used everywhere. Always carries
-- @{"error": "<code>"}@.
errorObj :: Text -> Value
errorObj code = Aeson.object ["error" .= code]

-- | Abort with @{ "error": code }@ at the given HTTP status.
abort :: Status -> Text -> Handler a
abort status code = sendStatusJSON status $ errorObj code

abortBadRequest, abortUnauthorized, abortNotFound :: Text -> Handler a
abortBadRequest = abort status400
abortUnauthorized = abort status401
abortNotFound = abort status404

-- ----------------------------------------------------------------------------
-- Health + game

getHealthR :: Handler Value
getHealthR = pure $ Aeson.object ["status" .= ("ok" :: Text)]

postGameR :: Handler Value
postGameR = do
  result <- liftIO runSetup
  case result of
    Left err -> abortBadRequest $ T.pack err
    Right game -> pure $ Aeson.toJSON game

-- ----------------------------------------------------------------------------
-- Admin: maintenance window

data MaintenanceReq = MaintenanceReq
  { mrUntil :: UTCTime
  , mrMessage :: Maybe Text
  }

instance FromJSON MaintenanceReq where
  parseJSON = Aeson.withObject "MaintenanceReq" \o ->
    MaintenanceReq <$> o .: "until" <*> o .:? "message"

getMaintenanceR :: Handler Value
getMaintenanceR = do
  requireAdmin
  app <- getYesod
  ms <- liftIO $ atomically $ readMaintenanceSTM app.lobby
  pure $ Aeson.toJSON ms

postMaintenanceR :: Handler Value
postMaintenanceR = do
  requireAdmin
  MaintenanceReq{mrUntil, mrMessage} <- requireCheckJsonBody
  app <- getYesod
  let ms = MaintenanceState {until = mrUntil, message = mrMessage}
  liftIO $ atomically do
    setMaintenanceSTM app.lobby (Just ms)
    broadcastMaintenance app.lobby (Just ms)
  sendStatusJSON status200 $ Aeson.toJSON ms

deleteMaintenanceR :: Handler Value
deleteMaintenanceR = do
  requireAdmin
  app <- getYesod
  liftIO $ atomically do
    setMaintenanceSTM app.lobby Nothing
    broadcastMaintenance app.lobby Nothing
  sendNoContent

-- | Bearer-token auth for admin endpoints. If 'WHI_ADMIN_TOKEN' is unset
-- the admin endpoints are disabled (every call 404s in spirit, but
-- 401 is what we surface — the route still exists).
requireAdmin :: Handler ()
requireAdmin = do
  app <- getYesod
  case app.adminToken of
    Nothing -> abortUnauthorized "admin_disabled"
    Just expected -> do
      mbearer <- lookupBearer
      case mbearer of
        Just tok | tok == expected -> pure ()
        _ -> abortUnauthorized "unauthorized"

-- ----------------------------------------------------------------------------
-- Auth: register / login / refresh / logout / me

data RegisterReq = RegisterReq
  { rrEmail :: Text
  , rrPassword :: Text
  , rrDisplayName :: Text
  }

instance FromJSON RegisterReq where
  parseJSON = Aeson.withObject "RegisterReq" \o ->
    RegisterReq <$> o .: "email" <*> o .: "password" <*> o .: "displayName"

postRegisterR :: Handler Value
postRegisterR = do
  RegisterReq{rrEmail, rrPassword, rrDisplayName} <- requireCheckJsonBody
  validateEmail rrEmail
  validatePassword rrPassword
  validateDisplayName rrDisplayName
  mexisting <- runHandlerDB $ findUserByEmail rrEmail
  whenJust mexisting \_ -> abort status409 "email_in_use"
  hashTxt <- liftIO (hashPassword rrPassword) >>= \case
    Just h -> pure h
    Nothing -> abortBadRequest "password_too_long"
  now <- liftIO getCurrentTime
  uid <- liftIO nextRandom
  let user = User rrEmail hashTxt rrDisplayName now now
  runHandlerDB $ P.insertKey (UserKey uid) user
  issueSession uid rrEmail rrDisplayName status201

data LoginReq = LoginReq
  { lrEmail :: Text
  , lrPassword :: Text
  }

instance FromJSON LoginReq where
  parseJSON = Aeson.withObject "LoginReq" \o ->
    LoginReq <$> o .: "email" <*> o .: "password"

postLoginR :: Handler Value
postLoginR = do
  LoginReq{lrEmail, lrPassword} <- requireCheckJsonBody
  muser <- runHandlerDB $ findUserByEmail lrEmail
  case muser of
    Nothing -> abortUnauthorized "invalid_credentials"
    Just (Entity (UserKey uid) user) ->
      if verifyPassword lrPassword (userPasswordHash user)
        then issueSession uid (userEmail user) (userDisplayName user) status200
        else abortUnauthorized "invalid_credentials"

postRefreshR :: Handler Value
postRefreshR = do
  tok <- lookupRefreshCookie >>= maybe (abortUnauthorized "no_refresh_token") pure
  now <- liftIO getCurrentTime
  let h = sha256Hex $ encodeUtf8 tok
  mrow <- runHandlerDB $ findRefreshRow h
  case mrow of
    Nothing -> abortUnauthorized "invalid_refresh_token"
    Just (Entity rkey rt)
      | refreshTokenExpiresAt rt < now -> abortUnauthorized "refresh_expired"
      | Just _ <- refreshTokenRevokedAt rt -> abortUnauthorized "refresh_revoked"
      | otherwise -> do
          runHandlerDB $ P.update rkey [RefreshTokenRevokedAt P.=. Just now]
          muser <- runHandlerDB $ P.get (refreshTokenUserId rt)
          case muser of
            Nothing -> abortUnauthorized "user_missing"
            Just user -> do
              let UserKey uid = refreshTokenUserId rt
              issueSession uid (userEmail user) (userDisplayName user) status200

postLogoutR :: Handler Value
postLogoutR = do
  mtok <- lookupRefreshCookie
  whenJust mtok \tok -> do
    now <- liftIO getCurrentTime
    let h = sha256Hex $ encodeUtf8 tok
    runHandlerDB $ E.update \r -> do
      E.set r [RefreshTokenRevokedAt E.=. E.val (Just now)]
      E.where_ (r E.^. RefreshTokenTokenHash E.==. E.val h)
  clearRefreshCookie
  sendNoContent

getMeR :: Handler Value
getMeR = do
  Entity (UserKey uid) user <- requireUser
  pure $ userJsonInline uid (userEmail user) (userDisplayName user)

issueSession :: UUID -> Text -> Text -> Status -> Handler Value
issueSession uid email displayName status = do
  app <- getYesod
  now <- liftIO getCurrentTime
  access <- liftIO $ issueJwt app.jwtSecret app.accessTtl uid
  rawRefresh <- liftIO $ randomToken 32
  let refreshHash = sha256Hex $ encodeUtf8 rawRefresh
      refreshExpires = addUTCTime app.refreshTtl now
      row = RefreshToken (UserKey uid) refreshHash refreshExpires Nothing now
  rid <- liftIO nextRandom
  runHandlerDB $ P.insertKey (RefreshTokenKey rid) row
  setRefreshCookie rawRefresh refreshExpires
  sendStatusJSON status $
    Aeson.object
      [ "accessToken" .= access
      , "expiresIn" .= (truncate app.accessTtl :: Int)
      , "user" .= userJsonInline uid email displayName
      ]

userJsonInline :: UUID -> Text -> Text -> Value
userJsonInline uid email displayName =
  Aeson.object
    [ "id" .= UUID.toText uid
    , "email" .= email
    , "displayName" .= displayName
    ]

-- ----------------------------------------------------------------------------
-- Decks

data DeckInput = DeckInput
  { diName :: Text
  , diCapital :: Maybe Text
  , diCards :: Value
  }

instance FromJSON DeckInput where
  parseJSON = Aeson.withObject "DeckInput" \o ->
    DeckInput
      <$> o .: "name"
      <*> o .:? "capital"
      <*> (fromMaybe (Aeson.Object mempty) <$> o .:? "cards")

getDecksR :: Handler Value
getDecksR = do
  Entity uk _ <- requireUser
  rows <- runHandlerDB $ E.select do
    d <- E.from $ E.table @Deck
    E.where_ (d E.^. DeckUserId E.==. E.val uk)
    E.orderBy [E.desc (d E.^. DeckUpdatedAt)]
    pure d
  pure $ Aeson.toJSON $ map deckJson rows

postDecksR :: Handler Value
postDecksR = do
  Entity uk _ <- requireUser
  DeckInput{diName, diCapital, diCards} <- requireCheckJsonBody
  validateCapital diCapital
  validateDeckName diName
  now <- liftIO getCurrentTime
  did <- liftIO nextRandom
  let deck = Deck uk diName diCapital diCards now now
  runHandlerDB $ P.insertKey (DeckKey did) deck
  sendStatusJSON status201 $ deckJson $ Entity (DeckKey did) deck

getDeckR :: UUID -> Handler Value
getDeckR did = do
  Entity uk _ <- requireUser
  deck <- requireOwnedDeck uk did
  pure $ deckJson $ Entity (DeckKey did) deck

putDeckR :: UUID -> Handler Value
putDeckR did = do
  Entity uk _ <- requireUser
  DeckInput{diName, diCapital, diCards} <- requireCheckJsonBody
  validateCapital diCapital
  validateDeckName diName
  _ <- requireOwnedDeck uk did
  now <- liftIO getCurrentTime
  runHandlerDB $
    P.update (DeckKey did)
      [ DeckName P.=. diName
      , DeckCapital P.=. diCapital
      , DeckCards P.=. diCards
      , DeckUpdatedAt P.=. now
      ]
  updated <- runHandlerDB $ P.get (DeckKey did)
  case updated of
    Just d -> pure $ deckJson $ Entity (DeckKey did) d
    Nothing -> abortNotFound "not_found"

deleteDeckR :: UUID -> Handler Value
deleteDeckR did = do
  Entity uk _ <- requireUser
  _ <- requireOwnedDeck uk did
  runHandlerDB $ P.delete (DeckKey did)
  sendNoContent

-- | Fetch a deck and confirm it belongs to the given user. 404s otherwise
-- (no leaking whether the deck exists for someone else).
requireOwnedDeck :: UserId -> UUID -> Handler Deck
requireOwnedDeck uk did = do
  mdeck <- runHandlerDB $ P.get (DeckKey did)
  case mdeck of
    Just deck | deckUserId deck == uk -> pure deck
    _ -> abortNotFound "not_found"

deckJson :: Entity Deck -> Value
deckJson (Entity (DeckKey did) d) =
  Aeson.object
    [ "id" .= UUID.toText did
    , "name" .= deckName d
    , "capital" .= deckCapital d
    , "cards" .= deckCards d
    , "createdAt" .= deckCreatedAt d
    , "updatedAt" .= deckUpdatedAt d
    ]

-- ----------------------------------------------------------------------------
-- Auth helpers

requireUser :: Handler (Entity User)
requireUser = do
  app <- getYesod
  tok <- lookupBearer >>= maybe (abortUnauthorized "missing_bearer") pure
  now <- liftIO getCurrentTime
  case verifyJwt app.jwtSecret now tok of
    Left e -> abortUnauthorized $ jwtErrCode e
    Right claims -> do
      muser <- runHandlerDB $ P.get (UserKey claims.sub)
      case muser of
        Nothing -> abortUnauthorized "user_missing"
        Just u -> pure $ Entity (UserKey claims.sub) u

jwtErrCode :: JwtError -> Text
jwtErrCode = \case
  MalformedToken -> "malformed_token"
  BadSignature -> "bad_signature"
  TokenExpired -> "token_expired"
  UnsupportedAlg -> "unsupported_alg"

lookupBearer :: Handler (Maybe Text)
lookupBearer = do
  mhdr <- lookupHeader "Authorization"
  pure do
    bs <- mhdr
    if "Bearer " `BS.isPrefixOf` bs
      then Just $ decodeUtf8 $ BS.drop 7 bs
      else Nothing

-- ----------------------------------------------------------------------------
-- Refresh cookie

refreshCookieName :: ByteString
refreshCookieName = "whi_refresh"

baseRefreshCookie :: Bool -> SetCookie
baseRefreshCookie secure = defaultSetCookie
  { setCookieName = refreshCookieName
  , setCookieHttpOnly = True
  , setCookieSecure = secure
  , setCookieSameSite = Just sameSiteLax
  , setCookiePath = Just "/api/auth"
  }

setRefreshCookie :: Text -> UTCTime -> Handler ()
setRefreshCookie value expiry = do
  app <- getYesod
  setCookie (baseRefreshCookie app.cookieSecure)
    { setCookieValue = encodeUtf8 value
    , setCookieExpires = Just expiry
    }

clearRefreshCookie :: Handler ()
clearRefreshCookie = do
  app <- getYesod
  setCookie (baseRefreshCookie app.cookieSecure)
    { setCookieValue = ""
    , setCookieMaxAge = Just 0
    }

lookupRefreshCookie :: Handler (Maybe Text)
lookupRefreshCookie = do
  mhdr <- lookupHeader "Cookie"
  pure do
    bs <- mhdr
    val <- lookup refreshCookieName (parseCookies bs)
    pure $ decodeUtf8 val

-- ----------------------------------------------------------------------------
-- DB helpers

findUserByEmail :: MonadIO m => Text -> SqlPersistT m (Maybe (Entity User))
findUserByEmail e = do
  rows <- E.select do
    u <- E.from $ E.table @User
    E.where_ (E.lower_ (u E.^. UserEmail) E.==. E.val (T.toLower e))
    E.limit 1
    pure u
  pure $ listToMaybe rows

findRefreshRow :: MonadIO m => Text -> SqlPersistT m (Maybe (Entity RefreshToken))
findRefreshRow h = do
  rows <- E.select do
    r <- E.from $ E.table @RefreshToken
    E.where_ (r E.^. RefreshTokenTokenHash E.==. E.val h)
    E.limit 1
    pure r
  pure $ listToMaybe rows

-- ----------------------------------------------------------------------------
-- Validation

-- | Generic length-bounds check. Used by every text-field validator below;
-- collapsing the @< min || > max@ branch keeps the validators one-liners.
validateLength :: Int -> Int -> Text -> Text -> Handler ()
validateLength minLen maxLen code t =
  when (T.length t < minLen || T.length t > maxLen) $ abortBadRequest code

validateEmail :: Text -> Handler ()
validateEmail e = do
  validateLength 3 254 "invalid_email" e
  unless (T.any (== '@') e) $ abortBadRequest "invalid_email"

validatePassword :: Text -> Handler ()
validatePassword p
  | T.length p < 8 = abortBadRequest "password_too_short"
  | T.length p > 200 = abortBadRequest "password_too_long"
  | otherwise = pure ()

validateDisplayName :: Text -> Handler ()
validateDisplayName n = do
  validateLength 1 50 "invalid_display_name" n
  unless (T.all niceChar n) $ abortBadRequest "invalid_display_name"
  where
    niceChar c = isAlpha c || isDigit c || c `elem` (" _-.'" :: String)

validateCapital :: Maybe Text -> Handler ()
validateCapital Nothing = pure ()
validateCapital (Just t) =
  unless (t `elem` validCapitals) $ abortBadRequest "invalid_capital"
  where
    validCapitals = ["empire", "dwarf", "high_elf", "chaos", "orc", "dark_elf"] :: [Text]

validateDeckName :: Text -> Handler ()
validateDeckName = validateLength 1 80 "invalid_deck_name"

-- ----------------------------------------------------------------------------
-- Misc

randomToken :: Int -> IO Text
randomToken n = do
  bs <- getRandomBytes n :: IO BS.ByteString
  pure $ decodeUtf8 $ BAE.convertToBase BAE.Base64URLUnpadded bs

sha256Hex :: ByteString -> Text
sha256Hex bs =
  let digest = hash bs :: Digest SHA256
   in decodeUtf8 $ BAE.convertToBase BAE.Base16 (BA.convert digest :: BS.ByteString)

-- ----------------------------------------------------------------------------
-- Run

runServer :: App -> Int -> IO ()
runServer app port = do
  putStrLn $ "Yesod server listening on http://localhost:" <> show port
  waiApp <- toWaiApp app
  let wsEnv = WsEnv
        { lobby = app.lobby
        , dbPool = app.dbPool
        , jwtSecret = app.jwtSecret
        }
  _ <- forkIO $ idleSweeperLoop wsEnv
  run port $ logStdoutDev $ wsMiddleware wsEnv waiApp
