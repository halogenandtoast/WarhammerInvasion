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

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import Data.Aeson qualified as Aeson
import Data.Aeson (FromJSON, Value, (.:), (.:?), (.=))
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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
  }

mkYesod "App" [parseRoutes|
/api/health           HealthR    GET
/api/game             GameR      POST
/api/auth/register    RegisterR  POST
/api/auth/login       LoginR     POST
/api/auth/refresh     RefreshR   POST
/api/auth/logout      LogoutR    POST
/api/auth/me          MeR        GET
/api/decks            DecksR     GET POST
/api/decks/#UUID      DeckR      GET PUT DELETE
|]

instance Yesod App where
  yesodMiddleware = defaultYesodMiddleware

-- ----------------------------------------------------------------------------
-- DB helper

runHandlerDB :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> Handler a
runHandlerDB action = do
  pool <- (.dbPool) <$> getYesod
  liftIO (runDB pool action)

-- ----------------------------------------------------------------------------
-- Health + game

getHealthR :: Handler Value
getHealthR = pure $ Aeson.object ["status" .= ("ok" :: Text)]

postGameR :: Handler Value
postGameR = do
  result <- liftIO runSetup
  case result of
    Left err -> sendStatusJSON status400 $ errorObj (T.pack err)
    Right game -> pure $ Aeson.toJSON game

-- ----------------------------------------------------------------------------
-- Auth: register / login / refresh / logout / me

data RegisterReq = RegisterReq
  { rrEmail :: Text
  , rrPassword :: Text
  , rrDisplayName :: Text
  }

instance FromJSON RegisterReq where
  parseJSON = Aeson.withObject "RegisterReq" $ \o ->
    RegisterReq <$> o .: "email" <*> o .: "password" <*> o .: "displayName"

postRegisterR :: Handler Value
postRegisterR = do
  RegisterReq{rrEmail, rrPassword, rrDisplayName} <- requireCheckJsonBody
  validateEmail rrEmail
  validatePassword rrPassword
  validateDisplayName rrDisplayName
  mexisting <- runHandlerDB (findUserByEmail rrEmail)
  case mexisting of
    Just _ -> sendStatusJSON status409 (errorObj "email_in_use")
    Nothing -> do
      mhash <- liftIO (hashPassword rrPassword)
      hashTxt <- case mhash of
        Just h -> pure h
        Nothing -> sendStatusJSON status400 (errorObj "password_too_long")
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
  parseJSON = Aeson.withObject "LoginReq" $ \o ->
    LoginReq <$> o .: "email" <*> o .: "password"

postLoginR :: Handler Value
postLoginR = do
  LoginReq{lrEmail, lrPassword} <- requireCheckJsonBody
  muser <- runHandlerDB (findUserByEmail lrEmail)
  case muser of
    Nothing -> sendStatusJSON status401 (errorObj "invalid_credentials")
    Just (Entity (UserKey uid) user) ->
      if verifyPassword lrPassword (userPasswordHash user)
        then issueSession uid (userEmail user) (userDisplayName user) status200
        else sendStatusJSON status401 (errorObj "invalid_credentials")

postRefreshR :: Handler Value
postRefreshR = do
  mtok <- lookupRefreshCookie
  case mtok of
    Nothing -> sendStatusJSON status401 (errorObj "no_refresh_token")
    Just tok -> do
      now <- liftIO getCurrentTime
      let h = sha256Hex (encodeUtf8 tok)
      mrow <- runHandlerDB (findRefreshRow h)
      case mrow of
        Nothing -> sendStatusJSON status401 (errorObj "invalid_refresh_token")
        Just (Entity rkey rt)
          | (refreshTokenExpiresAt rt) < now -> sendStatusJSON status401 (errorObj "refresh_expired")
          | Just _ <- (refreshTokenRevokedAt rt) -> sendStatusJSON status401 (errorObj "refresh_revoked")
          | otherwise -> do
              runHandlerDB $ P.update rkey [RefreshTokenRevokedAt P.=. Just now]
              muser <- runHandlerDB (P.get (refreshTokenUserId rt))
              case muser of
                Nothing -> sendStatusJSON status401 (errorObj "user_missing")
                Just user -> do
                  let UserKey uid = (refreshTokenUserId rt)
                  issueSession uid (userEmail user) (userDisplayName user) status200

postLogoutR :: Handler Value
postLogoutR = do
  mtok <- lookupRefreshCookie
  case mtok of
    Just tok -> do
      now <- liftIO getCurrentTime
      let h = sha256Hex (encodeUtf8 tok)
      runHandlerDB $ E.update $ \r -> do
        E.set r [RefreshTokenRevokedAt E.=. E.val (Just now)]
        E.where_ (r E.^. RefreshTokenTokenHash E.==. E.val h)
    Nothing -> pure ()
  clearRefreshCookie
  sendStatusJSON status204 Aeson.Null

getMeR :: Handler Value
getMeR = do
  Entity (UserKey uid) user <- requireUser
  pure $ userJsonInline uid (userEmail user) (userDisplayName user)

issueSession :: UUID -> Text -> Text -> Status -> Handler Value
issueSession uid email displayName status = do
  app <- getYesod
  now <- liftIO getCurrentTime
  access <- liftIO (issueJwt app.jwtSecret app.accessTtl uid)
  rawRefresh <- liftIO (randomToken 32)
  let refreshHash = sha256Hex (encodeUtf8 rawRefresh)
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
  parseJSON = Aeson.withObject "DeckInput" $ \o ->
    DeckInput
      <$> o .: "name"
      <*> o .:? "capital"
      <*> (fromMaybe (Aeson.Object mempty) <$> o .:? "cards")

getDecksR :: Handler Value
getDecksR = do
  Entity uk _ <- requireUser
  rows <- runHandlerDB $ E.select $ do
    d <- E.from $ E.table @Deck
    E.where_ (d E.^. DeckUserId E.==. E.val uk)
    E.orderBy [E.desc (d E.^. DeckUpdatedAt)]
    pure d
  pure $ Aeson.toJSON (map deckJson rows)

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
  sendStatusJSON status201 (deckJson (Entity (DeckKey did) deck))

getDeckR :: UUID -> Handler Value
getDeckR did = do
  Entity uk _ <- requireUser
  mdeck <- runHandlerDB (P.get (DeckKey did))
  case mdeck of
    Nothing -> sendStatusJSON status404 (errorObj "not_found")
    Just deck
      | (deckUserId deck) /= uk -> sendStatusJSON status404 (errorObj "not_found")
      | otherwise -> pure (deckJson (Entity (DeckKey did) deck))

putDeckR :: UUID -> Handler Value
putDeckR did = do
  Entity uk _ <- requireUser
  DeckInput{diName, diCapital, diCards} <- requireCheckJsonBody
  validateCapital diCapital
  validateDeckName diName
  mdeck <- runHandlerDB (P.get (DeckKey did))
  case mdeck of
    Nothing -> sendStatusJSON status404 (errorObj "not_found")
    Just deck
      | (deckUserId deck) /= uk -> sendStatusJSON status404 (errorObj "not_found")
      | otherwise -> do
          now <- liftIO getCurrentTime
          runHandlerDB $
            P.update (DeckKey did)
              [ DeckName P.=. diName
              , DeckCapital P.=. diCapital
              , DeckCards P.=. diCards
              , DeckUpdatedAt P.=. now
              ]
          updated <- runHandlerDB (P.get (DeckKey did))
          case updated of
            Just d -> pure (deckJson (Entity (DeckKey did) d))
            Nothing -> sendStatusJSON status404 (errorObj "not_found")

deleteDeckR :: UUID -> Handler Value
deleteDeckR did = do
  Entity uk _ <- requireUser
  mdeck <- runHandlerDB (P.get (DeckKey did))
  case mdeck of
    Nothing -> sendStatusJSON status404 (errorObj "not_found")
    Just deck
      | (deckUserId deck) /= uk -> sendStatusJSON status404 (errorObj "not_found")
      | otherwise -> do
          runHandlerDB $ P.delete (DeckKey did)
          sendStatusJSON status204 Aeson.Null

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
  mbearer <- lookupBearer
  case mbearer of
    Nothing -> sendStatusJSON status401 (errorObj "missing_bearer")
    Just tok -> do
      now <- liftIO getCurrentTime
      case verifyJwt app.jwtSecret now tok of
        Left e -> sendStatusJSON status401 (errorObj (jwtErrCode e))
        Right claims -> do
          muser <- runHandlerDB (P.get (UserKey claims.sub))
          case muser of
            Nothing -> sendStatusJSON status401 (errorObj "user_missing")
            Just u -> pure (Entity (UserKey claims.sub) u)

jwtErrCode :: JwtError -> Text
jwtErrCode = \case
  MalformedToken -> "malformed_token"
  BadSignature -> "bad_signature"
  TokenExpired -> "token_expired"
  UnsupportedAlg -> "unsupported_alg"

lookupBearer :: Handler (Maybe Text)
lookupBearer = do
  mhdr <- lookupHeader "Authorization"
  pure $ do
    bs <- mhdr
    if "Bearer " `BS.isPrefixOf` bs
      then Just (decodeUtf8 (BS.drop 7 bs))
      else Nothing

-- ----------------------------------------------------------------------------
-- Refresh cookie

refreshCookieName :: ByteString
refreshCookieName = "whi_refresh"

setRefreshCookie :: Text -> UTCTime -> Handler ()
setRefreshCookie value expiry = do
  app <- getYesod
  setCookie $
    defaultSetCookie
      { setCookieName = refreshCookieName
      , setCookieValue = encodeUtf8 value
      , setCookieHttpOnly = True
      , setCookieSecure = app.cookieSecure
      , setCookieSameSite = Just sameSiteLax
      , setCookiePath = Just "/api/auth"
      , setCookieExpires = Just expiry
      }

clearRefreshCookie :: Handler ()
clearRefreshCookie = do
  app <- getYesod
  setCookie $
    defaultSetCookie
      { setCookieName = refreshCookieName
      , setCookieValue = ""
      , setCookieHttpOnly = True
      , setCookieSecure = app.cookieSecure
      , setCookieSameSite = Just sameSiteLax
      , setCookiePath = Just "/api/auth"
      , setCookieMaxAge = Just 0
      }

lookupRefreshCookie :: Handler (Maybe Text)
lookupRefreshCookie = do
  mhdr <- lookupHeader "Cookie"
  pure $ do
    bs <- mhdr
    val <- lookup refreshCookieName (parseCookies bs)
    Just (decodeUtf8 val)

-- ----------------------------------------------------------------------------
-- DB helpers

findUserByEmail :: MonadIO m => Text -> SqlPersistT m (Maybe (Entity User))
findUserByEmail e = do
  rows <- E.select $ do
    u <- E.from $ E.table @User
    E.where_ (E.lower_ (u E.^. UserEmail) E.==. E.val (T.toLower e))
    E.limit 1
    pure u
  pure (listToMaybe rows)

findRefreshRow :: MonadIO m => Text -> SqlPersistT m (Maybe (Entity RefreshToken))
findRefreshRow h = do
  rows <- E.select $ do
    r <- E.from $ E.table @RefreshToken
    E.where_ (r E.^. RefreshTokenTokenHash E.==. E.val h)
    E.limit 1
    pure r
  pure (listToMaybe rows)

-- ----------------------------------------------------------------------------
-- Validation

validateEmail :: Text -> Handler ()
validateEmail e
  | T.length e < 3 || T.length e > 254 = invalid "invalid_email"
  | not (T.any (== '@') e) = invalid "invalid_email"
  | otherwise = pure ()
  where
    invalid = sendStatusJSON status400 . errorObj

validatePassword :: Text -> Handler ()
validatePassword p
  | T.length p < 8 = sendStatusJSON status400 (errorObj "password_too_short")
  | T.length p > 200 = sendStatusJSON status400 (errorObj "password_too_long")
  | otherwise = pure ()

validateDisplayName :: Text -> Handler ()
validateDisplayName n
  | T.length n < 1 || T.length n > 50 = sendStatusJSON status400 (errorObj "invalid_display_name")
  | not (T.all niceChar n) = sendStatusJSON status400 (errorObj "invalid_display_name")
  | otherwise = pure ()
  where
    niceChar c = isAlpha c || isDigit c || c `elem` (" _-.'" :: String)

validateCapital :: Maybe Text -> Handler ()
validateCapital Nothing = pure ()
validateCapital (Just t) =
  if t `elem` (["empire", "dwarf", "high_elf", "chaos", "orc", "dark_elf"] :: [Text])
    then pure ()
    else sendStatusJSON status400 (errorObj "invalid_capital")

validateDeckName :: Text -> Handler ()
validateDeckName n
  | T.length n < 1 || T.length n > 80 = sendStatusJSON status400 (errorObj "invalid_deck_name")
  | otherwise = pure ()

-- ----------------------------------------------------------------------------
-- Misc

errorObj :: Text -> Value
errorObj code = Aeson.object ["error" .= code]

randomToken :: Int -> IO Text
randomToken n = do
  bs <- getRandomBytes n :: IO BS.ByteString
  pure (decodeUtf8 (BAE.convertToBase BAE.Base64URLUnpadded bs))

sha256Hex :: ByteString -> Text
sha256Hex bs =
  let digest = hash bs :: Digest SHA256
   in decodeUtf8 (BAE.convertToBase BAE.Base16 (BA.convert digest :: BS.ByteString))

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

-- ----------------------------------------------------------------------------
-- Run

runServer :: App -> Int -> IO ()
runServer app port = do
  putStrLn $ "Yesod server listening on http://localhost:" <> show port
  waiApp <- toWaiApp app
  run port (logStdoutDev waiApp)
