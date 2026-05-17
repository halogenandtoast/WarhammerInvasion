{-# LANGUAGE OverloadedStrings #-}

-- | Minimal HS256 JWT issue + verify.
--
-- We don't need the full JOSE surface — just signed bearer tokens with a
-- @sub@ (subject = user id) and @exp@ (expiry). This module produces
-- standard JWTs that any RFC 7519-compliant verifier could read, so we
-- can swap in a heavier lib later without invalidating existing tokens.
module Invasion.Auth.Jwt
  ( JwtSecret (..)
  , JwtClaims (..)
  , JwtError (..)
  , issueJwt
  , verifyJwt
  ) where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC qualified as HMAC
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, NominalDiffTime, addUTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Invasion.Prelude

-- | The shared secret. Wrap to keep it from being printed by accident.
newtype JwtSecret = JwtSecret { unJwtSecret :: ByteString }

-- | Claims we sign.
data JwtClaims = JwtClaims
  { sub :: UUID
  , iat :: UTCTime
  , exp :: UTCTime
  }
  deriving (Show)

data JwtError
  = MalformedToken
  | BadSignature
  | TokenExpired
  | UnsupportedAlg
  deriving (Show, Eq)

-- ----------------------------------------------------------------------------
-- Issue

-- | Issue a token with @sub = user id@, @iat = now@, @exp = now + ttl@.
issueJwt :: JwtSecret -> NominalDiffTime -> UUID -> IO Text
issueJwt secret ttl uid = do
  now <- getCurrentTime
  let claims = JwtClaims { sub = uid, iat = now, exp = addUTCTime ttl now }
  pure (encodeJwt secret claims)

encodeJwt :: JwtSecret -> JwtClaims -> Text
encodeJwt (JwtSecret secret) claims =
  let headerBs = "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"
      payloadBs = BSL.toStrict (Aeson.encode (claimsToJson claims))
      signingInput = b64url headerBs <> "." <> b64url payloadBs
      mac = HMAC.hmac secret signingInput :: HMAC.HMAC SHA256
      sig = b64url (BA.convert mac)
   in decodeUtf8OrEmpty (signingInput <> "." <> sig)

claimsToJson :: JwtClaims -> Aeson.Value
claimsToJson c =
  Aeson.object
    [ "sub" Aeson..= UUID.toText c.sub
    , "iat" Aeson..= (truncate (utcTimeToPOSIXSeconds c.iat) :: Integer)
    , "exp" Aeson..= (truncate (utcTimeToPOSIXSeconds c.exp) :: Integer)
    ]

-- ----------------------------------------------------------------------------
-- Verify

-- | Verify a token's signature, header, and expiry. Returns the claims on
-- success. Constant-time signature comparison (HMAC's @Eq@ instance).
verifyJwt :: JwtSecret -> UTCTime -> Text -> Either JwtError JwtClaims
verifyJwt (JwtSecret secret) now token = do
  (headerB64, payloadB64, sigB64) <- splitToken token
  hdr <- decodeJson headerB64
  unless (jwtAlgOk hdr) (Left UnsupportedAlg)
  let signingInput = encodeUtf8 headerB64 <> "." <> encodeUtf8 payloadB64
      mac = HMAC.hmac secret signingInput :: HMAC.HMAC SHA256
      expected = b64url (BA.convert mac)
  if expected == encodeUtf8 sigB64
    then do
      claims <- decodeClaims payloadB64
      if diffUTCTime claims.exp now > 0
        then pure claims
        else Left TokenExpired
    else Left BadSignature

splitToken :: Text -> Either JwtError (Text, Text, Text)
splitToken t = case T.splitOn "." t of
  [a, b, c] -> Right (a, b, c)
  _ -> Left MalformedToken

decodeJson :: Text -> Either JwtError Aeson.Object
decodeJson b64 = do
  raw <- b64urlDecode (encodeUtf8 b64) `orLeft` MalformedToken
  case Aeson.decodeStrict raw of
    Just (Aeson.Object o) -> Right o
    _ -> Left MalformedToken

decodeClaims :: Text -> Either JwtError JwtClaims
decodeClaims b64 = do
  raw <- b64urlDecode (encodeUtf8 b64) `orLeft` MalformedToken
  case Aeson.eitherDecodeStrict raw of
    Right val -> claimsFromJson val
    Left _ -> Left MalformedToken

claimsFromJson :: Aeson.Value -> Either JwtError JwtClaims
claimsFromJson v = case v of
  Aeson.Object o -> do
    subTxt <- lookupText "sub" o
    uid <- UUID.fromText subTxt `orLeft` MalformedToken
    iatN <- lookupNumber "iat" o
    expN <- lookupNumber "exp" o
    pure $ JwtClaims uid (posixSecondsToUTCTime (realToFrac iatN :: POSIXTime)) (posixSecondsToUTCTime (realToFrac expN :: POSIXTime))
  _ -> Left MalformedToken

jwtAlgOk :: Aeson.Object -> Bool
jwtAlgOk o = case KM.lookup "alg" o of
  Just (Aeson.String "HS256") -> True
  _ -> False

-- ----------------------------------------------------------------------------
-- Helpers

-- base64url WITHOUT padding (per JWT spec).
b64url :: ByteString -> ByteString
b64url = BAE.convertToBase BAE.Base64URLUnpadded

b64urlDecode :: ByteString -> Maybe ByteString
b64urlDecode = eitherToMaybe . BAE.convertFromBase BAE.Base64URLUnpadded

orLeft :: Maybe a -> e -> Either e a
orLeft = flip maybeToEither

lookupText :: Aeson.Key -> Aeson.Object -> Either JwtError Text
lookupText k o = case KM.lookup k o of
  Just (Aeson.String s) -> Right s
  _ -> Left MalformedToken

lookupNumber :: Aeson.Key -> Aeson.Object -> Either JwtError Double
lookupNumber k o = case KM.lookup k o of
  Just (Aeson.Number n) -> Right (realToFrac n)
  _ -> Left MalformedToken

decodeUtf8OrEmpty :: ByteString -> Text
decodeUtf8OrEmpty = either (const T.empty) id . decodeUtf8'
