{-# LANGUAGE OverloadedStrings #-}

-- | Password hashing via bcrypt. Cost factor 12 is the modern OWASP floor.
module Invasion.Auth.Password
  ( hashPassword
  , verifyPassword
  ) where

import Crypto.BCrypt qualified as BCrypt
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Invasion.Prelude

-- | OWASP-aligned cost factor. ~250ms / hash on a 2026 droplet.
bcryptCost :: Int
bcryptCost = 12

-- | Hash a plaintext password. Returns @Nothing@ if bcrypt rejects it
-- (currently only when the input is longer than 72 bytes).
hashPassword :: Text -> IO (Maybe Text)
hashPassword password =
  fmap decodeUtf8 <$> BCrypt.hashPasswordUsingPolicy policy (encodeUtf8 password)
  where
    policy = BCrypt.HashingPolicy bcryptCost (BCrypt.fastBcryptHashingPolicy.preferredHashAlgorithm)

-- | Constant-time password verification.
verifyPassword :: Text -> Text -> Bool
verifyPassword plain hash =
  BCrypt.validatePassword (encodeUtf8 hash) (encodeUtf8 plain)
