{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Persistent entity definitions.
--
-- The SQL schema is owned by @db/migrations/@ (dbmate). These entities
-- are the Haskell-side mirror so persistent + esqueleto can build typed
-- queries against it. Keep them in lockstep: any column change in a
-- migration is a change here too.
module Invasion.Model
  ( User (..)
  , UserId
  , Key (UserKey, DeckKey, RefreshTokenKey)
  , Deck (..)
  , DeckId
  , RefreshToken (..)
  , RefreshTokenId
  , EntityField
      ( UserId
      , UserEmail
      , UserPasswordHash
      , UserDisplayName
      , UserCreatedAt
      , UserUpdatedAt
      , DeckId
      , DeckUserId
      , DeckName
      , DeckFaction
      , DeckCards
      , DeckCreatedAt
      , DeckUpdatedAt
      , RefreshTokenId
      , RefreshTokenUserId
      , RefreshTokenTokenHash
      , RefreshTokenExpiresAt
      , RefreshTokenRevokedAt
      , RefreshTokenCreatedAt
      )
  ) where

import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist (EntityField, Key, PersistField (..))
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import Database.Persist.TH
import Database.Persist.Types (LiteralType (Escaped), PersistValue (..))
import Invasion.Prelude
import Web.PathPieces (PathPiece (..))

-- ----------------------------------------------------------------------------
-- Orphan instances for UUID
--
-- persistent-postgresql doesn't ship PersistField UUID by default. The
-- representation matches what postgresql-simple sends for the @uuid@
-- column type: an escaped text literal that the driver casts on bind.

instance PersistField UUID where
  toPersistValue = PersistLiteral_ Escaped . BS8.pack . UUID.toString
  fromPersistValue = \case
    PersistLiteral_ _ bs -> case UUID.fromASCIIBytes bs of
      Just u -> Right u
      Nothing -> Left "invalid uuid bytes"
    PersistText t -> case UUID.fromText t of
      Just u -> Right u
      Nothing -> Left "invalid uuid text"
    PersistByteString bs -> case UUID.fromASCIIBytes bs of
      Just u -> Right u
      Nothing -> Left "invalid uuid byte-string"
    _ -> Left "expected uuid"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther (T.pack "uuid")

instance PathPiece UUID where
  toPathPiece = UUID.toText
  fromPathPiece = UUID.fromText

share
  [mkPersist sqlSettings]
  [persistLowerCase|
User sql=users
    Id UUID sqltype=uuid default=gen_random_uuid()
    email Text sqltype=text
    passwordHash Text sql=password_hash sqltype=text
    displayName Text sql=display_name sqltype=text
    createdAt UTCTime sql=created_at sqltype=timestamptz
    updatedAt UTCTime sql=updated_at sqltype=timestamptz
    deriving Show

Deck sql=decks
    Id UUID sqltype=uuid default=gen_random_uuid()
    userId UserId sql=user_id sqltype=uuid
    name Text sqltype=text
    faction Text Maybe sqltype=text
    cards Value sqltype=jsonb
    createdAt UTCTime sql=created_at sqltype=timestamptz
    updatedAt UTCTime sql=updated_at sqltype=timestamptz
    deriving Show

RefreshToken sql=refresh_tokens
    Id UUID sqltype=uuid default=gen_random_uuid()
    userId UserId sql=user_id sqltype=uuid
    tokenHash Text sql=token_hash sqltype=text
    expiresAt UTCTime sql=expires_at sqltype=timestamptz
    revokedAt UTCTime Maybe sql=revoked_at sqltype=timestamptz
    createdAt UTCTime sql=created_at sqltype=timestamptz
    deriving Show
|]
