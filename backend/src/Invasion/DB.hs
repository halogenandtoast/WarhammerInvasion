{-# LANGUAGE OverloadedStrings #-}

-- | Database connection pool wired around @DATABASE_URL@.
--
-- The pool is created once at process start and shared by every request
-- handler. Persistent's @SqlPersistT@ runs against it via 'runDB'.
module Invasion.DB
  ( DbPool
  , openPool
  , closePool
  , runDB
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString (ByteString)
import Data.Pool qualified as Pool
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, runSqlPool)
import Database.Persist.Sql (SqlPersistT)
import Invasion.Prelude

-- | Alias so callers don't need to import "Database.Persist.Postgresql".
type DbPool = ConnectionPool

-- | Open a pool against the given @postgres://@ connection string.
openPool :: ByteString -> Int -> IO DbPool
openPool connStr poolSize =
  runNoLoggingT $ createPostgresqlPool connStr poolSize

-- | Close the pool. Idempotent.
closePool :: DbPool -> IO ()
closePool = Pool.destroyAllResources

-- | Run a persistent action against the pool.
runDB :: MonadUnliftIO m => DbPool -> SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
runDB pool action =
  runResourceT . runNoLoggingT $ runSqlPool action pool
