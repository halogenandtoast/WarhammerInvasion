module Main (main) where

import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Invasion.Auth.Jwt (JwtSecret (..))
import Invasion.DB (closePool, openPool)
import Invasion.Prelude
import Invasion.Server (App (..), runServer)
import Invasion.Server.Lobby (newLobbyState)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import UnliftIO.Exception (bracket)

main :: IO ()
main = do
  port <- envInt "PORT" 3000
  dbUrl <- envText "DATABASE_URL"
  jwtSecret <- envText "WHI_JWT_SECRET"
  poolSize <- envInt "WHI_DB_POOL_SIZE" 10
  accessTtl <- envSeconds "WHI_JWT_ACCESS_TTL_SECONDS" (15 * 60)
  refreshTtl <- envSeconds "WHI_JWT_REFRESH_TTL_SECONDS" (60 * 60 * 24 * 30)
  cookieSecure <- envBool "WHI_COOKIE_SECURE" False
  adminToken <- envTextOptional "WHI_ADMIN_TOKEN"
  bracket (openPool (BS8.pack (T.unpack dbUrl)) poolSize) closePool $ \pool -> do
    lobby <- newLobbyState
    let app =
          App
            { dbPool = pool
            , jwtSecret = JwtSecret (BS8.pack (T.unpack jwtSecret))
            , accessTtl = accessTtl
            , refreshTtl = refreshTtl
            , cookieSecure = cookieSecure
            , lobby = lobby
            , adminToken = adminToken
            }
    runServer app port

-- ----------------------------------------------------------------------------
-- env helpers

envText :: String -> IO T.Text
envText name = do
  m <- lookupEnv name
  case m of
    Just v | not (null v) -> pure (T.pack v)
    _ -> die (name <> " is required (set it in .env or the environment)")

envTextOptional :: String -> IO (Maybe T.Text)
envTextOptional name = do
  m <- lookupEnv name
  pure $ case m of
    Just v | not (null v) -> Just (T.pack v)
    _ -> Nothing

envInt :: String -> Int -> IO Int
envInt name def =
  fmap (maybe def id . (>>= readMaybe)) (lookupEnv name)

envSeconds :: String -> NominalDiffTime -> IO NominalDiffTime
envSeconds name def = do
  i <- envInt name (round def)
  pure (fromIntegral i)

envBool :: String -> Bool -> IO Bool
envBool name def = do
  m <- lookupEnv name
  pure $ case fmap (map toLowerChar) m of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "0" -> False
    Just "false" -> False
    Just "no" -> False
    _ -> def
  where
    toLowerChar c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

die :: String -> IO a
die msg = do
  hPutStrLn stderr ("fatal: " <> msg)
  exitFailure
