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
  bracket (openPool (textToBs dbUrl) poolSize) closePool \pool -> do
    lobby <- newLobbyState
    let app = App
          { dbPool = pool
          , jwtSecret = JwtSecret $ textToBs jwtSecret
          , accessTtl
          , refreshTtl
          , cookieSecure
          , lobby
          , adminToken
          }
    runServer app port
  where
    textToBs = BS8.pack . T.unpack

-- ----------------------------------------------------------------------------
-- env helpers

-- | Read a non-empty env var. @Nothing@ for missing or empty; @Just t@
-- otherwise.
envOptional :: T.Text -> IO (Maybe T.Text)
envOptional name = do
  m <- lookupEnv $ T.unpack name
  pure $ case m of
    Just v | not (null v) -> Just $ T.pack v
    _ -> Nothing

envText :: T.Text -> IO T.Text
envText name =
  envOptional name >>= maybe (die $ T.unpack name <> " is required (set it in .env or the environment)") pure

envTextOptional :: T.Text -> IO (Maybe T.Text)
envTextOptional = envOptional

envInt :: T.Text -> Int -> IO Int
envInt name def =
  fromMaybe def . (>>= readMaybe . T.unpack) <$> envOptional name

envSeconds :: T.Text -> NominalDiffTime -> IO NominalDiffTime
envSeconds name def = fromIntegral <$> envInt name (round def)

envBool :: T.Text -> Bool -> IO Bool
envBool name def = do
  m <- envOptional name
  pure $ case T.toLower <$> m of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "0" -> False
    Just "false" -> False
    Just "no" -> False
    _ -> def

die :: String -> IO a
die msg = do
  hPutStrLn stderr $ "fatal: " <> msg
  exitFailure
