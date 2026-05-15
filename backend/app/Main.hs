module Main (main) where

import Invasion.Prelude
import Invasion.Server (runServer)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = maybe 3000 id (portStr >>= readMaybe)
  runServer port
