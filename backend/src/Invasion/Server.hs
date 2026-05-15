{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Invasion.Server (runServer) where

import Data.Text (Text)
import Invasion.Engine (runSetup)
import Invasion.Prelude
import Network.HTTP.Types.Status (status400)
import Network.Wai.Handler.Warp (run)
import Yesod.Core

data App = App

mkYesod "App" [parseRoutes|
/api/health HealthR GET
/api/game   GameR   POST
|]

instance Yesod App where
  yesodMiddleware = defaultYesodMiddleware

getHealthR :: Handler Value
getHealthR = pure $ object ["status" .= ("ok" :: Text)]

postGameR :: Handler Value
postGameR = do
  result <- liftIO runSetup
  case result of
    Left err -> sendStatusJSON status400 $ object ["error" .= err]
    Right game -> pure $ toJSON game

runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Yesod server listening on http://localhost:" <> show port
  waiApp <- toWaiApp App
  run port waiApp
