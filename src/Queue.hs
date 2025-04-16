module Queue (newQueue, Queue, HasQueue(..), push, pop, peek) where

import Prelude
import Data.IORef
import Control.Monad.IO.Class
import GHC.Records

newtype Queue msg = Queue (IORef [msg], IORef [msg])

instance HasField "inbox" (Queue msg) (IORef [msg]) where
  getField (Queue (inbox, _)) = inbox

instance HasField "actual" (Queue msg) (IORef [msg]) where
  getField (Queue (_, actual)) = actual

newQueue :: IO (Queue msg)
newQueue = do
  inbox <- newIORef []
  actual <- newIORef []
  pure $ Queue (inbox, actual)

class MonadIO m => HasQueue m where
  type QueueMessage m
  getQueue :: m (Queue (QueueMessage m))

pop :: HasQueue m => m (Maybe (QueueMessage m))
pop = do
  q <- getQueue
  liftIO do
    mail <- atomicModifyIORef' q.inbox ([],)
    atomicModifyIORef' q.actual \msgs ->
      let msgs' = reverse mail <> msgs
      in case msgs' of
        [] -> ([], Nothing)
        (x:xs') -> (xs', Just x)

peek :: HasQueue m => m (Maybe (QueueMessage m))
peek = do
  q <- getQueue
  mail <- liftIO $ readIORef q.inbox
  case reverse mail of
    [] -> liftIO $ readIORef q.actual >>= \case
      [] -> pure Nothing
      (x:_) -> pure (Just x)
    (x : _) -> pure (Just x)

push :: HasQueue m => QueueMessage m -> m ()
push x = do
  q <- getQueue
  liftIO $ atomicModifyIORef' q.inbox \xs -> (x:xs, ())
