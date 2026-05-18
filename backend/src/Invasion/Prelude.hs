-- | Project prelude. Re-exports the common surface and adds a handful of
-- helpers used widely enough across the engine + server that pulling them
-- into one place beats threading their imports through every module.
--
-- Keep this small. Only add a name here if it's used in ~3+ modules or
-- replaces a pattern that recurs throughout the codebase.
module Invasion.Prelude
  ( module X
    -- * Maybe / Either helpers
  , whenJust
  , whenJust_
  , eitherToMaybe
  , maybeToEither
    -- * Text helpers
  , tshow
    -- * Random
  , sample
  , sample2
  ) where

import Control.Applicative as X ((<|>))
import Control.Monad as X (forM_, replicateM_, unless, void, when)
import Control.Monad.IO.Class as X (MonadIO, liftIO)
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.Foldable as X (find, for_, traverse_)
import Data.List as X (partition, sortOn)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe as X (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text as X (Text)
import Data.Text qualified as T
import GHC.Records as X
import Prelude as X

-- | @whenJust m k@ runs @k@ on the value inside a 'Just', and is a no-op
-- on 'Nothing'. The dual of 'when' for optional values.
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = for_

-- | Flipped 'whenJust' — convenient when the action is a do-block and
-- the 'Maybe' is the last argument computed above.
whenJust_ :: Applicative m => (a -> m ()) -> Maybe a -> m ()
whenJust_ = traverse_

-- | Drop the error side of an 'Either'.
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

-- | Lift a 'Maybe' into an 'Either' by supplying the error to use for
-- 'Nothing'.
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

-- | Show a value and pack it as 'Text' in one step. Replaces the common
-- @T.pack (show x)@ idiom.
tshow :: Show a => a -> Text
tshow = T.pack . show

sample :: MonadRandom m => NonEmpty a -> m a
sample xs = do
  idx <- getRandomR (0, NE.length xs - 1)
  pure $ xs NE.!! idx

sample2 :: MonadRandom m => a -> a -> m a
sample2 x y = sample (x :| [y])
