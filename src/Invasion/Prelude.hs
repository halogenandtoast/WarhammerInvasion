module Invasion.Prelude (module Invasion.Prelude, module X) where

import Prelude as X
import Control.Monad.Random
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import GHC.Records as X
import Control.Monad as X (when, unless)
import Data.Foldable as X (traverse_)

sample :: MonadRandom m => NonEmpty a -> m a
sample xs = do
  idx <- getRandomR (0, NE.length xs - 1)
  pure $ xs NE.!! idx

sample2 :: MonadRandom m => a -> a -> m a
sample2 x y = sample (x :| [y])

