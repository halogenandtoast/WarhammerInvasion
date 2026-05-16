-- | Boot module: declares 'HasPromptIO' so 'CardDef' can use it in
-- the 'Receive' / 'ActionEffect' constraint set without forming a
-- cycle (Engine imports Card -> CardDef -> Engine).
module Invasion.Engine
  ( HasPromptIO (..)
  ) where

import {-# SOURCE #-} Invasion.Game (Prompt, PromptResult)
import Invasion.Prelude

class Monad m => HasPromptIO m where
  askPrompt :: Prompt -> m PromptResult
