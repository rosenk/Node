{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.State.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types                       as D
import qualified Enecuum.Core.HGraph.Language             as L
import qualified Enecuum.Core.Logger.Language             as L
import           Language.Haskell.TH.MakeFunctor

class State' m where
  newVarIO :: a -> m (D.StateVar a)
  readVarIO :: D.StateVar a -> m a
  writeVarIO :: D.StateVar a -> a -> m ()
  retry :: m a
  -- EvalGraph :: (Serialize c, D.StringHashable c) => D.TGraph c -> Free (L.HGraphF (D.TNodeL c)) x -> (x -> next) -> StateF next
  -- EvalDelayedLogger :: L.LoggerL () -> (() -> next) -> StateF next

-- ??
class StateIO m where
  atomically :: StateL a -> m a
  newVarIO :: a -> m (D.StateVar a)
  readVarIO :: D.StateVar a -> m a
  writeVarIO :: D.StateVar a -> a -> m ()

-- ??
-- instance L.Logger StateL where
--     logMessage level = evalDelayedLogger . L.logMessage level
