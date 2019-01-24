{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.State.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types                       as D
import qualified Enecuum.Core.HGraph.Language             as L
import qualified Enecuum.Core.Logger.Language             as L
import           Language.Haskell.TH.MakeFunctor

class (Monad m, L.Logger m) => State' m where
  newVar :: a -> m (D.StateVar a)
  readVar :: D.StateVar a -> m a
  writeVar :: D.StateVar a -> a -> m ()
  retry :: m a
  evalGraph :: (Serialize c, D.StringHashable c)
      => D.TGraph c -> Free (L.HGraphF (D.TNodeL c)) x -> m x
  -- EvalDelayedLogger :: L.LoggerL () -> (() -> next) -> StateF next

-- ??
class (Monad m, L.Logger m) => StateIO m where
  atomically :: (forall m'. State' m' => m' a) -> m a
  newVarIO :: a -> m (D.StateVar a)
  readVarIO :: D.StateVar a -> m a
  writeVarIO :: D.StateVar a -> a -> m ()

-- ??
-- instance L.Logger StateL where
--     logMessage level = evalDelayedLogger . L.logMessage level

-- | Modify variable with function.
modifyVar :: State' m => D.StateVar a -> (a -> a) -> m ()
modifyVar var f = readVar var >>= writeVar var . f
