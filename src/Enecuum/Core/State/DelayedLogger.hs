module Enecuum.Core.State.DelayedLogger where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language       as L
import qualified Enecuum.Core.Runtime        as Rt

import           Control.Monad.Trans.Reader (ReaderT, ask)

instance L.Logger (ReaderT TVar Rt.DelayedLog STM) where
    logMessage level msg = do
        delayedLog <- ask
        modifyTVar delayedLog (Rt.DelayedLogEntry level msg :)
