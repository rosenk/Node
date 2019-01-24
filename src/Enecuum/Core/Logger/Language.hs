{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Logger.Language where

import qualified Enecuum.Core.Types as T (LogLevel (..), Message)
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor

class Logger m where
  logMessage :: T.LogLevel -> T.Message -> m ()

-- | Log message with Info level.
logInfo :: Logger m => T.Message -> m ()
logInfo = logMessage T.Info

-- | Log message with Error level.
logError :: Logger m => T.Message -> m ()
logError = logMessage T.Error

-- | Log message with Debug level.
logDebug :: Logger m => T.Message -> m ()
logDebug = logMessage T.Debug

-- | Log message with Warning level.
logWarning :: Logger m => T.Message -> m ()
logWarning = logMessage T.Warning
