{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Logger.Language where

import qualified Enecuum.Core.Types as T (LogLevel (..), Message)
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor

class Logger m where
  logMessage :: T.LogLevel -> T.Message -> m ()
