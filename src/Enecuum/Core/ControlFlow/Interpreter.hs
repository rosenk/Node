module Enecuum.Core.ControlFlow.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.ControlFlow.Language as L
import qualified Enecuum.Runtime as Rt

instance L.ControlFlow IO where
    delay = threadDelay
