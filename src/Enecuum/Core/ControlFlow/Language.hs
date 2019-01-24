module Enecuum.Core.ControlFlow.Language where

import           Enecuum.Prelude


class ControlFlow m where
    delay :: Int -> m ()
