module Enecuum.Core.ControlFlow.Language where

import           Enecuum.Prelude


class Monad m => ControlFlow m where
    delay :: Int -> m ()
