{-# LANGUAGE TemplateHaskell #-}
module Enecuum.Core.CoreEffect.Language where

import           Enecuum.Core.ControlFlow.Language (ControlFlow (..))
import           Enecuum.Core.FileSystem.Language
import           Enecuum.Core.Logger.Language      (Logger, logMessage)
import           Enecuum.Core.Random.Language
import           Enecuum.Core.Time.Language        (Time (..))
import           Enecuum.Prelude                   hiding (readFile, writeFile)
import           Language.Haskell.TH.MakeFunctor   (makeFunctorInstance)


-- -- | Core effects container language.
-- data CoreEffectF next where
--   -- | Logger effect
--   EvalLogger      :: LoggerL ()     -> (() -> next) -> CoreEffectF next
--   -- | Random effect
--   EvalRandom      :: ERandomL a     -> (a  -> next) -> CoreEffectF next
--   -- | FileSystem effect
--   EvalFileSystem  :: FileSystemL a  -> (a -> next) -> CoreEffectF next
--   -- | ControlFlow effect
--   EvalControlFlow :: ControlFlowL a -> (a  -> next) -> CoreEffectF next
--   -- | Time effect
--   EvalTime        :: TimeL a        -> (a  -> next) -> CoreEffectF next
--   -- | Impure effect. Avoid using it in production code (it's not testable).
--   EvalIO          :: IO a           -> (a  -> next) -> CoreEffectF next



type CoreEffect m =
    ( Time m
    , Logger m
    , Random m
    , FileSystem m
    , ControlFlow m
    , Time m
    -- , IO m     -- ????
    )
