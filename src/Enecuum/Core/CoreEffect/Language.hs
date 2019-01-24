{-# LANGUAGE TemplateHaskell #-}
module Enecuum.Core.CoreEffect.Language where

import           Enecuum.Core.ControlFlow.Language (ControlFlow (..))
import           Enecuum.Core.FileSystem.Language
import           Enecuum.Core.Crypto.Language
import           Enecuum.Core.Logger.Language      (Logger, logMessage)
import           Enecuum.Core.Random.Language
import           Enecuum.Core.Time.Language        (Time (..))
import           Enecuum.Prelude                   hiding (readFile, writeFile)
import           Language.Haskell.TH.MakeFunctor   (makeFunctorInstance)




type CoreEffect m =
    ( Time m
    , Logger m
    , ERandom m
    , Crypto m
    , FileSystem m
    , ControlFlow m
    , Time m
    , MonadIO m     -- ????
    )

-- ???
-- class IOL m where
--     evalIO :: IO a -> m a
--
-- instance IOL CoreEffectL where
--   evalIO io = liftF $ EvalIO io id
