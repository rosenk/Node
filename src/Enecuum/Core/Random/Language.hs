-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Random.Language where

import           Control.Monad.Random             hiding (Random, next)
import qualified Data.ByteString.Internal         as BSI
import           Enecuum.Core.Crypto.Language     as L
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor
import           Data.UUID (UUID)

class ERandom m where
    -- evalCoreCrypto      :: CryptoL a -> m a      -- ??
    getRandomInt        :: (Int,Int) -> m Int
    getRandomByteString :: Int -> m BSI.ByteString
    nextUUID            :: m UUID
