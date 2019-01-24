{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE TemplateHaskell     #-}

module Enecuum.Core.Crypto.Language where

import           Control.Monad.Random            hiding (Random, next)
import           Data.ByteString.Char8           (pack)
import           Enecuum.Core.Crypto.Crypto
import           Enecuum.Prelude                 hiding (Key)
import           Language.Haskell.TH.MakeFunctor

type Key = ByteString
type Msg = ByteString
type CipheredMsg = ByteString

class Crypto m where
    generateKeyPair :: m KeyPair
    sign :: (Serialize msg) => PrivateKey -> msg -> m Signature
    encrypt :: Key -> Msg -> m CipheredMsg
    decrypt :: Key -> CipheredMsg -> m (Maybe Msg)
