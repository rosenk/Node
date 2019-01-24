{-# LANGUAGE PackageImports #-}
module Enecuum.Core.Crypto.Interpreter where

import           "cryptonite" Crypto.Random (MonadRandom)
import           Crypto.TripleSec           --(decryptIO, encryptIO)
import           Data.ByteString.Char8      (pack)
import           Enecuum.Core.Crypto.Crypto (generateNewRandomAnonymousKeyPair, sign)
import           Enecuum.Core.Crypto.Crypto
import qualified Enecuum.Core.Language      as L
import           Enecuum.Prelude

instance L.Crypto IO where
    generateKeyPair = generateNewRandomAnonymousKeyPair
    sign key msg = sign key msg
    encrypt key msg = encryptIO key msg
    decrypt key encryptedMsg = do
      eDecryptedMsg :: Either SomeException L.Key <- try $ decryptIO key encryptedMsg
      pure $ case eDecryptedMsg of
            Left e -> Nothing
            Right decryptedMsg -> Just decryptedMsg
