module Enecuum.Core.Random.Interpreter where

import qualified Enecuum.Core.Language            as L
import           Enecuum.Prelude
import           System.Entropy
import           System.Random                    hiding (next)
import qualified Enecuum.Core.Crypto.Interpreter as I
import           Data.UUID.V1 (nextUUID)


instance L.ERandom IO where
    -- evalCoreCrypto      :: CryptoL a -> m a  -- ??
    getRandomInt        = andomRIO
    getRandomByteString = getEntropy
    nextUUID            = fromJust <$> nextUUID


-- interpretERandomL (L.EvalCoreCrypto a next) = do
--     r <- I.runCryptoL a
--     pure $ next r
