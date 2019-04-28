module Enecuum.Core.Random.Interpreter where

import           Data.UUID.V1                    (nextUUID)
import qualified Enecuum.Core.Crypto.Interpreter as I
import qualified Enecuum.Core.Language           as L
import           Enecuum.Prelude
import           System.Entropy
import           System.Random                   hiding (next)


instance L.ERandom IO where
    getRandomInt        = randomRIO
    getRandomByteString = getEntropy
    nextUUID            = fromJust <$> nextUUID
