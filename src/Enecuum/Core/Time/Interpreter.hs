module Enecuum.Core.Time.Interpreter where

import           Data.Time                  (getCurrentTime)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import qualified Enecuum.Core.Time.Language as L
import           Enecuum.Prelude

instance L.Time IO where
    getUTCTime   = getCurrentTime
    getPosixTime = getPOSIXTime

-- runTimeL :: Free L.TimeF a -> IO a
-- runTimeL = foldFree interpretTimeF
