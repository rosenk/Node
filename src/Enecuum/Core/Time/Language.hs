module Enecuum.Core.Time.Language where

import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Enecuum.Prelude

class Time m where
    getUTCTime   :: m UTCTime
    getPosixTime :: m POSIXTime
