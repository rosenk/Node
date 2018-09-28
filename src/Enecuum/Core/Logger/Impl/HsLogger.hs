module Enecuum.Core.Logger.Impl.HsLogger where

import           Enecuum.Prelude

import qualified Data.Text                   as TXT (unpack)
import           System.IO                   (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler          (close, setFormatter)
import           System.Log.Handler.Simple   (GenericHandler, fileHandler, streamHandler)
import           System.Log.Logger

import qualified Enecuum.Core.Language       as L
import qualified Enecuum.Core.Types          as T (Format, LogLevel (..), LoggerConfig(..))

-- | Opaque type covering all information needed to teardown the logger.
data HsLoggerHandle = HsLoggerHandle
  { rootLogHandler :: GenericHandler Handle
  , serverLogHandler :: GenericHandler Handle
  }

component :: String
component = "Node.Main"

-- | Bracket an IO action which denotes the whole scope where the loggers of
-- the application are needed to installed. Sets them up before running the action
-- and tears them down afterwards. Even in case of an exception.
withLogger :: T.LoggerConfig -> (HsLoggerHandle -> IO c) -> IO c
withLogger config = bracket setupLogger' teardownLogger
  where setupLogger' = setupLogger config

-- | Dispatch log level from the LoggerL language
-- to the relevant log level of hslogger package
dispatchLogLevel :: T.LogLevel -> Priority
dispatchLogLevel T.Debug   = DEBUG
dispatchLogLevel T.Info    = INFO
dispatchLogLevel T.Warning = WARNING
dispatchLogLevel T.Error   = ERROR

-- | Interpret LoggerL language.
interpretLoggerL :: HsLoggerHandle -> L.LoggerF a -> IO a
interpretLoggerL _ (L.LogMessage level msg next) = do
  logM component (dispatchLogLevel level) $ TXT.unpack msg
  pure $ next ()

runLoggerL :: Maybe HsLoggerHandle -> L.LoggerL () -> IO ()
runLoggerL (Just h) l = foldFree (interpretLoggerL h) l
runLoggerL Nothing _ = pure ()

-- | Setup logger required by the application.
setupLogger :: T.LoggerConfig -> IO HsLoggerHandle
setupLogger (T.LoggerConfig format level logFileName isConsoleLog) = do
  logHandler <- fileHandler logFileName (dispatchLogLevel level) >>=
        \lh -> pure $ setFormatter lh (simpleLogFormatter format)

  stdoutLog <- streamHandler stdout (dispatchLogLevel level) >>=
    \lh -> pure $ setFormatter lh (simpleLogFormatter format)

  -- root Log
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [logHandler])

  let handlers = if isConsoleLog == True then [stdoutLog] else []

  updateGlobalLogger component (setLevel DEBUG . setHandlers handlers)

  -- return opaque HsLoggerHandle handle
  pure $ HsLoggerHandle logHandler stdoutLog

-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger :: HsLoggerHandle -> IO ()
teardownLogger handle = do
  close $ serverLogHandler handle
  close $ rootLogHandler handle
