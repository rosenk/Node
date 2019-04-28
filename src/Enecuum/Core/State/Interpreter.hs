-- TODO: this is almost exact copy-paste from testing runtime. Unify it.

module Enecuum.Core.State.Interpreter where

import           Enecuum.Prelude

import qualified Data.Map                             as Map
import           Unsafe.Coerce                        (unsafeCoerce)

import           Data.HGraph.StringHashable           (StringHash (..), toHash)
import           Enecuum.Core.HGraph.Interpreters.STM
import qualified Enecuum.Core.Language                as L
import qualified Enecuum.Core.Logger.Impl.HsLogger    as Impl
import qualified Enecuum.Core.RLens                   as RLens
import qualified Enecuum.Core.Runtime                 as Rt
import           Enecuum.Core.State.DelayedLogger
import qualified Enecuum.Core.Types                   as D

import           Control.Monad.Trans.Reader           (ReaderT, runReaderT)

getVarNumber :: Rt.StateRuntime -> STM Rt.VarNumber
getVarNumber stateRt = do
    number <- Rt.getNextId stateRt
    pure $ Rt.VarNumber number

newVar' :: Rt.StateRuntime -> a -> STM D.VarId
newVar' stateRt a = do
    varNumber <- getVarNumber stateRt
    tvar      <- newTVar $ unsafeCoerce a
    nodeState <- takeTMVar $ stateRt ^. RLens.state
    let varId = D.toHash varNumber
    putTMVar (stateRt ^. RLens.state) $ Map.insert varId (Rt.VarHandle varId tvar) nodeState
    pure varId

readVar' :: Rt.StateRuntime -> D.StateVar a -> STM a
readVar' stateRt (D.StateVar varId) = do
    nodeState <- readTMVar $ stateRt ^. RLens.state
    case Map.lookup varId nodeState of
        Nothing                    -> error $ "Var not found: " +|| varId ||+ "."
        Just (Rt.VarHandle _ tvar) -> unsafeCoerce <$> readTVar tvar

writeVar' :: Rt.StateRuntime -> D.StateVar a -> a -> STM ()
writeVar' stateRt (D.StateVar varId) val = do
    nodeState <- readTMVar $ stateRt ^. RLens.state
    case Map.lookup varId nodeState of
        Nothing                    -> error $ "Var not found: " +|| varId ||+ "."
        Just (Rt.VarHandle _ tvar) -> writeTVar tvar $ unsafeCoerce val


instance L.State' (ReaderT Rt.StateRuntime STM) where
    newVar val = do
        stateRt <- ask
        D.StateVar <$> lift (newVar' stateRt val)
    readVar var = do
        stateRt <- ask
        lift $ readVar' stateRt var
    writeVar var val = do
        stateRt <- ask
        lift $ writeVar' stateRt var val
    retry = lift retry
    -- EvalGraph :: (Serialize c, D.StringHashable c) => D.TGraph c -> Free (L.HGraphF (D.TNodeL c)) x -> (x -> next) -> StateF next
    -- EvalDelayedLogger :: L.LoggerL () -> (() -> next) -> StateF next

instance L.Logger (ReaderT Rt.StateRuntime STM) where
  logMessage lvl msg = do
      stateRt <- ask
      lift $ runReaderT (L.logMessage lvl msg) (stateRt ^. RLens.delayedLog)

-- interpretStateL _       (L.EvalGraph gr act next) = next <$> runHGraphSTM gr act

-- | Writes all delayed entries into real logger.
flushDelayedLogger :: Rt.StateRuntime -> Rt.LoggerRuntime -> IO ()
flushDelayedLogger stateRt loggerRt = do
    l <- atomically $ do
            l <- readTVar $ stateRt ^. RLens.delayedLog
            writeTVar (stateRt ^. RLens.delayedLog) []
            pure l

    let Rt.RuntimeLogger logMessage' = Rt.mkRuntimeLogger loggerRt
    mapM_ (\(Rt.DelayedLogEntry level msg) -> logMessage' level msg) l
