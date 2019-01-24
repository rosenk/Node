{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}


module Enecuum.Framework.Node.Language where

import qualified Enecuum.Core.Language                      as L
import qualified Enecuum.Core.Types                         as D
import qualified Enecuum.Framework.Domain.Networking        as D
import           Enecuum.Framework.Handler.Network.Language
import qualified Enecuum.Framework.Networking.Language      as L
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor

-- | Node language.
data NodeF next where
    -- | Eval stateful action atomically.
    EvalStateAtomically :: (forall m. L.State' m => m a) -> (a -> next) -> NodeF next
    -- | Eval networking.
    EvalNetworking :: L.NetworkingL a -> (a -> next) -> NodeF next

    -- | Eval core effect.
    EvalCoreEffect :: (forall m. L.CoreEffect m => m a) -> (a -> next) -> NodeF next

    -- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
    EvalGraphIO :: (Serialize c, D.StringHashable c) => D.TGraph c -> Free (L.HGraphF (D.TNodeL c)) x -> (x -> next) -> NodeF next
    -- | Create new graph instance.
    NewGraph  :: (Serialize c, D.StringHashable c) => (D.TGraph c -> next) -> NodeF next
    -- | Open connection to the node.
    OpenTcpConnection :: D.Address -> NetworkHandlerL D.Tcp NodeL () -> (Maybe (D.Connection D.Tcp) -> next) -> NodeF next
    OpenUdpConnection :: D.Address -> NetworkHandlerL D.Udp NodeL () -> (Maybe (D.Connection D.Udp) -> next) -> NodeF next
    -- | Close existing connection.
    CloseTcpConnection :: D.Connection D.Tcp -> (() -> next) -> NodeF  next
    CloseUdpConnection :: D.Connection D.Udp -> (() -> next) -> NodeF  next
    -- | Create database with config.
    InitDatabase :: D.DBConfig db -> (D.DBResult (D.Storage db) -> next) -> NodeF next

    -- | Eval database.
    EvalDatabase :: D.Storage db -> (forall m. L.Database m => m a) -> (a -> next) -> NodeF next

type NodeL = Free NodeF

makeFunctorInstance ''NodeF

-- | Eval stateful action atomically.
evalStateAtomically :: (forall m. L.State' m => m a) -> NodeL a
evalStateAtomically statefulAction = liftF $ EvalStateAtomically statefulAction id

-- TODO: makeLanguage ''NodeF
-- | Eval networking.
evalNetworking :: L.NetworkingL a -> NodeL a
evalNetworking newtorking = liftF $ EvalNetworking newtorking id

-- | Eval core effect.
evalCoreEffect :: (forall m. L.CoreEffect m => m a) -> NodeL a
evalCoreEffect coreEffect = liftF $ EvalCoreEffect coreEffect id

-- | Init database with the options passed for the specific storage type.
initDatabase :: D.DBConfig db -> NodeL (D.DBResult (D.Storage db))
initDatabase config = liftF $ InitDatabase config id

-- | Eval database.
evalDatabase :: D.Storage db -> (forall m. L.Database m => m a) -> NodeL a
evalDatabase dbStorage db = liftF $ EvalDatabase dbStorage db id

-- | Eval database.
withDatabase :: D.Storage db -> (forall m. L.Database m => m a) -> NodeL a
withDatabase = evalDatabase

withConnection
    :: (Monad m, Connection m con)
    => con -> D.Address -> (D.Connection con -> m b) -> m (Maybe b)
withConnection protocol address f = do
    mCon <- open protocol address $ pure ()
    case mCon of
        Just con -> do
            !a <- f con
            close con
            pure $ Just a
        Nothing -> pure Nothing

listener :: (Connection m con, Typeable con, Typeable t,
            Typeable m, Monad m, FromJSON t) =>
            (t -> m ()) -> NetworkHandlerL con m ()
listener !f = handler (\a conn -> void (close conn) >> f a)

class Connection a con where
    close :: D.Connection con -> a ()
    open  :: con -> D.Address -> NetworkHandlerL con NodeL () -> a (Maybe (D.Connection con))

instance Connection (Free NodeF) D.Tcp where
    close   conn       = liftF $ CloseTcpConnection conn id
    open  _ addr handl = liftF $ OpenTcpConnection  addr handl id

instance Connection (Free NodeF) D.Udp where
    close   conn       = liftF $ CloseUdpConnection conn id
    open  _ addr handl = liftF $ OpenUdpConnection  addr handl id

instance L.Send a (Free L.NetworkingF) => L.Send a NodeL where
    send conn   = evalNetworking . L.send conn

instance L.SendUdp NodeL where
    notify conn = evalNetworking . L.notify conn

-- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
evalGraphIO :: (D.StringHashable c, Serialize c) => D.TGraph c -> Free (L.HGraphF (D.TNodeL c)) a -> NodeL a
evalGraphIO g graphAction = liftF $ EvalGraphIO g graphAction id

newGraph :: (Serialize c, D.StringHashable c) => NodeL (D.TGraph c)
newGraph = liftF $ NewGraph id

-- instance L.IOL NodeL where
--     evalIO = evalCoreEffect

instance L.Logger NodeL where
    logMessage level msg = evalCoreEffect (L.logMessage level msg)

instance L.Crypto NodeL where
    generateKeyPair = evalCoreEffect L.generateKeyPair
    sign key msg = evalCoreEffect $ L.sign key msg
    encrypt key msg = evalCoreEffect $ L.encrypt key msg
    decrypt key msg = evalCoreEffect $ L.decrypt key msg

instance L.ERandom NodeL where
    getRandomInt        v = evalCoreEffect $ L.getRandomInt v
    getRandomByteString v = evalCoreEffect $ L.getRandomByteString v
    nextUUID              = evalCoreEffect   L.nextUUID

instance L.FileSystem NodeL where
    readFile filename = evalCoreEffect (L.readFile filename)
    writeFile filename text = evalCoreEffect $ L.writeFile filename text
    appendFile filename text = evalCoreEffect $ L.appendFile filename text
    getHomeDirectory = evalCoreEffect L.getHomeDirectory
    createFilePath filepath  = evalCoreEffect $ L.createFilePath filepath
    doesFileExist filepath   = evalCoreEffect $ L.doesFileExist filepath

instance L.ControlFlow NodeL where
    delay i = evalCoreEffect (L.delay i)

instance L.StateIO NodeL where
    atomically     v = evalStateAtomically v
    newVarIO       v = evalStateAtomically (L.newVar v)
    readVarIO      v = evalStateAtomically (L.readVar v)
    writeVarIO var v = evalStateAtomically (L.writeVar var v)

-- instance L.Time NodeL where
--     getUTCTime   = evalCoreEffect L.getUTCTime
--     getPosixTime = evalCoreEffect L.getPosixTime
