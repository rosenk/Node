{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module PoA.PoAServer (
        servePoA
    ,   serverPoABootNode
  )  where


import              Node.Data.NetPackage
import              Control.Monad (forM_, void, forever, unless, when)
import qualified    Network.WebSockets                  as WS
import              Service.Network.Base
import              Service.Network.WebSockets.Server
import qualified    Control.Concurrent.Chan as C
import              Control.Concurrent.Chan.Unagi.Bounded
import              Node.Node.Types
import              Service.InfoMsg as I
import              Service.Types
import              System.Random.Shuffle
import              Data.Aeson as A
import              Control.Exception
import              Node.Data.GlobalLoging
import              PoA.Types
import              Control.Concurrent.MVar
import qualified    Control.Concurrent as C
import              Node.FileDB.FileServer
import              PoA.Pending

import              Control.Concurrent.Async
import              Node.Data.Key
import              Data.Maybe()


serverPoABootNode :: PortNumber -> InChan InfoMsg -> InChan FileActorRequest -> IO ()
serverPoABootNode aRecivePort aInfoChan aFileServerChan = do
    writeLog aInfoChan [ServerBootNodeTag, InitTag] Info $
        "Init. ServerPoABootNode: a port is " ++ show aRecivePort
    runServer aRecivePort $ \_ aPending -> do
        aConnect <- WS.acceptRequest aPending
        writeLog aInfoChan [ServerBootNodeTag] Info "ServerPoABootNode.Connect accepted."
        WS.forkPingThread aConnect 30
        forever $ do
            aMsg <- WS.receiveData aConnect
            case A.eitherDecodeStrict aMsg of
                Right a -> case a of
                    RequestConnects -> do
                        writeLog aInfoChan [ServerBootNodeTag] Info "Accepted request of connections."
                        aShuffledRecords <- shuffleM =<< getRecords aFileServerChan
                        let aConnects = snd <$> take 5 aShuffledRecords
                        WS.sendTextData aConnect $ A.encode $ ResponseConnects aConnects
                        writeLog aInfoChan [ServerBootNodeTag] Info $ "Send connections " ++ show aConnects
                    _  -> writeLog aInfoChan [ServerBootNodeTag] Warning $
                        "Broken message from PP " ++ show aMsg
                Left a ->
                    -- TODO: Include ID if exists.
                    writeLog aInfoChan [ServerBootNodeTag] Warning $
                        "Broken message from PP " ++ show aMsg ++ " " ++ a

getRecords :: InChan FileActorRequest -> IO [(NodeId, Connect)]
getRecords aChan = do
    aTmpRef <- newEmptyMVar
    writeChan aChan $ FileActorRequestNetLvl $ ReadRecordsFromNodeListFile aTmpRef
    NodeInfoListNetLvl aRecords <- takeMVar aTmpRef
    return aRecords

servePoA ::
       PortNumber
    -> InChan ManagerMiningMsgBase
    -> C.Chan Transaction
    -> InChan InfoMsg
    -> InChan FileActorRequest
    -> C.Chan Microblock
    -> IO ()
servePoA aRecivePort ch aRecvChan aInfoChan aFileServerChan aMicroblockChan = do
    writeLog aInfoChan [ServePoATag, InitTag] Info $
        "Init. servePoA: a port is " ++ show aRecivePort
    aPendingChan <- C.newChan
    void $ C.forkIO $ pendingActor aPendingChan aMicroblockChan aRecvChan aInfoChan
    runServer aRecivePort $ \_ aPending -> do
        aConnect <- WS.acceptRequest aPending
        WS.forkPingThread aConnect 30

        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP
        aId <- newEmptyMVar
        (aInpChan, aOutChan) <- newChan 64
        -- writeChan ch $ connecting to PoA, the PoA have id.
        void $ race
            (aSender aId aConnect aOutChan)
            (aReceiver aId aConnect aInpChan aPendingChan)
  where
    aSender aId aConnect aNewChan = forever (do
        aMsg <- readChan aNewChan
        WS.sendTextData aConnect $ A.encode aMsg) `finally` (do
            aIsEmpty <- isEmptyMVar aId
            unless aIsEmpty $ do
                aDeadId <- readMVar aId
                writeChan ch $ ppNodeIsDisconected aDeadId)

    aReceiver aId aConnect aNewChan aPendingChan = forever $ do
        aMsg <- WS.receiveData aConnect
        writeLog aInfoChan [ServePoATag] Info $ "Raw msg: " ++ show aMsg
        aOk <- isEmptyMVar aId
        case A.eitherDecodeStrict aMsg of
            Right a -> case a of
                -- REVIEW: Check fair distribution of transactions between nodes
                RequestTransaction aNum -> void $ C.forkIO $ do
                    aTmpChan <- C.newChan
                    C.writeChan aPendingChan $ GetTransaction aNum aTmpChan
                    aTransactions <- C.readChan aTmpChan
                    unless (null aTransactions) $
                        forM_ (take aNum $ cycle aTransactions) $ \aTransaction  -> do
                            writeLog aInfoChan [ServePoATag] Info $  "sendTransaction to poa " ++ show aTransaction
                            WS.sendTextData aConnect $ A.encode $ ResponseTransaction aTransaction
                MsgMicroblock aMicroblock
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Recived MBlock: " ++ show aMicroblock
                        sendMsgToNetLvlFromPP ch $ MicroblockFromPP aMicroblock aSenderId
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without PPId " ++ show aMsg
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP

                RequestBroadcast aRecipientType aBroadcastMsg
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Broadcast request " ++ show aMsg
                        sendMsgToNetLvlFromPP ch $
                            BroadcastRequestFromPP aBroadcastMsg (IdFrom aSenderId) aRecipientType
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without PPId " ++ show aMsg
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP
                RequestConnects -> do
                    aShuffledRecords <- shuffleM =<< getRecords aFileServerChan
                    let aConnects = snd <$> take 5 aShuffledRecords
                    writeLog aInfoChan [ServePoATag] Info $ "Send connections " ++ show aConnects
                    WS.sendTextData aConnect $ A.encode $ ResponseConnects aConnects

                RequestPoWList
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "PoWListRequest the msg from " ++ show aSenderId
                        sendMsgToNetLvlFromPP ch $ PoWListRequest (IdFrom aSenderId)

                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning "Can't send request without PPId "
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP


                ResponseNodeIdToNN aPPId aNodeType ->
                    when aOk $ do
                        putMVar aId aPPId
                        writeLog aInfoChan [ServePoATag] Info $
                            "Accept PPId " ++ show aPPId ++ " with type " ++ show aNodeType

                        sendMsgToNetLvlFromPP ch $ NewConnectWithPP aPPId aNodeType aNewChan

                MsgMsgToNN aDestination aMsgToNN
                    | not aOk       -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "Resending the msg from " ++ show aSenderId ++ " the msg is " ++ show aMsgToNN
                        sendMsgToNetLvlFromPP ch $ MsgResendingToPP (IdFrom aSenderId) (IdTo aDestination) aMsgToNN
                    | otherwise     -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Can't send request without PPId " ++ show aMsgToNN
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP

            Left a -> do
                -- TODO: Include ID if exist.
                writeLog aInfoChan [ServePoATag] Warning $
                    "Broken message from PP " ++ show aMsg ++ " " ++ a
                unless aOk $ WS.sendTextData aConnect $ A.encode RequestNodeIdToPP

-- TODO class sendMsgToNetLvl
sendMsgToNetLvlFromPP :: ManagerMsg a => InChan a -> MsgToMainActorFromPP -> IO ()
sendMsgToNetLvlFromPP aChan aMsg = writeChan aChan $ msgFromPP aMsg
