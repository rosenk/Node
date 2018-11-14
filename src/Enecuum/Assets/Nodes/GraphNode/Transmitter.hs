module Enecuum.Assets.Nodes.GraphNode.Transmitter where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.Config
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Assets.Nodes.GraphNode.Database
import           Enecuum.Assets.Nodes.Routing.Messages
import           Enecuum.Assets.Nodes.Routing.Runtime

-- | Start of graph node
graphNodeTransmitter :: NodeConfig GraphNode -> L.NodeDefinitionL ()
graphNodeTransmitter nodeCfg = do
    L.nodeTag "graphNodeTransmitter"
    eNodeData <- graphNodeInitialization nodeCfg
    either L.logError (graphNodeTransmitter' nodeCfg) eNodeData

graphNodeTransmitter' :: NodeConfig GraphNode -> GraphNodeData -> L.NodeDefinitionL ()
graphNodeTransmitter' cfg nodeData = do
    let myNodePorts = NodePorts
            (Enecuum.Assets.Nodes.GraphNode.Config._udpPort cfg)
            (Enecuum.Assets.Nodes.GraphNode.Config._tcpPort cfg )
            ((\(D.Address _ port) -> port) $ _rpc cfg)
    -- TODO: read from config
    let myHash      = D.toHashGeneric myNodePorts
    let bnPorts     = makeNodePorts1000 5000
    let bnId        = D.toHashGeneric bnPorts
    let bnAddress   = NodeAddress "127.0.0.1" bnPorts bnId
    routingData <- L.scenario $ makeRoutingRuntimeData myNodePorts myHash bnAddress

    case _rpcSynco cfg of
        Nothing -> pure ()
        Just rpcSyncoAddress -> L.process $ forever $ graphSynchro nodeData rpcSyncoAddress
    L.serving D.Udp (myNodePorts ^. udpPort) $ do
        -- network
        L.handler   methodPing
        -- PoA interaction
        L.handler $ udpBroadcastRecivedMessage routingData (acceptMBlock' nodeData)
        -- PoW interaction
        L.handler $ udpBroadcastRecivedMessage routingData (acceptKBlock' nodeData)

    L.serving D.Tcp (myNodePorts ^. tcpPort) $
        -- network
        L.handler   methodPing

    L.serving D.Rpc (myNodePorts ^. rpcPort) $ do
        -- network
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

        -- client interaction
        L.methodE $ getBalance nodeData
        L.methodE $ acceptTransaction nodeData

        -- db
        L.methodE $ handleDumpToDB nodeData
        L.methodE $ handleRestoreFromDB nodeData

        -- graph node interaction
        L.method  $ getChainLength nodeData
        L.methodE $ acceptChainFromTo nodeData
        L.methodE $ getMBlockForKBlocks nodeData

        -- PoW interaction
        L.method  $ getKBlockPending nodeData

        -- PoA interaction
        L.method  $ getTransactionPending nodeData
        L.method  $ getLastKBlock nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData

    L.process $ forever $ do
        L.awaitSignal $ nodeData ^. dumpToDBSignal
        dumpToDB nodeData

    L.process $ forever $ do
        L.awaitSignal $ nodeData ^. restoreFromDBSignal
        restoreFromDB nodeData

    L.process $ forever $ do
        L.awaitSignal $ nodeData ^. checkPendingSignal
        blockFound <- processKBlockPending' nodeData
        when blockFound $ L.writeVarIO (nodeData ^. checkPendingSignal) True
    
    routingWorker routingData
    L.awaitNodeFinished nodeData
