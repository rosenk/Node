{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW where

import Enecuum.Prelude
import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D

import Enecuum.Assets.Nodes.Address (powAddr)
import           Data.HGraph.StringHashable (StringHash (..))

data PoWNodeData = PoWNodeData
    {
      _prevHash :: D.StateVar StringHash
    }

makeFieldsNoPrefix ''PoWNodeData

data KeyBlockRequest  = KeyBlockRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data KeyBlockResponse = KeyBlockResponse { kBlock :: [D.KBlock] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Send bunch of kblocks
sendKBlock :: PoWNodeData -> Integer -> KeyBlockRequest -> L.NodeL KeyBlockResponse
sendKBlock nodeData from KeyBlockRequest = do
  prevKBlockHash <- L.atomically <$> L.readVar $ nodeData ^. prevHash
  kBlocks <- D.createKBlocks prevKBlockHash from D.RandomOrder
  pure $ KeyBlockResponse kBlocks


genKBlockProcess :: PoWNodeData -> L.NodeL ()
genKBlockProcess = do
    forM_ [0, D.kBlockInBunch ..] (\from -> do
        L.method $ sendKBlock nodeData from)


powNode :: L.NodeDefinitionL ()
powNode = do
    L.nodeTag "PoW node"
    L.logInfo "Generate Key Block"

    -- initialize with genesis hash
    nodeData <- L.initialization $ powNodeInitialization D.genesisHash

    forever $ do
        delay $ 1000 * 1000
        L.scenario $ genKBlockProcess nodeData

powNodeInitialization :: StringHash -> L.NodeL PoWNodeData
powNodeInitialization genesisHash = do
  h <- L.atomically $ L.newVar genesisHash
  pure $ PoWNodeData h
