{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Assets.Nodes.TstNodes.GraphNode.Config where

import qualified Data.Aeson                               as A
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.GraphService.Config
import           Enecuum.Config
import           Enecuum.Domain                           (NodePorts (..))
import qualified Enecuum.Domain                           as D
import           Enecuum.Prelude

data TstGraphNode = TstGraphNode
    deriving (Show, Generic)

data instance NodeConfig TstGraphNode = TstGraphNodeConfig
    { _graphServiceConfig :: GraphServiceConfig
    , _nodePorts          :: NodePorts
    }
    deriving (Show, Generic)

instance ToJSON   TstGraphNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstGraphNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TstGraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstGraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions


graphNodeTransmitterConfig :: D.NodeConfig TstGraphNode
graphNodeTransmitterConfig = TstGraphNodeConfig
  { _graphServiceConfig = GraphServiceConfig
      { _dbConfig = noDBConfig
      , _rpcSynco = Nothing
      }
  , _nodePorts = defaultGnNodePorts
  }

graphNodeReceiverConfig :: D.NodeConfig TstGraphNode
graphNodeReceiverConfig = TstGraphNodeConfig
  { _graphServiceConfig = GraphServiceConfig
      { _dbConfig = noDBConfig
      , _rpcSynco = Just $ getRpcAddress defaultGnNodeAddress
      }
  , _nodePorts = defaultGnReceiverNodePorts
  }
