{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.Messages where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import           Data.HGraph.StringHashable

data SuccessMsg = SuccessMsg 
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type WalletId = Integer

data GetLastKBlock = GetLastKBlock
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetWalletBalance = GetWalletBalance { walletId :: WalletId }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetGraphNode = GetGraphNode {hash :: StringHash}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WalletBalanceMsg = WalletBalanceMsg
  { walletId :: WalletId
  , balance  :: Integer
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)