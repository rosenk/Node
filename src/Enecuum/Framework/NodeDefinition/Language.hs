{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Framework.NodeDefinition.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Node.Language          as L
import qualified Enecuum.Framework.Domain                 as D
import           Enecuum.Framework.RpcMethod.Language     (RpcMethodL)
import           Enecuum.Legacy.Service.Network.Base
import qualified Enecuum.Framework.Domain.Networking      as D
import           Enecuum.Framework.MsgHandler.Language

-- TODO: it's possible to make these steps evaluating step-by-step, in order.
-- Think about if this really needed.

-- | Node description language.
-- Allows to specify what actions should be done when node starts.
data NodeDefinitionF next where
    -- | Set node tag. For example, "boot node".
    NodeTag        :: D.NodeTag -> (() -> next) -> NodeDefinitionF next
    -- | Evaluate some node model.
    EvalNodeL :: L.NodeL  a -> (a -> next) -> NodeDefinitionF next

    -- | Eval core effect.
    EvalCoreEffectNodeDefinitionF :: L.CoreEffect a -> (a -> next) -> NodeDefinitionF next
    -- | Serving of Rpc request.
    ServingRpc     :: PortNumber -> RpcMethodL (Free L.NodeF) () -> (() -> next) -> NodeDefinitionF next
    -- | Stop serving of Rpc server.
    StopServing    :: PortNumber -> (() -> next) -> NodeDefinitionF  next
    ServingMsg     :: PortNumber -> MsgHandlerL (Free L.NodeF) () -> (() -> next)-> NodeDefinitionF  next

instance Functor NodeDefinitionF where
    fmap g (NodeTag tag next)               = NodeTag tag               (g . next)
    fmap g (EvalNodeL nodeModel next)       = EvalNodeL nodeModel       (g . next)
    fmap g (ServingMsg a b next)                     = ServingMsg a b                     (g . next)
    fmap g (EvalCoreEffectNodeDefinitionF coreEffect next) = EvalCoreEffectNodeDefinitionF coreEffect (g . next)
    fmap g (ServingRpc port handlersF next)          = ServingRpc port handlersF          (g . next)
    fmap g (StopServing port next)                   = StopServing port                   (g . next)

type NodeDefinitionL  next = Free NodeDefinitionF next

-- | Sets tag for node.
nodeTag :: D.NodeTag -> NodeDefinitionL ()
nodeTag tag = liftF $ NodeTag tag id

-- | Runs node scenario.
evalNodeL :: L.NodeL  a -> NodeDefinitionL a
evalNodeL nodeModel = liftF $ EvalNodeL nodeModel id


-- | Eval core effect.
evalCoreEffectNodeDefinitionF :: L.CoreEffect a -> NodeDefinitionL  a
evalCoreEffectNodeDefinitionF coreEffect = liftF $ EvalCoreEffectNodeDefinitionF coreEffect id

-- | Runs scenario as initialization.
initialization :: L.NodeL a -> NodeDefinitionL  a
initialization = evalNodeL

-- | Runs scenario.
scenario :: L.NodeL a -> NodeDefinitionL  a
scenario = evalNodeL

-- | Runs RPC server.
servingRpc :: PortNumber -> RpcMethodL (Free L.NodeF) () -> NodeDefinitionL ()
servingRpc port handlersF = liftF $ ServingRpc port handlersF id

stopServing :: PortNumber -> NodeDefinitionL  ()
stopServing port = liftF $ StopServing port id



instance L.Logger (Free NodeDefinitionF) where
    logMessage level msg = evalCoreEffectNodeDefinitionF $ L.logMessage level msg

