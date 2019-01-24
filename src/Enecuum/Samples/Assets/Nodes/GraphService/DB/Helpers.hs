module Enecuum.Samples.Assets.Nodes.GraphService.DB.Helpers where

import           Enecuum.Prelude

import qualified Enecuum.Samples.Blockchain.DB                              as D
import qualified Enecuum.Samples.Blockchain.DB.Lens                         as Lens
import qualified Enecuum.Samples.Blockchain.Lens                            as Lens
import qualified Enecuum.Domain                                     as D
import qualified Enecuum.Language                                   as L

import           Enecuum.Samples.Assets.Nodes.GraphService.GraphServiceData (GraphServiceData (..))

withKBlocksDB
    :: forall s db a
    .  Lens.HasKBlocksDB s (D.Storage db)
    => s
    -> (forall m. L.Database m => m a)
    -> L.NodeL a
withKBlocksDB dbModel db = L.withDatabase (dbModel ^. Lens.kBlocksDB) db

withKBlocksMetaDB
    :: forall s db a
    .  Lens.HasKBlocksMetaDB s (D.Storage db)
    => s
    -> (forall m. L.Database m => m a)
    -> L.NodeL a
withKBlocksMetaDB dbModel db = L.withDatabase (dbModel ^. Lens.kBlocksMetaDB) db

withMBlocksDB
    :: forall s db a
    .  Lens.HasMBlocksDB s (D.Storage db)
    => s
    -> (forall m. L.Database m => m a)
    -> L.NodeL a
withMBlocksDB dbModel db = L.withDatabase (dbModel ^. Lens.mBlocksDB) db

withMBlocksMetaDB
    :: forall s db a
    .  Lens.HasMBlocksMetaDB s (D.Storage db)
    => s
    -> (forall m. L.Database m => m a)
    -> L.NodeL a
withMBlocksMetaDB dbModel db = L.withDatabase (dbModel ^. Lens.mBlocksMetaDB) db

withTransactionsDB
    :: forall s db a
    .  Lens.HasTransactionsDB s (D.Storage db)
    => s
    -> (forall m. L.Database m => m a)
    -> L.NodeL a
withTransactionsDB dbModel db = L.withDatabase (dbModel ^. Lens.transactionsDB) db

withTransactionsMetaDB
    :: forall s db a
    .  Lens.HasTransactionsMetaDB s (D.Storage db)
    => s
    -> (forall m. L.Database m => m a)
    -> L.NodeL a
withTransactionsMetaDB dbModel db = L.withDatabase (dbModel ^. Lens.transactionsMetaDB) db

withDBModel :: GraphServiceData -> (D.DBModel -> L.NodeL ()) -> L.NodeL ()
withDBModel (_db -> Just dbModel) act = act dbModel
withDBModel _ _                       = pure ()

-- | On `Right val`, evals `action` with `val`.
-- On `Left err`, returns `Left err`.
withResult
    :: Applicative f
    => Either a t
    -> (t -> f (Either a b))
    -> f (Either a b)
withResult (Left err)     _      = pure $ Left err
withResult (Right result) action = action result

-- | On `Right val`, evals `action` with `val` and returns a list of successes.
-- On `Left err`, logs error except it is KeyNotFound and returns empty list.
materialize
    :: Monad m
    => L.Logger m
    => D.DBResult t
    -> (t -> m [b])
    -> m [b]
materialize (Right result)                     action = action result
materialize (Left (D.DBError D.KeyNotFound _)) _      = pure []
materialize (Left err)                         _      = do
    L.logError $ show err
    pure []
