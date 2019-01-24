{-# LANGUAGE PackageImports #-}

module Enecuum.Core.Database.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language as L
import qualified Enecuum.Core.Types as D
import qualified "rocksdb-haskell" Database.RocksDB as Rocks

import           Control.Monad.Trans.Reader (ReaderT, ask)



-- TODO: think about read / write options.
-- https://task.enecuum.com/issues/2859

writeOpts :: Rocks.WriteOptions
writeOpts = Rocks.defaultWriteOptions { Rocks.sync = True }

instance L.Database db (ReaderT Rocks.DB IO) where
    hasKeyRaw key = do
        db <- ask
        mbVal <- lift $ Rocks.get db Rocks.defaultReadOptions key
        pure $ isJust mbVal
    getValueRaw key = do
        db <- ask
        mbVal <- lift $ Rocks.get db Rocks.defaultReadOptions key
        pure $ case mbVal of
            Nothing  -> Left $ D.DBError D.KeyNotFound (show key)
            Just val -> Right val
    putValueRaw key val = do
        db <- ask
        r <- lift $ Rocks.put db writeOpts key val
        pure $ Right r
