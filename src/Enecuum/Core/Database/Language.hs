{-# LANGUAGE AllowAmbiguousTypes #-}

module Enecuum.Core.Database.Language where

import           Data.Typeable               (typeOf)
import           Enecuum.Prelude

import qualified Enecuum.Core.Types.Database as D

class Monad m => Database m where
    -- | Check whether the key exists.
    hasKeyRaw   :: D.DBKeyRaw -> m Bool
    -- | Lookup a value from the DB.
    getValueRaw :: D.DBKeyRaw -> m (D.DBResult D.DBValueRaw)
    -- | Write a single value to the DB.
    putValueRaw :: D.DBKeyRaw -> D.DBValueRaw -> m (D.DBResult ())

-- | Puts a typed entity to the corresponding DB.
putEntity
    :: forall entity db m
    .  D.RawDBEntity db entity
    => Database m
    => D.DBKey entity
    -> D.DBValue entity
    -> m (D.DBResult ())
putEntity dbKey dbVal = let
    rawKey = D.toRawDBKey   @db dbKey
    rawVal = D.toRawDBValue @db dbVal
    in putValueRaw rawKey rawVal

-- | Puts a typed entity to the corresponding DB.
putEntity'
    :: forall entity db src m
    .  D.RawDBEntity db entity
    => D.ToDBKey   entity src
    => D.ToDBValue entity src
    => Database m
    => src
    -> m (D.DBResult ())
putEntity' src = let
    rawKey = D.toRawDBKey   @db @entity $ D.toDBKey   src
    rawVal = D.toRawDBValue @db @entity $ D.toDBValue src
    in putValueRaw rawKey rawVal

-- | Gets a typed entity from the corresponding DB.
getEntity
    :: forall entity db m
    . (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => Database m
    => D.DBKey entity
    -> m (D.DBResult (D.DBE entity))
getEntity dbKey = do
    let rawKey = D.toRawDBKey @db dbKey
    let proxyVal = error "Don't call me, I'm Proxy" :: D.DBValue entity
    eRawVal <- getValueRaw rawKey
    case eRawVal of
        Left err       -> pure $ Left err
        Right rawVal   -> case D.fromRawDBValue @db rawVal of
            Nothing    -> pure $ Left $ D.DBError D.InvalidType ("Expected type: " <> show (typeOf proxyVal)
                            <> ". Raw key: <" <> decodeUtf8 rawKey <>  ">. Raw data: <" <> decodeUtf8 rawVal <> ">")
            Just dbVal -> pure $ Right (dbKey, dbVal)

-- | Gets a typed value from the corresponding DB.
getValue
    :: forall entity db m
     . (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => Database m
    => D.DBKey entity
    -> m (D.DBResult (D.DBValue entity))
getValue dbKey = do
    eEntity <- getEntity @entity @db @m dbKey
    pure $ eEntity >>= Right . snd

-- | Gets a typed value from the corresponding DB.
getValue'
    :: forall entity db m src
     . (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => D.ToDBKey entity src
    => Database m
    => src
    -> m (D.DBResult (D.DBValue entity))
getValue' src = do
    eEntity <- getEntity @entity @db @m $ D.toDBKey src
    pure $ eEntity >>= Right . snd

-- | Gets a typed value from the corresponding DB.
-- The difference from @getValue@ is that it forgets about DB errors.
findValue
    :: forall entity db m
     . (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => Database m
    => D.DBKey entity
    -> m (Maybe (D.DBValue entity))
findValue key = do
    eVal <- getValue @entity @db @m key
    pure $ either (const Nothing) Just eVal

-- | Gets a typed value from the corresponding DB.
-- The difference from @getValue'@ is that it forgets about DB errors.
findValue'
    :: forall entity db m src
     . (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => D.ToDBKey entity src
    => Database m
    => src
    -> m (D.DBResult (Maybe (D.DBValue entity)))
findValue' src = do
    eVal <- getValue' @entity @db @m src
    case eVal of
        Left (D.DBError D.KeyNotFound _) -> pure $ Right Nothing
        Left err                         -> pure $ Left err
        Right val                        -> pure $ Right $ Just val
