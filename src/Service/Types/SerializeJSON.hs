{-# LANGUAGE
        OverloadedStrings
    ,   PackageImports
    ,   DisambiguateRecordFields
    ,   DuplicateRecordFields
    ,   ScopedTypeVariables
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Service.Types.SerializeJSON where

import              Data.Aeson
import              Data.Aeson.Types (typeMismatch)
import qualified "cryptonite"   Crypto.PubKey.ECC.ECDSA     as ECDSA
import Service.Types.PublicPrivateKeyPair
import Service.Types
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)

import           Data.ByteString.Conversion


instance FromJSON Trans
instance ToJSON   Trans

instance FromJSON MsgTo
instance ToJSON MsgTo

instance FromJSON Currency
instance ToJSON Currency

instance FromJSON PublicKey where
  parseJSON (String s) = return $ read $ unpack s
  parseJSON _          = error "PublicKey JSON parse error"

instance ToJSON PublicKey where
  toJSON key = String $ pack $ show key

instance FromJSON PrivateKey
instance ToJSON PrivateKey


encodeToText :: ByteString -> Text
encodeToText = T.decodeUtf8 . B.encode


decodeFromText :: (MonadPlus m) => Text -> m ByteString
decodeFromText aStr = case B.decode . T.encodeUtf8 $ aStr of
    Right a -> return a
    Left _  -> mzero

intToBase64Text :: Integer -> Text
intToBase64Text i = encodeToText $ toByteString' i

base64TextToInt :: (MonadPlus m) => Text -> m Integer
base64TextToInt b = do
     bs <- decodeFromText b 
     case fromByteString bs of
       Just i -> return i
       _      -> mzero
           

instance ToJSON Hash
instance FromJSON Hash

instance ToJSON ByteString where
  toJSON h = String $ encodeToText h

instance FromJSON ByteString where
  parseJSON (String s) = decodeFromText s
  parseJSON _          = error "Wrong object format"

instance ToJSON TransactionInfo
instance FromJSON TransactionInfo






instance FromJSON MicroblockV1 where
  parseJSON (Object v) = undefined
      {-MicroblockV1
                           <$> ((v .: "curr") >>= decodeFromText)
                           <*> ((v .: "prev") >>= decodeFromText)
                           <*> v .: "txs"
-}



instance ToJSON ECDSA.Signature where
  toJSON t = object [
    "sign_r" .= intToBase64Text  (ECDSA.sign_r t),
    "sign_s" .= intToBase64Text  (ECDSA.sign_s t) ]

instance FromJSON ECDSA.Signature where
  parseJSON (Object v) = do
    s_r <- base64TextToInt =<< v .: "sign_r"
    s_s <- base64TextToInt =<< v .: "sign_s"
    return $ ECDSA.Signature s_r s_s  
  parseJSON inv        = typeMismatch "Signature" inv


instance ToJSON TransactionAPI where
   toJSON tx = object  [
             "tx"   .= _txAPI tx
           , "hash" .= _txHashAPI tx
           ]

instance FromJSON TransactionAPI where
   parseJSON (Object o) = TransactionAPI
           <$> o .: "tx"
           <*> o .: "hash"
   parseJSON inv        = typeMismatch "TransactionAPI" inv 

instance ToJSON Transaction where
   toJSON tx = object  [
           "owner"     .= _owner tx,
           "receiver"  .= _receiver tx,
           "amount"    .= _amount tx,
           "currency"  .= _currency tx,
           "timestamp" .= _time tx,
           "sign"      .= _signature tx,
           "uuid"      .= _uuid tx
           ]

instance FromJSON Transaction where
   parseJSON (Object o) = Transaction
              <$> o .: "owner"
              <*> o .: "receiver"
              <*> o .: "amount"
              <*> o .: "currency"
              <*> o .: "timestamp"
              <*> o .: "sign"
              <*> o .: "uuid"


instance ToJSON MicroblockAPI where
    toJSON bl = object  [
            "prev_block"   .= _prevBlockAPI bl
         ,  "next_block"   .= _nextBlockAPI bl 
         ,  "k_block"      .= _keyBlockAPI bl
         ,  "team"         .= _teamKeysAPI bl
         ,  "publisher"    .= _publisherAPI bl
         ,  "sign"         .= _signAPI bl
--         ,  "txs_cnt"      .= length (_transactionsAPI bl)
         ,  "transactions" .= _transactionsAPI bl
       ]

instance FromJSON MicroblockAPI where
    parseJSON (Object o) = MicroblockAPI
               <$> o .: "prev_block"
               <*> o .: "next_block"
               <*> o .: "k_block"
               <*> o .: "sign"
               <*> o .: "team"
               <*> o .: "publisher"
               <*> o .: "transactions"
    parseJSON inv         = typeMismatch "Microblock" inv


instance ToJSON Microblock where
 toJSON aBlock = object [
       "msg" .= object [
           "K_hash"  .= encodeToText (_keyBlock aBlock),
           "wallets" .= _teamKeys aBlock,
           "Tx"      .= _transactions aBlock
--           "uuid"    .= _numOfBlock aBlock
         ],
       "sign" .= _sign aBlock
   ]


instance FromJSON Microblock where
 parseJSON (Object v) = do
     aMsg  <- v .: "msg"
     aSign <- v .: "sign"
     case aMsg of
       Object aBlock -> do
           aWallets <- aBlock .: "wallets"
           aTx      <- aBlock .: "Tx"
           -- aUuid    <- aBlock .: "uuid"
           aKhash   <- decodeFromText =<< aBlock .: "K_hash"
           return $ Microblock aKhash aSign aWallets aTx 0
       a -> mzero
parseJSON _ = mzero


instance ToJSON MacroblockAPI where
    toJSON bl = object  [
            "prev_hash"         .= _prevKBlockAPI bl
         ,  "next_hash"         .= _nextKBlockAPI bl
         ,  "difficulty"        .= _difficultyAPI bl
         ,  "height"            .= _heightAPI bl
         ,  "solver"            .= _solverAPI bl
         ,  "reward"            .= _rewardAPI bl
         ,  "txs_cnt"           .= _txsCntAPI bl
--         ,  "microblocks_cnt"   .= length (_mblocksAPI bl)
         ,  "microblocks"       .= _mblocksAPI bl
       ]

instance FromJSON MacroblockAPI where
    parseJSON (Object o) = MacroblockAPI
               <$> o .: "prev_hash"
               <*> o .: "next_hash"
               <*> o .: "difficulty"
               <*> o .: "height"
               <*> o .: "solver"
               <*> o .: "reward"
               <*> o .: "txs_cnt"
               <*> o .: "microblocks"
    parseJSON inv         = typeMismatch "Macroblock" inv


instance ToJSON ChainInfo where
    toJSON info = object  [
          "emission"   .= _emission info
        , "difficulty" .= _curr_difficulty info
        , "blocks_num" .= _blocks_num info
        , "txs_num"    .= _txs_num info
        , "nodes_num"  .= _nodes_num info
      ]

instance FromJSON ChainInfo where
    parseJSON (Object o) = ChainInfo
               <$> o .: "emission"
               <*> o .: "difficulty"
               <*> o .: "blocks_num"
               <*> o .: "txs_num"
               <*> o .: "nodes_num"
    parseJSON inv        = typeMismatch "ChainInfo" inv
