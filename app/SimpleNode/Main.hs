{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import qualified Control.Concurrent                    as C
import           Control.Monad
import qualified Data.Map                              as M
import           Service.System.Version
import           System.Environment                    (getEnv)

import           Control.Concurrent.Chan.Unagi.Bounded

import           CLI.CLI
import           CLI.RPC
import           Control.Exception                     (SomeException, try)
import           Data.IP
import           Data.Maybe                            (fromJust)
import           Node.Lib
import           Node.NetLvl.Router
import           Node.Node.Types
import           Service.InfoMsg
import           Service.Network.Base
import           Service.Timer
import           Data.Aeson                            (decode)
import qualified Data.ByteString.Lazy                  as L
import           Node.DataActor
import           Service.Transaction.Common            (connectOrRecoveryConnect)

configName :: String
configName = "configs/config.json"


main :: IO ()
main =  do
        putStrLn $ "Version: " ++ $(version)
        enc <- L.readFile configName
        case decode enc :: Maybe BuildConfig of
          Nothing   -> error "Please, specify config file correctly"
          Just conf -> do

            (aInfoChanIn, aInfoChanOut) <- newChan 64
            (aInContainerChan, aOut) <- newChan 64
            void $ C.forkIO $ startContainerActor M.empty aOut
            rocksDB   <- connectOrRecoveryConnect

            void $ startNode rocksDB conf aInfoChanIn routerActorStart $
                \(ch, _) _ _ aMyNodeId _ -> do
                    metronomeS 400000 (void $ tryWriteChan ch CleanAction)

                    (snbc, _, stat_h, stat_p, logs_h, logs_p, log_id) <- getConfigParameters aMyNodeId conf ch

                    cli_m   <- try (getEnv "cliMode") >>= \case
                            Right item              -> return item
                            Left (_::SomeException) -> return $ cliMode snbc

                    void $ C.forkIO $ serveInfoMsg (ConnectInfo stat_h stat_p) (ConnectInfo logs_h logs_p) aInfoChanOut log_id (cli_m /= "cli")

                    void $ C.forkIO $ case cli_m of
                      "rpc" -> do
                            rpcbc <- try (pure $ fromJust $ rpcBuildConfig snbc) >>= \case
                                       Right item              -> return item
                                       Left (_::SomeException) -> error "Please, specify RPCBuildConfig"

                            rpc_p <- try (getEnv "rpcPort") >>= \case
                                  Right item              -> return $ read item
                                  Left (_::SomeException) -> return $ rpcPort rpcbc

                            ip_en <- join $ enableIPsList <$> (try (getEnv "enableIP") >>= \case
                                  Right item              -> return $ read item
                                  Left (_::SomeException) -> return $ enableIP rpcbc)

                            serveRpc rocksDB rpc_p ip_en ch aInfoChanIn aInContainerChan
                      "cli" -> serveCLI rocksDB ch aInfoChanIn aInContainerChan
                      _     -> return ()
            forever $ C.threadDelay 10000000000


enableIPsList :: [String] -> IO [AddrRange IPv6]
enableIPsList []  = return [ read "::/0" ]
enableIPsList ips = sequence $ map (\ip_s -> try (readIO ip_s :: IO IPRange) >>= \case
                            Right (IPv4Range r) -> if r == read "0.0.0.0"
                                                   then return $ read "::/0"
                                                   else return $ ipv4RangeToIPv6 r
                            Right (IPv6Range r) -> if r == read "::"
                                                   then return $ read "::/0"
                                                   else return r
                            Left (_ :: SomeException) -> error $ "Wrong IP format"
                            )
                               ips
