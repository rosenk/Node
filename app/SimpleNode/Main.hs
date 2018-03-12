module Main where

import              Control.Monad
import              Control.Concurrent
import              Node.Node.Mining
import              Node.Node.Types
import              Service.Timer
import              Node.Lib
import              Service.Metrics
import              PoA
import              CLI.CLI (control)

main :: IO ()
main = do
    aExitChan <- newChan
    aAnswerChan  <- newChan
    metric $ increment "cl.node.count"
    void $ startNode "./data/miningInitData.bin"
        aExitChan aAnswerChan managerMining $ \ch aChan aMyNodeId -> do
            -- периодически проверяем, в каком состоянии относительно сети
            -- мы находимся
            metronomeS 400000 (writeChan ch connectivityQuery)
            metronomeS 1000000 (writeChan ch deleteOldestMsg)
            metronomeS 10000000 (writeChan ch deleteDeadSouls)
            metronomeS 3000000 $ writeChan ch deleteOldestVacantPositions
            idmain <- myThreadId
            print idmain
            idpoa <- forkIO $ servePoA  "1554" aMyNodeId ch aChan "1556"
            print idpoa
            idcli <- forkIO $ control ch
            print idcli
    void $ readChan aExitChan
