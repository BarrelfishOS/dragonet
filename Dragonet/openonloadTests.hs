{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Monad as M
--import qualified Util.Tap as TAP
--import qualified Util.Dpdk as Dpdk
import qualified Util.Openonload as SF
import qualified Util.ConcState as CS
import qualified System.Posix.User as SPU

import qualified Data.ByteString as BS

import Dragonet.Implementation as DNET
--import qualified Dragonet.Implementation.Algorithm as DNET.Alg

import LPGImplTH
import LPGImpl
import qualified LPGEx1 as LPG1

initialState = DNET.emptyGS

--receivedPacket state packet = DNET.Alg.execute LPGImpl.lpg packet state
receivedPacket state packet = fst $ CS.runConcSM f $ DNET.initSimState state packet
    where f = $(return $ generateFCall LPG1.lpg "lpg")



----------------------------------------------------------------

data NetEvent =
    RXEvent BS.ByteString |
    TXEvent BS.ByteString

rxThread c sfvi qid done = do
    M.forever $ do
        p <- SF.getPacket sfvi
        putStrLn ("received Packet on qid " ++ (show qid))
        STM.atomically $ TC.writeTChan c (RXEvent p)

txThread c sfvi qid done = do
    M.forever $ do
        (TXEvent p) <- STM.atomically $ TC.readTChan c
        putStrLn ("Send Packet on qid " ++ (show qid))
        SF.sendPacket sfvi p
    CC.putMVar done ()

simStep rxC txC state = do
    e <- TC.readTChan rxC
    return $ case e of
            RXEvent p -> (p,receivedPacket state p)


simThread rxC txC state = do
    (p,state') <- STM.atomically $ simStep rxC txC state

    -- Show Debug output
    putStrLn "SimStepDNET.Alg."
    if not $ null $ DNET.gsDebug state' then
        putStr $ unlines $ map ("    " ++) $ DNET.gsDebug state'
    else return ()

    -- Send out packets on TX queue
    let send p = STM.atomically $ TC.writeTChan txC (TXEvent p)
    mapM_ send $ DNET.gsTXQueue state'

    let state'' = state' { DNET.gsDebug = [], DNET.gsTXQueue = [] }
    simThread rxC txC state''



runSimIncremental = do

    done <- CC.newEmptyMVar

    -- create and open a SF device
    sfvi <- SF.init_openonload_setup "eth7"

    -- Initialize dpdk device on linux side
    uid <- SPU.getRealUserID
    if uid == 0 then do
        putStrLn ("SF device created, everything should be fine ")
    else do
        putStrLn ("Warning: You are a non-root user. So SF openonload may not work!")

    -- create rx/tx channels
    rxC <- TC.newTChanIO
    txC <- TC.newTChanIO

    -- spawn rx/tx threads to handle queue 0
    rxtid <- CC.forkIO $ rxThread rxC sfvi 0 done
    txtid <- CC.forkIO $ txThread txC sfvi 0 done

    -- FIXME: somewhere here, I need to add calls to create new connections
    -- Opening new connection
    let initialState' = addPortMappingTCPGS initialState 1234 "DummyAppTCP"
                            DNET.ListenSocket DNET.TCPListen
                            lpgRxL4TCPSocketClassifyImpl
--                            DNET.ListenSocket lpgRxTCPdynamicPortUsed
    simtid <- CC.forkIO $ simThread rxC txC initialState'

    CC.takeMVar done  -- blocks till MVar is full

--    l <- getLine
--    print l

main = do
    --debugshow
    runSimIncremental

