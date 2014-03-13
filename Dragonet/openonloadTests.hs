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
import qualified Dragonet.Implementation.IPv4 as IP4
--import qualified Dragonet.Implementation.Algorithm as DNET.Alg

import LPGImplTH
import LPGImpl
import qualified LPGEx1 as LPG1
import qualified LPGImpl.LPGImplBase as LPGBase

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
--        putStrLn ("received Packet on qid " ++ (show qid))
        STM.atomically $ TC.writeTChan c (RXEvent p)

txThread c sfvi qid done = do
    M.forever $ do
        (TXEvent p) <- STM.atomically $ TC.readTChan c
--        putStrLn ("Send Packet on qid " ++ (show qid))
        SF.sendPacket sfvi p
    CC.putMVar done ()

simStep rxC txC state = do
    e <- TC.readTChan rxC
    return $ case e of
            RXEvent p -> (p,receivedPacket state p)


simThread rxC txC state = do
    (p,state') <- STM.atomically $ simStep rxC txC state

    -- Show Debug output
{-    putStrLn "SimStepDNET.Alg."
    if not $ null $ DNET.gsDebug state' then
        putStr $ unlines $ map ("    " ++) $ DNET.gsDebug state'
    else return ()
-}
    -- Send out packets on TX queue
    let send p = STM.atomically $ TC.writeTChan txC (TXEvent p)
    mapM_ send $ DNET.gsTXQueue state'

    let state'' = state' { DNET.gsDebug = [], DNET.gsTXQueue = [] }
    simThread rxC txC state''



runSimIncremental = do

    done0 <- CC.newEmptyMVar
    done1 <- CC.newEmptyMVar

    -- verify that you are a root
    uid <- SPU.getRealUserID
    if uid == 0 then do
        putStrLn ("SF device created, everything should be fine ")
    else do
        putStrLn ("Warning: You are a non-root user. So SF openonload may not work!")

    -- open a SF device eth7
    sf_if <- SF.init_openonload_setup "eth7"

    -- Allocate a queue and set it to receive all the traffic
    sfvi0 <- SF.alloc_queue sf_if
    SF.alloc_filter_default sfvi0

    -- Allocate another queue and set it to receive specific traffic
    sfvi1 <- SF.alloc_queue sf_if
    --SF.alloc_filter_default sfvi1
--    let Just localip =  LPGBase.cfgLocalIP
    SF.alloc_filter_listen_ipv4 sfvi0 (fromIntegral IP4.protocolTCP) LPGBase.cfgLocalIP 1234
    SF.alloc_filter_listen_ipv4 sfvi1 (fromIntegral IP4.protocolTCP) LPGBase.cfgLocalIP 2234
    --SF.alloc_filter_listen_ipv4 sfvi1 (fromIntegral IP4.protocolTCP) 0 0


    -- create rx/tx channels
    rxC0 <- TC.newTChanIO
    txC0 <- TC.newTChanIO

    -- create rx/tx channels for queue 1
    rxC1 <- TC.newTChanIO
    txC1 <- TC.newTChanIO


    -- spawn rx/tx threads to handle queue 0
    rxtid0 <- CC.forkIO $ rxThread rxC0 sfvi0 0 done0
    txtid0 <- CC.forkIO $ txThread txC0 sfvi0 0 done0

    -- spawn rx/tx threads to handle queue 1
    rxtid1 <- CC.forkIO $ rxThread rxC1 sfvi1 1 done1
    txtid1 <- CC.forkIO $ txThread txC1 sfvi1 1 done1


    -- Opening new connection
    let initialState0 = addPortMappingTCPGS initialState 1234 "DummyAppTCP0"
                            DNET.ListenSocket DNET.TCPListen
                            lpgRxL4TCPSocketClassifyImpl

    -- Opening new connection
    let initialState1 = addPortMappingTCPGS initialState 2234 "DummyAppTCP1"
                            DNET.ListenSocket DNET.TCPListen
                            lpgRxL4TCPSocketClassifyImpl

    simtid0 <- CC.forkIO $ simThread rxC0 txC0 initialState0
    simtid1 <- CC.forkIO $ simThread rxC1 txC1 initialState1

    CC.takeMVar done0  -- blocks till MVar is full
    CC.takeMVar done1  -- blocks till MVar is full

--    l <- getLine
--    print l

main = do
    --debugshow
    runSimIncremental

