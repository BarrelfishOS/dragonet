{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Monad as M
--import qualified Util.Tap as TAP
--import qualified Util.Dpdk as TAP
import qualified Util.Dpdk as Dpdk
import qualified Util.ConcState as CS
import qualified System.Posix.User as SPU

import qualified Data.ByteString as BS

import Dragonet.Implementation as DNET
--import qualified Dragonet.Implementation.Algorithm as DNET.Alg

import LPGImplTH
import LPGImpl

initialState = DNET.emptyGS

--receivedPacket state packet = DNET.Alg.execute LPGImpl.lpg packet state
receivedPacket state packet = fst $ CS.runConcSM f $ DNET.initSimState state packet
    where f = $(return $ generateFCall lpg "lpg")



----------------------------------------------------------------

data NetEvent =
    RXEvent BS.ByteString |
    TXEvent BS.ByteString

rxThread c dpdk = M.forever $ do
    p <- Dpdk.getPacket dpdk
    putStrLn "received Packet"
    STM.atomically $ TC.writeTChan c (RXEvent p)

txThread c dpdk = M.forever $ do
    (TXEvent p) <- STM.atomically $ TC.readTChan c
    putStrLn "Send Packet"
    Dpdk.sendPacket dpdk p

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


main = do
    -- create and open a DPDK device
    dpdk1 <- Dpdk.init_dpdk_setup "dragonet01"

    -- Initialize dpdk device on linux side
    uid <- SPU.getRealUserID
    if uid == 0 then do
        putStrLn ("Dpdk device created, everything should be fine ")
    else do
        putStrLn ("Warning: You are a non-root user. So DPDK may not work!")

    -- create rx/tx channels
    rxC <- TC.newTChanIO
    txC <- TC.newTChanIO

    -- spawn rx/tx threads
    _ <- CC.forkIO $ rxThread rxC dpdk1
    _ <- CC.forkIO $ txThread txC dpdk1

    _ <- CC.forkIO $ simThread rxC txC initialState
    l <- getLine
    print l
