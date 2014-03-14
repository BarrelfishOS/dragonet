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
import qualified Dragonet.Implementation.Ethernet as ETH
import qualified Dragonet.Implementation.IPv4 as IP4
import Data.Maybe

import Dragonet.Implementation as DNET
--import qualified Dragonet.Implementation.Algorithm as DNET.Alg

import LPGImplTH
import LPGImpl
import qualified LPGEx1 as LPG1

initialState = st'
    where
    st = DNET.emptyGS
    mac = fromJust $ ETH.macFromString "00:0f:53:07:48:d5"
    ip = fromJust $ IP4.ipFromString "10.111.4.37"
    st' = setLocalMACandIP st mac ip

--receivedPacket state packet = DNET.Alg.execute LPGImpl.lpg packet state
receivedPacket state packet = fst $ CS.runConcSM f $ DNET.initSimState state packet
    where f = $(return $ generateFCall LPG1.lpg "lpg")

----------------------------------------------------------------

data NetEvent =
    RXEvent BS.ByteString |
    TXEvent BS.ByteString

rxThread c dpdk qid = M.forever $ do
--    p <- Dpdk.getPacket
    p <- Dpdk.getPacket_v2 0 0 qid
    putStrLn ("received Packet on qid " ++ (show qid))
    STM.atomically $ TC.writeTChan c (RXEvent p)

txThread c dpdk qid = M.forever $ do
    (TXEvent p) <- STM.atomically $ TC.readTChan c
    putStrLn ("Send Packet on qid " ++ (show qid))
--    Dpdk.sendPacket p
    Dpdk.sendPacket_v2 0 0 qid p

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
    --dpdk1 <- Dpdk.init_dpdk_setup "dragonet01"
    dpdk1 <- Dpdk.init_dpdk_setup_v2

    -- Insert the filter to separate two flows
    Dpdk.e10k5TAdd 1 "0.0.0.0" 0 "0.0.0.0" 51098 0

    -- Initialize dpdk device on linux side
    uid <- SPU.getRealUserID
    if uid == 0 then do
        putStrLn ("Dpdk device created, everything should be fine ")
    else do
        putStrLn ("Warning: You are a non-root user. So DPDK may not work!")

    -- create rx/tx channels
    rxC0 <- TC.newTChanIO
    txC0 <- TC.newTChanIO

    -- create rx/tx channels for queue 1
    rxC1 <- TC.newTChanIO
    txC1 <- TC.newTChanIO

    -- spawn rx/tx threads to handle queue 0
    _ <- CC.forkIO $ rxThread rxC0 dpdk1 0
    _ <- CC.forkIO $ txThread txC0 dpdk1 0

    -- spawn rx/tx threads to handle queue 0
    _ <- CC.forkIO $ rxThread rxC1 dpdk1 1
    _ <- CC.forkIO $ txThread txC1 dpdk1 1

    _ <- CC.forkIO $ simThread rxC0 txC0 initialState
    _ <- CC.forkIO $ simThread rxC1 txC1 initialState
    l <- getLine
    print l
