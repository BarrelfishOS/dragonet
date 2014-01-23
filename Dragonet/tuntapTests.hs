{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Monad as M
import qualified Util.Tap as TAP
import qualified Util.ConcState as CS
import qualified System.Posix.User as SPU

import qualified Data.ByteString as BS

import Dragonet.Implementation as DNET
--import qualified Dragonet.Implementation.Algorithm as DNET.Alg

import LPGImplTH
import LPGImpl
import qualified LPGEx1 as LPG1


--import qualified Text.Show.Pretty as Pr

initialState = DNET.emptyGS

--receivedPacket state packet = DNET.Alg.execute LPGImpl.lpg packet state
receivedPacket state packet = fst $ CS.runConcSM f $ DNET.initSimState state packet
    where f = $(return $ generateFCall LPG1.lpg "lpg")


funCallList = f
    where f = generateFCallList LPG1.lpg "lpg"

----------------------------------------------------------------

data NetEvent =
    RXEvent BS.ByteString |
    TXEvent BS.ByteString

rxThread c tap = M.forever $ do
    p <- TAP.readbs tap
    putStrLn "received Packet"
    STM.atomically $ TC.writeTChan c (RXEvent p)

txThread c tap = M.forever $ do
    (TXEvent p) <- STM.atomically $ TC.readTChan c
    putStrLn "Send Packet"
    TAP.writebs tap p

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

debugshow = do
    putStrLn $ show funCallList

runSim = do
    -- create and open a TAP device
    tap <- TAP.create "dragonet0"

    -- Initialize tap device on linux side
    uid <- SPU.getRealUserID
    if uid == 0 then do
        TAP.set_ip tap "192.168.123.100"
        TAP.set_mask tap "255.255.255.0"
        TAP.up tap
    else do
        putStrLn ("Warning: Cannot configure Linux-side of TAP device as " ++
                  "non-root user.")

    -- create rx/tx channels
    rxC <- TC.newTChanIO
    txC <- TC.newTChanIO

    -- spawn rx/tx threads
    _ <- CC.forkIO $ rxThread rxC tap
    _ <- CC.forkIO $ txThread txC tap

    _ <- CC.forkIO $ simThread rxC txC initialState
    l <- getLine
    print l

runSimIncremental = do
    -- create and open a TAP device
    tap <- TAP.create "dragonet0"

    -- Initialize tap device on linux side
    uid <- SPU.getRealUserID
    if uid == 0 then do
        TAP.set_ip tap "192.168.123.100"
        TAP.set_mask tap "255.255.255.0"
        TAP.up tap
    else do
        putStrLn ("Warning: Cannot configure Linux-side of TAP device as " ++
                  "non-root user.")

    -- create rx/tx channels
    rxC <- TC.newTChanIO
    txC <- TC.newTChanIO

    -- spawn rx/tx threads
    _ <- CC.forkIO $ rxThread rxC tap
    _ <- CC.forkIO $ txThread txC tap

    -- FIXME: somewhere here, I need to add calls to create new connections
    --
    _ <- CC.forkIO $ simThread rxC txC initialState
    l <- getLine
    print l


main = do
    --debugshow
    runSimIncremental
    -- runSim

