{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Monad as M
import qualified Util.Tap as TAP
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

import Numeric (showHex)

--import qualified LPGEx1 as LPG1
import qualified LPGicmp as LPG1
--import qualified LPGicmp as LPGicmp

--import qualified Text.Show.Pretty as Pr

initialState = st'
    where
    st = DNET.emptyGS
    mac = fromJust $ ETH.macFromString "00:1b:22:54:69:f8"
    ip = fromJust $ IP4.ipFromString "192.168.123.1"
    st' = setLocalMACandIP st mac ip

--receivedPacket state packet = DNET.Alg.execute LPGImpl.lpg packet state
receivedPacket state packet = fst $ CS.runConcSM f $ DNET.initSimState state packet
    where f = $(return $ generateFCall LPG1.lpg "lpg")


funCallList = f
    where f = generateFCallList LPG1.lpg "lpg"

----------------------------------------------------------------


prettyPrint :: BS.ByteString -> String
prettyPrint = concat . map (flip showHex ",0x") . BS.unpack

data NetEvent =
    RXEvent BS.ByteString |
    TXEvent BS.ByteString

rxThread c tap done = do
    M.forever $ do
        p <- TAP.readbs tap
--        putStrLn ("received Packet: [0x"  ++ (prettyPrint p) ++ "]")
        STM.atomically $ TC.writeTChan c (RXEvent p)
    CC.putMVar done ()

txThread c tap done = do
    M.forever $ do
        (TXEvent p) <- STM.atomically $ TC.readTChan c
--        putStrLn ("Send Packet: [0x"  ++ (prettyPrint p) ++ "]")
        TAP.writebs tap p
    CC.putMVar done ()

simStep rxC txC state = do
    e <- TC.readTChan rxC
    return $ case e of
            RXEvent p -> (p,receivedPacket state p)


simThread rxC txC state = do
    (p,state') <- STM.atomically $ simStep rxC txC state

    -- Show Debug output
--    putStrLn "SimStepDNET.Alg."
--    if not $ null $ DNET.gsDebug state' then
--        putStr $ unlines $ map ("    " ++) $ DNET.gsDebug state'
--    else return ()

    -- Send out packets on TX queue
    let send p = STM.atomically $ TC.writeTChan txC (TXEvent p)
    mapM_ send $ DNET.gsTXQueue state'

    let state'' = state' { DNET.gsDebug = [], DNET.gsTXQueue = [] }
    simThread rxC txC state''


debugshow = do
    putStrLn $ show funCallList

runSimIncremental = do

    done <- CC.newEmptyMVar

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
    rxtid <- CC.forkIO $ rxThread rxC tap done
    txtid <- CC.forkIO $ txThread txC tap done

    simtid <- CC.forkIO $ simThread rxC txC initialState

    CC.takeMVar done  -- blocks till MVar is full

--    l <- getLine
--    print l

main = do
    --debugshow
    runSimIncremental



