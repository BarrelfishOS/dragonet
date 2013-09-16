import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Monad as M
import qualified Util.Tap as TAP

import qualified Data.ByteString as BS

import qualified Dragonet.Implementation as DNET
import qualified Dragonet.Implementation.Algorithm as DNET.Alg

import qualified LPGImpl

initialState = DNET.emptyGS

receivedPacket state packet = DNET.Alg.execute LPGImpl.lpg packet state



----------------------------------------------------------------

data NetEvent =
    RXEvent BS.ByteString |
    TXEvent BS.ByteString

rxThread c tap = M.forever $ do
    p <- TAP.readbs tap
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
    

main = do
    -- create and open a TAP device
    tap <- TAP.create "dragonet0"
    TAP.set_ip tap "192.168.123.100"
    TAP.set_mask tap "255.255.255.0"
    TAP.up tap

    -- create rx/tx channels
    rxC <- TC.newTChanIO
    txC <- TC.newTChanIO

    -- spawn rx/tx threads
    _ <- CC.forkIO $ rxThread rxC tap
    _ <- CC.forkIO $ txThread txC tap

    simThread rxC txC initialState
