import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Monad as M
import qualified Network.TUNTAP as TAP

import qualified Data.ByteString as BS

import Dragonet.Implementation
import Dragonet.Implementation.Algorithm

import LPGImpl



initialState = emptyGS

receivedPacket state packet = execute lpg packet state



----------------------------------------------------------------

data NetEvent =
    RXEvent BS.ByteString |
    TXEvent BS.ByteString

rxThread c tap = M.forever $ do
    p <- TAP.readTAP tap
    STM.atomically $ TC.writeTChan c (RXEvent p)

txThread c tap = M.forever $ do
    (TXEvent p) <- STM.atomically $ TC.readTChan c
    TAP.writeTAP tap p

simStep rxC txC state = do
    e <- TC.readTChan rxC
    return $ case e of
            RXEvent p -> (p,receivedPacket state p)
    

simThread rxC txC state = do
    (p,state') <- STM.atomically $ simStep rxC txC state
    putStrLn ("SimStep")
    putStr $ unlines $ map ("    " ++) $ gsDebug state'
    let state'' = state' { gsDebug = [] }
    simThread rxC txC state''
    

main = do
    tap <- TAP.start
    fd <- TAP.openTAP tap "dragonet0"

    rxC <- TC.newTChanIO
    txC <- TC.newTChanIO

    rxT <- CC.forkIO $ rxThread rxC tap
    txT <- CC.forkIO $ txThread txC tap

    simThread rxC txC initialState

    TAP.closeTAP tap
    TAP.finish tap
       
