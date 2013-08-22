import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Monad as M
import qualified Network.TUNTAP as TAP

import qualified Data.ByteString as BS
import qualified Data.Map as M

import Implementation


data SimStateEntry = PQueue 

type SimState = M.Map String SimStateEntry










initialState = M.empty

receivedPacket :: SimState -> Packet



----------------------------------------------------------------

data NetEvent =
    RXEvent BS.ByteString |
    TXEvent BS.ByteString

rxThread c tap = M.forever $ do
    p <- TAP.readTAP tap
    STM.atomically $ TC.writeTChan c (RXEvent p)

txThread c tap = M.forever $ M.join $ STM.atomically $ do
    (TXEvent p) <- TC.readTChan c
    return $ TAP.writeTAP tap p

simStep rxC txC state = do
    e <- TC.readTChan rxC
    let (tx,state') = case e of
        RXEvent p -> receivedPacket state p
    return state'
    

simThread rxC txC state = do
    state' <- STM.atomically $ simStep rxC txC state
    simThread rxC txC state'
    

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
    
