module Runner.Common (
    AppIfState(..),
    runStack,

    dummyHwAction,
    dummyHwPolicy,

    tagNodes,
    renameQueues
) where

import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph  as PG
import Dragonet.DotGenerator (toDot, pipelinesDot)
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.Pipelines.Applications as APP
import qualified Dragonet.Incremental as INC

import Control.Monad
import Control.Concurrent (forkOS)
import qualified Control.Concurrent.STM as STM


import qualified Data.Map as M
import qualified Data.List as L
import Data.Function (on)

import System.IO  (hFlush,stdout)

import qualified Runner.LLVM as LLVM
--import qualified Runner.Dynamic as Dyn




-------------------------------------------------------------------------------
-- Initialization

runStack :: Show b => PG.PGraph -> INC.PolicyState a b ->
        (AppIfState a b -> b -> IO ()) -> String -> IO ()
runStack pgraph pstate hwact helpers = do
    writeFile "DELETEME.dot" $ toDot pgraph
    let plg = PL.generatePLG plAssign pgraph

    let linkMap pl = "pl_" ++ PL.plLabel pl ++ ".svg"
    writeFile "pipelines.dot" $ pipelinesDot (Just linkMap) plg
    mapM_ (\pl ->
        writeFile ("pl_" ++ PL.plLabel pl ++ ".dot") $ toDot $ PL.plGraph pl
        ) $ map snd $ DGI.labNodes plg

    let stackname = "dragonet"
    let runner pli = do
        runPipeline plg stackname helpers pli
        return pli
    (h,plis) <- PLI.runPipelines stackname plConnect runner plg

    let Just appPLI =
            L.find ((== "AppEcho") . PL.plLabel . PLI.pliPipeline) plis
    initAppInterface pstate hwact stackname h appPLI
    commandLineInterface

    putStrLn "Doing cleanup..."
    PLI.stopPipelines h

-- Wrapper to execute pipelines, but handle application pipelines separatly
runPipeline :: PL.PLGraph -> String -> String -> PLI.PipelineImpl -> IO ()
runPipeline plg stackname helpers pli
    | lbl == "AppEcho" = do
        putStrLn $ "Application Pipeline " ++ lbl
        putStrLn "  Input queues:"
        forM_ (PLI.pliInQs pli) $ \(ql,qc) -> do
            putStrLn $ "    " ++ ql ++ ": " ++ show qc
        putStrLn "  Output queues:"
        forM_ (PLI.pliOutQs pli) $ \(ql,qc) -> do
            putStrLn $ "    " ++ ql ++ ": " ++ show qc
    | lbl == "Tx" = LLVM.runPipeline plg stackname helpers pli
    | otherwise = LLVM.runPipeline plg stackname helpers pli

    where lbl = PL.plLabel $ PLI.pliPipeline pli

-- Assigns nodes to Pipelines
plAssign :: PG.PGNode -> PL.PLabel
plAssign (_,n)
    | lbl == "RxEchoAPP" = "AppEcho"
    | tag == "App" = "AppInterface"
    | take 2 lbl == "Tx" || take 5 lbl == "TapTx" = "Tx" ++ tag
    | otherwise = "Rx" ++ tag
    where
        lbl = nLabel n
        tag = nTag n

-- Decide on queue implementation between two pipelines
plConnect :: PL.Pipeline -> PL.Pipeline -> (PLI.POutput,PLI.PInput)
plConnect i o = (PLI.POQueue n, PLI.PIQueue n)
    where n = PL.plLabel i ++ "_to_" ++ PL.plLabel o

commandLineInterface :: IO ()
commandLineInterface = do
    putStr "> "
    hFlush stdout
    l <- getLine
    putStrLn ""
    done <- case l of
        "quit" -> return True
        "" -> return False
        _ -> do
            putStrLn "Unknown command"
            return False
    if done then return () else commandLineInterface

-------------------------------------------------------------------------------
-- Control Interface

data SocketDesc =
    SockUDPListen PLI.UDPListenHandle |
    SockUDPFlow PLI.UDPFlowHandle

data AppIfState a b = AppIfState {
    aiStackState :: PLI.StateHandle,
    aiSocketMap  :: STM.TVar (M.Map (APP.ChanHandle,APP.SocketId) SocketDesc),
    aiPLI        :: PLI.PipelineImpl,
    aiPState     :: STM.TVar (INC.PolicyState a b),
    aiHWAct      :: AppIfState a b -> b -> IO ()
}

-- Handles incoming messages from control channel
appEvent :: Show b => AppIfState a b -> APP.ChanHandle -> APP.Event -> IO ()
appEvent ais ch APP.EvAppRegister = do
    putStrLn "AppRegister"
    let pli = aiPLI ais
        inq = PLI.pliInQs pli
        outq = L.sortBy (compare `on` fst) $ PLI.pliOutQs pli
        inMsg = map (\(_,PLI.PIQueue l) -> APP.MsgInQueue l) inq
        outMsg = map (\(_,PLI.POQueue l) -> APP.MsgOutQueue l) outq

    APP.sendMessage ch $ APP.MsgWelcome 42 (length inq) (length outq)
    mapM_ (APP.sendMessage ch) $ inMsg ++ outMsg

appEvent ais ch (APP.EvSocketUDPListen sid (ip,port)) = do
    putStrLn $ "SocketUDPListen s=" ++ show sid ++ " p=" ++ show port
    addSocket ais ch sid $ INC.UDPIPv4Listen ip port
{-    lh <- PLI.udpAddListen (aiStackState ais) sid port
    let sm = aiSocketMap ais
    STM.atomically $ do
        m <- STM.readTVar sm
        STM.writeTVar sm $ M.insert (ch,sid) (SockUDPListen lh) m
    let muxID = 1 -- FIXME
        outQ  = fromIntegral (port `mod` 2)
        inQ = outQ
    if inQ /= 0
        then do
            let ftfTV = aiNext5t ais
            id <- STM.atomically $ do
                v <- STM.readTVar ftfTV
                STM.writeTVar ftfTV (v + 1)
                return v
            E10K.ftSet (aiStackState ais) id $ E10K.FTuple 1 inQ
                (Just E10K.L3IPv4) (Just E10K.L4UDP)
                Nothing Nothing Nothing (Just $ fromIntegral port)
        else return ()
    APP.sendMessage ch $ APP.MsgSocketInfo outQ muxID
    return ()-}

appEvent ais ch (APP.EvSocketUDPFlow sid (sIP,sPort) (dIP,dPort)) = do
    putStrLn $ "SocketUDPFlow s=" ++ show sid ++ " s=" ++
        show (sIP,sPort) ++ " d=" ++ show (dIP,dPort)
    addSocket ais ch sid $ INC.UDPIPv4Flow sIP dIP sPort dPort

appEvent _ ch ev = do
    putStrLn $ "appEvent " ++ show ch ++ " " ++ show ev

-- Helper
addSocket ais ch sid f = do
    sd <- appRunPolicy ais ch $ INC.policyAddSocket f
    let outQ = fromIntegral $ INC.sdQueue sd
        muxID = 1 -- FIXME
    APP.sendMessage ch $ APP.MsgSocketInfo outQ muxID


initAppInterface :: Show b =>
        INC.PolicyState a b -> (AppIfState a b -> b -> IO ()) ->
            String -> PLI.StackHandle -> PLI.PipelineImpl -> IO ()
initAppInterface pstate hwact stackname sh pli = do
    st <- PLI.stackState sh
    sm <- STM.newTVarIO M.empty
    psTV <- STM.newTVarIO $ pstate
    let ais = AppIfState {
            aiStackState = st,
            aiSocketMap = sm,
            aiPState = psTV,
            aiHWAct = hwact,
            aiPLI = pli }
    tid <- forkOS $ APP.interfaceThread stackname (appEvent ais)
    return ()


--------------------------------------------------------------------------------
-- Integration incremental embedding

policyAction :: Show b =>
    AppIfState a b -> APP.ChanHandle -> INC.PolicyAction b -> IO ()
policyAction ais ch (INC.PActLPGAddSocket sd q) =
    case INC.sdFlow sd of
        INC.UDPIPv4Listen 0 port -> do
            lh <- PLI.udpAddListen ss sid port
            STM.atomically $ do
                m <- STM.readTVar sm
                STM.writeTVar sm $ M.insert (ch,sid) (SockUDPListen lh) m

        INC.UDPIPv4Flow sIP dIP sP dP -> do
            fh <- PLI.udpAddFlow ss sid sIP sP dIP dP
            STM.atomically $ do
                m <- STM.readTVar sm
                STM.writeTVar sm $ M.insert (ch,sid) (SockUDPFlow fh) m

    where
        sid = fromIntegral $ INC.sdID sd
        ss = aiStackState ais
        sm = aiSocketMap ais

policyAction ais ch (INC.PActHWAction hw) = aiHWAct ais ais hw
policyAction ais ch act = do
    putStrLn $ "Unimplemented policy action: " ++ show act

appRunPolicy :: Show b =>
    AppIfState a b -> APP.ChanHandle -> INC.PolicyM a b c -> IO c
appRunPolicy ais ch p = do
    (acts,ret) <- STM.atomically $ do
        st <- STM.readTVar $ aiPState ais
        let (st',ac,r) = INC.policyMRun st p
        flip STM.writeTVar st' $ aiPState ais
        return (ac,r)
    mapM_ (policyAction ais ch) acts
    return ret


--------------------------------------------------------------------------------
-- Dummy PRG Policy part for TAP etc.

-- Translate action from inc embedding policy to hardware interaction
dummyHwAction :: AppIfState () () -> () -> IO ()
dummyHwAction _ _ = error "No hardware actions should be emited by policy"

-- Dummy HwPolicy that does nothing with the hardware and only has a single
-- queue
dummyHwPolicy :: INC.Policy () ()
dummyHwPolicy = INC.Policy {
        INC.pPRGAddSocket = dummyAddSocket,
        INC.pPRGRemoveSocket = dummyRemoveSocket,
        INC.pPRGRebalance = dummyRebalance
    }

dummyAddSocket :: INC.SocketDesc -> INC.QueueID -> INC.PolicyM () () INC.QueueID
dummyAddSocket _ 0 = return 0
dummyAddSocket _ _ = error "Invalid queue id for dummy policy"

dummyRemoveSocket :: INC.SocketDesc -> INC.PolicyM () () ()
dummyRemoveSocket _ = return ()

dummyRebalance :: INC.QueueID -> INC.QueueID -> INC.PolicyM () () Bool
dummyRebalance _ _ = return False


--------------------------------------------------------------------------------
-- Graph helpers
tagNodes :: String -> PGraph -> PGraph
tagNodes tag = DGI.nmap (\n -> n { nTag = tag })

renameQueues :: String -> String -> PGraph -> PGraph
renameQueues rx tx pg = DGI.nmap fixN pg
    where
        fixN n
            | l == "Queue" = n { nLabel = rx }
            | l == "TxQueue" = n { nLabel = tx }
            | otherwise = n
            where l = nLabel n




