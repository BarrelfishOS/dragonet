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
import qualified Dragonet.Unicorn.Parser as UnicornAST
import qualified Dragonet.Unicorn  as Unicorn
import qualified Util.GraphHelpers as GH

import Control.Monad
import Control.Arrow (second)
import Control.Concurrent (forkOS, threadDelay)
import qualified Control.Concurrent.STM as STM


import qualified Data.Map as M
import qualified Data.Bimap as BM
import qualified Data.List as L
import Data.Function (on)
import Data.Maybe

import System.IO  (hFlush,stdout)
import System.IO.Error (catchIOError)
import System.Posix.Signals (raiseSignal,keyboardSignal)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import qualified Runner.LLVM as LLVM
--import qualified Runner.Dynamic as Dyn



-------------------------------------------------------------------------------
-- Initialization


runStack :: Show b => (PG.PGraph -> PG.PGraph) -> INC.PolicyState a b ->
        (AppIfState a b -> b -> IO ()) -> String -> IO ()
runStack embed pstate hwact helpers = do
    let fname = "lpgImpl.unicorn"
    apps <- getArgs
    if null apps
        then do
            putStrLn "Error: No application slots specified on commandline"
            exitSuccess
        else return ()
    putStrLn $ "Running with app slots: " ++ show apps
    -- Parse graph and perform pseudo-embedding
    b <- readFile fname >>= UnicornAST.parseGraph
    let pgraph = embed $ addApps apps $ Unicorn.constructGraph b

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

    let appPLIs = mapMaybe getAppPLI plis
    initAppInterface pstate hwact stackname h appPLIs
    putStrLn "Pipelines are started, need module verification"
    noCommandInterface
    --commandLineInterface

    putStrLn "Doing cleanup..."
    PLI.stopPipelines h
    where
        getAppPLI pli
            | take 3 lbl == "App" = Just (drop 3 lbl,pli)
            | otherwise = Nothing
            where lbl = PL.plLabel $ PLI.pliPipeline pli

-- Wrapper to execute pipelines, but handle application pipelines separatly
runPipeline :: PL.PLGraph -> String -> String -> PLI.PipelineImpl -> IO ()
runPipeline plg stackname helpers pli
    | take 3 lbl == "App" = do
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
    | take 3 lbl == "App" = lbl
    | take 2 lbl == "Tx" || take 5 lbl == "TapTx" = "Tx" ++ tag
    | otherwise = "Rx" ++ tag
    where
        lbl = nLabel n
        tag = nTag n

-- Decide on queue implementation between two pipelines
plConnect :: PL.Pipeline -> PL.Pipeline -> (PLI.POutput,PLI.PInput)
plConnect i o = (PLI.POQueue n, PLI.PIQueue n)
    where n = PL.plLabel i ++ "_to_" ++ PL.plLabel o

noCommandInterface :: IO ()
noCommandInterface = do
    threadDelay $ 1000000 * 60
    putStrLn "1 min sleep is over, sleeping again"
    noCommandInterface

commandLineInterface :: IO ()
commandLineInterface = do
    putStr "> "
    hFlush stdout
    -- Treat EOF in stdin the same way as Ctrl-C
    l <- catchIOError getLine $ \_ -> do
        raiseSignal keyboardSignal
        return ""
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
    aiAppIDs     :: M.Map String APP.AppId,
    aiPLIs       :: M.Map APP.AppId PLI.PipelineImpl,
    aiHWAct      :: AppIfState a b -> b -> IO (),
    aiSocketMap  :: STM.TVar (M.Map (APP.ChanHandle,APP.SocketId) SocketDesc),
    aiAppChans   :: STM.TVar (BM.Bimap APP.AppId APP.ChanHandle),
    aiPState     :: STM.TVar (INC.PolicyState a b)
}

-- Handles incoming messages from control channel
appEvent :: Show b => AppIfState a b -> APP.ChanHandle -> APP.Event -> IO ()
appEvent ais ch (APP.EvAppRegister n) = do
    putStrLn $ "AppRegister: " ++ n
    maID <- case M.lookup n $ aiAppIDs ais of
        Just ai -> do
            acs <- STM.atomically $ STM.readTVar $ aiAppChans ais
            case BM.lookup ai acs of
                Nothing -> return $ Right ai
                Just _ -> return $ Left "Application already bound!"
        Nothing -> return $ Left "Invalid application name!"
    case maID of
        Right ai -> do
            let p = (aiPLIs ais) M.! ai
            -- Add application to channel map
            STM.atomically $ do
                let tv = aiAppChans ais
                acs <- STM.readTVar tv
                STM.writeTVar tv $ BM.insert ai ch acs
            -- Send welcome message specifying number of channels and app id
            APP.sendMessage ch $
                APP.MsgWelcome ai (length $ inq p) (length $ outq p)
            -- Send channel specifications
            mapM_ (APP.sendMessage ch) $ messages p

        Left err -> do
            putStrLn err
            APP.sendMessage ch $ APP.MsgStatus False
    where
        inq p = PLI.pliInQs p
        outq p = L.sortBy (compare `on` fst) $ PLI.pliOutQs p
        messages p =
            (map (\(_,PLI.PIQueue l) -> APP.MsgInQueue l) $ inq p) ++
            (map (\(_,PLI.POQueue l) -> APP.MsgOutQueue l) $ outq p)


appEvent ais ch (APP.EvSocketUDPListen (ip,port)) = do
    putStrLn $ "SocketUDPListen p=" ++ show port
    addSocket ais ch $ INC.UDPIPv4Listen ip port

appEvent ais ch (APP.EvSocketUDPFlow (sIP,sPort) (dIP,dPort)) = do
    putStrLn $ "SocketUDPFlow s=" ++ show (sIP,sPort) ++
        " d=" ++ show (dIP,dPort)
    addSocket ais ch $ INC.UDPIPv4Flow sIP dIP sPort dPort

appEvent _ ch ev = do
    putStrLn $ "appEvent " ++ show ch ++ " " ++ show ev

-- Helper
addSocket ais ch f = do
    sd <- appRunPolicy ais ch $ INC.policyAddSocket f
    let outQ = fromIntegral $ INC.sdQueue sd
        sockID = fromIntegral $ INC.sdID sd
        muxID = 6 -- FIXME
    APP.sendMessage ch $ APP.MsgSocketInfo sockID outQ muxID


initAppInterface :: Show b =>
        INC.PolicyState a b -> (AppIfState a b -> b -> IO ()) ->
            String -> PLI.StackHandle -> [(String,PLI.PipelineImpl)] -> IO ()
initAppInterface pstate hwact stackname sh plis = do
    st <- PLI.stackState sh
    sm <- STM.newTVarIO M.empty
    acs <- STM.newTVarIO BM.empty
    psTV <- STM.newTVarIO $ pstate
    let ais = AppIfState {
            aiStackState = st,
            aiSocketMap = sm,
            aiPState = psTV,
            aiHWAct = hwact,
            aiAppIDs = appIDs,
            aiPLIs = pliMap,
            aiAppChans = acs }
    tid <- forkOS $ APP.interfaceThread stackname (appEvent ais)
    return ()
    where
        apps = map fst plis
        appIDs = M.fromList $ zip apps [1..]
        mapPLI (l,p) = (appIDs M.! l,p)
        pliMap = M.fromList $ map mapPLI plis


--------------------------------------------------------------------------------
-- Integration incremental embedding

policyAction :: Show b =>
    AppIfState a b -> APP.ChanHandle -> INC.PolicyAction b -> IO ()
policyAction ais ch (INC.PActLPGAddSocket sd q) = do
    aid <- STM.atomically $ do
        acm <- STM.readTVar $ aiAppChans ais
        return $ acm BM.!> ch
    case INC.sdFlow sd of
        INC.UDPIPv4Listen 0 port -> do
            lh <- PLI.udpAddListen ss aid sid port
            STM.atomically $ do
                m <- STM.readTVar sm
                STM.writeTVar sm $ M.insert (ch,sid) (SockUDPListen lh) m

        INC.UDPIPv4Flow sIP dIP sP dP -> do
            fh <- PLI.udpAddFlow ss aid sid sIP sP dIP dP
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

-- Add the specified applications to the graph
addApps :: [String] -> PGraph -> PGraph
addApps apps pg = foldl addApp pg apps

addPort :: String -> PG.Node -> PG.Node
addPort p n = n { PG.nPorts = PG.nPorts n ++ [p] }

-- Add a single application to the graph
addApp :: PGraph -> String -> PGraph
addApp pg app =
    DGI.insEdges edges $ GH.updateN (addPort name) pcN $ DGI.insNode (n,l) pg
    where
        name = "App" ++ app
        [n] = DGI.newNodes 1 pg
        l = baseFNode name ["application","noreplicate"] ["out","drop"] Nothing
        Just (pcN,_) = pgFind pg "RxL4UDPPortClassifyDynamic"
        Just (txN,_) = pgFind pg "TxL4UDPInitiateResponse"
        edges = [(pcN,n,name),(n,txN,"out")]


