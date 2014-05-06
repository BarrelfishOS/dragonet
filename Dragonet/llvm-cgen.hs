
import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph  as PG
import Dragonet.Unicorn.Parser as UnicornAST
import Dragonet.Unicorn  as Unicorn
import Dragonet.DotGenerator (toDot, toDotWith, pipelinesDot)
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.Pipelines.Applications as APP
import qualified Dragonet.Incremental as INC
import Util.GraphHelpers (findNodeByL,mergeGraphsBy,delLEdges)
import qualified Util.GraphMonad as GM

import Control.Applicative
import Control.Monad
import Control.Concurrent (forkOS,yield)
import qualified Control.Concurrent.STM as STM

--import Control.Applicative

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromJust,isNothing)
import Data.Word (Word32, Word64, Word8, Word)
import Data.Char (ord)
import Data.Function (on)

import qualified Text.Show.Pretty as Pr
import Debug.Trace (trace, traceShow)
import System.Environment (getArgs, getProgName)
import System.IO  (writeFile,hFlush,stdout)
import System.Exit (exitFailure)
import Text.Printf (printf)

import qualified Runner.LLVM as LLVM
import qualified Runner.Dynamic as Dyn
import qualified Runner.E10KControl as E10K
import qualified Runner.E10KPolicy as E10KP

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



-- Simulates a basic embedding (replace rx and tx queue by tap specific node)
pg4tap :: PGraph -> PGraph
pg4tap pg = tagNodes "" $ renameQueues "TapRxQueue" "TapTxQueue" pg

-- Simplistic e10k multi queue embedding
pg4e10k :: Int -> PGraph -> PGraph
pg4e10k nQueues pg =
    appQueuePorts $ untagApp $ foldl1 merge $ map queueGraph queueIDs
    where
        queueIDs = [0..(nQueues - 1)] :: [Int]
        ql i = printf "%03d" i
        queueGraph q = tagNodes ("Queue" ++ ql q) $
            renameQueues ("RxE10kQueue" ++ ql q) ("TxE10kQueue" ++ ql q) pg
        merge = mergeGraphsBy nodesMatch
        nodesMatch a b =  nLabel a == "RxEchoAPP" && nLabel b == "RxEchoAPP"
        untagApp = DGI.nmap fixAppN
        fixAppN n
            | nLabel n == "RxEchoAPP" = n { nTag = "App" }
            | otherwise = n

updateN :: DGI.DynGraph gr => (a -> a) -> DGI.Node -> gr a b -> gr a b
updateN f n g = DGI.gmap change g
    where
        change ctx@(i,n',l,o)
            | n' == n = (i,n',f l,o)
            | otherwise = ctx

appQueuePorts :: PGraph -> PGraph
appQueuePorts pg = DGI.insEdges newE $ updateN addPorts n $ delLEdges oldE pg
    where
        Just (n,_) = findNodeByL (("RxEchoAPP" ==) . nLabel) pg
        tag m = nTag $ fromJust $ DGI.lab pg m
        sucs = L.sortBy (compare `on` (tag . fst)) $ DGI.lsuc pg n
        oldE = map (\(a,b) -> (n,a,b)) sucs
        newE = map (\(a,_) -> (n,a,tag a)) sucs
        ports = map (\(_,_,p) -> p) newE
        addPorts l = l { PG.nPorts = ports }

addTxQueueDemux :: PGraph -> PGraph
addTxQueueDemux pg = snd $ flip GM.runOn pg $ do
        -- Create node
        let dNode = baseFNode "TxQueueDemux" [] queues Nothing
            mNode = baseFNode "RxQueueMux" [] ["out"] Nothing
        dN <- GM.newNode $ dNode { nTag = "App" }
        mN <- GM.newNode $ mNode { nTag = "App" }
        forM sucs $ \(a,l) -> do
            let Just port = nTag <$> DGI.lab pg a
            GM.newEdge (dN,a,port)
            GM.delEdge (n,a,l)
        forM pres $ \(a,l) -> do
            GM.newEdge (a,mN,l)
            GM.delEdge (a,n,l)
        GM.newEdge (n,dN,"out")
        GM.newEdge (mN,n,"out")
    where
        Just (n,_) = findNodeByL (("RxEchoAPP" ==) . nLabel) pg
        sucs = DGI.lsuc pg n
        pres = DGI.lpre pg n
        queues = map (nTag . fromJust . DGI.lab pg . fst) sucs

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

e10kAction :: AppIfState E10KP.E10kPState E10KP.E10kPAction
        -> E10KP.E10kPAction -> IO ()
e10kAction ais (E10KP.E10kPAct5TSet idx ft) = do
    putStrLn $ "E10kPAct5TSet " ++ show idx
    E10K.ftSet (aiStackState ais) (fromIntegral idx) ft
e10kAction ais act = do
    putStrLn $ "Unimplemented e10k policy action: " ++ show act


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


addSocket ais ch sid f = do
    sd <- appRunPolicy ais ch $ INC.policyAddSocket f
    let outQ = fromIntegral $ INC.sdQueue sd
        muxID = 1 -- FIXME
    APP.sendMessage ch $ APP.MsgSocketInfo outQ muxID




initAppInterface ::
        Int -> String -> PLI.StackHandle -> PLI.PipelineImpl -> IO ()
initAppInterface nQueues stackname sh pli = do
    st <- PLI.stackState sh
    sm <- STM.newTVarIO M.empty
    let e10kS = E10KP.e10kPStateInit 128
    let e10kP = E10KP.e10kPolicy
    pstate <- STM.newTVarIO $ INC.policyStateInit nQueues e10kS e10kP
    let ais = AppIfState {
            aiStackState = st,
            aiSocketMap = sm,
            aiPState = pstate,
            aiHWAct = e10kAction,
            aiPLI = pli }
    tid <- forkOS $ APP.interfaceThread stackname (appEvent ais)
    return ()

failUsage :: IO ()
failUsage = do
    putStrLn "Usage: llvm-cgen <unicorn file>"
    exitFailure

main :: IO ()
main = do
    xargs <- getArgs
    if length xargs == 0
        then failUsage
        else return ()
    let fname = xargs !! 0

    pname <- getProgName
    let (helpers,embed,nQueues) = case pname of
            "llvm-cgen" -> ("llvm-helpers",pg4tap,1)
            "llvm-cgen-dpdk" -> ("llvm-helpers-dpdk",pg4tap,1)
            "llvm-cgen-e10k" -> ("llvm-helpers-e10k",pg4e10k 2,2)
            _ -> error "Unknown executable name, don't know what helpers to use :-/"

    txt <- readFile fname
    graph <- UnicornAST.parseGraph txt
    let pgraph = embed $ Unicorn.constructGraph graph
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
    initAppInterface nQueues stackname h appPLI
    commandLineInterface

    putStrLn "Doing cleanup..."
    PLI.stopPipelines h


