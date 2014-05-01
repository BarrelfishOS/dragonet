
import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph  as PG
import Dragonet.Unicorn.Parser as UnicornAST
import Dragonet.Unicorn  as Unicorn
import Dragonet.DotGenerator (toDot, toDotWith, pipelinesDot)
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.Pipelines.Applications as APP
import Util.GraphHelpers (findNodeByL,mergeGraphsBy)
import qualified Util.GraphMonad as GM

import Control.Applicative
import Control.Monad
import Control.Concurrent (forkOS,yield)
import qualified Control.Concurrent.STM as STM

--import Control.Applicative

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromJust,isNothing)
import Data.Word (Word32, Word64, Word)
import Data.Char (ord)
import Data.Function (on)

import qualified Text.Show.Pretty as Pr
import Debug.Trace (trace, traceShow)
import System.Environment (getArgs, getProgName)
import System.IO  (writeFile,hFlush,stdout)
import System.Exit (exitFailure)

import qualified Runner.LLVM as LLVM
import qualified Runner.Dynamic as Dyn

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
pg4e10k :: PGraph -> PGraph
pg4e10k pg =
    addTxQueueDemux $ untagApp $ foldl1 merge $ map queueGraph [0..(nQueues - 1)]
    where
        nQueues = 2
        queueGraph q = tagNodes ("Queue" ++ show q) $
            renameQueues ("RxE10kQueue" ++ show q) ("TxE10kQueue" ++ show q) pg
        merge = mergeGraphsBy nodesMatch
        nodesMatch a b =  nLabel a == "RxEchoAPP" && nLabel b == "RxEchoAPP"
        untagApp = DGI.nmap fixAppN
        fixAppN n
            | nLabel n == "RxEchoAPP" = n { nTag = "App" }
            | otherwise = n

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

data AppIfState = AppIfState {
    aiStackState :: PLI.StateHandle,
    aiSocketMap  :: STM.TVar (M.Map (APP.ChanHandle,APP.SocketId) SocketDesc),
    aiPLI        :: PLI.PipelineImpl
}

appEvent :: AppIfState -> APP.ChanHandle -> APP.Event -> IO ()
appEvent ais ch APP.EvAppRegister = do
    putStrLn "AppRegister"
    let pli = aiPLI ais
        inq = PLI.pliInQs pli
        outq = PLI.pliOutQs pli
        inMsg = map (\(_,PLI.PIQueue l) -> APP.MsgInQueue l) inq
        outMsg = map (\(_,PLI.POQueue l) -> APP.MsgOutQueue l) outq

    APP.sendMessage ch $ APP.MsgWelcome 42 (length inq) (length outq)
    mapM_ (APP.sendMessage ch) $ inMsg ++ outMsg

appEvent ais ch (APP.EvSocketUDPListen sid (0,port)) = do
    putStrLn $ "SocketUDPListen s=" ++ show sid ++ " p=" ++ show port
    lh <- PLI.udpAddListen (aiStackState ais) sid port
    let sm = aiSocketMap ais
    STM.atomically $ do
        m <- STM.readTVar sm
        STM.writeTVar sm $ M.insert (ch,sid) (SockUDPListen lh) m
    let muxID = 1 -- FIXME
        outQ  = 0
    APP.sendMessage ch $ APP.MsgSocketInfo outQ muxID
    return ()

appEvent ais ch (APP.EvSocketUDPFlow sid (sIP,sPort) (dIP,dPort)) = do
    putStrLn $ "SocketUDPFlow s=" ++ show sid ++ " s=" ++
        show (sIP,sPort) ++ " d=" ++ show (dIP,dPort)
    fh <- PLI.udpAddFlow (aiStackState ais) sid sIP sPort dIP dPort
    let sm = aiSocketMap ais
    STM.atomically $ do
        m <- STM.readTVar sm
        STM.writeTVar sm $ M.insert (ch,sid) (SockUDPFlow fh) m
    APP.sendMessage ch $ APP.MsgStatus True
    return ()

appEvent _ ch ev = do
    putStrLn $ "appEvent " ++ show ch ++ " " ++ show ev


initAppInterface :: String -> PLI.StackHandle -> PLI.PipelineImpl -> IO ()
initAppInterface stackname sh pli = do
    st <- PLI.stackState sh
    sm <- STM.newTVarIO $ M.empty
    let ais = AppIfState {
            aiStackState = st,
            aiSocketMap = sm,
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
    let (helpers,embed) = case pname of
            "llvm-cgen" -> ("llvm-helpers",pg4tap)
            "llvm-cgen-dpdk" -> ("llvm-helpers-dpdk",pg4tap)
            "llvm-cgen-e10k" -> ("llvm-helpers-e10k",pg4e10k)
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
    initAppInterface stackname h appPLI
    commandLineInterface

    putStrLn "Doing cleanup..."
    PLI.stopPipelines h


