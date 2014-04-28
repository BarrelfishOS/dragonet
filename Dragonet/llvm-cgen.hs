
import qualified Data.Graph.Inductive as DGI

import Dragonet.ProtocolGraph  as PG
import Dragonet.Unicorn.Parser as UnicornAST
import Dragonet.Unicorn  as Unicorn
import Dragonet.DotGenerator (toDot, toDotWith, pipelinesDot)
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.Pipelines.Applications as APP
import Util.GraphHelpers (findNodeByL)

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

import qualified Runner.LLVM as LLVM
import qualified Runner.Dynamic as Dyn



-- Simulates a basic embedding (replace rx and tx queue by tap specific node)
pg4tap :: PGraph -> PGraph
pg4tap pg = DGI.nmap fixN pg
    where
        fixN n
            | l == "Queue" = n { nLabel = "TapRxQueue" }
            | l == "TxQueue" = n { nLabel = "TapTxQueue" }
            | otherwise = n
            where l = nLabel n

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
    | lbl == "RxEchoAPP" = "AppEcho"
    | take 2 lbl == "Tx" || take 5 lbl == "TapTx" = "Tx"
    | otherwise = "Rx"
    where
        lbl = nLabel n

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

appEvent ::
    PLI.StateHandle ->
    STM.TVar (M.Map (APP.ChanHandle,APP.SocketId) SocketDesc) ->
     APP.ChanHandle -> APP.Event -> IO ()
appEvent ss _ ch APP.EvAppRegister = do
    putStrLn "AppRegister"
    APP.sendMessage ch $ APP.MsgWelcome 42
appEvent ss sm ch (APP.EvSocketUDPListen sid (0,port)) = do
    putStrLn $ "SocketUDPListen s=" ++ show sid ++ " p=" ++ show port
    lh <- PLI.udpAddListen ss sid port
    STM.atomically $ do
        m <- STM.readTVar sm
        STM.writeTVar sm $ M.insert (ch,sid) (SockUDPListen lh) m
    APP.sendMessage ch $ APP.MsgStatus True
    return ()
appEvent ss sm ch (APP.EvSocketUDPFlow sid (sIP,sPort) (dIP,dPort)) = do
    putStrLn $ "SocketUDPFlow s=" ++ show sid ++ " s=" ++
        show (sIP,sPort) ++ " d=" ++ show (dIP,dPort)
    fh <- PLI.udpAddFlow ss sid sIP sPort dIP dPort
    STM.atomically $ do
        m <- STM.readTVar sm
        STM.writeTVar sm $ M.insert (ch,sid) (SockUDPFlow fh) m
    APP.sendMessage ch $ APP.MsgStatus True
    return ()
appEvent ss _ ch ev = do
    putStrLn $ "appEvent " ++ show ch ++ " " ++ show ev

initAppInterface :: String -> PLI.StackHandle -> IO ()
initAppInterface stackname sh = do
    st <- PLI.stackState sh
    sm <- STM.newTVarIO $ M.empty
    tid <- forkOS $ APP.interfaceThread stackname (appEvent st sm)
    return ()

main :: IO ()
main = do
    let fname_def = "unicorn-tests/hello.unicorn"       -- default unicorn file name

    xargs <- getArgs
    let fname = if (length xargs) == 0 then fname_def else xargs !! 0

    pname <- getProgName
    let helpers = case pname of
            "llvm-cgen" -> "llvm-helpers"
            "llvm-cgen-dpdk" -> "llvm-helpers-dpdk"
            "llvm-cgen-e10k" -> "llvm-helpers-e10k"
            _ -> error "Unknown executable name, don't know what helpers to use :-/"

    txt <- readFile fname
    graph <- UnicornAST.parseGraph txt
    let pgraph = pg4tap $ Unicorn.constructGraph graph
    writeFile "DELETEME.dot" $ toDot pgraph
    let plg = PL.generatePLG plAssign pgraph
    writeFile "pipelines.dot" $ pipelinesDot Nothing plg

    let stackname = "dragonet"
    let runner = runPipeline plg stackname helpers
    h <- PLI.runPipelines stackname plConnect runner plg

    initAppInterface stackname h
    commandLineInterface

    putStrLn "Doing cleanup..."
    PLI.stopPipelines h


