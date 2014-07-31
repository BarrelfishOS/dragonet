import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.E10k as E10k
import qualified Runner.E10KControl as CTRL

import Stack

import Control.Monad (forever)
import Control.Concurrent(forkIO, ThreadId)
import qualified Control.Concurrent.STM as STM
import Data.Word


-- Does not really matter as we only have one config
costFunction :: StackState -> O.CostFunction Int
costFunction _ _ = 1

-- Start out with a dummy configuration for e10k
oracle :: PG.PGraph -> StackState -> [(String,C.Configuration)]
oracle _ _ = [("default",[
                ("RxCFDirFilter", PG.CVList []),
                ("RxC5TupleFilter", PG.CVList [
                     PG.CVTuple [
                        PG.CVMaybe Nothing,
                        PG.CVMaybe Nothing,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe Nothing,
                        PG.CVMaybe $ Just $ PG.CVInt 7,
                        PG.CVInt 1,
                        PG.CVInt 1 ]
                    ])
                ])]

data CfgAction =
    CfgASet5Tuple Word8 CTRL.FTuple |
    CfgAClear5Tuple Word8
    deriving (Eq,Show)

controlThread :: STM.TChan CfgAction -> PLI.StateHandle -> IO ()
controlThread chan sh = do
    -- Wait for card to be initialized
    CTRL.waitReady sh
    -- Start working on chan
    forever $ do
        act <- STM.atomically $ STM.readTChan chan
        case act of
            CfgASet5Tuple idx ft -> CTRL.ftSet sh idx ft
            CfgAClear5Tuple idx -> CTRL.ftUnset sh idx

-- Nothing to be implemented yet
implCfg :: STM.TMVar ThreadId -> STM.TChan CfgAction -> PLI.StateHandle
            -> C.Configuration -> IO ()
implCfg mtid chan sh _ = do
    mtid' <- STM.atomically $ STM.tryTakeTMVar mtid
    case mtid' of
        Nothing -> do
            tid <- forkIO $ controlThread chan sh
            STM.atomically $ STM.putTMVar mtid tid
        Just _ -> return ()
    return ()

-- Assign all nodes to same pipeline
plAssign :: StackState -> String -> PG.PGNode -> String
plAssign _ _ (_,n)
    | take 2 lbl == "Tx" = "Tx" ++ tag
    | otherwise = "Rx" ++ tag
    where
        lbl = PG.nLabel n
        tag = PG.nTag n

main = do
    -- Channel and MVar with thread id of control thread
    mtid <- STM.newEmptyTMVarIO
    chan <- STM.newTChanIO
    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 0 $ CTRL.FTuple {
                CTRL.ftPriority = 1,
                CTRL.ftQueue = 1,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Nothing,
                CTRL.ftL3Dst = Nothing,
                CTRL.ftL4Src = Nothing,
                CTRL.ftL4Dst = Just 7
            }
    -- Prepare graphs and so on
    prgH <- E10k.graphH
    instantiate prgH "llvm-helpers-e10k" costFunction oracle (implCfg mtid chan)
        plAssign

