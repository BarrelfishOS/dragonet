{-# LANGUAGE FlexibleInstances, UndecidableInstances,
               OverlappingInstances, ScopedTypeVariables #-}

import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.SF as SF
import qualified Runner.SFControl as CTRL

import qualified ReadArgs as RA

import Stack
import qualified Dragonet.Search as Search
import qualified Stack as SS

import Control.Monad (forever, forM_)
import Control.Applicative ((<$>))
import Control.Concurrent(forkIO, ThreadId)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Word

import qualified MachineDetails as MD

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

import qualified Scenarios.S1 as S1
import qualified Scenarios.S3 as S3


data CfgAction =
    CfgASet5Tuple Int CTRL.FTuple |
    CfgAClear5Tuple Int
    deriving (Eq,Show)

controlThread :: STM.TChan CfgAction -> PLI.StateHandle -> IO ()
controlThread chan sh = do
    -- Wait for card to be initialized
    putStrLn "SF-driver-thread: Started "
    CTRL.waitReady sh
    putStrLn "SF-driver-thread: Card ready "
    CTRL.waitReady sh
    -- Start working on chan
    forever $ do
        act <- STM.atomically $ STM.readTChan chan
        case act of
            CfgASet5Tuple idx ft -> CTRL.ftSet sh idx ft
            CfgAClear5Tuple idx -> CTRL.ftUnset sh idx

data CfgState = CfgState {
        csThread :: Maybe ThreadId,
        -- Maps 5-tuples to ids
        cs5Tuples :: M.Map CTRL.FTuple Int,
        -- Unused 5-tuple indexes
        cs5TUnused :: [Int]
    }

-- Implement specified configuration
implCfg :: STM.TVar CfgState -> STM.TChan CfgAction -> PLI.StateHandle
            -> C.Configuration -> IO ()
implCfg tcstate chan sh config = do
    putStrLn $ "sf-implCfg: " ++ show config
    cstate <- STM.atomically $ STM.readTVar tcstate
    -- Ensure control thread is running
    cstate' <- case csThread cstate of
        Nothing -> do
            tid <- forkIO $ controlThread chan sh
            return $ cstate { csThread = Just tid }
        Just _ -> return cstate
    let Just (PG.CVList tuples) = lookup "RxC5TupleFilter" config
    -- Make sure there are not too many entries
    if length tuples > (fromIntegral c5TupleFilters)
        then error ("More than " ++ (show c5TupleFilters) ++ " 5-tuples configured")
        else return ()

    let ftm = cs5Tuples cstate'
        -- Parse 5tuples into internal representation
        fts = S.fromList $ map parseTuple tuples
        existing = S.fromList $ M.keys ftm
        -- Currently existing tuples to remove
        toRemove = S.toList (existing S.\\ fts)
        -- New tuples to add
        toAdd = S.toList (fts S.\\ existing)
        toRemIds = map (ftm M.!) toRemove
        usableIds = toRemIds ++ cs5TUnused cstate'
        -- Add indexes to the new tuples
        toAddId = zip toAdd usableIds
        -- Remove old 5ts from map and add new ones
        ftm' = foldl (flip M.delete) ftm toRemove
        ftm'' = foldl (flip $ uncurry M.insert) ftm' toAddId
    -- If necessary, clear 5tuples in hardware
    cstate'' <- if length toAdd < length toRemIds
        then do
            let toFree = drop (length toAdd) toRemIds
            forM_ toFree $ \i ->
                STM.atomically $ STM.writeTChan chan $ CfgAClear5Tuple i
            return $ cstate' {
                        cs5TUnused = toFree ++ cs5TUnused cstate',
                        cs5Tuples = ftm'' }
        else do
            let toAlloc = (length toAdd) - (length toRemIds)
            return cstate' {
                        cs5TUnused = drop toAlloc $ cs5TUnused cstate',
                        cs5Tuples = ftm'' }
    forM_ toAddId $ \(ft,i) ->
        STM.atomically $ STM.writeTChan chan $ CfgASet5Tuple i ft
    STM.atomically $ STM.writeTVar tcstate cstate''
    return ()
    where
        parseTuple (PG.CVTuple [PG.CVMaybe msIP,
                                PG.CVMaybe mdIP,
                                PG.CVMaybe mProto,
                                PG.CVMaybe msP,
                                PG.CVMaybe mdP,
                                --PG.CVInt prio,
                                PG.CVInt queue]) =
            CTRL.FTuple {
                --CTRL.ftPriority = fromIntegral $ prio,
                CTRL.ftPriority = fromIntegral $ 1,
                CTRL.ftQueue = fromIntegral $ queue,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = parseProto <$> mProto,
                CTRL.ftL3Src = parseIntegral <$> msIP,
                CTRL.ftL3Dst = parseIntegral <$> mdIP,
                CTRL.ftL4Src = parseIntegral <$> msP,
                CTRL.ftL4Dst = parseIntegral <$> mdP
            }
        parseIntegral (PG.CVInt i) = fromIntegral i
        parseProto (PG.CVEnum 0) = CTRL.L4TCP
        parseProto (PG.CVEnum 1) = CTRL.L4UDP
        -- parseProto PG.CVEnum 2 = CTRL.L4SCTP
        -- parseProto PG.CVEnum 3 = CTRL.L4Other



-- Create separate pipelines for rx and tx per hardware queue
plAssign :: StackState -> String -> PG.PGNode -> String
plAssign _ _ (_,n)
    | take 2 lbl == "Tx" = "Tx" ++ tag
    | otherwise = "Rx" ++ tag
    where
        lbl = PG.nLabel n
        tag = PG.nTag n

-- Only create one pipeline per hardware queue
plAssignMerged :: StackState -> String -> PG.PGNode -> String
plAssignMerged _ _ (_,n) = PG.nTag n

llvm_helpers = "llvm-helpers-sf"

-- | Number of 5tuple filters supported by hardware
c5TupleFilters = CTRL.ftCount

main = do
    ((nq',costfn,oraclefn,concurrency,clients) :: (Int,String,String,Int,Int)) <- RA.readArgs
    -- to avoid off by one error as most of the code assumes
    let nq = nq' - 1

    print $ "Number of queues used: " ++ show nq
    print $ "Cost function: " ++ show costfn
    print $ "Oracle function: " ++ show oraclefn
    print $ "Concurrency (only for hardcoded oracle) : " ++ show concurrency
    print $ "clients (only for hardcoded oracle) : " ++ show clients

    let t5count = fromIntegral c5TupleFilters
        state = CfgState {
                    csThread = Nothing,
                    cs5Tuples = M.empty,
                    cs5TUnused = [0.. (t5count - 1)]
                }

    -- Channel and MVar with thread id of control thread
    tcstate <- STM.newTVarIO state
    chan <- STM.newTChanIO
    -- Prepare graphs and so on
    prgH@(prgU,_) <- SF.graphH

    let sfOracle = Search.sfOracleInit nq
        priFn      = S1.priorityCost nq
        balFn      = Search.balanceCost nq
        strategy   = Search.searchGreedyFlows
        costFns    = [("balance", balFn), ("priority", priFn)]
        costFn     = case lookup costfn costFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown cost function:" ++ costfn
        greedyParams    = Search.runSearch $ Search.initSearchParams {
                                               Search.sOracle = sfOracle
                                               , Search.sPrgU   = prgU
                                               , Search.sCostFn =  costFn
                                               , Search.sStrategy = strategy }

        -- FIXME: pass the argument "priority" to hardcodedOracleMemcached
        hardcodedParams = Search.runSearch $ S3.hardcodedOracleMemcachedSF
                        concurrency clients nq  costfn prgU    -- fpApp clients nq prgU

        searchFns = [
                        ("greedy", (greedyParams, 0)),
                        ("hardcoded", (hardcodedParams, (concurrency * clients) ))
                    ]
        (searchFn, trigger) = case lookup oraclefn searchFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown oracle function:" ++ oraclefn

    -- TODO: Create the driver-thread before you go and start flows
    --      This should speedup the initialization as driver will be ready
    --      before applications starts connecting in
    instantiateFlows
        searchFn                -- ^ returns configurations to evaluate
        prgH                    -- ^ PRG
        llvm_helpers            -- ^ llvm helpers
        (implCfg tcstate chan)  -- ^ Function to implement given conf
        plAssignMerged          -- ^ classifies graph-nodes into pipelines based on node-tag


--    instantiateFlows searchFn prgH llvm_helpers (implCfg tcstate chan) plAssignMerged
