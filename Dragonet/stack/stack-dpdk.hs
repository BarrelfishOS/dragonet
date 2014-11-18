{-# LANGUAGE FlexibleInstances, UndecidableInstances,
               OverlappingInstances, ScopedTypeVariables #-}

import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.E10k as E10k
import Graphs.Cfg (e10kCfgStr)
import qualified Runner.E10KControl as CTRL

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
import qualified Fitness as F

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

import qualified Scenarios.S1 as S1

tr a b = trace b a
trN a b = a

localIP :: Word32
localIP = MD.asiagoIP_E10K
--localIP = MD.burrataIP

--------------------------------------------------

--------------------------------------------------
{-|
 - Actions supported by the DPDK driver of Intel 82599 NIC
 -}
data CfgAction =
    CfgASet5Tuple Word8 CTRL.FTuple  |
    CfgAClear5Tuple Word8            |
    CfgASetFDir Int CTRL.FDirTuple |
    CfgAClearFDir Int
    deriving (Eq,Show)

{-|
 - State needed by the driver-thread.
 -      It holds information about used/unused filter-ids, and which
 -      filters are mapped to which ids.
 -
 -      These id's are used to update/delete filters
 -}
data CfgState = CfgState {
        -- | id of the thread which is listening for events
        csThread :: Maybe ThreadId,
        -- | Maps 5-tuples to ids
        cs5Tuples :: M.Map CTRL.FTuple Word8,
        -- | Unused 5-tuple indexes
        cs5TUnused :: [Word8],
        -- | Maps FDir to ids
        csFDirs :: M.Map CTRL.FDirTuple Int,
        -- | Unused FDir indexes
        csFDirsUnused :: [Int]
    }



{-|
 - a Driver control thread (or driver-thread).
 -    * Reads a Software Transaction based channel for commands (CfgAction)
 -          for driver and executes them.
 -    * The typical commands are in type CfgAction
 -      - set5Tuple, clear5Tuple, setFDir, clearFDir
 -
 - It keeps doing that in an infinite loop
 -}
controlThread ::
    -- | STM channel handle to moniter incomming events which will be converted
    --      to actions on NIC
       STM.TChan CfgAction
    -- | Handle to the NIC driver state.  Used by C code to store driver
    --      specific state
    -> PLI.StateHandle
    -> IO ()
controlThread chan sh = do
    -- Wait for card to be initialized
    CTRL.waitReady sh
    -- Start working on chan
    forever $ do
        act <- STM.atomically $ STM.readTChan chan
        putStrLn $ "e10k-driver-thread: received action request : " ++ (show act)
        case act of
            CfgASet5Tuple idx ft ->
                CTRL.ftSet sh idx ft
            CfgAClear5Tuple idx ->
                CTRL.ftUnset sh idx

            CfgASetFDir idx ft ->
                CTRL.fdirSet sh idx ft
            CfgAClearFDir idx ->
                CTRL.fdirUnset sh idx

runDriverThread::
      STM.TVar CfgState
    -- | STM channel handle to moniter incomming events which will be converted
    --      to actions on NIC
    -> STM.TChan CfgAction
    -- | Handle to the NIC driver state.  Used by C code to store driver
    --      specific state
    -> PLI.StateHandle
    -> IO ()
runDriverThread tcstate chan sh = do
    putStrLn $ "e10k-driver-thread: "
    cstate <- STM.atomically $ STM.readTVar tcstate
    -- Ensure control thread is running
    -- If not, create a haskell-level thread (instead of OS-level thread)
    cstate' <- case csThread cstate of
        Nothing -> do
            tid <- forkIO $ controlThread chan sh
            return $ cstate { csThread = Just tid }
        Just _ -> return cstate
    STM.atomically $ STM.writeTVar tcstate cstate'


{-|
 - Function to implement specified configuration changes for FDir filters.
 -   * Translate the configuration changes into filter specific actions
 -   * Find the differences in the existing filter configuration and the
 -      one requested.  This should tell which filters to remove and which to add
 -   * Send a message to driver-thread about removing and adding filters using STM
 -   * Update the current state to reflect the new allocation of filters
 -}
implCfgFDir ::
      STM.TVar CfgState
    -- | STM channel handle to moniter incomming events which will be converted
    --      to actions on NIC
    -> STM.TChan CfgAction
    -- | Handle to the NIC driver state.  Used by C code to store driver
    --      specific state
    -> PLI.StateHandle
    -- | modifications in configuration which needs to be handled
    -> C.Configuration
    -> IO ()
implCfgFDir tcstate chan sh config = do
    cstate' <- STM.atomically $ STM.readTVar tcstate

    putStrLn $ "e10k-implCfgFDir: " ++ show config
    -- Handling 5tuple filters
    let Just (PG.CVList tuples) = lookup "RxCFDirFilter" config

    -- Make sure there are not too many entries
    if length tuples > (fromIntegral cFDirFilters)
        then error ("More than " ++ (show cFDirFilters) ++ " fdir filters configured")
        else return ()

    let ftm = csFDirs cstate'
        -- Parse fdir into internal representation
        fts = S.fromList $ map parseFDir tuples
        existing = S.fromList $ M.keys ftm
        -- Currently existing filters to remove
        toRemove = S.toList (existing S.\\ fts)
        -- New filters to add
        toAdd = S.toList (fts S.\\ existing)

        -- We need IDs for new filters.
        --  Lets reuse the IDs of the filters that we are deleting
        toRemIds = map (ftm M.!) toRemove
        -- And if we need more, we will use unused IDs
        usableIds = toRemIds ++ csFDirsUnused cstate'
        -- Add indexes to the new tuples
        toAddId = zip toAdd usableIds

        -- Remove old fdirs from the state-map and add new ones
        ftm' = foldl (flip M.delete) ftm toRemove
        ftm'' = foldl (flip $ uncurry M.insert) ftm' toAddId

    -- If necessary, clear fdirs in hardware
    cstate'' <- if length toAdd < length toRemIds
        then do
            -- We are using the IDs from the filters that we are removing
            --  So, we must free them first, otherwise we will have conflict
            let toFree = drop (length toAdd) toRemIds
            forM_ toFree $ \i ->
                -- Sending a message to driver-thread about removing filters
                STM.atomically $ STM.writeTChan chan $ CfgAClearFDir i
            return $ cstate' {
                        -- Update the state to reflect correct status of unused
                        --  and used filters
                        csFDirsUnused = toFree ++ csFDirsUnused cstate',
                        csFDirs = ftm'' }
        else do
            let toAlloc = (length toAdd) - (length toRemIds)
            return cstate' {
                        -- Update the state to reflect correct status of unused
                        --  and used filters
                        csFDirsUnused = drop toAlloc $ csFDirsUnused cstate',
                        csFDirs = ftm'' }


    putStrLn $ "e10k-implFdir: adding flow " ++ (show toAddId)
    -- Send the message to the driver thread to add the filters
    forM_ toAddId $ \(ft,i) ->
        STM.atomically $ STM.writeTChan chan $ CfgASetFDir i ft
    STM.atomically $ STM.writeTVar tcstate cstate''
    return ()
    where
        {-|
         - Function to parse the  configuration and return 5tuple filter
         -}
        parseFDir (PG.CVTuple [PG.CVMaybe msIP,
                                PG.CVMaybe mdIP,
                                PG.CVMaybe mProto,
                                PG.CVMaybe msP,
                                PG.CVMaybe mdP,
                                PG.CVInt queue]) =
            CTRL.FDirTuple {
                CTRL.fdtQueue = fromIntegral $ queue,
                CTRL.fdtL3Proto = Just CTRL.L3IPv4,
                CTRL.fdtL4Proto = parseProto <$> mProto,
                CTRL.fdtL3Src = parseIntegral <$> msIP,
                CTRL.fdtL3Dst = parseIntegral <$> mdIP,
                CTRL.fdtL4Src = parseIntegral <$> msP,
                CTRL.fdtL4Dst = parseIntegral <$> mdP
            }
        parseIntegral (PG.CVInt i) = fromIntegral i
        parseProto (PG.CVEnum 0) = CTRL.L4TCP
        parseProto (PG.CVEnum 1) = CTRL.L4UDP
        --parseProto PG.CVEnum 2 = CTRL.L4SCTP
        --parseProto PG.CVEnum 3 = CTRL.L4Other


{-|
 - Function to implement specified configuration.
 -   * Make sure that there is a thread running to handle driver changes (driver-thread)
 -   * Translate the configuration changes into filter specific actions
 -   * Find the differences in the existing filter configuration and the
 -      one requested.  This should tell which filters to remove and which to add
 -   * Send a message to driver-thread about removing and adding filters using STM
 -   * Update the current state to reflect the new allocation of filters
 -}
implCfg5Tuple ::
      STM.TVar CfgState
    -- | STM channel handle to moniter incomming events which will be converted
    --      to actions on NIC
    -> STM.TChan CfgAction
    -- | Handle to the NIC driver state.  Used by C code to store driver
    --      specific state
    -> PLI.StateHandle
    -- | modifications in configuration which needs to be handled
    -> C.Configuration
    -> IO ()
implCfg5Tuple tcstate chan sh config = do
    cstate' <- STM.atomically $ STM.readTVar tcstate

    putStrLn $ "e10k-implCfg5Tuple: " ++ show config
    -- Handling 5tuple filters
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

        -- We need IDs for new filters.
        --  Lets reuse the IDs of the filters that we are deleting
        toRemIds = map (ftm M.!) toRemove
        -- And if we need more, we will use unused IDs
        usableIds = toRemIds ++ cs5TUnused cstate'
        -- Add indexes to the new tuples
        toAddId = zip toAdd usableIds

        -- Remove old 5ts from the state-map and add new ones
        ftm' = foldl (flip M.delete) ftm toRemove
        ftm'' = foldl (flip $ uncurry M.insert) ftm' toAddId

    -- If necessary, clear 5tuples in hardware
    cstate'' <- if length toAdd < length toRemIds
        then do
            -- We are using the IDs from the filters that we are removing
            --  So, we must free them first, otherwise we will have conflict
            let toFree = drop (length toAdd) toRemIds
            forM_ toFree $ \i ->
                -- Sending a message to driver-thread about removing filters
                STM.atomically $ STM.writeTChan chan $ CfgAClear5Tuple i
            return $ cstate' {
                        -- Update the state to reflect correct status of unused
                        --  and used filters
                        cs5TUnused = toFree ++ cs5TUnused cstate',
                        cs5Tuples = ftm'' }
        else do
            let toAlloc = (length toAdd) - (length toRemIds)
            return cstate' {
                        -- Update the state to reflect correct status of unused
                        --  and used filters
                        cs5TUnused = drop toAlloc $ cs5TUnused cstate',
                        cs5Tuples = ftm'' }

    -- Send the message to the driver thread to add the filters

    putStrLn $ "e10k-impl5tpl: adding flow " ++ (show toAddId)
    forM_ toAddId $ \(ft,i) ->
        STM.atomically $ STM.writeTChan chan $ CfgASet5Tuple i ft
    STM.atomically $ STM.writeTVar tcstate cstate''
    return ()
    putStrLn $ "e10k-impl5tuple: filters to add " ++ (show toAddId)
    where
        {-|
         - Function to parse the  configuration and return 5tuple filter
         -}
        parseTuple (PG.CVTuple [PG.CVMaybe msIP,
                                PG.CVMaybe mdIP,
                                PG.CVMaybe mProto,
                                PG.CVMaybe msP,
                                PG.CVMaybe mdP,
                                PG.CVInt prio,
                                PG.CVInt queue]) =
            CTRL.FTuple {
                CTRL.ftPriority = fromIntegral $ prio,
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


{-|
 - Function to implement specified configuration changes for FDir filters.
 -   * Translate the configuration changes into filter specific actions
 -   * Find the differences in the existing filter configuration and the
 -      one requested.  This should tell which filters to remove and which to add
 -   * Send a message to driver-thread about removing and adding filters using STM
 -   * Update the current state to reflect the new allocation of filters
 -}
implCfg ::
      STM.TVar CfgState
    -- | STM channel handle to moniter incomming events which will be converted
    --      to actions on NIC
    -> STM.TChan CfgAction
    -- | Handle to the NIC driver state.  Used by C code to store driver
    --      specific state
    -> PLI.StateHandle
    -- | modifications in configuration which needs to be handled
    -> C.Configuration
    -> IO ()
implCfg tcstate chan sh config = do
    runDriverThread tcstate chan sh
    implCfg5Tuple tcstate chan sh config
    implCfgFDir tcstate chan sh config

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

llvm_helpers = "llvm-helpers-dpdk"


-- | Number of 5tuple filters supported by hardware
c5TupleFilters = CTRL.ftCount

-- | Number of FDir filters supported by hardware
-- FIXME: It will not work for values more than 128 as somewhere we are using
--      Word8 to store and pass these values
cFDirFilters = CTRL.fdirCount


main = do
    ((nq,costfn) :: (Int,String)) <- RA.readArgs
    print $ "Number of queues used: " ++ show nq
    print $ "Cost function: " ++ show costfn

    let t5count :: Word8
        t5count = fromIntegral c5TupleFilters
        cfcount = fromIntegral cFDirFilters

        state = CfgState {
                    csThread = Nothing,
                    cs5Tuples = M.empty,
                    cs5TUnused = [0..(t5count - 1)],
                    csFDirs = M.empty,
                    csFDirsUnused = [0..(cfcount - 1)]
                    }

    -- Channel and MVar with all the state needed by driver-thread
    tcstate <- STM.newTVarIO state
    chan <- STM.newTChanIO
    -- Prepare graphs and so on
    prgH@(prgU,_) <- E10k.graphH

    let e10kOracle = Search.E10kOracleSt {Search.nQueues = nq}
        priFn      = S1.priorityCost nq
        balFn      = Search.balanceCost nq
        strategy   = Search.searchGreedyFlows
        costFns    = [("balance", balFn), ("priority", priFn)]
        costFn     = case lookup costfn costFns of
                        Just x  -> x
                        Nothing -> error $ "Uknown cost function:" ++ costfn
        sparams    = Search.initSearchParams {   Search.sOracle = e10kOracle
                                               , Search.sPrgU   = prgU
                                               , Search.sCostFn = costFn
                                               , Search.sStrategy = strategy }
        searchFn   = Search.runSearch sparams

    -- TODO: Create the driver-thread before you go and start flows
    --      This should speedup the initialization as driver will be ready
    --      before applications starts connecting in
    instantiateFlows
        searchFn                -- ^ returns configurations to evaluate
        prgH                    -- ^ PRG
        llvm_helpers            -- ^ llvm helpers
        (implCfg tcstate chan)  -- ^ Function to implement given conf
        plAssignMerged          -- ^ classifies graph-nodes into pipelines based on node-tag

