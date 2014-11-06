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

-- Start out with a dummy configuration for null device
oracleHardcoded ::  SS.OracleArgs -> [(String,C.Configuration)]
oracleHardcoded  args@(SS.OracleArgs{ SS.oracleOldConf = oldconf,
        SS.oraclePrg = oPrg, SS.oracleNewConns = eps , SS.oracleSS = ss}) = [
                ("default",[
                ("RxCFDirFilter", PG.CVList []),
                ("RxC5TupleFilter", PG.CVList [
                     PG.CVTuple [
                        PG.CVMaybe Nothing, -- $ Just $ PG.CVInt $ fromIntegral MD.ziger2IP, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral localIP, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 0 ]

                ])])]

-- This should return multiple tuples and not just one tuple list
oracleMultiQueue ::  Int -> SS.OracleArgs -> [(String,C.Configuration)]
oracleMultiQueue nq args@(SS.OracleArgs{ SS.oracleOldConf = oldconf,  SS.oraclePrg = oPrg, SS.oracleNewConns = eps , SS.oracleSS = ss}) = [
{-
                ("defaultOracle", -- Most stupid configuration, only default queue and default filters
                    [ ("RxCFDirFilter", PG.CVList []),
                      ("RxC5TupleFilter", PG.CVList [])
                    ]
                )
                , ("OracleOneQueueManyFilters",  -- oracle giving only queue 0 for everything, but adding filters
                    [ ("RxCFDirFilter", PG.CVList []),
                      ("RxC5TupleFilter", PG.CVList fiveTupleCAlt)
                    ]
                ),
-}
                ("MultiQueueOracle", -- separate queue  for each flow
                    [ ("RxCFDirFilter", PG.CVList []),
                      ("RxC5TupleFilter", PG.CVList fiveTupleC)
                    ]
                )
            ]
    where
        queues = [1..(nq-1)] ++ [0]
        -- returns 128 five-tuple endpoints from stack-state
        epss = M.elems $ ssEndpoints ss -- use in case you want to look into all flows in stack

        fiveTupleCAlt = map mkEp5T  -- make endpoint
                    $ zip (cycle [0])  -- use only queue 0
                    $ take 128 -- take top 128 endpoints
                    $ eps -- use only specified endpoints

        fiveTupleC = map mkEp5T  -- make endpoint
                    $ zip (cycle (map toInteger queues))  -- make tuple of cycled queue-id and endpoint
                    $ take 128 -- take top 128 endpoints
                    $ eps -- use only specified endpoints
        mkEp5T (queue,ep) = PG.CVTuple [
                                cvMInt sIP, cvMInt dIP,
                                PG.CVMaybe $ Just $ PG.CVEnum 1,
                                cvMInt sP, cvMInt dP,
                                PG.CVInt prio,
                                PG.CVInt queue ]
            where
                sIP = edIP4Src ep
                dIP = edIP4Dst ep
                sP = edUDPSrc ep
                dP = edUDPDst ep
                nJust Nothing = 0
                nJust (Just _) = 1
                cvMInt :: Integral a => Maybe a -> PG.ConfValue
                cvMInt mi =  PG.CVMaybe $ (PG.CVInt . fromIntegral) <$> mi
                prio = (1 +) $ sum [nJust sIP, nJust dIP, nJust sP, nJust dP]

oracle ::  Int -> SS.OracleArgs -> [(String, C.Configuration)]
oracle nq args@(SS.OracleArgs {SS.oracleOldConf = (oldconfname,oldconf), SS.oracleNewConns = eps}) = ret
    where ret_ = [ ("ORACLECONF", c) | c <- epsAllConfs nq eps oldconf ]
          ret  = trace ("---\n"
                        ++ "NEWCONF:  " ++ (ppShow ret_)
                        ++ "\nOLDCONF:" ++ (ppShow oldconf)
                        ++ "\nEPS: " ++ (ppShow eps)
                        ++ "---\n") ret_

epsAllConfs :: Int -> [EndpointDesc] -> C.Configuration -> [C.Configuration]
epsAllConfs nq [] c = [c]
epsAllConfs nq (e:es) c = L.concat [epsAllConfs nq es newc | newc <- cs]
    where cs = epAllConfs nq e c

epAllConfs :: Int -> EndpointDesc -> C.Configuration -> [C.Configuration]
epAllConfs nq ep oldConf = [ add5TupleToConf oldConf ep q |  q <- queues]
    where
        queues = [1..( (toInteger nq)-1)] ++ [0]

mk5TupleFromEP :: EndpointDesc -> Integer -> PG.ConfValue
mk5TupleFromEP ep q = PG.CVTuple [ cvMInt sIP, cvMInt dIP,
                                   PG.CVMaybe $ Just $ PG.CVEnum 1,
                                   cvMInt sP, cvMInt dP,
                                   PG.CVInt prio,
                                   PG.CVInt q]
            where
                sIP = edIP4Src ep
                dIP = edIP4Dst ep
                sP = edUDPSrc ep
                dP = edUDPDst ep
                nJust Nothing = 0
                nJust (Just _) = 1
                cvMInt :: Integral a => Maybe a -> PG.ConfValue
                cvMInt mi =  PG.CVMaybe $ (PG.CVInt . fromIntegral) <$> mi
                prio = (1 +) $ sum [nJust sIP, nJust dIP, nJust sP, nJust dP]


add5TupleToConf :: C.Configuration -> EndpointDesc -> Integer -> C.Configuration
add5TupleToConf conf ep q = ("RxC5TupleFilter", new5t):rest
    where new5t :: PG.ConfValue
          new5t  = addToCVL old5t (mk5TupleFromEP ep q)
          old5t :: PG.ConfValue
          old5t = case L.lookup "RxC5TupleFilter" conf of
                    Just l -> l
                    Nothing -> error "add5TupleToConf: Did not find RxC5TupleFilter"

          rest :: C.Configuration
          rest  = filter ((/="RxC5TupleFilter") . fst) conf

addToCVL :: PG.ConfValue -> PG.ConfValue -> PG.ConfValue
addToCVL (PG.CVList l) v = PG.CVList $ v:l


--------------------------------------------------
{-|
 - Actions supported by the DPDK driver of Intel 82599 NIC
 -}
data CfgAction =
    CfgASet5Tuple Word8 CTRL.FTuple  |
    CfgAClear5Tuple Word8            |
    CfgASetFDir Word8 CTRL.FDirTuple |
    CfgAClearFDir Word8
    deriving (Eq,Show)

{-|
 - a Driver control thread (or driver-thread).
 -    * Reads a Software Transaction based channel for commands (CfgAction)
 -          for driver and executes them.
 -    * The typical commands are in type CfgAction
 -      - set5Tuple, clear5Tuple, setFDir, clearFDir
 -
 - It keeps doing that in an infinite loop
 -}
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

            CfgASetFDir idx ft -> CTRL.fdirSet sh idx ft
            CfgAClearFDir idx -> CTRL.fdirUnset sh idx

data CfgState = CfgState {
        csThread :: Maybe ThreadId,
        -- Maps 5-tuples to ids
        cs5Tuples :: M.Map CTRL.FTuple Word8,
        -- Unused 5-tuple indexes
        cs5TUnused :: [Word8]
        -- FIXME: add state for FDir filters as well
    }

{-|
 - Function to implement specified configuration.
 -   * Make sure that there is a thread running to handle driver changes (driver-thread)
 -   * Translate the configuration changes into filter specific actions
 -   * Find the differences in the existing filter configuration and the
 -      one requested.  This should tell which filters to remove and which to add
 -   * Send a message to driver-thread about removing and adding filters using STM
 -   * Update the current state to reflect the new allocation of filters
 -}
implCfg :: STM.TVar CfgState -> STM.TChan CfgAction ->
        PLI.StateHandle -> C.Configuration -> IO ()
implCfg tcstate chan sh config = do
    putStrLn $ "e10k-implCfg: " ++ show config
    cstate <- STM.atomically $ STM.readTVar tcstate
    -- Ensure control thread is running
    -- If not, create a haskell-level thread (instead of OS-level thread)
    cstate' <- case csThread cstate of
        Nothing -> do
            tid <- forkIO $ controlThread chan sh
            return $ cstate { csThread = Just tid }
        Just _ -> return cstate
    let Just (PG.CVList tuples) = lookup "RxC5TupleFilter" config

    -- Make sure there are not too many entries
    if length tuples > 128
        then error "More than 128 5-tuples configured"
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

        -- Remove old 5ts from map and add new ones
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
    forM_ toAddId $ \(ft,i) ->
        STM.atomically $ STM.writeTChan chan $ CfgASet5Tuple i ft
    STM.atomically $ STM.writeTVar tcstate cstate''
    return ()
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

main = do
    ((nq,costfn) :: (Int,String)) <- RA.readArgs
    print $ "Number of queues used: " ++ show nq
    print $ "Cost function: " ++ show costfn

    let state = CfgState {
                    csThread = Nothing,
                    cs5Tuples = M.empty,
                    cs5TUnused = [0..127]
                    --, csFDirUnused = [0..1023] -- FIXME: enable this
                    }
    -- Channel and MVar with thread id of control thread
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
                                               -- FIXME: Shouldn't following be `costFn`
                                               , Search.sCostFn = priFn
                                               , Search.sStrategy = strategy }
        searchFn   = Search.runSearch sparams

    instantiateFlows
        searchFn                -- ^ returns configurations to evaluate
        prgH                    -- ^ PRG
        llvm_helpers            -- ^ llvm helpers
        (implCfg tcstate chan)  -- ^ Function to implement given conf
        plAssignMerged          -- ^ classifies graph-nodes into pipelines based on node-tag

