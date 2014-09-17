import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Data.Graph.Inductive as DGI

import qualified Graphs.Null as Null
import qualified Runner.NullControl as CTRL

import Stack
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

tr = flip . trace

numQueues :: Integer
numQueues = 3

localIP :: Word32
localIP = MD.asiagoIP_Intel


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
oracle_ ::  SS.OracleArgs -> [(String,C.Configuration)]
oracle_ args@(SS.OracleArgs{ SS.oracleOldConf = oldconf,  SS.oraclePrg = oPrg, SS.oracleNewConns = eps , SS.oracleSS = ss}) = [
                ("defaultOracle", -- Most stupid configuration, only default queue and default filters
                    [ ("RxCFDirFilter", PG.CVList []),
                      ("RxC5TupleFilter", PG.CVList [])
                    ]
                )
                , ("OracleOneQueueManyFilters",  -- oracle giving only queue 0 for everything, but adding filters
                    [ ("RxCFDirFilter", PG.CVList []),
                      ("RxC5TupleFilter", PG.CVList fiveTupleCAlt)
                    ]
                )

                , ("MultiQueueOracle", -- separate queue  for each flow
                    [ ("RxCFDirFilter", PG.CVList []),
                      ("RxC5TupleFilter", PG.CVList fiveTupleC)
                    ]
                )

            ]
    where
        queues = [1..(numQueues-1)] ++ [0]
        -- returns 128 five-tuple endpoints from stack-state
        epss = M.elems $ ssEndpoints ss -- use in case you want to look into all flows in stack

        fiveTupleCAlt = map mkEp5T  -- make endpoint
                    $ zip (cycle [0])  -- use only queue 0
                    $ take 128 -- take top 128 endpoints
                    $ eps -- use only specified endpoints

        fiveTupleC = map mkEp5T  -- make endpoint
                    $ zip (cycle queues)  -- make tuple of cycled queue-id and endpoint
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

oracle ::  SS.OracleArgs -> [(String, C.Configuration)]
oracle args@(SS.OracleArgs {SS.oracleOldConf = (oldconfname,oldconf), SS.oracleNewConns = eps}) = ret_
    where ret_ = [ ("ORACLECONF", c) | c <- epsAllConfs eps oldconf ]
          ret  = trace ("---\n"
                        ++ "NEWCONF:  " ++ (ppShow ret_)
                        ++ "\nOLDCONF:" ++ (ppShow oldconf)
                        ++ "\nEPS: " ++ (ppShow eps)
                        ++ "---\n") ret_

epsAllConfs :: [EndpointDesc] -> C.Configuration -> [C.Configuration]
epsAllConfs [] c = [c]
epsAllConfs (e:es) c = L.concat [epsAllConfs es newc | newc <- cs]
    where cs = epAllConfs e c

epAllConfs :: EndpointDesc -> C.Configuration -> [C.Configuration]
epAllConfs ep oldConf = [ add5TupleToConf oldConf ep q |  q <- queues]
    where
        queues = [1..(numQueues-1)] ++ [0]

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

data CfgState = CfgState {
        csThread :: Maybe ThreadId,
        -- Maps 5-tuples to ids
        cs5Tuples :: M.Map CTRL.FTuple Word8,
        -- Unused 5-tuple indexes
        cs5TUnused :: [Word8]
    }

-- Implement specified configuration
implCfg :: STM.TVar CfgState -> STM.TChan CfgAction -> PLI.StateHandle
            -> C.Configuration -> IO ()
implCfg tcstate chan sh config = do
    putStrLn $ "null-implCfg: " ++ show config
    cstate <- STM.atomically $ STM.readTVar tcstate
    -- Ensure control thread is running
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


-- Assign all nodes to same pipeline
plAssign :: StackState -> String -> PG.PGNode -> String
plAssign _ _ (_,n)
    | take 2 lbl == "Tx" = "Tx" ++ tag
    | otherwise = "Rx" ++ tag
    where
        lbl = PG.nLabel n
        tag = PG.nTag n


main = do
    let state = CfgState {
                    csThread = Nothing,
                    cs5Tuples = M.empty,
                    cs5TUnused = [0..127]
                }
    -- Channel and MVar with thread id of control thread
    tcstate <- STM.newTVarIO state
    chan <- STM.newTChanIO
    -- Prepare graphs and so on
    prgH <- Null.graphH
    --instantiate prgH "llvm-helpers-null" F.fitnessFunction oracle
    --instantiate prgH "llvm-helpers-null" F.priorityFitness  oracle
    instantiateGreedy prgH "llvm-helpers-null" F.priorityFitness oracle
        (implCfg tcstate chan) plAssign

