{-# LANGUAGE FlexibleInstances, UndecidableInstances,
               OverlappingInstances, ScopedTypeVariables #-}

import qualified Dragonet.Configuration            as  C
import qualified Dragonet.Optimization             as  O
import qualified Dragonet.Pipelines                as  PL
import qualified Dragonet.Pipelines.Implementation as  PLI
import qualified Dragonet.ProtocolGraph            as  PG
import qualified Dragonet.ProtocolGraph.Utils      as  PGU
import qualified Dragonet.Search                   as  SE

import qualified Graphs.E10k as E10k
import qualified Graphs.Tap as Tap

import qualified Runner.E10KControl as CTRL

import qualified Options.Applicative as OA

import qualified Stack as SS

import Control.Monad (forever, forM_)
import Control.Applicative ((<$>),(<*>))
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

tr a b = trace b a
trN a b = a

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
    putStrLn $ "e10k-implCfg: " ++ show config
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


-- Create separate pipelines for rx and tx per hardware queue
plAssign :: SS.StackState -> String -> PG.PGNode -> String
plAssign _ _ (_,n)
    | take 2 lbl == "Tx" = "Tx" ++ tag
    | otherwise = "Rx" ++ tag
    where
        lbl = PG.nLabel n
        tag = PG.nTag n

-- Only create one pipeline per hardware queue
plAssignMerged :: SS.StackState -> String -> PG.PGNode -> String
plAssignMerged _ _ (_,n) = PG.nTag n

data StackE10kOpts = StackE10kOpts {
      optNq     :: Int
    , optCostFn :: Int -> SE.CostQueueFn
    , optDummy  :: Bool
} deriving (Show)

e10kImplFunction :: IO (PLI.StateHandle -> C.Configuration -> IO ())
e10kImplFunction = do
    --  Channel and MVar with thread id of control thread
    let state = CfgState { csThread = Nothing
                         , cs5Tuples = M.empty
                         , cs5TUnused = [0..127]}
    tcstate <- STM.newTVarIO state
    chan    <- STM.newTChanIO
    return $ implCfg tcstate chan

e10kPrgArgs :: IO SS.StackPrgArgs
e10kPrgArgs = do
    prgH <- E10k.graphH
    implFn <- e10kImplFunction
    return SS.StackPrgArgs { SS.stPrgH = prgH
                           , SS.stLlvmH = "llvm-helpers-e10k"
                           , SS.stCfgImpl = implFn }

dummyImplFn :: PLI.StateHandle -> C.Configuration -> IO ()
dummyImplFn _ conf = do
    putStrLn $ E10k.cfgStr conf
    return ()

tapPrgArgs :: IO SS.StackPrgArgs
tapPrgArgs = do
    prgH <- Tap.graphH
    return SS.StackPrgArgs { SS.stPrgH = prgH
                           , SS.stLlvmH = "llvm-helpers-tap"
                           , SS.stCfgImpl = dummyImplFn }

runStackE10k :: StackE10kOpts -> IO ()
runStackE10k opts = do
    -- PRG
    (prgU,_) <- E10k.graphH
    let nq       = optNq opts
        costFn   = (optCostFn opts) nq
        dummy    = optDummy opts
        searchFn = SE.runSearch searchPs
        searchPs = (SE.initSearchParamsE10k nq) { SE.sPrgU = prgU
                                                , SE.sCostFn = costFn }
    prgArgs <- case dummy of
                True -> tapPrgArgs
                False -> e10kPrgArgs

    SS.instantiateFlows_ searchFn plAssignMerged prgArgs


costFnParser = OA.str >>= doParse
    where doParse :: String -> OA.ReadM (Int -> SE.CostQueueFn)
          doParse x
            | x == "balance" = return SE.balanceCost
            | x == "priority" = return S1.priorityCost
            | otherwise = OA.readerError "Unknown cost function"

stackE10kParserInfo:: OA.ParserInfo StackE10kOpts
stackE10kParserInfo = OA.info (OA.helper <*> parser) info
    where parser = StackE10kOpts
                <$> OA.argument OA.auto infoNq
                <*> OA.argument costFnParser infoCostF
                <*> OA.switch (OA.short 'd' OA.<> OA.help dummyTxt)
          info = OA.fullDesc OA.<> OA.header "Instantiate the e10k stack"
          infoNq = (OA.metavar "nqueues" OA.<> OA.help "number of queues")
          infoCostF = (OA.metavar "costF" OA.<> OA.help "cost function")
          dummyTxt = "Dummy instatiation: "
                   ++ "(e10k graph for the search, "
                   ++ "TAP for the network stack implementtation, "
                   ++ "empty implementation function)"

main = do
    opts <- OA.execParser stackE10kParserInfo
    putStrLn $ show opts
    runStackE10k opts
