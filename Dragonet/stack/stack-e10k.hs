{-# LANGUAGE FlexibleInstances, UndecidableInstances,
               OverlappingInstances, ScopedTypeVariables #-}

import qualified Dragonet.Configuration            as  C
import qualified Dragonet.Optimization             as  O
import qualified Dragonet.Pipelines                as  PL
import qualified Dragonet.Pipelines.Implementation as  PLI
import qualified Dragonet.ProtocolGraph            as  PG
import qualified Dragonet.ProtocolGraph.Utils      as  PGU
import qualified Dragonet.Search                   as  SE
import qualified Dragonet.Flows                    as  FL

import qualified Graphs.E10k as E10k
import qualified Graphs.Tap as Tap

import qualified Runner.E10KControl as CTRL

import qualified Options.Applicative as OA

import qualified Stack as SS

-- For DPDK based implementation of E10K NIC
import qualified DpdkImpl as DPDKIMPL


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
            -- XXX: CHECKME
            -- CfgASet5Tuple idx ft -> CTRL.ftSet sh idx ft
            CfgASet5Tuple idx ft -> CTRL.ftSet E10k.ftCount sh idx ft
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

e10kImplFunction :: IO (PLI.StateHandle -> C.Configuration -> IO ())
e10kImplFunction = do
    --  Channel and MVar with thread id of control thread
    let state = CfgState { csThread = Nothing
                         , cs5Tuples = M.empty
                         , cs5TUnused = [0..127]}
    tcstate <- STM.newTVarIO state
    chan    <- STM.newTChanIO
    return $ implCfg tcstate chan

{-
dpdkImplFunction :: IO (PLI.StateHandle -> C.Configuration -> IO ())
dpdkImplFunction = do
    let t5count :: Word8
        t5count = fromIntegral c5TupleFilters
        cfcount = fromIntegral cFDirFilters

    --  Channel and MVar with thread id of control thread
        state = CfgState {
                    csThread = Nothing,
                    cs5Tuples = M.empty,
                    cs5TUnused = [0..(t5count - 1)],
                    csFDirs = M.empty,
                    csFDirsUnused = [0..(cfcount - 1)]
                    }

    tcstate <- STM.newTVarIO state
    chan    <- STM.newTChanIO

    return $ implCfg tcstate chan
-}

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

dpdkPrgArgs :: IO SS.StackPrgArgs
dpdkPrgArgs = do
    prgH <- E10k.graphH
    implFn <- DPDKIMPL.dpdkImplFunction
    return SS.StackPrgArgs { SS.stPrgH = prgH
                           , SS.stLlvmH = DPDKIMPL.llvm_helpers
                           , SS.stCfgImpl = implFn }

data StackE10kOpts = StackE10kOpts {
      optNq          :: Int
    , optCostFn      :: (Int -> SE.CostQueueFn, [FL.Flow] -> [FL.Flow])
    , optPrgArgs     :: (String, IO SS.StackPrgArgs)
    , optIncremental :: Bool
} deriving (Show)

instance Show (String, IO SS.StackPrgArgs) where
    show (x,_) = show x

runStackE10k :: StackE10kOpts -> IO ()
runStackE10k opts@(StackE10kOpts {optIncremental = incremental}) = do
    case incremental of
        True  -> doRunStackE10kIncremental opts
        False -> doRunStackE10k opts

doRunStackE10kIncremental :: StackE10kOpts -> IO ()
doRunStackE10kIncremental opts = do
    -- PRG (use simple graph. Hope this works!)
    (prgU,_) <- E10k.graphH_ "Graphs/E10k/prgE10kImpl-simple.unicorn"
    -- FIXME: Using full graph did not worked (atleast with incremental run)
    --prgH@(prgU,_) <- E10k.graphH

    -- get options
    let nq       = optNq opts
        costFn   = (fst $ optCostFn opts) nq
        sortFn   = snd $ optCostFn opts

    -- search parameters
    let sParams = SE.initIncrSearchParams {
                          SE.isOracle = SE.initE10kOracle nq
                        , SE.isPrgU = prgU
                        , SE.isCostFn = costFn
                        , SE.isOrderFlows = sortFn }

    -- PRG
    prgArgs <- (snd $ optPrgArgs opts)

    se0 <- SE.initIncrSearchIO sParams
    SS.instantiateIncrFlowsIO_ SE.runIncrSearchIO se0 plAssignMerged prgArgs

doRunStackE10k :: StackE10kOpts -> IO ()
doRunStackE10k opts = do
    -- PRG
    (prgU,_) <- E10k.graphH
    -- get options
    let nq       = optNq opts
        costFn   = (fst $ optCostFn opts) nq
        sortFn   = snd $ optCostFn opts

    -- search parameters
    let sParams = SE.initSearchParams {
                        SE.sOracle = SE.initE10kOracle nq
                      , SE.sPrgU   = prgU
                      , SE.sCostFn = costFn
                      , SE.sOrderFlows = sortFn }

    -- [Flow] -> IO C.Configuration
    getConfIO <- SE.runSearchIO <$> SE.initSearchIO sParams

    -- PRG
    prgArgs <- (snd $ optPrgArgs opts)

    SS.instantiateFlowsIO_ getConfIO plAssignMerged prgArgs


costFnL fPerq = [
      ("balance",  (SE.balanceCost, id))
    , ("priority", ((S1.priorityCost' fPerq), (S1.prioritySort' fPerq)))
    , ("priority2", ((S1.priorityCost'' 2 fPerq), (S1.prioritySort'' 2)))
 ]

costFnParser = OA.str >>= doParse
    where doParse :: String -> OA.ReadM (Int -> SE.CostQueueFn, [FL.Flow] -> [FL.Flow])
          doParse x = case L.lookup x (costFnL fPerQ) of
                Nothing -> OA.readerError "Uknown cost function"
                Just y  -> return y

prgArgsL = [
      ("e10k", ("e10k", e10kPrgArgs))
    , ("dummy", ("dummy (tap)", tapPrgArgs))
    , ("dpdk", ("dpdk", dpdkPrgArgs))
 ]
prgArgsParser  = OA.str >>= doParse
    where doParse :: String -> OA.ReadM (String, IO SS.StackPrgArgs)
          doParse x = case L.lookup x prgArgsL of
                Nothing -> OA.readerError "Uknown prgArgs option"
                Just y  -> return y

-- Number of flows in single gold queue allowed.
--      Typically it should equal to  lows per application
-- TODO: Read this from commandline args
fPerQ = 1

stackE10kParserInfo:: OA.ParserInfo StackE10kOpts
stackE10kParserInfo = OA.info (OA.helper <*> parser) info
    where parser = StackE10kOpts
                <$> OA.argument OA.auto infoNq
                <*> OA.argument costFnParser infoCostF
                <*> OA.argument prgArgsParser infoPrgArgs
                <*> OA.switch (OA.short 'i' OA.<> OA.help incrTxt)
          --fPerQ = 2
          info = OA.fullDesc OA.<> OA.header "Instantiate the e10k stack"
          infoNq = (OA.metavar "nqueues" OA.<> OA.help "number of queues")
          infoCostF = (OA.metavar costMeta OA.<> OA.help costHelp)
          infoPrgArgs = (OA.metavar prgMeta OA.<> OA.help prgHelp)
          incrTxt = "Run Incremental stack"
          costHelp = "cost function"
          costMeta = "CostF (one of:" ++ (show $ map fst (costFnL fPerQ)) ++")"
          prgMeta = "prgArgs (one of:" ++ (show $ map fst prgArgsL) ++")"
          prgHelp = "PRG backend"

main = do
    opts <- OA.execParser stackE10kParserInfo
    putStrLn $ show opts
    runStackE10k opts
