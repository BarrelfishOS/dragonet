import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.SF as SF
import qualified Runner.SFControl as CTRL

import Stack


import Control.Monad (forever, forM_)
import Control.Applicative ((<$>))
import Control.Concurrent(forkIO, ThreadId)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word

import qualified MachineDetails as MD

numQueues :: Integer
numQueues = 4

localIP :: Word32
localIP = MD.asiagoIP_SF


-- Does not really matter as we only have one config
costFunction :: StackState -> O.CostFunction Int
costFunction _ _ = 1

-- Start out with a dummy configuration for SF
oracleHardcoded :: PG.PGraph -> StackState -> [(String,C.Configuration)]
oracleHardcoded _ ss = [("default",[
                ("RxCFDirFilter", PG.CVList []),
                ("RxC5TupleFilter", PG.CVList [

 -- ############# For only 1 core #############
                     PG.CVTuple [
                        PG.CVMaybe Nothing, -- $ Just $ PG.CVInt $ fromIntegral 175178809, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 0 ]

                    ])
                ])]



oracle :: PG.PGraph -> StackState -> [(String,C.Configuration)]
oracle _ ss = [("default",[
                    ("RxCFDirFilter", PG.CVList []),
                    ("RxC5TupleFilter", PG.CVList fiveTupleC)
                    ])]
    where
        queues = [1..(numQueues-1)] ++ [0]
        fiveTupleC = map mkEp5T $ zip (cycle queues) $ take 128 $
                    M.elems $ ssEndpoints ss
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
    prgH <- SF.graphH
    instantiate prgH "llvm-helpers-sf" costFunction oracle
        (implCfg tcstate chan) plAssign

