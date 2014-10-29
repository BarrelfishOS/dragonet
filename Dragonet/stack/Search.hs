{-# LANGUAGE RankNTypes, LiberalTypeSynonyms, ExistentialQuantification,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             ScopedTypeVariables, FunctionalDependencies #-}
-- Simple search functions
module Search (
  SearchParams(..),
  initSearchParams,
  SearchSt(..),
  initSearchSt,
  runSearch,
  E10kOracleSt(..),
  searchGreedyFlows,
  balanceCost,
  priorityCost,
  test,
) where

import qualified Dragonet.Configuration       as C
import qualified Dragonet.ProtocolGraph       as PG
import qualified Util.GraphHelpers            as GH
import qualified Dragonet.Predicate           as PR
import qualified Dragonet.Implementation.IPv4 as IP4

import Dragonet.DotGenerator (toDot)
import Dragonet.Conventions (rxQPref, isTruePort, isFalsePort)
import Dragonet.Flows(Flow (..), flowPred)

import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Graphs.E10k as E10k
import Graphs.Cfg (prgCfg, prgCfgEmpty, e10kCfgEmpty, e10kCfgStr)

import qualified Data.List            as L
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import qualified Data.Graph.Inductive as DGI

import Data.Word
import Data.Maybe
import Data.Int
import Data.Function (on)
import Data.Char (isDigit)

import qualified Control.Monad.ST        as ST
import qualified Data.STRef              as STR

import Control.Monad (forM)
import qualified Data.HashTable.ST.Basic as HB
import qualified Data.HashTable.Class    as H

import Control.Applicative ((<$>))

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

tr a b  = trace b a
trN a b = a

allQueues :: Int -> [QueueId]
allQueues nq = [1..(nq-1)] ++ [0] :: [QueueId]

-- NB that due to the ordering, costOK will always have the smaller value, and
-- costReject will always have the highest value.
-- NB: instead of using L.minimum we can implement a function that  can
-- short-circuit CostOK to save some iterations.
-- i.e., L.minimum $ CostOK:[CostVal $ 0.1 + i | i <- [1..]]
-- will never return
data Cost = CostOK         |
            CostVal Float  |
            CostReject Float -- small hack to rate rejected solutions
    deriving (Eq, Ord, Show)

-- cost functions based on how flows are mapped into queues
type QueueId     = Int
type QMap        = [(Flow, QueueId)]
--type QMap        = M.Map Flow QueueId
type CostQueueFn = QMap -> Cost

-- the basic form of a cost function for the search algorithms
--type CostFn s = [Flow] -> C.Configuration -> ST.ST s Cost
type CostFn a s = (ConfChange a)
                => [Flow] -> C.Configuration -> a -> ST.ST s Cost

class ConfChange a where
    applyConfChange :: C.Configuration -> a -> C.Configuration

-- Oracle: iterate configuration space
-- (we will probably have to rethink the interface)
class (ConfChange a) => OracleSt o a | o -> a where
    -- seems too OO, but not sure how to do it better
    emptyConf  ::       o -> C.Configuration
    showConf   ::       o -> C.Configuration -> String
    -- iterators
    flowConfChanges :: o -> C.Configuration -> Flow -> [a]
    -- used for incremental update of qmap
    affectedQueues  :: o -> C.Configuration -> a -> [QueueId]

type FlowCache s      = HB.HashTable s (PG.NLabel, Flow) PG.NPort

data SearchParams o a = (OracleSt o a) => SearchParams {
      sOracle     :: o
    , sPrgU       :: PG.PGraph
    , sCostFn     :: CostQueueFn -- cost function
    , sStrategy   :: SearchStrategy o a
    , sOrderFlows :: [Flow] -> [Flow] -- order (sort) flows as a heuristic
}

type SearchStrategy o a = (OracleSt o a) =>
  SearchSt s o a -> [Flow] -> ST.ST s C.Configuration


flowConfs :: (ConfChange a, OracleSt o a)
          => o -> C.Configuration -> Flow -> [C.Configuration]
flowConfs x conf f = map (applyConfChange conf) changes
    where changes = flowConfChanges x conf f

flowsSingleConfChanges :: (ConfChange a, OracleSt o a)
                       => o -> C.Configuration -> [Flow] -> [(Flow, a)]
flowsSingleConfChanges x c flows = L.concat res
   where res = [ [(f,cc) | cc <- flowConfChanges x c f ] | f <- flows]

flowsSingleConfs :: (ConfChange a, OracleSt o a)
                 => o -> C.Configuration -> [Flow] -> [(Flow, C.Configuration)]
flowsSingleConfs x conf fs =
    [(fl, applyConfChange conf change) | (fl,change) <- flowsSingleConfChanges x conf fs]


data E10kConfChange = E10kInsert5T PG.ConfValue

instance ConfChange E10kConfChange where
    --applyConfChange :: C.Configuration -> E10kConfChange -> C.Configuration
    applyConfChange conf (E10kInsert5T c5t) = ("RxC5TupleFilter", new5t):rest
        where new5t :: PG.ConfValue
              new5t = addToCVL old5t c5t
              old5t :: PG.ConfValue
              old5t = case L.lookup "RxC5TupleFilter" conf of
                        Just l -> l
                        Nothing -> error "add5TupleToConf: Did not find RxC5TupleFilter"

              rest :: C.Configuration
              rest  = L.filter ((/="RxC5TupleFilter") . fst) conf


-- E10K (simple for now) oracle
newtype E10kOracleSt = E10kOracleSt {nQueues :: Int }
e10kDefaultQ = 0

instance OracleSt E10kOracleSt E10kConfChange where
    flowConfChanges E10kOracleSt { nQueues = nq } c fl =
        [E10kInsert5T $ mk5TupleFromFl fl q |  q <- allQueues nq]
    emptyConf _ = e10kCfgEmpty
    showConf _  = e10kCfgStr

    affectedQueues E10kOracleSt { nQueues = nq } conf (E10kInsert5T c5t) =
        [e10kDefaultQ, xq]
        where xq = E10k.c5tQueue $ E10k.parse5t c5t

initSearchParams :: (OracleSt o a) => SearchParams o a
initSearchParams = SearchParams {
      sOracle = undefined
    , sPrgU   = undefined
    , sCostFn = undefined
    , sStrategy = undefined
    , sOrderFlows = id
}

data SearchSt s o a = (OracleSt o a) => SearchSt {
      sParams     :: SearchParams o a
    , sFlowCache  :: FlowCache s
}

-- initialize search state (add default values when applicable)
initSearchSt :: (OracleSt o a) => SearchParams o a -> ST.ST s (SearchSt s o a)
initSearchSt params = do
    --h <- H.newSized 100000
    h <- H.new
    return $ SearchSt {
          sParams     = params
        , sFlowCache  = h
    }

runSearch :: (OracleSt o a) => SearchParams o a -> [Flow] -> C.Configuration
runSearch params flows = ST.runST $ do
   st <- initSearchSt params
   doSearch st flows

-- NOTE: There is still some code around trying to compute qmap with
-- different ways:
--  - E10k.flowQueue: old E10k-specific version of flowQueue
--  - yQmap and xQmap
qMap_ :: (OracleSt o a) =>
         SearchSt s o a -> PG.PGraph -> [Flow] -> ST.ST s QMap
qMap_ st prgC flows = do
    let flowC = sFlowCache st
    qmap <- forM flows $ \fl -> do
        q <- flowQueue flowC prgC fl
        return (fl, q)
    return qmap

qMap :: (OracleSt o a)
     => SearchSt s o a -> C.Configuration -> a -> [Flow] -> ST.ST s QMap
qMap st conf confChange flows = do
    let prgU = sPrgU $ sParams st
        newConf = applyConfChange conf confChange
        prgC = C.applyConfig newConf prgU
    qMap_ st prgC flows

qmapHT :: QMap -> ST.ST s (HB.HashTable s Flow QueueId)
qmapHT qmap = H.fromList qmap

qMapIncremental :: (OracleSt o a)
                => SearchSt s o a -> C.Configuration -> a -> [Flow] -> QMap
                -> ST.ST s (PG.PGraph, C.Configuration, QMap)
qMapIncremental st conf confChange flows oldQmap_ = do
    let prgU      = sPrgU $ sParams st
        oracle    = sOracle $ sParams st
        newConf   = applyConfChange conf confChange
        prgC      = C.applyConfig newConf prgU
        invalidQs = affectedQueues oracle conf confChange
        flowC     = sFlowCache st
        oldQmap   = M.fromList oldQmap_
    qmap <- forM flows $ \fl -> do
        q <- case M.lookup fl oldQmap of
            Nothing -> flowQueue flowC prgC fl
            Just oldQ  -> case oldQ `elem` invalidQs of
                False -> return oldQ
                True  -> flowQueue flowC prgC fl
        return (fl, q)

    {-
    qmapCorrect <- qMap_ st prgC flows
    let test = qmap == qmapCorrect
        qmap' = case test of
            True  -> qmap
            False -> error $ "Incremental qmap failed"
    -}

    return (prgC, newConf, qmap)


doSearch :: (OracleSt o a)
         => (SearchSt s o a) -> [Flow] -> ST.ST s C.Configuration
doSearch st flows = do
    let params = sParams st
        costFn = sCostFn params
        sortFn = sOrderFlows params
        oracle = sOracle params
        search = sStrategy params
        prgU   = sPrgU params
        flowC  = sFlowCache st

    let flows' = sortFn flows
    conf <- search st flows'
    return conf
    --ht_size <- length <$> H.toList flowC
    --return $ tr conf ("SIZE: " ++ (show ht_size))

-- searchGreedyFlows: examine one flow at a time. Depends on the ordering of
-- flows. We can use that as a heuristic
searchGreedyFlows :: SearchStrategy o a
searchGreedyFlows st flows = searchGreedyFlows_ st x0 flows
    where x0 = (emptyConf $ sOracle $ sParams st, [], [])

searchGreedyFlows_ :: (OracleSt o a)
                   => SearchSt s o a
                   -> (C.Configuration, [Flow], QMap)
                   -> [Flow]
                   -> ST.ST s C.Configuration

searchGreedyFlows_ st (cnf,_,_) [] = return $ trN cnf msg
    where msg = ("searchGreedyFlows_:" ++ (showConf (sOracle $ sParams st) cnf))

searchGreedyFlows_ st (curCnf, curFlows, curQmap) (f:fs) = do
    let oracle      = sOracle $ sParams st
        costFn      = sCostFn $ sParams st
        prgU        = sPrgU $ sParams st
        confChanges = flowConfChanges oracle curCnf f
        newCurFs    = f:curFlows

    conf_costs <- forM confChanges $ \cc -> do
        (prgC, newCnf, qmap) <- zQmapIncremental st curCnf cc newCurFs  curQmap
        return (newCnf, costFn qmap, qmap)

    let snd3 (_,x,_) = x
        (best_cnf, lower_cost, qmap') =  L.minimumBy (compare `on` snd3) conf_costs
        msg = "searchGreedyFlows_: step:"
            ++ (show $ length curFlows)
            ++ " cost is " ++ (show lower_cost)
            ++ "\nSELECTED " ++ (showConf oracle best_cnf)

    recurse <- searchGreedyFlows_ st (best_cnf, newCurFs, qmap') fs
    return $ trN recurse msg

-- searchGreedyConf :
--  examines all flows, and determines a single
--  configuration value. Removes the flow that is paired with the configuration
--  values and moves on. This can avoid some problems of the previous one
--  (it really depends on the cost function), but is more expensive.
searchGreedyConf :: SearchStrategy o a
searchGreedyConf st flows = searchGreedyConf_ st x0 flows
    where x0 = (emptyConf $ sOracle $ sParams st, []) -- initial state

searchGreedyConf_ :: OracleSt o a
                   => SearchSt s o a
                   -> (C.Configuration, [Flow])
                   -> [Flow]
                   -> ST.ST s C.Configuration

searchGreedyConf_ st (cnf,_) [] = return $ trN cnf msg
    where msg = ("searchGreedyConf_:" ++ (showConf (sOracle $ sParams st) cnf))

searchGreedyConf_ st (curCnf, curFlows) flows = do
    let oracle      = sOracle $ sParams st
        costFn      = sCostFn $ sParams st
        prgU        = sPrgU $ sParams st
        confs :: [(Flow, C.Configuration)]
        confs = flowsSingleConfs oracle curCnf flows
        all_flows = curFlows ++ flows

    costs <- forM confs $ \x@(fl, cnf) -> do
        let prgC = C.applyConfig cnf prgU
        qmap <- qMap_ st prgC all_flows
        return (x, costFn qmap)

    let ((xFl, xCnf), xCost) = L.minimumBy (compare `on` snd) costs
        xFlows = L.delete xFl flows

    recurse <- searchGreedyConf_ st (xCnf, xFl:curFlows) xFlows

    let msg = "searchGreedyConf_: step:"
            ++ (show $ length curFlows)
            ++ " cost is " ++ (show xCost)
            ++ "\nSELECTED "
            ++ (e10kCfgStr xCnf)
            ++ "\nALL CONFS"
    return $ trN recurse msg

--
-- import Control.Parallel.Strategies as P

nLabelNode :: PG.PGraph -> String -> PG.PGNode
nLabelNode g l = case GH.filterNodesByL (\x -> (PG.nLabel x) == l) g of
    []  -> error $ "nLabelnode: node" ++ l ++ "node found"
    [x] -> x
    _  -> error $ "nLabelnode: more that one " ++ l ++ "node found"

nLabelPred :: PG.PGraph -> String -> [(PG.PGNode, PR.PredExpr)]
nLabelPred g l = [ (n, PR.nodePred g n) | n <- nodes ]
    where nodes = GH.filterNodesByL (\x -> (PG.nLabel x) == l) g

nLabelSinglePred :: PG.PGraph -> String -> PR.PredExpr
nLabelSinglePred g l = case nLabelPred g l of
    [x] -> snd x
    []  -> error "nLabelSinglePred: no node found"
    _   -> error "more than one nodes found"

xQmap :: PG.PGraph -> [Flow] -> [(Flow, QueueId)]
xQmap gr fls = qmap
    where flPreds = map flowPred fls
          allQs  = allQueues nQueues
          qNodes = [ nLabelNode gr $ rxQPref ++ (show i) | i <- allQs ]
          qPreds = PR.computePredMany gr qNodes
          --qPreds = map (nLabelSinglePred gr) qNodes
          qmap   = [ (fl, qid) | (qid,qpred) <- zip allQs qPreds,
                                 (fl,flpred) <- zip fls flPreds,
                                  check (qid,qpred) (fl,flpred)]
          nQueues = 10
          bld = PR.predBuildDNF
          check (qid,p1) (fl,p2) = trN sat msg
            where expr = PR.buildAND bld $ [p1,p2]
                  sat  = isJust sat_
                  sat_  = PR.dnfSAT expr
                  msg = "QID:    " ++ (ppShow qid) ++
                        "\nQ pred: " ++ (ppShow p1) ++
                        "\nFL:     " ++ (ppShow fl) ++
                        "\nFL pred: " ++ (ppShow p2) ++
                        "\nAND:     " ++ (ppShow expr) ++
                        "\nSAT:"      ++ (ppShow $ sat_)

-- This assumes that each flow is mapped on a single queue
-- (xQmap does not)
yQmap :: PG.PGraph -> [Flow] -> [(Flow, QueueId)]
yQmap gr fls = qmap
    where flPreds = map flowPred fls
          allQs  = allQueues nQueues
          --qNodes = [ rxQPref ++ (show i) | i <- allQs ]
          --qPreds = map (nLabelSinglePred gr) qNodes
          qNodes = [ nLabelNode gr $ rxQPref ++ (show i) | i <- allQs ]
          qPreds = PR.computePredMany gr qNodes
          qPreds' = zip allQs qPreds
          qmap   =  map
                   (\flt -> (fst flt, getQmap flt qPreds'))
                   (zip fls flPreds)

          getQmap :: (Flow, PR.PredExpr) -> [(QueueId, PR.PredExpr)] -> QueueId
          getQmap f (q:qs)  = case check q f of
                              True  -> fst q
                              False -> getQmap f qs
          getQmap f []      = error "yQmap: This is not supposed to happen"

          nQueues = 10
          bld = PR.predBuildDNF
          check (qid,p1) (fl,p2) = trN sat msg
            where expr = PR.buildAND bld $ [p1,p2]
                  sat  = isJust sat_
                  sat_  = PR.dnfSAT expr
                  msg = "QID:    " ++ (ppShow qid) ++
                        "\nQ pred: " ++ (ppShow p1) ++
                        "\nFL:     " ++ (ppShow fl) ++
                        "\nFL pred: " ++ (ppShow p2) ++
                        "\nAND:     " ++ (ppShow expr) ++
                        "\nSAT:"      ++ (ppShow $ sat_)


-- http://rosettacode.org/wiki/Standard_deviation#Haskell
sd :: RealFloat a => [a] -> a
sd l = sqrt $ sum (map ((^2) . subtract mean) l) / n
  where n = L.genericLength l
        mean = sum l / n

balanceCost_ :: [QueueId] -> QMap -> Cost
balanceCost_ allQs qmap = ret
    where qLoad :: M.Map QueueId Integer
          qList  =  [qId | (_,qId) <- qmap]
          qLoad = L.foldl foldFn M.empty qList
          foldFn :: M.Map QueueId Integer -> QueueId -> M.Map QueueId Integer
          foldFn m qid = M.insertWith (+) qid 1 m

          getQLoad :: QueueId -> Integer
          getQLoad q = M.findWithDefault 0 q qLoad
          load    = map getQLoad allQs
          --maxLoad = maximum load
          --minLoad = minimum load
          --ret_     =  CostVal $ fromIntegral $ maxLoad - minLoad
          ret_ = CostVal $ sd $ map fromIntegral load
          ret = trN ret_ ("\nqlist" ++ (ppShow qList) ++ "\nLOAD:" ++ ppShow (load))

dummyCost :: [(Flow, QueueId)] -> Cost
dummyCost _ = CostVal 1.0

balanceCost :: Int -> QMap -> Cost
balanceCost nq fls = balanceCost_ (allQueues nq) fls

-- gold/best-effort priorities
priorityCost :: (Flow -> Bool) -> Integer -> Int -> QMap -> Cost
priorityCost isGold goldFlowsPerQ nq qmap = trN cost msg
    where (goldFls, beFls) = L.partition (isGold . fst) qmap
          goldQs = S.fromList $ [qid | (_,qid) <- goldFls]
          beQs   = S.fromList $ [qid | (_,qid) <- beFls]
          allQs  = S.fromList (allQueues nq)
          restQs = allQs `S.difference` goldQs
          goldQsExcl = goldQs `S.difference` beQs
          beQsExcl   = beQs `S.difference` goldQs
          msg = "qmap:\n" ++ (ppShow qmap)
                ++ "\ngoldQs:" ++ (ppShow goldQs)
                ++ "\ngoldQsExcl:" ++ (ppShow goldQsExcl)
                ++ "\ngoldNQs:" ++ (ppShow goldNQs)
                ++ "\nbeQs:" ++ (ppShow beQs)
                ++ "\ncost" ++ (ppShow cost)
          cost
            -- no gold flows: just balance best-effort across all queues
            | length goldFls == 0 = balanceCost_ (allQueues nq) beFls
            -- no best effort flows: just balance gold across all queues
            | length beFls   == 0 = balanceCost_ (allQueues nq)  goldFls
            -- we should have enough queues for the gold class
            | goldNeeded > 0 = CostReject $ 100*(fromIntegral goldNeeded)
            -- we should have enough queues for the best effort class
            | beNeeded > 0 = CostReject $ (fromIntegral beNeeded)
            -- if all is OK, it depends on how well the classes are balanced
            | otherwise               = CostVal $ balGold + balBe

          -- determine number of gold queues
          goldNQs = (min $ nq -1)
                   $ ceiling
                   $ (toRational $ length goldFls) / (toRational goldFlowsPerQ)
          goldNeeded = goldNQs - (S.size goldQsExcl)
          -- number of best efforst queues
          beNQs = nq - goldNQs
          beNeeded = beNQs - (S.size beQsExcl)

          CostVal balGold = balanceCost_ (S.toList goldQs) goldFls
          CostVal balBe   = balanceCost_ (S.toList restQs) beFls

---- greedy back-track search. Nothing is returned if no suitable configuration is
---- found
--data SearchGBSt = SearchGBSt {
--      -- this is actually a stack. The top of the list contains the last
--      -- endpoint we added, and the current configuration
--      sgbOptPath  :: [(Flow, C.Configuration)]
--    , sgbCostFn   :: CostFn
--    , sgbOrdEs    :: [Flow] -> [Flow]
--}
----
---- initial state
--searchGBSt0 fn = SearchGBSt {
--      sgbOptPath  = []
--    , sgbCostFn   = fn
--    , sgbOrdEs    = id
--}
--
---- greedy back-track search. Nothing is returned if no suitable configuration is
-- found
--searchGBaddFL :: Int
--              -> SearchGBSt
--              -> [Flow]
--              -> Maybe (SearchGBSt, C.Configuration)
--searchGBaddFL nq st (f:fs) = error "foo"
--    where ret = case best_cost of
--                CostReject -> backtrack
--                _          -> onward
--
--          costF = sgbCostFn st
--          path  = sgbOptPath st
--          curConf = case length path of
--                      0 -> []
--                      _ -> snd $ head path
--          confs = flsAllConfs nq curConf [f]
--          conf_costs = [(cnf,costF cnf) | cnf <- confs]
--          (best_cnf,best_cost) = L.minimumBy (compare `on` snd) conf_costs
--
--          onward = case length fs of
--                      0 -> Just (onwardSt, best_cnf)
--                      _ -> searchGBaddFL onwardSt fs
--          onwardSt = st {sgbOptPath = (f, best_cnf):path }
--
--          -- backtrack
--          backtrack  = case (length path, same_order) of
--                        -- nowhere to go
--                        (0, _) -> Nothing
--                        -- if the order is the same we are doing the same thing
--                        -- again, so give up. Hopefully, this is enough to
--                        -- protect us from cycles
--                        (_, True)  -> Nothing
--                        (_, False) -> searchGBaddFL backSt backEs
--          backSt = st { sgbOptPath = (drop 1 path) }
--          backEs = (fst $ head path):fs
--          backEsReord = (sgbOrdEs st) backEs
--          same_order  = backEs == backEsReord

mk5TupleFromFl :: Flow -> QueueId -> PG.ConfValue
mk5TupleFromFl fl@(FlowUDPv4 {}) q =
    PG.CVTuple [ cvMInt $ sIP,
    cvMInt $ dIP,
    PG.CVMaybe $ Just $ PG.CVEnum 1,
    cvMInt $ sP,
    cvMInt $ dP,
    PG.CVInt prio,
    PG.CVInt $ fromIntegral q]
    where
       sIP  = flSrcIp   fl
       dIP  = flDstIp   fl
       sP   = flSrcPort fl
       dP   = flDstPort fl
       prio = 1

add5TupleToConf :: C.Configuration -> Flow -> QueueId -> C.Configuration
add5TupleToConf conf fl q = ("RxC5TupleFilter", new5t):rest
    where new5t :: PG.ConfValue
          new5t  = addToCVL old5t (mk5TupleFromFl fl q)
          old5t :: PG.ConfValue
          old5t = case L.lookup "RxC5TupleFilter" conf of
                    Just l -> l
                    Nothing -> error "add5TupleToConf: Did not find RxC5TupleFilter"

          rest :: C.Configuration
          rest  = L.filter ((/="RxC5TupleFilter") . fst) conf

addToCVL :: PG.ConfValue -> PG.ConfValue -> PG.ConfValue
addToCVL (PG.CVList l) v = PG.CVList $ v:l

cvMInt :: Integral a => Maybe a -> PG.ConfValue
cvMInt mi =  PG.CVMaybe $ (PG.CVInt . fromIntegral) <$> mi

---

type FlowQSt = Int

reachedQueue :: PG.PGNode -> Maybe QueueId
reachedQueue (_,PG.ONode {PG.nLabel = name}) = ret
    where n = filter isDigit name
          ret = case length n of
                  0 -> Nothing
                  _ -> case "RxQ" ++ n ++ "Valid" == name of
                            True  -> Just $ read n
                            False -> Nothing
reachedQueue (_,PG.FNode {PG.nLabel = name})
    | name == "RxToDefaultQueue" =  Just 0
    | otherwise  = Nothing

doFlowQueueNextPort_ :: PR.PredExpr -> [(PG.NPort, PR.PredExpr)] -> PG.NPort
doFlowQueueNextPort_ flPred ((port,portPred_):rest) = ret
    where sat     = isJust $ PR.dnfSAT andExpr
          andExpr = (PR.buildAND bld) [portPred,flPred]
          portPred = PR.predDoBuild bld portPred_
          bld = PR.predBuildDNF
          -- NB: we assume that each flow is assigned exclusively to a single
          -- queue, so we return the first match
          ret = case sat of
                True  -> port
                False -> doFlowQueueNextPort_ flPred rest

doFlowNextPort :: (Flow, PR.PredExpr) -> PG.Node -> ST.ST s PG.NPort
doFlowNextPort (_, flPred) node = return $ doFlowQueueNextPort_ flPred nPreds
    where nPreds = PG.nPredicates node

doFlowNextPortCache :: FlowCache s
                    -> (Flow, PR.PredExpr)
                    -> PG.Node
                    -> ST.ST s PG.NPort
doFlowNextPortCache flowCache x@(fl, flPred) node = do
    let key = (PG.nLabel node, fl)
    ret <- H.lookup flowCache key
    case ret of
        Just x -> return $ trN x "HIT"
        Nothing -> do
            ret <- doFlowNextPort x node
            H.insert flowCache key ret
            return $ trN ret "MISS"


doFlowQueue :: FlowCache s -> PG.PGGDecomp -> (Flow, PR.PredExpr)
            -> ST.ST s QueueId
--doFlowQueue fc g (nid,node@(PG.FNode {PG.nLabel = lbl})) flowT
doFlowQueue fc d@(ctx, g) flowT
    | Just q <- reachedQueue lnode = return q
    | otherwise = do
          nextPort <- case node of
                PG.FNode {} -> doFlowNextPortCache fc flowT (snd lnode)
                -- In theory we need to determine the next port based on the
                -- incoming port.  However, currently there is no way to
                -- determine what are the "true" incoming edges, so we resort
                -- into a hack where we always pick the "true" port as the
                -- nextPort when encountering an ONode
                PG.ONode {} -> return "true"
          let d' = sucPortDecomp d nextPort
          doFlowQueue fc d' flowT
    where lnode = DGI.labNode' ctx
          node = snd lnode
          lbl = PG.nLabel node

flowQueue :: FlowCache s -> PG.PGraph -> Flow -> ST.ST s QueueId
flowQueue fc prgC flow = do
    let flPred = flowPred flow
        d0 = flowQueueStart prgC
    doFlowQueue fc d0 (flow, flPred)

flowPortGroups :: FlowCache s -> PG.PGGDecomp -> [(Flow, PR.PredExpr)]
     -> ST.ST s [(PG.NPort, [(Flow, PR.PredExpr)])]
flowPortGroups fc d@(ctx, g) flowTs = do
    -- [(Flow, PredExpr, PG.NPort)]
    flowTPs <- forM flowTs $ \ flT@(fl,flPred) -> do
        p <- doFlowNextPortCache fc flT (snd $ DGI.labNode' ctx)
        return (fl, flPred, p)
    let  t3 (_, _, p) = p
         grouped :: [[(Flow, PR.PredExpr, PG.NPort)]]
         grouped = L.groupBy ((==) `on` t3) flowTPs
         ret = [ (t3 $ head l, [(f,p) | (f,p,_) <- l])  | l <- grouped]
    return ret

doZQmap :: FlowCache s -> PG.PGGDecomp -> [(Flow, PR.PredExpr)] -> ST.ST s QMap
doZQmap fc d@(ctx, g) flowTs
    | Just q <- reachedQueue lnode = return $ [(fl,q) | (fl,_) <- flowTs]
    | otherwise = do
        groups <- case node of
            PG.FNode {} -> flowPortGroups fc d flowTs
            PG.ONode {} -> return $ [("true", flowTs)]
        qmaps <- forM groups $ \ (nextPort,flowTs') -> do
            let d' = sucPortDecomp d nextPort
            doZQmap fc d' flowTs'
        return $ L.concat qmaps
    where lnode = DGI.labNode' ctx
          node = snd lnode
          lbl = PG.nLabel node


zQmap :: (OracleSt o a)
    => SearchSt s o a -> C.Configuration -> a -> [Flow]
    -> ST.ST s (PG.PGraph, C.Configuration, QMap)
zQmap st conf confChange flows = do
    let prgU      = sPrgU $ sParams st
        oracle    = sOracle $ sParams st
        newConf   = applyConfChange conf confChange
        prgC      = C.applyConfig newConf prgU
        flowC     = sFlowCache st
        d0        = flowQueueStart prgC
        flowTs    = [(fl, flowPred fl) | fl <- flows]
    qmap <- doZQmap flowC d0 flowTs

    {--
    qmapCorrect <- qMap_ st prgC flows
    let test = qmap == qmapCorrect
        qmap' = case test of
            True  -> qmap
            False -> error $ "Incremental qmap failed"
    --}

    return (prgC, newConf, qmap)

zQmapIncremental :: (OracleSt o a)
      => SearchSt s o a -> C.Configuration -> a -> [Flow] -> QMap
      -> ST.ST s (PG.PGraph, C.Configuration, QMap)
zQmapIncremental st conf confChange flows oldQmap_ = do
    let oracle    = sOracle $ sParams st
        invalidQs = affectedQueues oracle conf confChange
        oldQmap   = M.fromList oldQmap_

        foldFn :: ((QMap, [Flow]) -> Flow -> (QMap,[Flow]))
        foldFn (qmap, flows) fl = case M.lookup fl oldQmap of
            Nothing -> retFail
            Just oldQ -> case oldQ `elem` invalidQs of
                False -> ((fl,oldQ):qmap, flows)
                True  -> retFail
            where
                retFail = (qmap, fl:flows)
        (qmap0, restFlows) = L.foldl' foldFn ([],[]) flows

    (prgC, conf, qmap) <- zQmap st conf confChange restFlows

    return $ (prgC, conf, qmap0 ++ qmap)

sucPortCtx_ :: PG.PGContext -> PG.NPort -> DGI.Node
sucPortCtx_ ctx port = fst next
    where sucs = DGI.lsuc' ctx
          next = case L.find (\ (_,s) -> port == PGU.edgePort_ s) sucs of
                Just x  -> x
                Nothing -> error $ "Cannot find successor of node:" ++ nodeL ++ " on port:" ++ port
          nodeL = PG.nLabel $ DGI.lab' ctx

sucPortDecomp :: PG.PGGDecomp -> PG.NPort -> PG.PGGDecomp
sucPortDecomp (ctx, g) port = ret
    where next = sucPortCtx_ ctx port
          ret  = doMatch next g

doMatch :: DGI.Node -> PG.PGraph -> PG.PGGDecomp
doMatch nid g = case DGI.match nid g of
    (Just ctx, g') -> (ctx, g')
    (Nothing, _) -> error "doMatch failed"

flowQueueStart :: PG.PGraph -> PG.PGGDecomp
flowQueueStart prgC = d1
    where node0_ = "RxL2EtherClassifyL3_"
          port0 = "other"
          node0 = case GH.findNodeByL (\x -> (PG.nLabel x) == node0_) prgC of
              Just x -> x
              Nothing -> error $ "More than one matches for node:" ++ node0_
          d0 :: PG.PGGDecomp
          d0 = doMatch (fst node0) prgC
          d1 = sucPortDecomp d0 port0

-- Code for performing simple tests

e10kT = E10k.graphH
e10kU = fst <$> e10kT
e10kH = snd <$> e10kT
e10kC = (C.applyConfig prgCfg) <$> e10kU

e10kT_simple = E10k.graphH_ "Graphs/E10k/prgE10kImpl-simple.unicorn"
e10kU_simple = fst <$> e10kT_simple
e10kH_simple = snd <$> e10kT_simple
e10kC_simple = (C.applyConfig prgCfg) <$> e10kU_simple

-- Let us for now consider that only new connections apper. For handling
-- connectiong going away, we can add a property to the endpoint description
-- about wheter a connection was added or removed so that the oracle can act
-- accordingly.
fs = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ 1000 + i
   , flSrcIp    = Nothing
   , flSrcPort  = Nothing } | i <- [1..40] ]

isGoldFl FlowUDPv4 {flDstPort = Just port} = isJust $ L.find (==port) [1001,1002]
goldFlPerQ = 1
priorityCost' = priorityCost isGoldFl goldFlPerQ


test = do
    -- unconfigured PRG
    prgU <- e10kU
    --prgU <- e10kU_simple
    let nq = 10
        e10kOracle   = E10kOracleSt {nQueues = nq}
        priFn        = (priorityCost' nq)
        balFn        = (balanceCost nq)
        dummyFn      = dummyCost

        costFn = balFn
        params = initSearchParams {  sOracle = e10kOracle
                                   , sPrgU   = prgU
                                   , sCostFn = costFn
                                   , sStrategy = searchGreedyFlows}

        conf = runSearch params fs
    return ()


--fs2 = [ FlowUDPv4 {
--     flDstIp    = Just 127
--   , flDstPort  = Just $ fromIntegral $ 7777
--   , flSrcIp    = IP4.ipFromString "10.113.4.51"
--   , flSrcPort  = Just $ fromIntegral $ 8000 + i } | i <- [0..30] ]
--
---- These two should be the gold flows
---- ziger1 : 10.113.4.51:8000
---- ziger2 : 10.113.4.57:8000
--
--myFromMaybe (Just x) = x
--myFromMaybe _ = error "No IP address"
--
--isGoldFl2M FlowUDPv4 {flSrcPort = Just sport, flSrcIp = Just sip} = ans
--    where
--        ans
--            |           (sport == 8000) && (sip == (myFromMaybe $ IP4.ipFromString "10.113.4.51")) =  True
--            |           (sport == 8000) && (sip == (myFromMaybe $ IP4.ipFromString "10.113.4.57")) =  True
--            | otherwise = False
--isGoldFl2M _ = False
--
--
--sortFlows isImp fl = sortedFlows
--    where
--        allFlows = fl
--        impList = filter isImp $ allFlows
--        otherList = filter (not . isImp) allFlows
--        sortedFlows = impList ++  otherList
--
--fs2Sorted = sortFlows isGoldFl2M   fs2
--
--
----sortedRealFlows = sortFlows isGoldFl2M real40Flows
--
---- All flows are:
--real40Flows = [FlowUDPv4 {flSrcIp = Just 175178803, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
--            FlowUDPv4 {flSrcIp = Just 175178803, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
--            FlowUDPv4 {flSrcIp = Just 175178803, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
--            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
--            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
--            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
--            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
--            FlowUDPv4 {flSrcIp = Just 175178809, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
--            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
--            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
--            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
--            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
--            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
--            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
--            FlowUDPv4 {flSrcIp = Just 175178819, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
--            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
--            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
--            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
--            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
--            FlowUDPv4 {flSrcIp = Just 175178772, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
--            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
--            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
--            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
--            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
--            FlowUDPv4 {flSrcIp = Just 175178778, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
--            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
--            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
--            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
--            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
--            FlowUDPv4 {flSrcIp = Just 175178781, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
--            FlowUDPv4 {flSrcIp = Just 175178847, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
--            FlowUDPv4 {flSrcIp = Just 175178803, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
--            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
--            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
--            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002},
--            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8003},
--            FlowUDPv4 {flSrcIp = Just 175178823, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8004},
--            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8000},
--            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8001},
--            FlowUDPv4 {flSrcIp = Just 175178848, flDstIp = Just 175178847, flDstPort = Just 7777, flSrcPort = Just 8002}]

