{-# LANGUAGE RankNTypes, LiberalTypeSynonyms, ExistentialQuantification,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             ScopedTypeVariables, FunctionalDependencies #-}
-- Simple search functions
module Dragonet.Search (
  SearchParams(..),
  initSearchParams,
  SearchSt(..),
  initSearchSt,
  runSearch,
  OracleSt(..),
  sfOracleInit,
  searchGreedyFlows,
  hardcodedSearch,
  balanceCost,
  priorityCost,
  CostQueueFn,
  SearchStIO, initSearchIO, runSearchIO,
  E10kOracleSt(..),
  E10kOracleHardCoded(..),
  initSearchParamsE10k,

  IncrSearchParams(..),
  initIncrSearchParams,
  runIncrSearch,

  test, test_incr,
) where

import qualified Dragonet.Configuration       as C
import qualified Dragonet.ProtocolGraph       as PG
import qualified Util.GraphHelpers            as GH
import qualified Dragonet.Predicate           as PR
import qualified Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.FlowMap             as FM

import Dragonet.DotGenerator (toDot)
import Dragonet.Conventions (rxQPref, isTruePort, isFalsePort, QueueId)
import Dragonet.Flows(Flow (..), flowPred, flowStr)

import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Graphs.E10k                  as E10k

import Graphs.Cfg (prgCfg)

import qualified Data.List            as L
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import qualified Data.Graph.Inductive as DGI

import Data.Maybe
import Data.Int
import Data.Function (on)
import Data.Char (isDigit)
import qualified Data.Word as DW

import qualified Control.Monad.ST        as ST

import Control.Monad (forM, foldM)
import qualified Data.HashTable.ST.Basic as HB
import qualified Data.HashTable.Class    as H

import Control.Applicative ((<$>))

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

tr a b  = trace b a
trN a b = a

allQueues :: Int -> [QueueId]
allQueues nq = [1..(nq-1)] ++ [0] :: [QueueId]

-- NB: due to ordering, costOK will always have the smaller value, and
-- costReject will always have the highest value.
-- NB: instead of using L.minimum we can implement a function that  can
-- short-circuit CostOK to save some iterations.
-- e.g., L.minimum $ CostOK:[CostVal $ 0.1 + i | i <- [1..]]
-- will never return
data Cost = CostOK         |
            CostVal Float  |
            CostReject Float -- reject value allows to rate rejected solutions
    deriving (Eq, Ord, Show)

type QMap        = [(Flow, QueueId)]
--type QMap        = M.Map Flow QueueId

qmapStr_ :: (Flow,QueueId) -> String
qmapStr_ (f,q) = (flowStr f) ++ "-> Q" ++ (show q)

qmapStr :: [(Flow,QueueId)] -> String
qmapStr flows = "QMAP:\n" ++ L.intercalate "\n" [ "  " ++ qmapStr_ f | f <- flows ]

-- There are two types of cost functions:
-- cost functions based on how flows are mapped into queues
--   User-defined cost functions use this form
type CostQueueFn = QMap -> Cost
-- the basic form of a cost function for the search algorithms
type CostFn a s = (C.ConfChange a)
                => [Flow] -> C.Configuration -> a -> ST.ST s Cost


{-|
  Class 'OracleSt' is the NIC-specific implementation for an Oracle
  It provides means to iterate the configuration space,
  and means to initialize and show the configuration.
  NOTE: (we will probably have to rethink the interface)
  TODO: Understand what '|' means in class declaration --PS
        see: https://www.haskell.org/haskellwiki/Functional_dependencies --AKK
-}
class (C.ConfChange a) => OracleSt o a | o -> a where
    -- sometimes we have an oracle instance, so it's much easier to use it
    -- to print a configuration that using C.showConfig
    showConf   :: o -> C.Configuration -> String
    showConf _ c = C.showConfig (undefined::a) c

    -- | 'flowConfChanges' returns a list of configuration changes for a
    -- flow. Note that the list may be empty, if there are no available
    -- configuration changes that map to the given flow.
    flowConfChanges ::
        -- | Oracle
           o
        -- | Current configuration
        -> C.Configuration
        -- | New flow to consider
        -> Flow
        -- | A set of configuration changes
        -> [a]

    -- affected queues: used for incremental update of qmap
    -- | Returns the queues that may get affected by this change.
    -- | Note that it always includes the default queue as it will
    -- | get affected as some traffic will stop appearing there.
    affectedQueues  :: o -> C.Configuration -> a -> [QueueId]


{-|
 The class 'SearchParams' contains the search parameters
-}
data SearchParams o a = (OracleSt o a) => SearchParams {
    -- | The actual Oracle function
      sOracle     :: o
    -- | PRG graph of the underlying NIC which will be used for reasoning
    , sPrgU       :: PG.PGraph
    -- | A cost function to evaluate and compare different configurations.
    -- |  function with minimum cost will be selected.  Example cost functions
    -- |  are balance, priority, etc.
    , sCostFn     :: CostQueueFn -- cost function
    -- | Strategy about how and which configuation space should be selected
    --      for evaluation.  Example strategies: greedy search, all space.
    , sStrategy   :: SearchStrategy o a
    -- | Order (sort) flows as a heuristic
    , sOrderFlows :: [Flow] -> [Flow]
}

{-|
  This is the type which will be used by other part of the code to
  encapsulate everything about searching configuration space.
 -}
type SearchStrategy o a = (OracleSt o a) =>
  SearchSt s o a -> [Flow] -> ST.ST s C.Configuration


flowConfs :: (C.ConfChange a, OracleSt o a)
          => o -> C.Configuration -> Flow -> [C.Configuration]
flowConfs x conf f = map (C.applyConfChange conf) changes
    where changes = flowConfChanges x conf f

flowsSingleConfChanges :: (C.ConfChange a, OracleSt o a)
                       => o -> C.Configuration -> [Flow] -> [(Flow, a)]
flowsSingleConfChanges x c flows = L.concat res
   where res = [ [(f,cc) | cc <- flowConfChanges x c f ] | f <- flows]

flowsSingleConfs :: (C.ConfChange a, OracleSt o a)
                 => o -> C.Configuration -> [Flow] -> [(Flow, C.Configuration)]
flowsSingleConfs x conf fs =
    [(fl, C.applyConfChange conf change) | (fl,change) <- flowsSingleConfChanges x conf fs]


-- E10K (simple for now) oracle
newtype E10kOracleSt = E10kOracleSt {nQueues :: Int }
e10kDefaultQ = 0

{-|
  Making the type 'E10kOracleSt' member of class "Oracle state" to work as
  as Oracle for Intel 82599 NIC.
  It will provide a way to generate initial empty configuation, showing
  configuration, and querying about affected queues from current configuration.
 -}
instance OracleSt E10kOracleSt E10k.ConfChange where
    {-|
     - Basic implementation which suggests a putting current flow in all
     - the queues, generating nQueue number of configurations.
     - This implementation does not care for current state of queues,
     -  and counts on cost-function to help in selecting proper queue.
    -}
    flowConfChanges = e10kFlowConfChanges

    -- QUESTION: Is it assumed that there will be only one operation in
    --      conf change?
    -- affectedQueues E10kOracleSt { nQueues = nq } _ _ = allQueues nq
    affectedQueues E10kOracleSt { nQueues = nq } conf (E10k.Insert5T c5t) =
        [e10kDefaultQ, xq]
        where xq = E10k.c5tQueue $ E10k.parse5t c5t
    affectedQueues E10kOracleSt { nQueues = nq } conf (E10k.InsertFDir cFdir) =
        [e10kDefaultQ, xq]
        where xq = E10k.cfdtQueue $ E10k.parseFDT cFdir

-- FIXME: FDir filters are not working properly with priority
-- TODO: avoid symmetric allocation
-- TODO: smart-wildcard allocation if possible?
e10kFlowConfChanges :: E10kOracleSt -> C.Configuration -> Flow
                    -> [E10k.ConfChange]
e10kFlowConfChanges E10kOracleSt {nQueues = nq} cnf fl
    -- allocate 5-tuple filters first, if we can
    | not rx5tFull  = [E10k.insert5tFromFl fl q | q <- allQs]
    | not rxCfdFull = catMaybes $ [E10k.insertFdirFromFl fl q | q <- allQs]
    | otherwise     = []
    where allQs = allQueues nq
          rx5tFull  = E10k.rx5tFilterTableFull cnf
          rxCfdFull = E10k.rxCfdFilterTableFull cnf

myThird (_,_,x) = x
mySnd (_,x,_) = x

-- E10K (simple for now) oracle
newtype E10kOracleHardCoded = E10kOracleHardCoded {
        nnHardcoded :: (Int, [(Flow, Int, Int)])
                    -- (totalQueues, [(Flow, Qid, FilterType(1==fidr, 2 == Ftuple))])
        }

{-|
  Making the type 'E10kOracleHardCoded' member of class "Oracle state" to work as
  as Oracle for Intel 82599 NIC.
  It will provide a way to generate initial empty configuation, showing
  configuration, and querying about affected queues from current configuration.
 -}
instance OracleSt E10kOracleHardCoded E10k.ConfChange where
    {-|
     - Basic implementation which suggests a putting current flow in presicely
     - fixed queue based on initial hardcoded values.
    -}
    flowConfChanges E10kOracleHardCoded { nnHardcoded = (nq, tbl) } c fl = ans
        where
        -- perform a lookup of a flow in the hardcoded table
        -- the lookup should give a filter type and queue-no.
        -- return that.
            f = filter (\(f, q, fil) -> f == fl) $ tbl

            ans
              | length(f) > 1 = error ("ERROR: there must be repeat flow, as more than one flow matches.")
              | length(f) == 0 = []
              | (myThird $ head f) == 1 = [E10k.InsertFDir $ fromJust $ E10k.mkFDirFromFl fl (mySnd $ head f)]
              | (myThird $ head f) == 2 = [E10k.Insert5T   $ fromJust $ E10k.mkFDirFromFl fl (mySnd $ head f)]
              | otherwise = error ("ERROR: wrong type of flow")


--    emptyConf _ = e10kCfgEmpty -- ^ Creates initial empty configuration
--    showConf _  = e10kCfgStr -- ^ Converts conf to string

    -- QUESTION: Is it assumed that there will be only one operation in
    --      conf change?
    -- affectedQueues E10kOracleHardCoded { nnQueues = nq } _ _ = allQueues nq
    affectedQueues E10kOracleHardCoded { nnHardcoded = nhw } conf (E10k.Insert5T c5t) =
        [e10kDefaultQ, xq]
        where xq = E10k.c5tQueue $ E10k.parse5t c5t
    affectedQueues E10kOracleHardCoded { nnHardcoded = nhw } conf (E10k.InsertFDir cFdir) =
        [e10kDefaultQ, xq]
        where xq = E10k.cfdtQueue $ E10k.parseFDT cFdir




type SFOracleSt = E10kOracleSt

sfOracleInit nqueues = E10kOracleSt { nQueues = nqueues }

initSearchParams :: (OracleSt o a) => SearchParams o a
initSearchParams = SearchParams {
      sOracle = undefined
    , sPrgU   = undefined
    , sCostFn = undefined
    , sStrategy = searchGreedyFlows
    , sOrderFlows = id
}

initSearchParamsE10k nqueues = initSearchParams {sOracle = oracle}
    where oracle = E10kOracleSt {nQueues = nqueues}

type FlowCache s      = HB.HashTable s (PG.NLabel, Flow) PG.NPort
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


type SearchStIO o a = SearchSt ST.RealWorld o a

initSearchIO :: (OracleSt o a)
              => SearchParams o a
              -> IO (SearchStIO o a)
initSearchIO params = ST.stToIO $ initSearchSt params

runSearchIO :: (OracleSt o a)
            => SearchStIO o a
            -> [Flow]
            -> IO C.Configuration
runSearchIO st flows = ST.stToIO $ doSearch st flows


{-|
 - Runs the given Oracle, on given set of flows, and returns the selected
 -  configuration
 -}
runSearch :: (OracleSt o a) =>
    SearchParams o a    -- ^ Oracle: in form of a group of all relevant parameters
    -> [Flow]           -- ^ List of flows for which configuration is to be searched
    -> C.Configuration  -- ^ Selected configuration
runSearch params flows = ST.runST $ do
   st <- initSearchSt params
   doSearch st flows


-- NOTE: There is still some code around trying to compute qmap with
-- different ways:
--  - E10k.FlowQueue.flowQueue: old E10k-specific version of flowQueue
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
        newConf = C.applyConfChange conf confChange
        prgC = C.applyConfig newConf prgU
    qMap_ st prgC flows

-- qmapHT :: QMap -> ST.ST s (HB.HashTable s Flow QueueId)
-- qmapHT qmap = H.fromList qmap

-- NOTE: This is not used, but is kept around for future reference
qMapIncremental :: (OracleSt o a)
                => SearchSt s o a -> C.Configuration -> a -> [Flow] -> QMap
                -> ST.ST s (PG.PGraph, C.Configuration, QMap)
qMapIncremental st conf confChange flows oldQmap_ = do
    let prgU      = sPrgU $ sParams st
        oracle    = sOracle $ sParams st
        newConf   = C.applyConfChange conf confChange
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


{-|
 - It sorts the flow list using the function 'sOrderFlows' from parameter
 - Then it applies the search strategy given in 'sStrategy' parameter
 -      with current search state to find a configuation
 -}
doSearch :: (OracleSt o a)
         => (SearchSt s o a)        -- ^ Current search state
         -> [Flow]                  -- ^ List of flows to evalaute
         -> ST.ST s C.Configuration -- ^ selected configuration
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

-- hardcodedSearch: return same conf irrespective of the current flow
hardcodedSearch :: forall o a. C.ConfChange a =>
                C.Configuration
            ->  SearchStrategy o a
hardcodedSearch hardConf st [] = return $
    [
        ("RxC5TupleFilter", PG.CVList []),
        ("RxCFDirFilter", PG.CVList [])
    ]
hardcodedSearch hardConf st fs = return $ hardConf

{-
            return $ trN confChanges_head msg
        where
        cnf0                = C.emptyConfig (undefined::a)
        oracle              = sOracle $ sParams st
        confChanges         = flowConfChanges oracle cnf0 $ head fs
        newConf             = C.applyConfChange cnf0 confChanges
        confChanges_head    = head confChanges
        msg                 = "hardcodedSearch: returning same conf"
-}
-- searchGreedyFlows: examine one flow at a time. Depends on the ordering of
-- flows. We can use that as a heuristic
searchGreedyFlows :: forall o a. C.ConfChange a => SearchStrategy o a
searchGreedyFlows st flows = searchGreedyFlows_ st x0 flows
    where x0 = (cnf0, [], [])
          cnf0 = C.emptyConfig (undefined::a)

{-|
 - Recursive search in configuration space where
 -      * We consider one flow at time, and find best configuration
 -          for that flow from current state.
 -      * Update current state by adding best configuration selected above
 -      * Recursively consider rest of the flow, but with updated current state
 -  Complexity of the algorithm
 -      * q == number of queues
 -      * n == number of flows
 -      * For every flow, we will evaluate 'O(q)' configurations
 -      * So, the overall complexity of [[searchGreedyFlows]] is
 -          'O(nq) * O(cost-functions)'
 -}
searchGreedyFlows_ :: (OracleSt o a)
                   -- | Current state of search algorithm
                   => SearchSt s o a
                   -- | Configuration buit till now for already configured flows
                   -> (C.Configuration, [Flow], QMap)
                   -- | Flows for which configuration should be checked
                   -> [Flow]
                   -- | Final configuration
                   -> ST.ST s C.Configuration

{-|
 - Terminating condidtion for recursion: when there are no more flows left,
 -  use the configuration built till now as solution configuration.
 -}
searchGreedyFlows_ st (cnf,_,_) [] = return $ trN cnf msg
    where msg = ("searchGreedyFlows_:" ++ (showConf oracle cnf))
          oracle = sOracle $ sParams st

{-|
 - Process single flow on top of the flow list
 - And apply recursion for rest of the list, with updated configuration
 -}
searchGreedyFlows_ st (curCnf, curFlows, curQmap) (f:fs) = do
    let oracle      = sOracle $ sParams st
        costFn      = sCostFn $ sParams st
        prgU        = sPrgU $ sParams st

        -- Configuration changes suggested by Oracle for adding single flow f
        confChanges  = flowConfChanges oracle curCnf f
        newCurFs = xtr f:curFlows
        -- spew out a warning when processing the same flow more than once,
        -- because it might lead to problems. One example is that it breaks the
        -- current assumption of affectedQueues when incrementally calculating
        -- the qmap
        xtr = case f `elem` curFlows of
            False -> id
            True ->  trace  ("Flow " ++ (flowStr f) ++ " was seen before!"
                             ++ " this might cause problems")


    -- Finding out the costs for all the configurations suggested by Oracle
    conf_costs <- forM confChanges $ \cc -> do
        (prgC, newCnf, qmap) <- zQmapIncremental st curCnf cc newCurFs  curQmap
        return (newCnf, costFn qmap, qmap)
    let msg_all_costs = L.intercalate "\n"
                           ["C" ++ (show idx) ++ "\n " ++ (showConf oracle cnf)
                             ++ "\n COST:" ++ (show cost)
                             ++ "\n " ++ (qmapStr qmap) ++ "\n"
                           | (idx,(cnf,cost,qmap)) <- zip [1..] conf_costs]

    -- selecting the configuration with minimum cost cost
    let snd3 (_,x,_) = x
        (best_cnf, lower_cost, qmap') =  L.minimumBy (compare `on` snd3) conf_costs
        msg = "searchGreedyFlows_: step:"
            ++ (show $ length curFlows)
            -- ++ "\n" ++ msg_all_costs
            ++ "  => lower cost is " ++ (show lower_cost)
            ++ "\n=> SELECTED configuration is " ++ (showConf oracle best_cnf)
            ++ "\n"

    -- Apply same stragegy for rest of the flows, but with modified running conf
    recurse <- searchGreedyFlows_ st (best_cnf, newCurFs, qmap') fs
    return $ trN recurse msg

-- searchGreedyConf :
--  examines all flows, and determines a single
--  configuration value. Removes the flow that is paired with the configuration
--  values and moves on. This can avoid some problems of the previous one
--  (it really depends on the cost function), but is more expensive.
searchGreedyConf :: forall o a. C.ConfChange a => SearchStrategy o a
searchGreedyConf st flows = searchGreedyConf_ st x0 flows
    where x0 = (cnf0, [])
          cnf0 = C.emptyConfig (undefined::a)

searchGreedyConf_ :: OracleSt o a
                   => SearchSt s o a
                   -> (C.Configuration, [Flow])
                   -> [Flow]
                   -> ST.ST s C.Configuration

searchGreedyConf_ st (cnf,_) [] = return $ trN cnf msg
    where msg = ("searchGreedyConf_:" ++ (showConf oracle cnf))
          oracle = sOracle $ sParams st

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
            ++ (E10k.cfgStr xCnf)
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

doFlowQueueNextPort__ :: PR.PredExpr -> [(PG.NPort, PR.PredExpr)] -> PG.NPort
doFlowQueueNextPort__ flPred aa = tr (doFlowQueueNextPort_ flPred aa) (
        "pred = [" ++ (show flPred) ++ "], list =[" ++ (show aa) ++ "]" )


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
doFlowNextPort (fl, flPred) node = return ret'
    where nPreds = PG.nPredicates node
          ret = doFlowQueueNextPort_ flPred nPreds
          ret' = trN ret $ "Flow:" ++ (flowStr fl) ++
                          " Node:"  ++ (PG.nLabel node) ++
                          " Port:" ++ ret

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
    | Just q <- reachedQueue lnode = return $ trN q
                                            $ "Flow:" ++ (flowStr $ fst flowT)
                                            ++ " Reached Queue:" ++ (show q)
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
        newConf   = C.applyConfChange conf confChange
        prgC      = C.applyConfig newConf prgU
        flowC     = sFlowCache st
        d0        = flowQueueStart prgC
        -- flow tuple: (flow, flow predicate)
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

-- this is what we are currently using
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


sucPortCtx_ :: PG.PGraph -> PG.PGContext -> PG.NPort -> DGI.Node
sucPortCtx_ g ctx port = fst next
    where sucs = DGI.lsuc' ctx
          sucsPort =  L.filter (\ (_,s) -> port == PGU.edgePort_ s) sucs
          next = case sucsPort of
                [x] -> x
                []  -> error $ "Cannot find successor of node:" ++ nodeL ++" on port:" ++ port
                xs  -> error $ "More than one successors on node:" ++ nodeL ++
                               " for port:" ++ port ++
                               " -> " ++  (show nextLs)
          nodeL = PG.nLabel $ DGI.lab' ctx
          nextLs = [ PG.nLabel n | (x,_) <- sucsPort
                                 , let n = fromJust $ DGI.lab g x ]

sucPortDecomp :: PG.PGGDecomp -> PG.NPort -> PG.PGGDecomp
sucPortDecomp (ctx, g) port = ret
    where next = sucPortCtx_ g ctx port
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


--------------------------------------------------------------------------------
-- Incremental search
-- [TODO: this file is getting too big]
--  Eventually, we should be using a list of ConfChange everywhere so that
--  we end up only applying in the driver the relevant configuration changes
--------------------------------------------------------------------------------
type IncrSearchStrategy o cc = (OracleSt o cc) =>
    IncrSearchSt s o cc -> [Flow] -> ST.ST s C.Configuration

data IncrSearchParams o cc = (OracleSt o cc) => IncrSearchParams {
      isOracle     :: o
    , isPrgU       :: PG.PGraph
    , isCostFn     :: CostQueueFn
    , isStrategy   :: IncrSearchStrategy o cc
    , isOrderFlows :: [Flow] -> [Flow]
}

initIncrSearchParams :: (OracleSt o a) => IncrSearchParams o a
initIncrSearchParams = IncrSearchParams {
      isOracle = undefined
    , isPrgU   = undefined
    , isCostFn = undefined
    , isStrategy = incSearchGreedyFlows
    , isOrderFlows = id
}

data IncrSearchSt s o cc = (OracleSt o cc) =>
    IncrSearchSt {
          isParams     :: IncrSearchParams o cc
        -- incremental map is implemented by maintaing a flow map
        -- here's its state
        , isFlowMapSt  :: FM.FlowMapSt s cc
}

-- initialize search state (add default values when applicable)
initIncrSearchSt :: (OracleSt o a) => IncrSearchParams o a -> ST.ST s (IncrSearchSt s o a)
initIncrSearchSt params = do
    fmSt <- FM.initFlowMapSt (isPrgU params)
    return $ IncrSearchSt {
          isParams     = params
        , isFlowMapSt  = fmSt
    }

runIncrSearch :: (OracleSt o a) =>
    IncrSearchParams o a    -- ^ Oracle: in form of a group of all relevant parameters
    -> [Flow]           -- ^ List of flows for which configuration is to be searched
    -> C.Configuration  -- ^ Selected configuration
runIncrSearch params flows = ST.runST $ do
   st <- initIncrSearchSt params
   doIncrSearch st flows

doIncrSearch :: (OracleSt o a)
         => (IncrSearchSt s o a)        -- ^ Current search state
         -> [Flow]                  -- ^ List of flows to evalaute
         -> ST.ST s C.Configuration -- ^ selected configuration
doIncrSearch st flows = do
    let params = isParams st
        costFn = isCostFn params
        sortFn = isOrderFlows params
        oracle = isOracle params
        search = isStrategy params
        prgU   = isPrgU params
    let flows' = sortFn flows
    conf <- search st flows'
    return conf

incSearchMinCost :: forall s cc. C.ConfChange cc
                 => CostQueueFn
                 -> FM.FlowMapSt s cc
                 -> Flow
                 -> [cc]
                 -> ST.ST s (FM.FlowMapSt s cc)
incSearchMinCost costFn fmSt flow (cc0:restCC) = do


    let foldFn :: (C.ConfChange cc)
               => (Cost, FM.FlowMapSt s cc)
               -> cc
               -> ST.ST s (Cost, FM.FlowMapSt s cc)
        foldFn (stCost, st) newCc = do
            (newQmap, newSt) <- incSearchQmap fmSt newCc flow
            let newCost_ = costFn newQmap
                newCost = trN newCost_ $ "New cost=" ++ (show newCost_) ++ " newCc=" ++ (C.show newCc) ++ " for qmap=" ++ (qmapStr newQmap)
            if newCost < stCost then return (newCost, newSt)
            else return (stCost, st)

    (cc0Qmap, cc0St) <- incSearchQmap fmSt cc0 flow
    let cc0Cost = costFn cc0Qmap
        x0 = (cc0Cost, cc0St)

    (lowerCost, bestSt) <- foldM foldFn x0 restCC
    return bestSt

incSearchGreedyFlows :: forall o cc. C.ConfChange cc => IncrSearchStrategy o cc
incSearchGreedyFlows st flows_ = do
    let uniqueFlows = (length flows_) == (length $ L.nub flows_)
        dup_msg = "Duplicated flows in flow list:\n" ++ (ppShow flows_) ++ " This might cause problems"
        flows = case uniqueFlows of
            False -> trace dup_msg flows_
            True  -> flows_
    st' <- incSearchGreedyFlows_ st flows
    return $ C.foldConfChanges $ FM.fmCnfChanges $ isFlowMapSt st'

incSearchGreedyFlows_ :: (OracleSt o cc)
                      => IncrSearchSt s o cc
                      -> [Flow]       -- remaining flows to examine
                      -> ST.ST s (IncrSearchSt s o cc)
-- no more flows: return configuration
incSearchGreedyFlows_ st [] = return st
-- process a single flow
incSearchGreedyFlows_ st (flow:flows) = do
    -- get basic information from current state
    let oracle = isOracle $ isParams st
        costFn = isCostFn $ isParams st
        fmSt = isFlowMapSt st
        ccs  = FM.fmCnfChanges fmSt
        conf = C.foldConfChanges ccs
    -- get configuration changes for current state
    let flowCCs = flowConfChanges oracle conf flow
    -- compute new costs and minimum cost, and update state
    bestFmSt <- incSearchMinCost costFn fmSt flow flowCCs
    let st' = st { isFlowMapSt = bestFmSt }
    -- recurse
    incSearchGreedyFlows_ st' flows


incSearchQmap :: forall s cc . C.ConfChange cc
              => FM.FlowMapSt s cc
              -> cc
              -> Flow
              -> ST.ST s (QMap, FM.FlowMapSt s cc)
incSearchQmap st cc flow = do
    st1_  <- FM.incrConfigure st cc -- add configuration option
    let st1 = trN st1_ $ (C.showConfig (undefined::cc) (C.foldConfChanges $ FM.fmCnfChanges st1_))
    st2  <- FM.addFlow st1 flow   -- add new flow
    qmap <- FM.getRxQMap st2       -- get qmap
    return $ (qmap, st2)

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

connectFlows :: Int -> [Flow]
connectFlows nflows = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ 1000 + i
   , flSrcIp    = Just 123
   , flSrcPort  = Just 7777 } | i <- [1..nflows] ]

test = do
    putStrLn $ "Running normal search!"
    -- unconfigured PRG
    prgU <- e10kU_simple
    --prgU <- e10kU_simple
    let nq = 10
        e10kOracle   = E10kOracleSt {nQueues = nq}
        priFn        = priorityCost' nq
        balFn        = balanceCost nq
        dummyFn      = dummyCost

        costFn = balFn
        params = initSearchParams {  sOracle = e10kOracle
                                   , sPrgU   = prgU
                                   , sCostFn = costFn
                                   , sStrategy = searchGreedyFlows}

        conflows = connectFlows 200
        conf = runSearch params conflows
        prgC = C.applyConfig conf prgU
        --conf = runSearch params $ connectFlows 10
    --print $ "connected flows : " ++ (ppShow conflows)
    putStrLn $ "Configuration: " ++ (showConf  e10kOracle conf)
    writeFile "tests/search-prg-result.dot" $ toDot prgC

    return ()

test_incr = do
    putStrLn $ "Running incremental search!"
    prgU <- e10kU_simple
    let nq = 10
        e10kOracle   = E10kOracleSt {nQueues = nq}
        priFn        = priorityCost' nq
        balFn        = balanceCost nq
        dummyFn      = dummyCost

        costFn = balFn
        params = initIncrSearchParams {  isOracle = e10kOracle
                                       , isPrgU   = prgU
                                       , isCostFn = costFn }

        conflows = connectFlows 20
        conf = runIncrSearch params conflows
        prgC = C.applyConfig conf prgU
        --conf = runSearch params $ connectFlows 10
    --print $ "connected flows : " ++ (ppShow conflows)
    putStrLn $ "Configuration: " ++ (showConf  e10kOracle conf)
    writeFile "tests/incr-search-prg-result.dot" $ toDot prgC
