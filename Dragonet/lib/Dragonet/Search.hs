-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

{-# LANGUAGE RankNTypes, LiberalTypeSynonyms, ExistentialQuantification,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             ScopedTypeVariables, FunctionalDependencies #-}

-- NOTE: Some of the functions below are exported just for the purpose of
-- documenting them.
module Dragonet.Search (
  -- * Cost Functions
  Cost(..),
  -- $cost
  QMap, qmapStr_, qmapStr,
  CostQueueFn, CostFn,
  -- cost functions
  dummyCost,
  balanceCost,
  priorityCost, prioritySort,
  staticCost,
  -- * Oracle
  OracleSt(..),
  oracleQueueJumpStack, oracleQueueJump,
  -- * Search
  runSearch,
  SearchParams(..), initSearchParams,
  SearchStrategy,
  SearchSt(..), initSearchSt,
  SearchStIO, initSearchIO, runSearchIO,
  -- * Search Strategies
  searchGreedyFlows,
  searchGreedyConf,
  hardcodedSearch,
  -- * Incremental search
  -- $incsearch
  runIncrSearch,
  IncrSearchParams(..), initIncrSearchParams,
  IncrConf(..),
  IncrSearchSt(..), initIncrSearchSt,
  IncrSearchStrategy,
  incrSearchGreedyFlows,
  IncrSearchStIO, initIncrSearchIO, runIncrSearchIO,
  -- * QMap computation
  -- $qmap
  qMap, qMap_, flowQueue,
  xQmap, yQmap, zQmap,
  qMapIncremental, zQmapIncremental,
  incrSearchQmap,
  --
  -- * E10k Oracle
  E10kOracleSt(..), initE10kOracle,
  E10kOracleHardCoded(..),
  initSearchParamsE10k,
  -- * SF Oracle
  initSFOracle,
  SFOracleSt(..),
  SFOracleHardCoded(..),
  initSearchParamsSF,
  -- * Tests
  test, test_incr, test_incr_add, test_incr_pravin_testcase,
  test_incr_pravin_testcase_mixed,
) where

import qualified Dragonet.Configuration       as C
import qualified Dragonet.ProtocolGraph       as PG
import qualified Util.GraphHelpers            as GH
import qualified Dragonet.Predicate           as PR
import qualified Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.FlowMap             as FM
import qualified Dragonet.Flows               as FL

import Dragonet.DotGenerator (toDot)
import Dragonet.Conventions (rxQPref, isTruePort, isFalsePort, QueueId)
import Dragonet.Flows(Flow (..), flowPred, flowStr)

import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Graphs.E10k                  as E10k
import qualified Graphs.SF                    as SF

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

import Util.XTimeIt (doTimeIt, dontTimeIt)

-- | helper funciton for adding a prefix to each line
sAddPr pre str = L.unlines $ [ pre ++ s | s <- L.lines str ]

-- | tracing function that uses
tr a b  = trace b a
trN a b = a

-- | Generate a queue list based on the number of queues.
-- We reserve queue 0 for as a catch-all queue, so it's not part of
allQueues :: Int -> [QueueId]
allQueues nq = [1..(nq-1)] :: [QueueId]

-- |Generate a queue list based on the number of queues, starting from a given
-- queue. The list will wrap-around.
--
-- For example:
--
-- >>> allQueues_ 5 10
-- [6,7,8,9,1,2,3,4,5]
--
-- We reserve queue 0 for as a catch-all queue, so it's not part of the output
allQueues_ :: Int -> Int -> [QueueId]
allQueues_ start nq = [ 1 + ((start+i) `mod` (nq-1)) | i <- [0..(nq-2)] ]


-- |Cost datatype. This is what cost functions return.
--
-- NB #1: We are using floats for ordering, but we could have a class using Ord
-- datatypes for a more generic cost.
--
-- NB #2: due to ordering, 'costOK' will always have the smaller value, and
-- 'costReject' will always have the highest value.
--
-- NB #3: TODO: instead of using L.minimum in the search (as we do now) we can
-- implement a function that short-circuits 'CostOK' to save iterations.
--
-- For example:
--
-- > L.minimum $ CostOK:[CostVal $ 0.1 + i | i <- [1..]]
--
-- will never return in the current scheme
data Cost = CostOK           -- ^ cost can be accepted immediately (min cost)
          | CostVal Float    -- ^ normal (acceptable) cost
          | CostReject Float -- ^ rejected cost (value allows to order rejected
                             -- solutions). This is needed because it is
                             -- possible to start the search with only rejected
                             -- solutions
    deriving (Eq, Ord, Show)


-- | is cost acceptable for a search (not if is CostReject)
costAcceptable :: Cost -> Bool
costAcceptable CostOK = True
costAcceptable (CostVal _) = True
costAcceptable (CostReject _) = False


-- |Qmap is what user-defined cost functions typically operate on. It's a map of
-- flows into queues.
type QMap        = [(Flow, QueueId)]

qmapStr_ :: (Flow,QueueId) -> String
qmapStr_ (f,q) = (flowStr f) ++ "-> Q" ++ (show q)

qmapStr :: [(Flow,QueueId)] -> String
qmapStr flows = "QMAP:\n" ++ L.intercalate "\n" [" " ++ qmapStr_ f | f <- flows]

-- $cost
-- There are two types of cost functions:
--
--     * user-defined cost functions ('CostQueueFn')
--
--     * cost functions used in the search algorithms ('CostFn')
--
--  Dragonet computes the necessary information to bridge these two.

-- |user-defined cost function
type CostQueueFn = QMap -> Cost

-- |CostFn: cost functions used by search algorithms. They are defined as  ST.ST
-- monads so that they to enable stateful datastructures (hashtables in
-- particularly)
type CostFn a s = (C.ConfChange a)
         => [Flow] -- ^ a set of flows
         -> C.Configuration -- ^ the current configuration
         -> a -- ^ a confugration change
         -> ST.ST s Cost -- return the cost

{-|
  An Oracle is a NIC-specific component used by the search. It encodes
  NIC-specific information in a generic way. It is not strictly needed, but it
  is essential for performance.

  For understanding the class declaration, you might want to check
  <https://www.haskell.org/haskellwiki/Functional_dependencies>
-}
class (C.ConfChange cc) => OracleSt o cc | o -> cc where
    -- | show a configuration (for convinience, no need to specialize)
    showConf   :: o -> C.Configuration -> String
    showConf _ c = C.showConfig (undefined::cc) c

    -- | 'flowConfChanges' returns a list of configuration changes for a
    -- flow. Note that the list may be empty, if there are no available
    -- configuration changes that map to the given flow.
    flowConfChanges :: o -- ^ Oracle
                    -> C.Configuration -- ^ Current configuration
                    -> Flow -- ^ New flow to consider
                    -> [cc] -- ^ A set of configuration changes

    -- | Similar to `flowConfChanges` but provides the current configuration
    -- state to the Oracle. This is  used in incremental search and mainly
    -- for dealing with flow removal.
    --
    -- During the search, each configuration will be paired with a flow. We can
    -- view the configuration state as @[(cc, Maybe Flow)]@. The @Maybe@ is used
    -- to  indicate that while a configuration change is still part of the
    -- configuration, its corresponding flow has been removed from the system.
    -- When a flow is removed, we just change the state lazily by turning its
    -- corresponding tuple into @(cc, Nothing)@.
    --
    -- This information is passed to the Oracle so that it can /repurpose/ the
    -- nodes of this (unsued) configuration change. This is done via a special
    -- class of @cc@s that operate by replacing the node label of the nodes
    -- generated by an existing @cc@. See 'C.ConfChange' for more details.
    flowConfChangesS :: o
                     -> [(cc, Maybe  Flow)]
                     -> Flow
                     -> ([cc], o)

    -- | affectedQueues is used for incremental update of the 'QMap' It returns
    -- the queues that may get affected by a given configuration change. Note
    -- that it always includes the default queue.
    --
    -- also see: 'zQmapIncremental'
    affectedQueues  :: o -> C.Configuration -> cc -> [QueueId]

    -- | Perform a jump on the configuration input. It accepts the current
    -- configuration and the flows that were configured.  It returns: A
    -- partition of the flows and configuration changes to keep, and the flows
    -- and configurations to discard.
    --  by default, it discards all configurations
    confJump :: o -> [(cc, Maybe Flow)]
             -> ([(cc, Maybe Flow)], [(cc, Maybe Flow)])
    confJump _ orig = ([], orig)

-- | generic jump function that deallocates a queue (this treats the
-- configuration changes as a stack, where you can only remove flows by popping)
oracleQueueJumpStack :: (C.ConfChange cc)
                => (cc -> QueueId)
                -> [(cc,Flow)] -- initial configuration
                -> ([(cc,Flow)], [(cc,Flow)]) -- keep, remove partition
oracleQueueJumpStack ccQueue ts = L.splitAt splitIdx ts
    where getQ = ccQueue . fst
          lastQ = getQ $ last ts -- last queue
          splitIdx = fromJust $ L.findIndex (\x -> getQ x == lastQ) ts

-- | jump: remove flows from last queue
oracleQueueJump :: (C.ConfChange cc)
                => (cc -> QueueId)
                -> [(cc,Flow)] -- initial configuration
                -> ([(cc,Flow)], [(cc,Flow)]) -- keep, remove partition
oracleQueueJump ccQueue ts = tr ret msg
    where ret = L.partition ((/=lastQ) . getQ) ts
          getQ = ccQueue . fst
          lastQ = getQ $ last ts -- last queue
          msg = "oracleQueueJump removing flows from q=" ++ (show lastQ)



-- | This is the simplest version of search. Callers cannot maintain state
-- across different invocations.
runSearch :: (OracleSt o a)
   => SearchParams o a    -- ^ search paramaters
   -> [Flow]              -- ^ current list of flows
   -> C.Configuration     -- ^ confugration result
runSearch params flows = ST.runST $ do
   st <- initSearchSt params
   doSearch st flows

-- | main search function
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
    -- order flows, and call search function
    let flows' = sortFn flows
    conf <- search st flows'
    --ht_size <- length <$> H.toList flowC
    --return $ tr conf ("SIZE: " ++ (show ht_size))
    return conf


-- | Search parameters. Defined by the user (see 'initSearchParams')
data SearchParams o a = (OracleSt o a) => SearchParams {
      sOracle     :: o -- ^ oracle
    , sPrgU       :: PG.PGraph -- ^ Unconfigured PRG graph of the underlying NIC
    , sCostFn     :: CostQueueFn -- ^ cost function
    , sStrategy   :: SearchStrategy o a -- ^ how to search the conf space
    , sOrderFlows :: [Flow] -> [Flow]
    -- ^ Order flows as heuristic. The main search strategy we currently use is
    -- a greedy search. Since it is not guranteed to always reach a good
    -- solution, users can provide a flows sorting function, to guide the
    -- search. In the non-incremental case sorting is applied before starting
    -- the search.
}

-- | Wrapper to initialize 'SearchParams'
-- sOracle, sPrgU, sCostFn need to be set by the user.
-- The default sStrategy is 'searchGreedyFlows'.
initSearchParams :: (OracleSt o a) => SearchParams o a
initSearchParams = SearchParams {
      sOracle = undefined
    , sPrgU   = undefined
    , sCostFn = undefined
    , sStrategy = searchGreedyFlows
    , sOrderFlows = id
}

-- | a way to search the configuration space
--
-- It is implemented as a ST Monad to allow for things like hashtables in the
-- SearchST
type SearchStrategy o a = (OracleSt o a)
  => SearchSt s o a  -- ^ search state
  -> [Flow] -- ^ current set of flows
  -> ST.ST s C.Configuration -- ^ resulting configuration

-- | State maintained by the search
data SearchSt s o a = (OracleSt o a) => SearchSt {
      sParams        :: SearchParams o a -- ^ search parametrs
    , sFlowCache     :: FM.FlowCache s -- ^ caching to avoid predicate computation
}


-- | initialize search state (add default values when applicable)
initSearchSt :: (OracleSt o a)
   => SearchParams o a
   -> ST.ST s (SearchSt s o a)
initSearchSt params = do
    --h <- H.newSized 100000
    h <- H.new
    return $ SearchSt {
          sParams     = params
        , sFlowCache  = h
    }

-- 'SearchSt' IO Monad wrapper
type SearchStIO o a = SearchSt ST.RealWorld o a

-- 'initSearch' IO Monad wrapper
initSearchIO :: (OracleSt o a)
              => SearchParams o a
              -> IO (SearchStIO o a)
initSearchIO params = ST.stToIO $ initSearchSt params

-- 'runSearch' IO Monad wrapper
runSearchIO :: (OracleSt o a)
            => SearchStIO o a
            -> [Flow]
            -> IO C.Configuration
runSearchIO st flows = ST.stToIO $ doSearch st flows



-- | greedy search strategy examine one flow at a time. Depends on the ordering
-- of flows.
--
-- This search algorithm works as follows:
--
--  * Consider one flow at each step
--
--  * Use the oracle to get a set of possible configuration changes for this
--    flow
--
--  * Evaluate all of these configurations and select the one that minimizes the
--    cost function
--
searchGreedyFlows :: forall o a. C.ConfChange a => SearchStrategy o a
searchGreedyFlows st flows = searchGreedyFlows_ st x0 flows
    where x0 = (cnf0, [], [])
          cnf0 = C.emptyConfig (undefined::a)

-- | 'searchGreedyFlows' helper
searchGreedyFlows_ :: (OracleSt o a)
                   => SearchSt s o a
                   -- ^ Current state of search algorithm
                   -> (C.Configuration, [Flow], QMap)
                   -- ^ Configuration buit till now for already configured flows
                   -> [Flow]
                   -- ^ Flows for which configuration should be checked
                   -> ST.ST s C.Configuration
                   -- ^ Final configuration
-- | no more flows: terminate
searchGreedyFlows_ st (cnf,_,_) [] = return $ trN cnf msg
    where msg = ("searchGreedyFlows_:" ++ (showConf oracle cnf))
          oracle = sOracle $ sParams st
-- | operate on a single flow, and recurse
searchGreedyFlows_ st (curCnf, curFlows, curQmap) (f:fs) = do
    let oracle      = sOracle $ sParams st
        costFn      = sCostFn $ sParams st
        prgU        = sPrgU $ sParams st
        -- Configuration changes suggested by Oracle for adding a single flow
        confChanges  = case flowConfChanges oracle curCnf f of
                [] -> error "Oracle did not return any confs, bailing out"
                l  -> l
        newCurFs = xtr f:curFlows
        -- spew out a warning when processing the same flow more than once,
        -- because it might lead to problems. One example is that it breaks the
        -- current assumption of affectedQueues when incrementally calculating
        -- the qmap
        xtr = case f `elem` curFlows of
            False -> id
            True ->  trace  ("Flow " ++ (flowStr f) ++ " was seen before!"
                             ++ " this might cause problems")
    -- find out the costs for all the configurations suggested by Oracle
    conf_costs <- forM confChanges $ \cc -> do
        (prgC, newCnf, qmap) <- zQmapIncremental st curCnf cc newCurFs  curQmap
        return (newCnf, costFn qmap, qmap)
    let msg_all_costs = L.intercalate "\n"
                           ["C" ++ (show idx) ++ "\n " ++ (showConf oracle cnf)
                             ++ "\n COST:" ++ (show cost)
                             ++ "\n " ++ (qmapStr qmap) ++ "\n"
                           | (idx,(cnf,cost,qmap)) <- zip [1..] conf_costs]
    -- select the configuration with the minimum cost
    let snd3 (_,x,_) = x
        (best_cnf, lower_cost, qmap') = L.minimumBy (compare `on` snd3) conf_costs
        msg = "searchGreedyFlows_: step:"
            ++ (show $ length curFlows)
            -- ++ "\n" ++ msg_all_costs
            ++ "  => lower cost is " ++ (show lower_cost)
            ++ "\n=> SELECTED configuration is " ++ (showConf oracle best_cnf)
            ++ "\n"
    -- recurse
    recurse <- searchGreedyFlows_ st (best_cnf, newCurFs, qmap') fs
    return $ trN recurse msg


-- | 'searchGreedyConf': This is also a greedy search, but operates differently
-- than 'searchGreedyFlows'.
--
--  At each step, examines all flows, and determines a single configuration
--  value. Removes the flow that is paired with the configuration values and
--  moves on. This can avoid some problems of 'searchGreedyFlows' (it really
--  depends on the cost function), but is more expensive.
--
--  NB: Not very well tested.
searchGreedyConf :: forall o a. C.ConfChange a => SearchStrategy o a
searchGreedyConf st flows = searchGreedyConf_ st x0 flows
    where x0 = (cnf0, [])
          cnf0 = C.emptyConfig (undefined::a)

searchGreedyConf_ :: OracleSt o a
                   => SearchSt s o a
                   -> (C.Configuration, [Flow])
                   -> [Flow]
                   -> ST.ST s C.Configuration

searchGreedyConf_ st (cnf,_) [] = return $ tr cnf msg
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

-- | hardcodedSearch: return same conf irrespective of the current flow
hardcodedSearch :: forall o a. C.ConfChange a =>
                C.Configuration
            ->  SearchStrategy o a
hardcodedSearch hardConf st [] = return $
    [
        ("RxC5TupleFilter", PG.CVList []),
        ("RxCFDirFilter", PG.CVList [])
    ]
hardcodedSearch hardConf st fs = return $ hardConf

-- $incsearch
--
-- Some notes on incremental search. There are two aspects of doing an
-- incremental search:
--
--  1. incremental computation of the flow map.
--
--  2. incremental search from the previous solution when flows come/go.
--
-- For 2, instead of starting from @['Flow']@, we start from a previous solution
-- and a set of added and a set of removed flows.
--
-- There several approaches for 2, which can be combined to build more complex
-- strategies:
--
--  a) start with the previous configuration solution, and continue searching
--     by adding the new flows. Note that this means that filters for deleted
--     flows remain in the solution.
--
--  b) change solution, removing (Flow,cc) tuples that correspond to flows that
--     are removed and start from the resulting solution and add the new flows.
--     Note, that this might lead to weird solutions.
--
--  c) Find the largest relevant part of the solution. Something like:
--     @L.takeWhile ( (f,cc) -> f `S.notMember` rmFlows )@. and start from that.
--     In this case (contrarily to b) the starting solution from the search is
--     one that we have arrived by actually evaluating a cost function, so in
--     general it has better potential than the starting solution of b).
--
--  d) Do a NIC-specific /jump/ in the search. This jump should be a part of the
--     oracle. One example of a jump is make a queue available. We can take
--     multiple jumps to reach a better solution. An extreme case of a jump is
--     to start from scratch
--
-- Note that we can use the CostReject, CostAccept distinction to decide if a
-- solution is good enough or try another approach.


-- | perform an incremental search
runIncrSearch :: (OracleSt o a)
    => IncrSearchParams o a -- ^ search parametrs
    -> FL.FlowsSt -- ^ flow state changes
    -> C.Configuration -- ^ resulting configuration
runIncrSearch params flows = ST.runST $ do
   st <- initIncrSearchSt params
   (cnf, _, st') <- doIncrSearch st flows
   return cnf

-- | incremental search parameters
data IncrSearchParams o cc = (OracleSt o cc) => IncrSearchParams {
      isOracle     :: o -- ^ oracle
    , isPrgU       :: PG.PGraph -- ^ unconfigured PRG for the NIC
    , isCostFn     :: CostQueueFn -- ^ user-defined cost function
    , isStrategy   :: IncrSearchStrategy o cc -- ^ incremental search strategy
    , isOrderFlows :: [Flow] -> [Flow] -- ^ sorting flows
}

doIncrSearch :: (OracleSt o cc)
         => (IncrSearchSt s o cc)
         -> FL.FlowsSt
         -> ST.ST s (C.Configuration, IncrConf cc, IncrSearchSt s o cc)
doIncrSearch st flowsSt = do
    let params = isParams st
        costFn = isCostFn params
        sortFn = isOrderFlows params
        oracle = isOracle params
        search = isStrategy params
        prgU   = isPrgU params
    (conf, ic, st) <- search st flowsSt
    return (conf, ic, st)

-- | incremental search strategy
type IncrSearchStrategy o cc = (OracleSt o cc)
    => IncrSearchSt s o cc -- ^ incremental search state
    -> FL.FlowsSt -- ^ flows state changes
    -> ST.ST s (C.Configuration, IncrConf cc, IncrSearchSt s o cc)
    -- ^ returns: the final configuration, the incremental configuration
    -- relative to the previous configuration and the current incremental search
    -- state (can be used for the next search)


-- | Notes on incremental configuration:
--
-- A configuration is a list of configuration changes @[cc]@
--
-- NB: The list is  a better model than a set, because order matters if we have
-- packets that are matched by multiple filters.
--
-- To represent incremental configuration, We need a way to represent removal of
-- ccs.  We can assume that @[cc]@ is a stack, and that we can remove ccs only
-- from the top. This is not necessary: we can have arbitrary ways to remove ccs
-- from the list.
--
-- Using a stack is simple, it works well with our search that essentially
-- pushes ccs to the end of the list, and ultimately works well with NIC filter
-- tables (i.e., you just remove entries from the bottom)
--
-- On the other hand, Removing arbitrary ccs (e.g., jumping by freeing a queue)
-- needs to remove more ccs that it would normally not have to. A jump that
-- frees a queue when a stack is used would have to iterate the list from the
-- start and remove all elements after and including the first filter matching
-- the queue it tries to remove.
--
-- In our current implementation we assume that we do not program multiple
-- filters that match the same packet. In this case, ordering does not matter.
-- We represent an incremental configuration as: @[cc]@ to add, @[cc]@ to
-- remove.
--
-- XXX: move this to Configuration.hs
data IncrConf cc = (C.ConfChange cc) => IncrConf {
      icRmCCs  :: [cc] -- ^ configuration changes to add
    , icAddCCs :: [cc] -- ^ configuration changes to remove
}

-- |
initIncrConf :: (C.ConfChange cc) => IncrConf cc
initIncrConf = IncrConf [] []

-- ugh! :/
instance Show (IncrConf cc) where
    show IncrConf { icRmCCs = xrm, icAddCCs = xadd } = ret
        where addedS :: [Char]
              addedS = L.intercalate " \n" [ C.show cc | cc <- xadd]
              rmS    = L.intercalate " \n" [ C.show cc | cc <- xrm ]
              ret    = "IncrConf {icRmCCs=[\n" ++ rmS ++ "]\n"
                               ++ "icAddCCs=[\n" ++ addedS ++ "]\n"

-- | incremental search state
data IncrSearchSt s o cc = (OracleSt o cc) => IncrSearchSt {
          isParams     :: IncrSearchParams o cc -- ^ search parametrs
        , isFlowMapSt  :: FM.FlowMapSt s cc
        -- ^ incremental map is implemented by maintaing a flow map. This is
        -- its state
}

-- | initialize incremental search params. Default strategy is
-- 'incrSearchGreedyFlows'
initIncrSearchParams :: (OracleSt o a) => IncrSearchParams o a
initIncrSearchParams = IncrSearchParams {
      isOracle = undefined
    , isPrgU   = undefined
    , isCostFn = undefined
    , isStrategy = incrSearchGreedyFlows
    , isOrderFlows = id
}

-- | IncrSearchSt IO Monad wrapper
type IncrSearchStIO o cc = IncrSearchSt ST.RealWorld o cc

-- | initIncrSearchSt IO Monad wrapper
initIncrSearchIO :: (OracleSt o cc)
              => IncrSearchParams o cc
              -> IO (IncrSearchStIO o cc)
initIncrSearchIO params = ST.stToIO $ initIncrSearchSt params

-- | initialize search state (add default values when applicable)
initIncrSearchSt :: (OracleSt o a)
                 => IncrSearchParams o a -> ST.ST s (IncrSearchSt s o a)
initIncrSearchSt params = do
    fmSt <- FM.initFlowMapSt (isPrgU params)
    return $ IncrSearchSt {
          isParams     = params
        , isFlowMapSt  = fmSt
    }

-- | runIncrSearch IO Monad wrapper
runIncrSearchIO :: (OracleSt o cc)
            => IncrSearchStIO o cc
            -> FL.FlowsSt
            -> IO (C.Configuration, IncrConf cc, IncrSearchStIO o cc)
runIncrSearchIO st flows = ST.stToIO $ doIncrSearch st flows




-- $qmap
--
-- To bridge 'CostQueueFn' and  'CostFn' Dragonet needs to compute 'QMap', i.e.,
-- how flows are mapped into queues. This is where most time is spent in the
-- search, so we try to optimize it
--
-- NOTE: There is still some code around trying to compute qmap with
-- different ways:
--  - E10k.FlowQueue.flowQueue: old E10k-specific version of flowQueue
--  - yQmap and xQmap
--  - ...
--
-- For incremental search, we actually use 'FM.FlowMapSt' for incremental
-- implementation of 'QMap' so the code below is mainly for historic purposes.
-- Eventually, we might want to remove/move them.

-- | compute 'QMap'
-- This is fairly straightfoward. First, we configure the PRG. Afterwords, using
-- the flow predicates, we iterate the graph for each flow and figure out at
-- which queue each flow will end up.
--
-- We use 'FlowCache' to avoid predicate computation for nodes/flows we 've
-- seen before.
qMap :: (OracleSt o a)
     => SearchSt s o a  -- ^ current search state
     -> C.Configuration -- ^ current configuration
     -> a               -- ^ configuration change
     -> [Flow]          -- ^ set of flows
     -> ST.ST s QMap    -- ^ resulting 'QMap'
qMap st conf confChange flows = do
    let prgU = sPrgU $ sParams st
        newConf = C.applyConfChange conf confChange
        prgC = C.applyConfig newConf prgU
    qMap_ st prgC flows

-- | helper for 'qMap'
qMap_ :: (OracleSt o a)
      => SearchSt s o a
      -> PG.PGraph
      -> [Flow]
      -> ST.ST s QMap
qMap_ st prgC flows = do
    let flowC = sFlowCache st
    qmap <- forM flows $ \fl -> do
        q <- flowQueue flowC prgC fl
        return (fl, q)
    return qmap

-- | compute which flow a queue will end up
flowQueue :: FM.FlowCache s  -- ^ flow cache
          -> PG.PGraph       -- ^ configured PRG
          -> Flow            -- ^ flow
          -> ST.ST s QueueId -- ^ resulting queue
flowQueue fc prgC flow = do
    let flPred = flowPred flow
        d0 = flowQueueStart prgC
    doFlowQueue fc d0 (flow, flPred)

sucPortCtx_ :: PG.PGraph -> PG.PGContext -> PG.NPort -> DGI.Node
sucPortCtx_ g ctx port = fst next
    where sucs = DGI.lsuc' ctx
          sucsPort =  L.filter (\ (_,s) -> port == PGU.edgePort_ s) sucs
          next = case sucsPort of
                [x] -> x
                []  -> error $ "Cannot find successor of node:" ++ nodeL ++
                                " on port:" ++ port
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


-- | find starting point for the graph
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


-- | 'flowQueue' main  helper
doFlowQueue :: FM.FlowCache s
            -> PG.PGGDecomp
            -> (Flow, PR.PredExpr)
            -> ST.ST s QueueId
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


-- find next node port (use flow cache)
doFlowNextPortCache :: FM.FlowCache s         -- ^ flow cache
                    -> (Flow, PR.PredExpr)    -- ^ the flow and its predicate
                    -> PG.Node                -- ^ current node
                    -> ST.ST s PG.NPort       -- ^ node port for the flow
doFlowNextPortCache flowCache x@(fl, flPred) node = do
    let key = (PG.nLabel node, fl)
    ret <- H.lookup flowCache key
    case ret of
        Just x -> return $ trN x "HIT"
        Nothing -> do
            ret <- doFlowNextPort x node
            H.insert flowCache key ret
            return $ trN ret "MISS"

-- | compute  next port for flow in a node
doFlowNextPort :: (Flow, PR.PredExpr)  -- ^ the flow and its predicate
                -> PG.Node             -- ^ node
                -> ST.ST s PG.NPort    -- ^ node port for the flow
doFlowNextPort (fl, flPred) node = return ret'
    where nPreds = PG.nPredicates node
          ret = doFlowQueueNextPort_ flPred nPreds
          ret' = trN ret $ "Flow:" ++ (flowStr fl) ++
                          " Node:"  ++ (PG.nLabel node) ++
                          " Port:" ++ ret

-- | find matching port, given the flow predicate and all port predicatesb
doFlowQueueNextPort_ :: PR.PredExpr               -- ^ flow predicate
                     -> [(PG.NPort, PR.PredExpr)] -- ^ port predicates
                     -> PG.NPort                  -- ^ resulting port
doFlowQueueNextPort_ flPred ((port,portPred_):rest) = ret
    where sat     = isJust $ PR.dnfSAT andExpr
          andExpr = (PR.buildAND bld) [portPred,flPred]
          portPred = PR.predDoBuild bld portPred_
          bld = PR.predBuildDNF
          -- NB: we assume that each flow is assigned exclusively to a single
          -- queue, so we return the first match
          ret = case sat of
                True  -> port -- found port
                False -> doFlowQueueNextPort_ flPred rest -- recurse

-- | determine if we reached a queue node in the graph
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


-- | 'xQmap' computes the combined predicates in the queue nodes of the graph,
-- and checks satisfiability for each flow across all these predicates.
xQmap :: PG.PGraph -> [Flow] -> QMap
xQmap gr fls = qmap
    where flPreds = map flowPred fls
          nQueues = 10 -- XXX: Hack
          allQs  = allQueues nQueues
          qNodes = [ nLabelNode gr $ rxQPref ++ (show i) | i <- allQs ]
          qPreds = PR.computePredMany gr qNodes
          --qPreds = map (nLabelSinglePred gr) qNodes
          qmap   = [ (fl, qid) | (qid,qpred) <- zip allQs qPreds,
                                 (fl,flpred) <- zip fls flPreds,
                                  check (qid,qpred) (fl,flpred)]
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

-- | 'yQmap' is simlar to 'xQmap', but it assumes that each flow is mapped on a
-- in a single queue. This allows for short-circuting the matching of flows
-- against queue predicates
yQmap :: PG.PGraph -> [Flow] -> QMap
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


doZQmap :: FM.FlowCache s
        -> PG.PGGDecomp
        -> [(Flow, PR.PredExpr)]
        -> ST.ST s QMap
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

-- | given a graph node, group flows based on which port they match
flowPortGroups :: FM.FlowCache s
               -> PG.PGGDecomp
               -> [(Flow, PR.PredExpr)]
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

-- | (yet) Another way to compute 'QMap'
--
-- This approach traverses the graph only once. It starts with all flows in the
-- starting node. It groups flows based on which port they match. Then, it
-- recursively traverses the graphs by following the port  with the matching
-- flows. Recursion ends when a queue is reached.
zQmap :: (OracleSt o a)
    => SearchSt s o a
    -> C.Configuration
    -> a
    -> [Flow]
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
     -- for sanity checks:
    qmapCorrect <- qMap_ st prgC flows
    let test = qmap == qmapCorrect
        qmap' = case test of
            True  -> qmap
            False -> error $ "Incremental qmap failed"
    --}
    return (prgC, newConf, qmap)

-- | Incremental QMap computation
--
-- This is an incremental computation of 'QMap' (NB: not an incremental search)
-- because we use the old 'QMap' (i.e., the 'QMap' from the old configuration
-- without the configuration change).
--
-- It depends on a NIC-specific facility (provided by 'OracleSt') where given a
-- configuration change, the oracle tells us which queues are affected by this
-- change ('affectedQueues').  We maintain the mappings for the unaffacted
-- queues, and re-compute the mappings for the where on the affected queues
-- using.
qMapIncremental :: (OracleSt o a)
                => SearchSt s o a
                -> C.Configuration
                -> a
                -> [Flow]
                -> QMap
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

-- | This is what is currently used for 'searchGreedyFlows' (i.e., the default
-- non-incremental search).
--
-- This is similar to 'qMapIncremental', but it uses 'zQmap' for the flows in
-- the affected queues.
zQmapIncremental :: (OracleSt o a)
      => SearchSt s o a   -- ^ search state
      -> C.Configuration  -- ^ current configuration
      -> a                -- ^ configuration change
      -> [Flow]           -- ^ set of flows
      -> QMap             -- ^ old queue map
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


-- | 'QMap' computation for incremental search. The mechanics for this are
-- implemented using 'FM.FlowMapSt'
incrSearchQmap :: (OracleSt o cc) => IncrSearchSt s o cc -> ST.ST s QMap
incrSearchQmap st = FM.getRxQMap $ isFlowMapSt st

-- | incremental search strategy
incrSearchGreedyFlows
  :: forall s o cc. (C.ConfChange cc, OracleSt o cc)
  => IncrSearchSt s o cc
  -> FL.FlowsSt
 -> ST.ST s (C.Configuration, IncrConf cc, IncrSearchSt s o cc)
incrSearchGreedyFlows st flowsSt = do
    let ic0 = initIncrConf
    (ic', st') <- doIncSearchGreedyFlows ic0 st flowsSt
    let cnf' = C.foldConfChanges $ FM.fmGetCCs $ isFlowMapSt st'
        ret_ = (cnf', ic', st')
        ret  = trN ret_ $ "incrSearchGreedyFlows:" ++ (show ic')
    return ret

-- | entry for incremental search algorithm
-- NB: the current (i.e., the last) solution is in flowmap
doIncSearchGreedyFlows :: forall s o cc. (C.ConfChange cc, OracleSt o cc)
                     => IncrConf cc
                     -> IncrSearchSt s o cc
                     -> FL.FlowsSt
                     -> ST.ST s (IncrConf cc, IncrSearchSt s o cc)
doIncSearchGreedyFlows ic st flowsSt_ = do
    let params    = isParams st
        oracle    = isOracle params
        sortFn    = isOrderFlows params

        flowsSt   = trN flowsSt_ ("=========> FLOWS ST:" ++ (ppShow flowsSt_))
        curFlows  = FL.fsCurrent flowsSt
        delFlows  = FL.fsRemoved flowsSt
        addFlows  = FL.fsAdded flowsSt

    -- search arguments
    (searchSt, searchFlows) <- case S.null delFlows of
           -- no removed flows, keep state as is and return new flows
           True -> do
               let flows = sortFn $ S.toList addFlows
               return $ trN (st, flows) "NO DELETED FLOWS"
           -- removed, rebuild the whole flow map
           False -> do -- removed flows
                fm' <- foldM FM.rmFlow (isFlowMapSt st) (S.toList delFlows)
                let st' = st {isFlowMapSt = fm'}
                    flows = sortFn $ S.toList addFlows
                return $ trN (st', flows) "DELETED FLOWS"
    -- search
    let msg = "Search flows:\n" ++ (FL.flowsStr searchFlows)
             ++ "\nState flows:\n"
             ++ (FL.flowsStr $ catMaybes $ map snd
                             $ FM.fmCnfState $ isFlowMapSt searchSt)
    (st', cost') <- case searchFlows of
                     [] -> return (searchSt, CostOK)
                     _  -> incSearchGreedyFlows_ searchSt (trN searchFlows msg)

    qmap' <- incrSearchQmap st'
    case costAcceptable $ trN cost' ("SEARCH: " ++ (show cost')
                                  ++ " QMAP:" ++ (qmapStr qmap')) of
        True  -> let ic' = error "incremental configuration: NYI!"
                 in return (ic', st')
        False -> do
            -- confJump
            --  check if we reached the end!
            let fm = isFlowMapSt st
                confS = [(fromJust mcc, mfl) | (mcc, mfl) <- FM.fmCnfState fm,
                                               isJust mcc]
                (keep, discard) = confJump oracle confS
                (rmCcs, rmFlows_) = unzip discard
                rmFlows = case catMaybes rmFlows_ of
                            [] -> error "JUMPED but no flows removed"
                            _  -> tr rmFlows_ $ "JUMP!"
                                      ++ "\nRemoved flows:\n"
                                      ++ (FL.flowsStr $ catMaybes rmFlows_)

            fm'_ <- foldM FM.rmFlow fm $ catMaybes rmFlows
            let fm' = trN fm'_ $ " AFTER JUMP CnfSt: \n" ++
                                 (sAddPr " >" $ ppShow $ FM.fmCnfState fm'_) ++
                                 "\n" ++
                                 "BEFORE JUMP CnfSt:\n" ++
                                 (sAddPr " >" $ ppShow $ FM.fmCnfState fm)

            let st'_ = st { isFlowMapSt = fm' }
                -- new flows state
                flowsSt' = FL.FlowsSt {
                      FL.fsCurrent  = S.fromList $ catMaybes $ map snd $ keep
                    , FL.fsAdded    = (S.fromList $ catMaybes $ rmFlows) `S.union` addFlows
                    , FL.fsRemoved  = S.empty
                }
                ic' = ic { icRmCCs = (icRmCCs ic) ++ rmCcs }
                st' = (trN st'_ $ "JUMP QMAP="++ (qmapStr qmap'))
            -- recurse
            case confS of
                [] -> error "incrSearchGreedyFlows: cannot find acceptable conf"
                _  -> doIncSearchGreedyFlows ic' st' flowsSt'

-- returns:
--  . search state
--  . cost
--  . accumulated configuration changes
incSearchGreedyFlows_ :: (OracleSt o cc)
                      => IncrSearchSt s o cc
                      -> [Flow]       -- remaining flows to examine
                      -> ST.ST s (IncrSearchSt s o cc, Cost)
-- process a single flow
incSearchGreedyFlows_ st (flow:flows) = do
    -- get basic information from current state
    let params = isParams st
        oracle = isOracle params
        costFn = isCostFn params
        fmSt = trN (isFlowMapSt st) ("flow=" ++ (FL.flowStr flow))
        conf = C.foldConfChanges $ FM.fmGetCCs fmSt
    -- get configuration changes for current state
    let (flowCCs_, oracle') = case True of
               True -> flowConfChangesS oracle (FM.fmGetConf fmSt) flow
               False -> (flowConfChanges oracle conf flow, oracle)
    let flowCCs = case flowCCs_ of
                [] -> error "Run out of CCs"
                ccs -> trN ccs $ "flowCCs:\n" ++ (ppShow [ C.ccShow x | x <- flowCCs_ ])
    -- compute new costs and minimum cost, and update state
    (lowerCost, bestFmSt, bestCc) <- incSearchMinCost costFn fmSt flow flowCCs
    let msg = "--\nLOWER COST:" ++ (show lowerCost) ++ "\n"
              ++ " Best CC:" ++ (C.ccShow bestCc) ++ "\n"
              ++ " Flows in state:" ++ (show $ length $ FM.fmCnfState fmSt) ++ "\n"
              ++ " Flow examined:" ++ (flowStr flow) ++ "\n"
              ++ " Remaining flows:" ++ (show $ length flows)
              -- ++ " resulting FM: \n" ++ (FM.strFlowMap bestFmSt)
              -- ++ " resulting CnfSt: \n" ++ (sAddPr " >" $ ppShow $ FM.fmCnfState bestFmSt)
              -- ++ " original  CnfSt: \n" ++ (sAddPr " >" $ ppShow $ FM.fmCnfState fmSt)
              ++ "\n--"
        params' = params { isOracle = oracle' }
        st' = st {isFlowMapSt = trN bestFmSt msg, isParams = params'}
    -- recurse
    case flows of
        -- no more flows: we are done
        [] -> return (st', lowerCost)
        _  -> incSearchGreedyFlows_ st' flows

-- M.foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
stopFoldM :: Monad m => (a -> b -> m a)
                   -> (a -> Bool)
                   -> a
                   -> [b]
                   -> m a
stopFoldM foldFn stopFn a0 (b:bs) = do
    aNew <- foldFn a0 b
    case stopFn aNew of
        True -> return aNew
        False -> stopFoldM foldFn stopFn aNew bs
stopFoldM foldFn stopFn a0 [] = return a0

-- return the flowmap that minimizes cost and the cost. NB: flowmap contains a
-- list of configuration changes, i.e., the solution.
-- NB: we might want to bail out early here if there is an accepted solution
incSearchMinCost :: forall s cc. C.ConfChange cc
                 => CostQueueFn
                 -> FM.FlowMapSt s cc
                 -> Flow
                 -> [cc]
                 -> ST.ST s (Cost, FM.FlowMapSt s cc, cc)
incSearchMinCost costFn fmSt flow (cc0:restCC) = do

    let foldFn :: (C.ConfChange cc)
               => (Cost, FM.FlowMapSt s cc, Int, cc)
               -> cc
               -> ST.ST s (Cost, FM.FlowMapSt s cc, Int, cc)
        foldFn (stCost, st, cnt, oldCc) newCc = do
            (newQmap, newSt) <- incSearchQmap fmSt newCc flow
            let newCost_ = costFn newQmap
                newCost = trN newCost_ msg
                cnt'    = cnt + 1
                msg     =  "New cost=" ++ (show newCost_) ++
                           " newCc=" ++ (C.ccShow newCc) ++
                           " for qmap=" ++ (qmapStr newQmap)
            if newCost < stCost then return (newCost, newSt, cnt', newCc)
            else return (stCost, st, cnt', oldCc)

        stopFn (cost, _, cnt, _) = costAcceptable cost && cnt >= 5

    (cc0Qmap, cc0St) <- incSearchQmap fmSt cc0 flow
    let cc0Cost_ = costFn cc0Qmap
        cc0Cost  = trN cc0Cost_ msg
        msg     =  "New cost=" ++ (show cc0Cost_) ++
                   "newCc=" ++ (C.ccShow cc0) ++
                   "for qmap=" ++ (qmapStr cc0Qmap)
        x0 = (cc0Cost, cc0St, 0, cc0)

    --(lowerCost, bestSt, _, bestCc) <- foldM foldFn x0 restCC
    (lowerCost, bestSt, _, bestCc) <- (stopFoldM foldFn stopFn) x0 restCC
    return (lowerCost, bestSt, bestCc)

incSearchQmap :: forall s cc . C.ConfChange cc
              => FM.FlowMapSt s cc
              -> cc
              -> Flow
              -> ST.ST s (QMap, FM.FlowMapSt s cc)
incSearchQmap st cc flow = do
    st_  <- FM.incrConfigure st (cc, Just flow) -- add configuration option and flow
    let ccs = FM.fmGetCCs st_
        st = trN st_ $ (C.showConfig (undefined::cc) (C.foldConfChanges $ ccs))
    qmap <- FM.getRxQMap st       -- get qmap
    return $ (qmap, st)


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
    [(fl, C.applyConfChange conf change)
     | (fl,change) <- flowsSingleConfChanges x conf fs]

-- | E10K (simple for now) oracle
data E10kOracleSt = E10kOracleSt {nQueues :: Int, startQ :: Int}
e10kDefaultQ = 0

initE10kOracle nq = E10kOracleSt { nQueues = nq, startQ = 0 }

-- | implement oracle class for 'E10kOracleSt'
instance OracleSt E10kOracleSt E10k.ConfChange where
    flowConfChanges  = e10kFlowConfChanges
    flowConfChangesS = e10kFlowConfChangesS

    --affectedQueues E10kOracleSt { nQueues = nq } _ _ = allQueues nq
    affectedQueues E10kOracleSt { nQueues = nq } conf (E10k.Insert5T c5t) =
        [e10kDefaultQ, xq]
        where xq = E10k.c5tQueue $ E10k.parse5t (snd c5t)
    affectedQueues E10kOracleSt { nQueues = nq } conf (E10k.InsertFDir cFdir) =
        [e10kDefaultQ, xq]
        where xq = E10k.cfdtQueue $ E10k.parseFDT (snd cFdir)

    --confJump o = e10kQueueJump

-- TODO: avoid symmetric allocation
-- TODO: smart-wildcard allocation if possible?
e10kFlowConfChanges :: E10kOracleSt -> C.Configuration -> Flow
                    -> [E10k.ConfChange]
e10kFlowConfChanges E10kOracleSt {nQueues = nq} cnf fl
    -- allocate 5-tuple filters first, if we can
    | not rx5tFull  = [E10k.insert5tFromFl rx5tId fl q | q <- allQs]
    | not rxCfdFull = catMaybes $ [E10k.insertFdirFromFl rxCfdId fl q | q <- allQs]
    | otherwise     = []
    where allQs = allQueues nq
          rx5tFull  = E10k.rx5tFilterTableFull cnf
          rx5tId    = E10k.rx5tFilterTableLen cnf
          rxCfdFull = E10k.rxCfdFilterTableFull cnf
          rxCfdId   = E10k.rxCfdFilterTableLen cnf


-- TODO: see above
e10kFlowConfChangesS :: E10kOracleSt
                     -> [(E10k.ConfChange, Maybe Flow)]
                     -> Flow
                     -> ([E10k.ConfChange], E10kOracleSt)
e10kFlowConfChangesS o@(E10kOracleSt {nQueues = nq, startQ = startQ}) cnf fl
 -- can we replace existing filters?
 | not $ null replaced = (replaced', o')
 -- allocate 5-tuple filters first, if we can
 | not rx5tFull  = (alloc5t, o')
 | not rxCfdFull = (allocCfd, o')
 | otherwise     = ([], o)
 where allQs = allQueues_ startQ nq
       -- allocate filters starting from a different queue each time
       o' =  o { startQ = (startQ + 1) `mod` nq}
       conf = C.foldConfChanges $ map fst cnf
       rx5tFull  = E10k.rx5tFilterTableFull conf
       rx5tId    = E10k.rx5tFilterTableLen conf
       alloc5t   = [E10k.insert5tFromFl rx5tId fl q | q <- allQs]
       rxCfdFull = E10k.rxCfdFilterTableFull conf
       rxCfdId   = E10k.rxCfdFilterTableLen conf
       allocCfd  = catMaybes $ [E10k.insertFdirFromFl rxCfdId fl q | q <- allQs]
       -- try to find if there are unused filters, i.e., configuration
       -- changes in the current state for which the correspnding flows have
       -- been removed (this happens when we lazily remove flows)
       unusedFs = filter (isNothing . snd) cnf
       -- try to replace unused filters to use current flow
       replaced = catMaybes $ [E10k.replaceCcFromFl cc fl | (cc,_) <- unusedFs]
       -- The queue of the repalced filter needs to remain the same (replace
       -- filters do not change the structure of the graph) For all queues
       -- that are not covered by replaced filtres, generate 5t filters if
       -- the 5t filter table is not full.
       replacedQs = S.fromList $ [ E10k.ccQueue cc | cc <- replaced ]
       replacedMissingQs = (S.fromList allQs) `S.difference` replacedQs
       replaced'
         | not rx5tFull =  replaced
                        ++ [E10k.insert5tFromFl rx5tId fl q
                           | q <- S.toList replacedMissingQs]
         | otherwise = replaced

e10kQueueJump
 :: [(E10k.ConfChange,Maybe Flow)] -- ^ initial configuration
 -> ([(E10k.ConfChange,Maybe Flow)], [(E10k.ConfChange, Maybe Flow)])
    -- ^ keep and remove partition
e10kQueueJump ts = tr ret $ "e10kQueueJump removing flows from q=" ++ (show rmQ)
    where ret = L.partition keepF ts
          --rmQ = 5
          rmQ = lastQ
          -- remove non-5t filters, because otherwise the incremental flowmap
          -- will not work
          lastQ  = getQ $ last $ filter (isJust . snd) ts
          firstQ = getQ $ head $ filter (isJust . snd) ts
          getQ = E10k.ccQueue . fst
          keepF :: (E10k.ConfChange, Maybe Flow) -> Bool
          keepF (cc,fl) = ((E10k.ccQueue cc) /= rmQ) && (E10k.ccIs5t cc)


myThird (_,_,x) = x
mySnd (_,x,_) = x

-- | initSearchParams wrapper for E10k oracle
initSearchParamsE10k nqueues = initSearchParams {sOracle = oracle}
    where oracle = initE10kOracle nqueues

-- | hardcoded E10k oracle
newtype E10kOracleHardCoded = E10kOracleHardCoded {
        nnHardcoded :: (Int, [(Flow, Int, Int)])
    }

{-|
  Making the type 'E10kOracleHardCoded' member of class "Oracle state" to work
  as as Oracle for Intel 82599 NIC.  It will provide a way to generate initial
  empty configuation, showing configuration, and querying about affected queues
  from current configuration.
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
            fId = 0 -- TODO: FIXME (proper filter id)
            ans
              | length(f) > 1 = error ("ERROR: there must be repeat flow, as more than one flow matches.")
              | length(f) == 0 = []
              | (myThird $ head f) == 1 = [E10k.InsertFDir $ (fId, fromJust $ E10k.mkFDirFromFl fl (mySnd $ head f))]
              | (myThird $ head f) == 2 = [E10k.Insert5T   $ (fId, fromJust $ E10k.mkFDirFromFl fl (mySnd $ head f))]
              | otherwise = error ("ERROR: wrong type of flow")

    flowConfChangesS = error "E10kOracleHardCoded: NYI flowConfChangesS!"
    affectedQueues = error "E10kOracleHardCoded: NYI affectedQueues"

-- ###################################### SF Oracle ########################


-- SF (simple for now) oracle
data SFOracleSt = SFOracleSt {nQueuesSF :: Int, startQSF :: Int}
sfDefaultQ = 0

initSFOracle nq = SFOracleSt { nQueuesSF = nq, startQSF = 0 }

{-|
  Making the type 'SFOracleSt' member of class "Oracle state" to work as
  as Oracle for Intel 82599 NIC.
  It will provide a way to generate initial empty configuation, showing
  configuration, and querying about affected queues from current configuration.
 -}
instance OracleSt SFOracleSt SF.ConfChange where
    {-|
     - Basic implementation which suggests a putting current flow in all
     - the queues, generating nQueue number of configurations.
     - This implementation does not care for current state of queues,
     -  and counts on cost-function to help in selecting proper queue.
    -}
    flowConfChanges = sfFlowConfChanges
    flowConfChangesS = sfFlowConfChangesS

    -- affectedQueues SFOracleSt { nQueuesSF = nq } _ _ = allQueues nq
    affectedQueues SFOracleSt { nQueuesSF = nq } conf (SF.Insert5T c5t) =
        [sfDefaultQ, xq]
        where xq = SF.c5tQueue $ SF.parse5t c5t

    --confJump o = oracleQueueJump SF.ccQueue

-- TODO: avoid symmetric allocation
-- TODO: smart-wildcard allocation if possible?
sfFlowConfChanges :: SFOracleSt -> C.Configuration -> Flow
                    -> [SF.ConfChange]
sfFlowConfChanges SFOracleSt {nQueuesSF = nq} cnf fl
    -- allocate 5-tuple filters first, if we can
    | not rx5tFull  = [SF.insert5tFromFl fl q | q <- allQs]
    | otherwise     = []
    where allQs = allQueues nq
          rx5tFull  = SF.rx5tFilterTableFull cnf
          --rx5tId    = SF.rx5tFilterTableLen conf
          --alloc5t   = [SF.insert5tFromFl rx5tId fl q | q <- allQs]

-- TODO: see above
sfFlowConfChangesS :: SFOracleSt
                    -> [(SF.ConfChange, Maybe Flow)]
                    -> Flow
                    -> ([SF.ConfChange], SFOracleSt)
sfFlowConfChangesS o@(SFOracleSt {nQueuesSF = nq, startQSF = startQ}) cnf fl
    -- allocate 5-tuple filters first, if we can
    | not rx5tFull  = ([SF.insert5tFromFl fl q | q <- allQs], o')
    | otherwise     = ([], o)
    where allQs = allQueues_ startQ nq
          o' =  o { startQSF = startQ + 1 `mod` nq}
          conf = C.foldConfChanges $ map fst cnf
          rx5tFull  = SF.rx5tFilterTableFull conf


-- SF (simple for now) oracle
newtype SFOracleHardCoded = SFOracleHardCoded {
        nnHardcodedSF :: (Int, [(Flow, Int, Int)])
                    -- (totalQueues, [(Flow, Qid, FilterType(1==fidr, 2 == Ftuple))])
        }

{-|
  Making the type 'SFOracleHardCoded' member of class "Oracle state" to work as
  as Oracle for SF NIC.
  It will provide a way to generate initial empty configuation, showing
  configuration, and querying about affected queues from current configuration.
 -}
instance OracleSt SFOracleHardCoded SF.ConfChange where
    {-|
     - Basic implementation which suggests a putting current flow in presicely
     - fixed queue based on initial hardcoded values.
    -}
    flowConfChanges SFOracleHardCoded { nnHardcodedSF = (nq, tbl) } c fl = ans
        where
        -- perform a lookup of a flow in the hardcoded table
        -- the lookup should give a filter type and queue-no.
        -- return that.
            f = filter (\(f, q, fil) -> f == fl) $ tbl

            ans
              | length(f) > 1 = error ("ERROR: there must be repeat flow, as more than one flow matches.")
              | length(f) == 0 = []
              | (myThird $ head f) == 1 = [SF.Insert5T $ SF.mk5TupleFromFl fl (mySnd $ head f)]
              | otherwise = error ("ERROR: wrong type of flow")

    flowConfChangesS = error "SFOracleHardCoded: flowConfChangesS: NYI!"

    -- QUESTION: Is it assumed that there will be only one operation in
    --      conf change?
    -- affectedQueues SFOracleHardCoded { nnQueues = nq } _ _ = allQueues nq
    affectedQueues SFOracleHardCoded { nnHardcodedSF = nhw } conf (SF.Insert5T c5t) =
        [sfDefaultQ, xq]
        where xq = SF.c5tQueue $ SF.parse5t c5t

initSearchParamsSF nqueues = initSearchParams {sOracle = oracle}
    where oracle = initSFOracle nqueues

-- ###################################### END: SF Oracle ########################



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

-- | load-balancing cost funtion
balanceCost :: Int -> QMap -> Cost
balanceCost nq fls = balanceCost_ (allQueues nq) fls

-- | dummy cost function (always returns 1)
dummyCost :: [(Flow, QueueId)] -> Cost
dummyCost _ = CostVal 1.0

prioritySort :: (Flow -> Bool) -- ^ returns true for high-priority flows
             -> [Flow]
             -> [Flow]
prioritySort isGold flows = let (hp,be) = L.partition isGold flows
                           in hp ++ be

-- | dynamic priority cost function (high-priortity/best-effort priorities)
priorityCost :: (Flow -> Bool)  -- ^ returns true for HP flows
             -> Integer -> Int -> QMap -> Cost
priorityCost isGold goldFlowsPerQ nq qmap = trN cost msg
    where (goldFls, beFls) = L.partition (isGold . fst) qmap
          goldQs = S.fromList $ [qid | (_,qid) <- goldFls]
          beQs   = S.fromList $ [qid | (_,qid) <- beFls]
          allQsL = allQueues nq
          allQs  = S.fromList allQsL
          restQs = allQs `S.difference` goldQs
          goldQsExcl = goldQs `S.difference` beQs
          beQsExcl   = beQs `S.difference` goldQs
          msg = "---->\nqmap:\n" ++ (qmapStr qmap)
                ++ "\ngoldQs:" ++ (ppShow goldQs)
                ++ "\ngoldQsExcl:" ++ (ppShow goldQsExcl)
                ++ "\ngoldNQs:" ++ (ppShow goldNQs)
                ++ "\nbeQs:" ++ (ppShow beQs)
                ++ "\ncost" ++ (ppShow cost)
                ++ "\n<-----"
          cost
            -- no gold flows: just balance best-effort across all queues
            | length goldFls == 0 = balanceCost_ (allQueues nq) beFls
            -- no best effort flows: penalize extra queues and
            -- balance gold across all queues
            | length beFls   == 0 =
                let CostVal bcost = balanceCost_ allQsL goldFls
                    extraCost = if goldExtra > 0 then goldExtra else 0
                in CostVal $ bcost + (fromIntegral extraCost)
            -- we should have enough queues for the gold class
            | goldNeeded  > 0 = CostReject $ 100*(fromIntegral goldNeeded)
            -- we should not have non-gold exclusive queues
            | goldNonExcl > 0 = CostReject $ (fromIntegral goldNonExcl)
            -- we should have enough queues for the best effort class
            -- beNeeded > 0 = CostReject $ (fromIntegral beNeeded)
            -- penalize needing more be queues, but do not reject solution
            -- (it just means we have some slack for gold flows)
            | beNeeded > 0 = CostVal $ 100*(fromIntegral beNeeded) + balBe
            -- if all is OK, it depends on how well the classes are balanced
            | otherwise               = CostVal $ balGold + balBe

          -- determine number of gold queues
          goldNQs = (min $ nq -1)
                   $ ceiling
                   $ (toRational $ length goldFls) / (toRational goldFlowsPerQ)
          goldNeeded = goldNQs - (S.size goldQsExcl)
          -- gold non-exclusive queues
          goldNonExcl = (S.size goldQs) - (S.size goldQsExcl)
          goldExtra  = -goldNeeded
          -- number of best effort queues
          beNQs = min (nq - goldNQs) (length beFls)
          beNeeded = beNQs - (S.size beQsExcl)

          CostVal balGold = balanceCost_ (S.toList goldQs) goldFls
          CostVal balBe   = balanceCost_ (S.toList restQs) beFls



-- | static cost function (flows that match predicate get a number of queues)
staticCost ::  (Flow -> Bool) -> Int -> Int -> QMap -> Cost
staticCost isGold nGoldQs' nq qmap = trN cost msg
    where
          -- FIXME: Currenly hardcoding the logic of how many gold queues are
          --        there based on total number of queues
          nGoldQs
            | nq == 5 = 2
            | nq == 10 = 4
            | nq == 9 = 4
--            | nq == 10 = 2  # for fancyecho as it will not have that many active flows
            | otherwise = error ("no. of queues is non-standard" ++
                    " (not 5 or 10). Given queues: " ++ (show nq))
          assignedGoldQsL = take nGoldQs $ allQueues nq -- static gold queues
          assignedGoldQsS = S.fromList assignedGoldQsL

          assignedRestQsL = drop nGoldQs $ allQueues nq -- static rest queues
          assignedRestQsS = S.fromList assignedRestQsL

          (goldFls, restFls) = L.partition (isGold . fst) qmap

          goldOK = and [ qid `S.member` assignedGoldQsS | (_,qid) <- goldFls ]
          restOK = and [ qid `S.member` assignedRestQsS | (_,qid) <- restFls ]

          CostVal balGold = balanceCost_ assignedGoldQsL goldFls
          CostVal balRest = balanceCost_ assignedRestQsL restFls
          msg = (qmapStr qmap) ++ " \n" ++ (show cost)
          cost
               | not goldOK = CostReject 1
               | not restOK = CostReject 1
               | length goldFls == 0 = CostVal balRest
               | length restFls == 0 = CostVal balGold
               | otherwise = CostVal $ balGold + balRest


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

connectFlows :: Int -> [Flow]
connectFlows start = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ start + i
   , flSrcIp    = Just 123
   , flSrcPort  = Just 7777 } | i <- [1..] ]

flAddedFlows :: [Flow] -> FL.FlowsSt
flAddedFlows flows = foldl FL.fsAddFlow FL.flowsStInit flows

hpIp = 222
beIp = 100
hpFlowsPerQ = 1
isHp FlowUDPv4 {flSrcIp = Just srcIp} = srcIp == hpIp
priorityCost' = priorityCost isHp hpFlowsPerQ
prioritySort' = prioritySort isHp

staticCost' = staticCost isHp 4

beFlows_ :: Int -> [Flow]
beFlows_ start = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ start + i
   , flSrcIp    = Just beIp
   , flSrcPort  = Just 7777 } | i <- [1..] ]

hpFlows_ :: Int -> [Flow]
hpFlows_ start = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ start + i
   , flSrcIp    = Just hpIp
   , flSrcPort  = Just 7777 } | i <- [1..] ]

test = do
    putStrLn $ "Running normal search!"
    -- unconfigured PRG
    prgU <- e10kU_simple
    --prgU <- e10kU_simple
    let nq = 10
        e10kOracle   = initE10kOracle nq
        priFn        = priorityCost' nq
        balFn        = balanceCost nq
        dummyFn      = dummyCost

        costFn = balFn
        params = initSearchParams {  sOracle = e10kOracle
                                   , sPrgU   = prgU
                                   , sCostFn = costFn
                                   , sStrategy = searchGreedyFlows}

        conflows = take 200 $ connectFlows 1000
        conf = runSearch params conflows
        prgC = C.applyConfig conf prgU
    --print $ "connected flows : " ++ (ppShow conflows)
    putStrLn $ "Configuration: " ++ (showConf  e10kOracle conf)
    writeFile "tests/search-prg-result.dot" $ toDot prgC

    return ()

test_normal_pravin = do
    putStrLn $ "Running normal search testcase!"
    -- unconfigured PRG
    prgU <- e10kU_simple
    --prgU <- e10kU_simple
    let nq = 5
        hpPort   = 6000
        bePort   = 1000

        e10kOracle   = initE10kOracle nq
        priFn        = priorityCost' nq
        sortFn       = prioritySort'
        balFn        = balanceCost nq
        dummyFn      = dummyCost
        costFn       = priFn

        params = initSearchParams {  sOracle = e10kOracle
                                   , sPrgU   = prgU
                                   , sCostFn = costFn
                                   , sOrderFlows = sortFn }

        initHpNr = 32
        initBeNr = (16 * 18)
        beFs = take initBeNr $ beFlows_ bePort
        hpFs = take initHpNr $ hpFlows_ hpPort
        flows = beFs ++ hpFs
        conf = runSearch params (flows)
        prgC = C.applyConfig conf prgU
    print $ "connected flows : " ++ (ppShow flows)
    putStrLn $ "Configuration: " ++ (showConf  e10kOracle conf)
    return ()


test_incr = do
    putStrLn $ "Running incremental search!"
    prgU <- e10kU_simple
    let nq = 10
        e10kOracle   = initE10kOracle nq
        priFn        = priorityCost' nq
        balFn        = balanceCost nq
        dummyFn      = dummyCost

        costFn = balFn
        params = initIncrSearchParams {  isOracle = e10kOracle
                                       , isPrgU   = prgU
                                       , isCostFn = costFn }

        conflows = flAddedFlows $ take 20 $ connectFlows 1000
        conf = runIncrSearch params conflows
        prgC = C.applyConfig conf prgU
    --print $ "connected flows : " ++ (ppShow conflows)
    putStrLn $ "Configuration: " ++ (showConf  e10kOracle conf)
    writeFile "tests/incr-search-prg-result.dot" $ toDot prgC

test_incr_add = do
    putStrLn $ "Running incremental search!"
    prgU <- e10kU_simple
    let nq = 10
        hpPort   = 6000
        bePort   = 1000
        e10kOracle = initE10kOracle nq
        pri = (priorityCost' nq, prioritySort')
        bal = (balanceCost nq, id)
        sta = (staticCost' nq, id)

        fns = pri
        params = initIncrSearchParams {  isOracle = e10kOracle
                                       , isPrgU   = prgU
                                       , isCostFn = fst fns
                                       , isOrderFlows = snd fns}

    ss0 <- initIncrSearchIO params
    qmap <- ST.stToIO $ incrSearchQmap ss0

    let initHpNr = 64
        initBeNr = 0
    (ss1, flst) <- doTimeIt ("INIT:" ++ (show initBeNr) ++ " BE Flows, " ++ (show initHpNr) ++ " HP flows") $ do
        let beFs = take initBeNr $ beFlows_ bePort
            hpFs = take initHpNr $ hpFlows_ hpPort
            flst0 = flAddedFlows beFs
            flst1 = foldl FL.fsAddFlow flst0 hpFs
        (conf, _, ss1) <- runIncrSearchIO ss0 flst1
        return (ss1, flst1)

    let n2 = 64
    (ss2,flst2) <- doTimeIt ("Add:" ++ (show n2) ++ " be flow(s)") $ do
        let beFls = take n2 $ beFlows_ (bePort + initBeNr)
            flst2 = L.foldl FL.fsAddFlow (FL.fsReset flst) beFls
        (conf, _, ss2) <- runIncrSearchIO ss1 flst2
        --putStrLn $ "Configuration: " ++ (showConf  e10kOracle conf)
        return (ss2,flst2)

    let n3 = 0
    (ss3,flst3) <- doTimeIt ("Add:" ++ (show n3) ++ " hp flow(s)") $ do
        let hpFls = take n3 $ hpFlows_ (hpPort + initHpNr)
            flst3 = L.foldl FL.fsAddFlow (FL.fsReset flst2) hpFls
        (conf, _, ss3) <- runIncrSearchIO ss2 flst3
        --putStrLn $ "Configuration: " ++ (showConf  e10kOracle conf)
        return (ss3,flst3)

    let n4 = 0
    ss4 <- doTimeIt ("Add:" ++ (show n4) ++ " hp flow(s)") $ do
        let hpFls = take n4 $ hpFlows_ (hpPort + initHpNr + n3)
            flst4 = L.foldl FL.fsAddFlow (FL.fsReset flst3) hpFls
        (conf, _, ss4) <- runIncrSearchIO ss3 flst4
        --putStrLn $ "Configuration: " ++ (showConf  e10kOracle conf)
        return (ss4)

    qmap <- ST.stToIO $ incrSearchQmap ss4
    putStrLn $ qmapStr qmap
    --let flows = FM.fmFlows $ isFlowMapSt ss3
    --putStrLn $ "Flows:\n" ++ (FL.flowsStr flows)
    return ()

    --let prgC = C.applyConfig conf prgU
    --writeFile "tests/incr-search-prg-result.dot" $ toDot prgC
    return ()


test_incr_pravin_testcase_mixed = do

    putStrLn $ "Running incremental search usecase used for mixed benchmarking"
    prgU <- e10kU_simple
    let nq = 10
    --let nq = 5
        flowsPerGQ   = 16
        hpPort   = 6000
        bePort   = 1000
        e10kOracle = initE10kOracle nq
        priFn = (priorityCost isHp flowsPerGQ) nq
        balFn = balanceCost nq
        costFn = priFn
        sortFn = prioritySort'
        pri = (priFn, prioritySort')
        bal = (balFn, id)
        myStaticCost = (staticCost isHp 4)
        sta = (myStaticCost nq, id)
        --fns = sta
        fns = pri -- <-- priority does not work for 5 queues
                  -- For 10 queues, search works, but there is flow remapping
                  --    happening which is breaking the stack

        params = initIncrSearchParams {  isOracle = e10kOracle
                                       , isPrgU   = prgU
                                       , isCostFn = fst fns
                                       , isOrderFlows = snd fns}

    ss0 <- initIncrSearchIO params

    -- Initial load of 2 HP and 18 BF clients
    --      each client with 16 flows
    let initHpNr = (16 * 2)
        initBeNr = (16 * 18)
    (ss1, flst) <- doTimeIt ("INIT:" ++ (show initBeNr) ++
                " BE Flows ++ " ++ (show initHpNr) ++ " HP flow:") $ do
        let beFs = take initBeNr $ beFlows_ bePort
            hpFs = take initHpNr $ hpFlows_ hpPort
            flst0 = flAddedFlows beFs
            flst1 = foldl FL.fsAddFlow flst0 hpFs
        (conf, _, ss1) <- runIncrSearchIO ss0 flst1
        return (ss1, flst1)

    qmap <- ST.stToIO $ incrSearchQmap ss1
    putStrLn $ qmapStr qmap


{-
    -- Initial load of 2 HP and 18 BF clients
    --      each client with 16 flows
    (ss1', flst') <- doTimeIt ("INIT:" ++ (show 16) ++
                " HP Flows ++ " ++ (show 16) ++ " BE flow:") $ do
        let beFs' = take 16 $ beFlows_ bePort
            hpFs' = take 16 $ hpFlows_ hpPort
            flst0' = flAddedFlows hpFs'
            flst1' = foldl FL.fsAddFlow flst0' beFs'
        (conf, _, ss1') <- runIncrSearchIO ss1 flst1'
        return (ss1', flst1')

    qmap' <- ST.stToIO $ incrSearchQmap ss1'
    putStrLn $ qmapStr qmap'
-}


    -- now add 1 BE client (16 BE flows)
    let n2 = 16
    (ss2,flst2) <- doTimeIt ("Add:" ++ (show n2) ++ " BE flow(s)") $ do

        let beFls = take n2 $ beFlows_ (bePort + initBeNr)
            flst2 = L.foldl FL.fsAddFlow (FL.fsReset flst) beFls
        (conf, _, ss2) <- runIncrSearchIO ss1 flst2
        putStrLn $ "Configuration after adding first BE client: "
                ++ (showConf  e10kOracle conf)
        return (ss2,flst2)

{-
    -- now add 1 HP client (16 HP flows)
    let n3 = 16
    (ss3,flst2) <- doTimeIt ("Add:" ++ (show n3) ++ " HP flow(s)") $ do

        let hpFls = take n3 $ hpFlows_ (hpPort + initHpNr)
            flst3 = L.foldl FL.fsAddFlow (FL.fsReset flst) hpFls
        (conf, _, ss3) <- runIncrSearchIO ss2 flst3
        putStrLn $ "Configuration after adding second HP client: "
                ++ (showConf  e10kOracle conf)
        return (ss3,flst2)
-}
    let ss3 = ss2
    qmap <- ST.stToIO $ incrSearchQmap ss3
    putStrLn $ qmapStr qmap
    --let flows = FM.fmFlows $ isFlowMapSt ss2
    --putStrLn $ "Flows:\n" ++ (FL.flowsStr flows)
    return ()

    --let prgC = C.applyConfig conf prgU
    --writeFile "tests/incr-search-prg-result.dot" $ toDot prgC
    return ()


test_incr_pravin_testcase = do
    putStrLn $ "Running incremental search usecase that is cause problem!"
    prgU <- e10kU_simple
    let nq = 5
        hpPort   = 6000
        bePort   = 1000
        e10kOracle = initE10kOracle nq
        priFn = priorityCost' nq
        balFn = balanceCost nq
        costFn = priFn
        sortFn = prioritySort'
        params = initIncrSearchParams {  isOracle = e10kOracle
                                       , isPrgU   = prgU
                                       , isCostFn = costFn
                                       , isOrderFlows = sortFn}

    ss0 <- initIncrSearchIO params

    -- initially 2 HP, 8 BE flows
    let initHpNr = 2
        initBeNr = 8
    (ss1, flst) <- doTimeIt ("INIT:" ++ (show initBeNr) ++
                " BE Flows ++ " ++ (show initHpNr) ++ " HP flow:") $ do
        let beFs = take initBeNr $ beFlows_ bePort
            hpFs = take initHpNr $ hpFlows_ hpPort
            flst0 = flAddedFlows beFs
            flst1 = foldl FL.fsAddFlow flst0 hpFs
        (conf, _, ss1) <- runIncrSearchIO ss0 flst1
        return (ss1, flst1)

    qmap <- ST.stToIO $ incrSearchQmap ss1
    putStrLn $ "QMAP:" ++ qmapStr qmap

    let n2 = 1
    (ss2,flst2) <- doTimeIt ("Add:" ++ (show n2) ++ " HP flow(s)") $ do
        let hpFls = take n2 $ hpFlows_ (hpPort + initHpNr)
            flst2 = L.foldl FL.fsAddFlow (FL.fsReset flst) hpFls
        (conf, _, ss2) <- runIncrSearchIO ss1 flst2
        --putStrLn $ "Configuration: " ++ (showConf  e10kOracle conf)
        return (ss2,flst2)

    qmap2 <- ST.stToIO $ incrSearchQmap ss2
    putStrLn $ qmapStr qmap2
    --let flows = FM.fmFlows $ isFlowMapSt ss2
    --putStrLn $ "Flows:\n" ++ (FL.flowsStr flows)
    return ()

    --let prgC = C.applyConfig conf prgU
    --writeFile "tests/incr-search-prg-result.dot" $ toDot prgC
    return ()

-- there used to be a bug when configuring a 5t filter and then a cfdir filter
--testFmCfdir :: IO ()
--testFmCfdir = do
--    prgU <- e10kU_simple
--    let flows@(fl0:fl1:[]) = take 2 $ connectFlows 1000
--        cc0 = E10k.insert5tFromFl fl0 1
--        cc1 = fromJust $ E10k.insertFdirFromFl fl1 1
--
--    fmSt1 <- ST.stToIO $ do
--        s0 <- FM.initFlowMapSt prgU
--        s1 <- foldM FM.addFlow s0 flows
--        s2 <- FM.incrConfigure s1 cc0
--        s3 <- FM.incrConfigure s2 cc1
--        return s3
--
--    fmSt2 <- ST.stToIO $ do
--        s0 <- FM.initFlowMapSt prgU
--        s1 <- FM.incrConfigure s0 cc0
--        s2 <- FM.incrConfigure s1 cc1
--        s3 <- foldM FM.addFlow s2 flows
--        return s3
--
--    qmap  <- ST.stToIO $ FM.getRxQMap fmSt2
--    putStrLn $ qmapStr qmap
--
--    let prg = FM.fmGraph fmSt2
--    writeFile "tests/incr-search-prg-result.dot" $ toDot prg
--    return ()


