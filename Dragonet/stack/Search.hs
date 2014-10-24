{-# LANGUAGE RankNTypes, LiberalTypeSynonyms, ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
-- Simple search functions
module Search (
  evalSearch,
  runSearch,
  SearchSt(..),
  initSearchSt,
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
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Implementation.IPv4 as IP4

import Dragonet.DotGenerator (toDot)
import Dragonet.Conventions (rxQPref, isTruePort, isFalsePort)
import Dragonet.Flows(Flow (..), flowPred)

import qualified Graphs.E10k as E10k
import Graphs.Cfg (prgCfg, prgCfgEmpty, e10kCfgEmpty, e10kCfgStr)

import qualified Data.List       as L
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import Data.Word
import Data.Maybe
import Data.Int
import Data.Function (on)
import Data.Char (isDigit)

import qualified Control.Monad.State.Strict as ST
import Control.Applicative ((<$>))

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

tr a b  = trace b a
trN a b = a

allQueues :: Int -> [QueueId]
allQueues nq = [1..(nq-1)] ++ [0] :: [QueueId]

-- Search: provide the best configuration given a set of flows
-- TODO: incremental

-- Oracle: iterate configuration space
-- (we will probably have to rethink the interface)
class OracleSt a where
 -- seems too OO, but not sure how to do it better
 emptyConf  ::       a -> C.Configuration
 showConf   ::       a -> C.Configuration -> String
 -- iterators
 flowConfs  ::       a -> C.Configuration -> Flow   -> [C.Configuration]
 flowsConfs ::       a -> C.Configuration -> [Flow] -> [C.Configuration]
 flowsSingleConfs :: a -> C.Configuration -> [Flow] -> [(Flow, C.Configuration)]
 -- default implementations
 flowsConfs x c [] = [c]
 flowsConfs x c (f:fs) = L.concat [flowsConfs x newc fs | newc <- newcs]
    where newcs = flowConfs x c f
 flowsSingleConfs x c flows =
    L.concat [[(f,newc) | newc <- flowConfs x c f ] | f <- flows]

-- E10K (simple for now) oracle
newtype E10kOracleSt = E10kOracleSt { nQueues :: Int }
instance OracleSt E10kOracleSt where
    flowConfs E10kOracleSt { nQueues = nq } c fl =
        [add5TupleToConf c fl q |  q <- allQueues nq]
    emptyConf _ = e10kCfgEmpty
    showConf _  = e10kCfgStr

type SearchStrategy o =
  (OracleSt o) => CostFnM -> o -> [Flow] -> Search o C.Configuration

data SearchSt o = (OracleSt o) => SearchSt {
      sOracle     :: o
    , sPrgU       :: PG.PGraph
    , sCostFn     :: CostQueueFn -- cost function
    , sOrderFlows :: [Flow] -> [Flow] -- order (sort) flows as a heuristic
    , sStrategy   :: SearchStrategy o
    , sFlowCache  :: M.Map (PG.NLabel, Flow) PG.NPort
}

-- initialize search state (add default values when applicable)
initSearchSt :: (OracleSt o) => SearchSt o
initSearchSt = SearchSt {
      sOracle     = undefined
    , sPrgU       = undefined
    , sCostFn     = undefined
    , sStrategy   = undefined
    , sOrderFlows = id
    , sFlowCache  = M.empty
}


newtype Search o a = Search {unSearch :: ST.State  (SearchSt o) a}
    deriving (Monad, Functor, ST.MonadState (SearchSt o))

doSearch :: (OracleSt o) => [Flow] -> (Search o) C.Configuration
doSearch flows = do
    costFn <- ST.gets sCostFn
    sortFn <- ST.gets sOrderFlows
    oracle <- ST.gets sOracle
    search <- ST.gets sStrategy
    prgU   <- ST.gets sPrgU
    let flows' = sortFn flows
        costFn' :: (OracleSt o) => [Flow] -> C.Configuration -> (Search o) Cost
        costFn' fls conf = do
            let prgC = C.applyConfig conf prgU
            -- NOTE: There is still some code around trying to compute qmap with
            -- different ways:
            --  - E10k.flowQueue: old E10k-specific version of flowQueue
            --  - yQmap and xQmap
            qmap <- ST.forM fls $ \fl -> do
                q <- flowQueue prgC fl
                return (fl, q)
            return $ costFn qmap

    conf <- search costFn' oracle flows'
    return conf

runSearch :: (OracleSt o)
          => SearchSt o -> [Flow] -> (C.Configuration, SearchSt o)
runSearch st flows = ST.runState (unSearch $ doSearch flows) st

evalSearch :: (OracleSt o)
          => SearchSt o -> [Flow] -> C.Configuration
evalSearch st flows = ST.evalState (unSearch $ doSearch flows) st


-- searchGreedyFlows: examine one flow at a time. Depends on the ordering of
-- flows. We can use that as a heuristic
searchGreedyFlows :: SearchStrategy o
searchGreedyFlows costFn oracle flows = searchGreedyFlows_ costFn oracle st0 flows
    where st0 = (emptyConf oracle, []) -- initial state

searchGreedyFlows_ :: OracleSt o
                   => CostFnM
                   -> o
                   -> (C.Configuration, [Flow])
                   -> [Flow]
                   -> (Search o) C.Configuration

searchGreedyFlows_ _ o (cnf,_) [] = return $ trN cnf msg
    where msg = ("searchGreedyFlows_:" ++ (showConf o cnf))

searchGreedyFlows_ costFn oracle (curCnf, curFlows) (f:fs) = do
    let confs    = flowConfs oracle curCnf f
        newCurFs = f:curFlows

    conf_costs <- ST.forM confs $ \cnf -> do
        cnfCost <- costFn newCurFs cnf
        return (cnf, cnfCost)

    let (best_cnf, lower_cost) =  L.minimumBy (compare `on` snd) conf_costs
        msg = "searchGreedyFlows_: step:"
            ++ (show $ length curFlows)
            ++ " cost is " ++ (show lower_cost)
            ++ "\nSELECTED " ++ (showConf oracle best_cnf)

    recurse <- searchGreedyFlows_ costFn oracle (best_cnf, newCurFs) fs
    return $ trN recurse msg


-- searchGreedyConf :
--  examines all flows, and determines a single
--  configuration value. Removes the flow that is paired with the configuration
--  values and moves on. This can avoid some problems of the previous one
--  (it really depends on the cost function), but is more expensive.
searchGreedyConf :: SearchStrategy o
searchGreedyConf costFn oracle flows = searchGreedyConf_ costFn oracle st0 flows
    where st0 = (emptyConf oracle, []) -- initial state

searchGreedyConf_ :: OracleSt o
                   => CostFnM
                   -> o
                   -> (C.Configuration, [Flow])
                   -> [Flow]
                   -> (Search o) C.Configuration

searchGreedyConf_ _ o (cnf,_) [] = return $ trN cnf msg
    where msg = ("searchGreedyConf_:" ++ (showConf o cnf))

searchGreedyConf_ costFn oracle (curCnf, curFlows) flows = do
    let confs :: [(Flow, C.Configuration)]
        confs = flowsSingleConfs oracle curCnf flows
        all_flows = curFlows ++ flows

    costs <- ST.forM confs $ \x@(fl, cnf) -> do
        cnfCost <- costFn all_flows cnf
        return (x, cnfCost)

    let ((xFl, xCnf), xCost) = L.minimumBy (compare `on` snd) costs
        xFlows = L.delete xFl flows

    recurse <- searchGreedyConf_ costFn oracle (xCnf, xFl:curFlows) xFlows

    let msg = "searchGreedyConf_: step:"
            ++ (show $ length curFlows)
            ++ " cost is " ++ (show xCost)
            ++ "\nSELECTED "
            ++ (e10kCfgStr xCnf)
            ++ "\nALL CONFS"
    return $ trN recurse msg

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

-- the basic form of a cost function for the search algorithms
type CostFn  = [Flow] -> C.Configuration -> Cost
type CostFnM  = (OracleSt o) => [Flow] -> C.Configuration -> (Search o) Cost
-- cost functions based on how flows are mapped into queues
type QueueId = Int
type CostQueueFn = [(Flow, QueueId)] -> Cost

-- cost functions

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

balanceCost_ :: [QueueId] -> [(Flow, QueueId)] -> Cost
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

balanceCost :: Int -> [(Flow, QueueId)] -> Cost
balanceCost nq fls = balanceCost_ (allQueues nq) fls

-- gold/best-effort priorities
priorityCost :: (Flow -> Bool) -> Integer -> Int -> [(Flow, QueueId)] -> Cost
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
            | length beFls   == 0 = balanceCost_ (allQueues nq) goldFls
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

doFlowNextPort :: (OracleSt o)
               => (Flow, PR.PredExpr) -> PG.Node -> (Search o) PG.NPort
doFlowNextPort (_, flPred) node = return $ doFlowQueueNextPort_ flPred nPreds
    where nPreds = PG.nPredicates node

doFlowNextPortCache :: (OracleSt o)
               => (Flow, PR.PredExpr) -> PG.Node -> (Search o) PG.NPort
doFlowNextPortCache x@(fl, flPred) node = do
    let key = (PG.nLabel node, fl)
    flowCache <- ST.gets sFlowCache
    case M.lookup key flowCache of
        Just x -> return $ trN x "HIT"
        Nothing -> do
            ret <- doFlowNextPort x node
            ST.modify $ \s -> s {sFlowCache = M.insert key ret flowCache}
            return $ trN ret "MISS"


doFlowQueue :: (OracleSt o)
            => PG.PGraph -> PG.PGNode -> (Flow, PR.PredExpr) -> (Search o) QueueId
doFlowQueue g (nid,node@(PG.FNode {PG.nLabel = lbl})) flowT
    | Just q <- reachedQueue (nid,node) = return q
    | otherwise = do
          nextPort <- doFlowNextPortCache flowT node
          let nextNodel = [ n | (n,e) <- PGU.edgeSucc g (nid,node),
                                         PGU.edgePort e == nextPort]
              nextNode = case nextNodel of
                  [n] -> doFlowQueue g n flowT
                  []  -> error $ "doFlowQueue: port" ++ nextPort ++ "not found"
                  _   -> error $ "doFlowQueue: more than one edges " ++ nextPort ++ " found in F-node " ++ lbl
          nextNode

doFlowQueue g (nid,node@(PG.ONode {PG.nLabel = lbl})) flowT
    | Just q <- reachedQueue (nid,node) = return q
    | otherwise = nextNode
    -- In theory we need to determine the next port based on the incoming port.
    -- However, currently there is no way to determine what are the "true"
    -- incoming edges, so we resort into a hack where we always pick the "true"
    -- port as the nextPort when encountering an ONode
    where nextPort = "true"
          nextNodel = [ n | (n,e) <- PGU.edgeSucc g (nid,node),
                            PGU.edgePort e == nextPort]
          nextNode = case nextNodel of
             [n] -> doFlowQueue g n flowT
             []  -> error $ "doFlowQueue: port" ++ nextPort ++ "not found"
             _  -> error $ "doFlowQueue: more than one edges " ++ nextPort ++ " found in O-node " ++ lbl

flowQueue :: (OracleSt o) => PG.PGraph -> Flow -> (Search o) QueueId
flowQueue prgC flow = do
    let flPred = flowPred flow
        node1 = flowQueueStart prgC
    doFlowQueue prgC node1 (flow, flPred)

flowQueueStart :: PG.PGraph -> PG.PGNode
flowQueueStart prgC = node1
    where
          node0_ = "RxL2EtherClassifyL3_"
          port0 = "other"
          node0 = case GH.filterNodesByL (\x -> (PG.nLabel x) == node0_) prgC of
            [x] -> x
            []  -> error $ "No matches for node:" ++ node0_
            _   -> error $ "More than one matches for node:" ++ node0_
          node1 = case [ n | (n,e) <- PGU.edgeSucc prgC node0,
                          PGU.edgePort e == port0] of
            [x] -> x
            _   -> error $ "More than one connection matches for node:"
                           ++ node0_ ++ " port:" ++ port0


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
        searchSt = initSearchSt {  sOracle = e10kOracle
                                 , sPrgU   = prgU
                                 , sCostFn = costFn
                                 , sStrategy = searchGreedyFlows}
        cnf = evalSearch searchSt fs

    return (cnf)

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

