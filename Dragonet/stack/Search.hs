-- Simple search functions
module Search (
  e10kCost,
  sfCost,
  balanceCost,
  searchGreedyE10k,
  searchGreedySF,
  test
) where


import Dragonet.Configuration as C
import Dragonet.ProtocolGraph as PG
import Dragonet.DotGenerator (toDot)
import Dragonet.Flows (Flow(..))
import Dragonet.Conventions (rxQPref)
import Dragonet.Conventions (isTruePort, isFalsePort)

import qualified Util.GraphHelpers as GH
import qualified Dragonet.Predicate as PR
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word
import Data.Maybe
import Data.Int
import Data.Function (on)
import Data.Char (isDigit)

import Graphs.Cfg as CFG

import Dragonet.Flows (Flow (..), flowPred)

import qualified Graphs.E10k as E10k
import Graphs.Cfg (prgCfg, prgCfgEmpty, e10kCfgEmpty, e10kCfgStr)

import Control.Applicative ((<$>))
import Debug.Trace (trace)

import Text.Show.Pretty (ppShow)


tr a b  = trace b a
trN a b = a

allQueues :: Int -> [QueueId]
allQueues nq = [1..(nq-1)] ++ [0] :: [QueueId]

--
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

-- cost functions

--
-- import Control.Parallel.Strategies as P
-- cost functions based on how flows are mapped into queues
type QueueId = Int
type CostQueueFn = [(Flow, QueueId)] -> Cost

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

--- Take a graph and a cost function using queue assignments and return a cost
--function using flows and the current configuration.
e10kCost :: PG.PGraph -> CostQueueFn -> CostFn
e10kCost prgU costQFn = retFn
    where retFn :: [Flow] -> C.Configuration -> Cost
          retFn flows conf = ret
              where prgC = C.applyConfig conf prgU
                    flowQ1 :: PG.PGraph -> Flow -> QueueId
                    flowQ1 = E10k.flowQueue
                    flowQ2 = flowQueue
                    --flowQ _ _ = 0
                    qmap :: [(Flow, QueueId)]
                    qmap_ = [(fl, flowQ1 prgC fl) | fl <- flows]
                    qmap2 = yQmap prgC flows
                    qmap3 = [(fl, flowQ2 prgC fl) | fl <- flows]

                    test  = (L.sort qmap_) == (L.sort qmap2)
                    test3 = (L.sort qmap_) == (L.sort qmap3)
                    qmap = case test3 of
                            True -> qmap3
                            False -> error msg
                    msg = ("CHECKME ---->\n"
                            ++ ppShow qmap_
                            ++ "========"
                            ++ ppShow qmap2
                            ++ "<-----")

                    ret_ = costQFn qmap_
                    ret = trN ret_ $ "conf:" ++ (e10kCfgStr conf)
                                    ++ "\ncost:" ++ (show ret_)

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

dummyCost :: Int -> [(Flow, QueueId)] -> Cost
dummyCost _ _ = CostVal 1.0

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

-- greedy search
--  searchGreedyFlowsE10k: examine one flow at a time. Depends on the ordering
--  of flows. We can use that as a heuristic
--  searchGreedyConfE10k: examines all flows, and determines a single
--  configuration value. Removes the flow that is paired with the configuration
--  values and moves on. This can avoid some problems of the previous one
--  (it really depends on the cost function), but is more expensive.

searchGreedyE10k :: Int -> CostFn -> [Flow] -> C.Configuration
searchGreedyE10k = searchGreedyFlowsE10k

searchGreedyFlowsE10k :: Int -> CostFn -> [Flow] -> C.Configuration
searchGreedyFlowsE10k nq fn flows =
    searchGreedyFlows_ nq fn (e10kCfgEmpty,[]) flows


searchGreedyFlows_ :: Int
                   -> CostFn
                   -> (C.Configuration, [Flow])
                   -> [Flow]
                   -> C.Configuration
searchGreedyFlows_ nq costF (cnf,_) []= tr cnf $ "searchGreedyFlows_:"
                                              ++ (e10kCfgStr cnf)
searchGreedyFlows_ nq costF (curCnf,curFs) (f:fs) = trN recurse msg
    where confs  = flAllConfs nq f curCnf
          newCurFs = f:curFs
          conf_costs = [(cnf,costF newCurFs cnf) | cnf <- confs]
          (best_cnf, lower_cost)  = L.minimumBy (compare `on` snd) conf_costs
          recurse  = searchGreedyFlows_ nq costF (best_cnf, newCurFs) fs
          msg = "searchGreedyConf_: step:"
                ++ (show $ length curFs)
                ++ " cost is " ++ (show lower_cost)
                ++ "\nSELECTED " ++ (e10kCfgStr best_cnf)
                -- ++ "\nALL CONFS" ++ (L.intercalate "--\n--" $ map (e10kCfgStr . fst) confs)

searchGreedyConfE10k :: Int -> CostFn -> [Flow] -> C.Configuration
searchGreedyConfE10k nq fn flows
    = searchGreedyConf_ nq fn (e10kCfgEmpty,[]) flows


searchGreedyConf_ :: Int
                  -> CostFn
                  -> (C.Configuration, [Flow])
                  -> [Flow]
                  -> C.Configuration
searchGreedyConf_ nq costF (cnf,_) [] = trN cnf $ "searchGreedyConf_: "
                                              ++ (e10kCfgStr cnf)
searchGreedyConf_ nq costF (curCnf, curFs) flows = trN recurse msg
    where confs :: [(C.Configuration, Flow)]
          confs = flsAllSingleConfs nq curCnf flows
          all_flows = curFs ++ flows
          costs = [ ((fl, cnf), costF all_flows cnf) | (cnf,fl) <- confs ]
          ((xFl, xCnf), xCost) = L.minimumBy (compare `on` snd) costs
          xFlows = L.delete xFl flows
          recurse = searchGreedyConf_ nq costF (xCnf, xFl:curFs) xFlows

          msg = "searchGreedyConf_: step:"
                ++ (show $ length curFs)
                ++ " cost is " ++ (show xCost)
                ++ "\nSELECTED "
                ++ (e10kCfgStr xCnf)
                ++ "\nALL CONFS"
                ++ (L.intercalate "--\n--" $ map (e10kCfgStr . fst) confs)

-- greedy back-track search. Nothing is returned if no suitable configuration is
-- found
data SearchGBSt = SearchGBSt {
      -- this is actually a stack. The top of the list contains the last
      -- endpoint we added, and the current configuration
      sgbOptPath  :: [(Flow, C.Configuration)]
    , sgbCostFn   :: CostFn
    , sgbOrdEs    :: [Flow] -> [Flow]
}
--
-- initial state
searchGBSt0 fn = SearchGBSt {
      sgbOptPath  = []
    , sgbCostFn   = fn
    , sgbOrdEs    = id
}

-- greedy back-track search. Nothing is returned if no suitable configuration is
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


-- TODO: add the empty configuration in the functions below

-- generate all possible configurations given an existing configuration and a
-- set of endpoints
flsAllConfs :: Int -> C.Configuration -> [Flow] -> [C.Configuration]
flsAllConfs nq c [] = [c]
flsAllConfs nq c (e:fs) = L.concat [ flsAllConfs nq newc fs | newc <- cs]
    where cs = flAllConfs nq e c

-- generate all posible configurations that consist of a single change
-- configurations are paired with the correpoding flow
flsAllSingleConfs :: Int -> C.Configuration -> [Flow] -> [(C.Configuration,Flow)]
flsAllSingleConfs nq c0 fls =
    L.concat [[(c,f) | c <- flAllConfs nq f c0] | f <- fls]

-- generate all possible configurations based on a single flow
flAllConfs :: Int -> Flow -> C.Configuration -> [C.Configuration]
flAllConfs nq fl oldConf = [ add5TupleToConf oldConf fl q |  q <- (allQueues nq)]

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

doFlowQueueNextPort :: PR.PredExpr -> [(PG.NPort, String)] -> PG.NPort
doFlowQueueNextPort flPred ((port,portPredStr):rest) = ret
    where sat     = isJust $ PR.dnfSAT andExpr
          andExpr = (PR.buildAND bld) [portPred,flPred]
          portPred = PR.parseStr_ bld portPredStr
          bld = PR.predBuildDNF
          -- NB: we assuem that each flow is assigned exclusively to a singel
          -- queue, so we return the first match
          ret = case sat of
                True  -> port
                False -> doFlowQueueNextPort flPred rest

doFlowQueue :: PG.PGraph -> PG.PGNode -> PG.NPort -> PR.PredExpr -> QueueId
doFlowQueue g (nid,node@(PG.FNode {PG.nLabel = lbl})) inPort flPred
    | Just q <- reachedQueue (nid,node) = q
    | otherwise = nextNode
    where nPreds = PG.nPredicates node
          nPorts = PG.nPorts node
          nextPort = doFlowQueueNextPort flPred nPreds
          nextNodel = [ n | (n,e) <- PGU.edgeSucc g (nid,node),
                            PGU.edgePort e == nextPort]
          nextNode = case nextNodel of
             [n] -> doFlowQueue g n nextPort flPred
             []  -> error $ "doFlowQueue: port" ++ nextPort ++ "not found"
             _  -> error $ "doFlowQueue: more than one edges " ++ nextPort ++ " found in F-node " ++ lbl

doFlowQueue g (nid,node@(PG.ONode {PG.nLabel = lbl})) inPort flPred
    | Just q <- reachedQueue (nid,node) = q
    | otherwise = nextNode
    where nextPort = "true" -- small hack because it's not trivial to determine the true port
          nextNodel = [ n | (n,e) <- PGU.edgeSucc g (nid,node),
                            PGU.edgePort e == nextPort]
          nextNode = case nextNodel of
             [n] -> doFlowQueue g n nextPort flPred
             []  -> error $ "doFlowQueue: port" ++ nextPort ++ "not found"
             _  -> error $ "doFlowQueue: more than one edges " ++ nextPort ++ " found in O-node " ++ lbl

flowQueue :: PG.PGraph -> Flow ->  QueueId
flowQueue prgC flow = ret
    where ret = doFlowQueue prgC node1 port0 flPred
          flPred = flowPred flow
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
    let nq = 10
    --e10k <- e10kC_simple
    --writeFile "tests/e10kC.dot" $ toDot e10k
    --prgU <- e10kU
    prgU <- e10kU_simple

    let priFn = e10kCost prgU (priorityCost' nq)
        balFn = e10kCost prgU (balanceCost nq)
        dummyFn = e10kCost prgU (dummyCost nq)

        costFn = priFn

        --conf  = searchGreedyConfE10k nq costFn fs
        conf  = searchGreedyFlowsE10k nq costFn fs

    putStrLn $ e10kCfgStr conf
    putStrLn $ "Cost:" ++ (show $ costFn fs conf)
    return (prgU, conf)


-- Solarflare specific function
--      Currently they just a copy of E10k functions
sfCost = e10kCost

searchGreedySF = searchGreedyE10k


