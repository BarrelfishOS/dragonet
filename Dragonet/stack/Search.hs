-- Simple search functions
module Search (
  e10kCost,
  balanceCost,
  searchGreedyE10k,
  test
) where


import Dragonet.Configuration as C
import Dragonet.ProtocolGraph as PG
import Dragonet.DotGenerator (toDot)
import Dragonet.Flows (Flow(..))

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word
import Data.Maybe
import Data.Int
import Data.Function (on)

import Dragonet.Flows (Flow (..))

import Text.Show.Pretty (ppShow)

import qualified Graphs.E10k as E10k
import Graphs.Cfg (prgCfg, prgCfgEmpty, e10kCfgEmpty, e10kCfgStr)

import Control.Applicative ((<$>))

import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)

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

-- cost functions based on how flows are mapped into queues
type QueueId = Int
type CostQueueFn = [(Flow, QueueId)] -> Cost

--- Take a graph and a cost function using queue assignments and return a cost
--function using flows and the current configuration.
e10kCost :: PG.PGraph -> CostQueueFn -> CostFn
e10kCost prgU costQFn = retFn
    where retFn :: [Flow] -> C.Configuration -> Cost
          retFn flows conf = ret
              where prgC = C.applyConfig conf prgU
                    flowQ :: PG.PGraph -> Flow -> QueueId
                    flowQ = E10k.flowQueue
                    --flowQ _ _ = 0
                    qmap = [(fl, flowQ prgC fl) | fl <- flows]
                    ret_ = costQFn qmap
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
searchGreedyFlows_ nq costF (cnf,_) []= trN cnf $ "searchGreedyFlows_:"
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
                ++ "\nSELECTED " ++ (e10kCfgStr xCnf)
                ++ "\nALL CONFS" ++ (L.intercalate "--\n--" $ map (e10kCfgStr . fst) confs)

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



-- Code for performing simple tests

e10kT = E10k.graphH
e10kU = fst <$> e10kT
e10kH = snd <$> e10kT

e10kC = (C.applyConfig prgCfg) <$> e10kU

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
    --e10k <- e10kC
    --writeFile "tests/e10kC.dot" $ toDot e10k
    prgU <- e10kU

    let priFn = e10kCost prgU (priorityCost' nq)
        balFn = e10kCost prgU (balanceCost nq)

        costFn = priFn

        conf  = searchGreedyConfE10k nq costFn fs
        --conf  = searchGreedyFlowsE10k nq costFn fs

    putStrLn $ e10kCfgStr conf
    putStrLn $ "Cost:" ++ (show $ costFn fs conf)
    return (prgU, conf)
