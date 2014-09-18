-- Simple search functions
module Search (
  e10kCost,
  balanceCost,
  searchGreedyE10k
) where


import Dragonet.Configuration as C
import Dragonet.ProtocolGraph as PG
import Dragonet.DotGenerator (toDot)
import Dragonet.Flows (Flow(..))

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Word
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
            CostReject
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

balanceCost :: Int -> [(Flow, QueueId)] -> Cost
balanceCost nq qmap = ret
    where qLoad :: M.Map QueueId Integer
          qList  =  [qId | (_,qId) <- qmap]
          qLoad = L.foldl foldFn M.empty qList
          foldFn :: M.Map QueueId Integer -> QueueId -> M.Map QueueId Integer
          foldFn m qid = M.insertWith (+) qid 1 m

          getQLoad :: QueueId -> Integer
          getQLoad q = M.findWithDefault 0 q qLoad
          load    = map getQLoad (allQueues nq)
          maxLoad = maximum load
          minLoad = minimum load
          ret_     =  CostVal $ fromIntegral $ maxLoad - minLoad
          ret = trN ret_ ("\nqlist" ++ (ppShow qList) ++ "\nLOAD:" ++ ppShow (load))

priorityCost :: (Flow -> Bool) -> [(Flow, QueueId)] -> Cost
priorityCost = error "NYI!"


-- simple greedy search

searchGreedyE10k :: Int -> CostFn -> [Flow] -> C.Configuration
searchGreedyE10k nq fn flows = searchGreedy_ nq fn (e10kCfgEmpty,[]) flows

searchGreedy_ :: Int -> CostFn -> (C.Configuration, [Flow]) -> [Flow] -> C.Configuration
searchGreedy_ nq costF (cnf,_)        []     = tr cnf $ "searchGreedy_: " ++ (e10kCfgStr cnf)
searchGreedy_ nq costF (curCnf,curFs) (f:fs) = recurse
    where confs      = flAllConfs nq f curCnf
          newCurFs   = f:curFs
          conf_costs = [(cnf,costF newCurFs cnf) | cnf <- confs]
          best_cnf_  = fst $ L.minimumBy (compare `on` snd) conf_costs
          best_cnf   = trN best_cnf_ $ "SELECTED " ++ (e10kCfgStr best_cnf_)
          recurse    = searchGreedy_ nq costF (best_cnf, newCurFs) fs

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


-- generate all possible configurations given an existing configuration and a
-- set of endpoints
flsAllConfs :: Int -> C.Configuration -> [Flow] -> [C.Configuration]
flsAllConfs nq c [] = [c]
flsAllConfs nq c (e:fs) = L.concat [ flsAllConfs nq newc fs | newc <- cs]
    where cs = flAllConfs nq e c

flAllConfs :: Int -> Flow -> C.Configuration -> [C.Configuration]
flAllConfs nq fl oldConf = [ add5TupleToConf oldConf fl q |  q <- (allQueues nq)]

mk5TupleFromFl :: Flow -> QueueId -> PG.ConfValue
mk5TupleFromFl fl@(FlowUDPv4Conn {}) q =
    PG.CVTuple [ cvMInt $ Just sIP,
    cvMInt $ Just dIP,
    PG.CVMaybe $ Just $ PG.CVEnum 1,
    cvMInt $ Just sP,
    cvMInt $ Just dP,
    PG.CVInt prio,
    PG.CVInt $ fromIntegral q]
    where
       sIP  = flSrcIp   fl
       dIP  = flDstIp   fl
       sP   = flSrcPort fl
       dP   = flDstPort fl
       prio = 1

mk5TupleFromFl fl@(FlowUDPv4List {}) q =
    PG.CVTuple [ cvMInt $ Nothing,
    cvMInt $ Nothing,
    PG.CVMaybe $ Just $ PG.CVEnum 1,
    cvMInt $ Nothing,
    cvMInt $ Just dP,
    PG.CVInt prio,
    PG.CVInt $ fromIntegral q]
    where
       dIP  = flIp   fl
       dP   = flPort fl
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
fs = [ FlowUDPv4List {
     flIp    = Just 127
   , flPort  = fromIntegral $ 1000 + i} | i <- [1..20] ]

test :: IO ()
test = do
    let nq = 4
    --e10k <- e10kC
    --writeFile "tests/e10kC.dot" $ toDot e10k
    prgU <- e10kU
    let balFn = e10kCost prgU (balanceCost nq)
        conf  = searchGreedyE10k nq balFn fs
    putStrLn $ e10kCfgStr conf
    return ()
