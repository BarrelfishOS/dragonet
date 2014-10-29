{-# LANGUAGE BangPatterns #-}

import qualified Graphs.E10k as E10k
import qualified Graphs.Cfg as Cfg
import qualified Dragonet.Configuration as C
import qualified Search as S

import Dragonet.Flows (Flow(..))

import Data.Maybe
import Data.List as L

import Control.Applicative ((<$>))

import Criterion.Main (defaultMainWith, defaultMain, nf, whnf, bench)
import Criterion.Config

import Text.Show.Pretty (ppShow)

e10kT_simple = E10k.graphH_ "Graphs/E10k/prgE10kImpl-simple.unicorn"
e10kU_simple = fst <$> e10kT_simple
e10kH_simple = snd <$> e10kT_simple

flows :: Int -> [Flow]
flows nflows = [ FlowUDPv4 {
     flDstIp    = Just 127
   , flDstPort  = Just $ fromIntegral $ 1000 + i
   , flSrcIp    = Nothing
   , flSrcPort  = Nothing } | i <- [1..nflows] ]


isGoldFl FlowUDPv4 {flDstPort = Just port} = isJust $ L.find (==port) [1001,1002]
goldFlPerQ = 1
priorityCost' = S.priorityCost isGoldFl goldFlPerQ


--
main = do
    let nq = 10 -- number of queues
        --nflowsl = [1,5,10,20,40,80]
        nflowsl = [100,200]
        samples = 3
    prgU <- e10kU_simple
    let priFn = priorityCost' nq
        balFn = S.balanceCost nq
        costFn = balFn

        oracle   = S.E10kOracleSt {S.nQueues = nq}
        searchParams = S.initSearchParams {  S.sOracle  = oracle
                                          , S.sPrgU     = prgU
                                          , S.sCostFn   = costFn
                                          , S.sStrategy = S.searchGreedyFlows}

        benchBal nflows = show $ S.runSearch searchParams (flows nflows)
        benchNf nflows  = bench ("search: " ++ (show nflows)) $ nf benchBal nflows
        benchs = [benchNf x | x <- nflowsl]


    defaultMainWith
                defaultConfig { cfgSamples = ljust samples }
                (return ())
                benchs
