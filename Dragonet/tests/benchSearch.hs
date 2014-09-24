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
    let nq = 10
    prgU <- e10kU_simple
    let priFn = S.e10kCost prgU (priorityCost' nq)
        balFn = S.e10kCost prgU (S.balanceCost nq)
        benchBal nflows = show $ (S.searchGreedyFlowsE10k nq balFn (flows nflows))
    defaultMainWith
                defaultConfig { cfgSamples = ljust 5 }
                (return ())
                [   bench "10" $ nf benchBal 10
                  , bench "20" $ nf benchBal 20
                  , bench "40" $ nf benchBal 40]
