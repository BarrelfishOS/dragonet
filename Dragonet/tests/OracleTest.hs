{-# LANGUAGE FlexibleInstances, UndecidableInstances,
               OverlappingInstances, ScopedTypeVariables #-}

import qualified Graphs.E10k as E10k
import qualified Dragonet.Search as Search
import qualified Scenarios.S1 as S1
import Text.Show.Pretty (ppShow)

import Control.Applicative ((<$>))

main = do
    -- Prepare graphs and so on
    prgH@(prgU,_) <- E10k.graphH
    let nq = 10
        e10kOracle = Search.E10kOracleSt {Search.nQueues = nq}
        priFn      = S1.priorityCost nq
        balanceFn  = Search.balanceCost nq
        sparams    = Search.initSearchParams {
                 Search.sOracle = e10kOracle
                 , Search.sPrgU   = prgU
                 --, Search.sCostFn =  priFn
                 , Search.sCostFn = balanceFn
                 , Search.sStrategy = Search.searchGreedyFlows
            }
        conf = Search.runSearch sparams S1.sortedRealFlows
    putStrLn $ "connected flows used: " ++ (ppShow S1.sortedRealFlows)
    putStrLn $ "Cost function: \n" ++  ppShow conf

