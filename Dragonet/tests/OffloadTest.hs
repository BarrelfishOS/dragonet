-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Graph.Inductive as DGI

import Dragonet.Predicate
import Dragonet.ProtocolGraph (PGraph, PGNode, PGEdge, Node(..), NPort, Edge(..), NOperator(..), ESAttribute(..), NLabel)
import Dragonet.ProtocolGraph.Utils (getFNodeByName, getFNodeByName', getPGNodeByName)
import Dragonet.DotGenerator (toDot)
import qualified Dragonet.Unicorn as U
import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive.Query.DFS as DFS

import Control.Monad (Monad, mapM_)
import Control.Exception (assert)
import Control.Applicative ((<$>))
import Data.Maybe
import Data.List (intercalate)
import Text.Show.Pretty (ppShow)

import Debug.Trace (trace, traceShow)
import Text.RawString.QQ (r)

import qualified Graphs.LPG as L
import qualified Dragonet.Configuration as C

import Graphs.Cfg (lpgCfg)

------------------------------------------------
-- helpers (to be eventually moved to other files)

traceMessages = False
xtrace = if traceMessages then trace else \a b -> b

{--
-- domination (does not consider spawn edges)
dominates :: PGraph -> (PGNode, NPort) -> PGNode -> Bool
-- F-node
dominates g (src, p) dst@(_, FNode {})
    | ndeps == 0 = False -- nowhere to go, cannot find a connection to @dst
    | ndeps > 1  = error $ "F-nodes have at most one incoming edge" ++ (nLabel $ snd dst)
    -- ndeps == 1
    | pgEqLabel dep_node src = (dep_port == p) -- same node
    | otherwise = dominates g (src, p) dep_node
    where deps = pgDeps g dst
          ndeps = length deps
          (dep_node,dep_edge) = deps !! 0
          dep_port            = pgEdgePort dep_edge
-- O-Node
dominates g (src, p) dst@(_, ONode { nOperator = op}) = comb_op $ map check_fn deps
    where deps = pgDeps g dst
          check_fn :: (PGNode, PGEdge) -> Bool
          check_fn (xnode, xedge)
             | pgEqLabel xnode src && (pgEdgePort xedge) == p = True
             | otherwise = dominates g (src, p) xnode
          comb_op = case op of
                     NOpAnd -> or   -- we only need one to dominate
                     NOpOr  -> and  -- we need both to dominate
                     otherwise -> error "NYI: not sure about the semantics. We might need to map with not"

-- silly helper
dominatesStr :: PGraph -> (String, String) -> String -> Bool
dominatesStr g (src, psrc) dst = dominates g (src', psrc) dst'
    where src' = getFNodeByName g src
          dst' = getFNodeByName g dst

--}

-- silly hellper
pathPredicate :: PGraph -> String -> PredicateExpr
pathPredicate g n = computePred $ initPredCompSt g n

------------------------------------------------
-- Embedding

--

-- print a path predicate
prPathPredicate :: PGraph -> String -> IO (PredicateExpr)
prPathPredicate g node = do
    let x = pathPredicate g node
    putStrLn $"path to " ++ node  ++ " has a path predicate: " ++ (show x)
    return x


main = do
    --writeFile "unicorn-tests/lpg-offload.dot"  $ toDot lpgC
    --writeFile "unicorn-tests/prg-dummy.dot" $ toDot dummy_prg
    --let (x,s) = runEmbeddingTx $ initEmbedSt { lpg = lpgC, prg = dummy_prg }
    --writeFile "unicorn-tests/lpg-offload-dummy.dot" $ toDot (lpg s)
    --pred <- prPathPredicate lpgC "TxL4UDPFillChecksum"
    --putStrLn $ "simplified:" ++ (show $ lpgPredRemUnreachable lpgC "TxQueue" pred)

    return ()


-- Notes on i82599's offload functions
--
-- Disclaimer: this is based on reading the datasheet without investigating all
-- details.
--

-- Tx side:
--
-- i82599 operates with transmit contexts. A TX context can be configured with
-- specific offload functions on the NIC. For example, to offload IPv4 header
-- checksum and UDP checksum you need to setup a context by specifying:
--  . TUCMD.IPV4: this is for IPv4 packets
--  . MACLEN:     offset from the start of the DMAed region to where the IPv4
--                checksum should start
--  . IPLEN:      length of the IPv4 header
--  . TUCMD.L4T:  L4 protocol (00b for UDP, 01b for TCP)
--
-- Once a TX context is setup, you can send packets via this context by
-- speficying it in the descriptors. There are 2 TX contexts per queue
-- available.
--
-- This means that packets that do not adhere to the setup parameters need to
-- sent via another context.
--
-- We can only offload nodes that:
--  . have no dependencies
--  . all of their dependencies can be offloaded
--
-- Assumming that we have setup two contexts in the i82599:
--  CTX0: no offload functions, everything in software
--  CTX1: IPv4 header/UDP checksum offload
-- We need to use CTX1 for packets that match the context configuration and CTX0
-- for everything else.
--
-- From a reading of the ixgbe linux driver source code, I believe that linux
-- willl set the TX context for every packet and so it ends up  using a single
-- context
--

test_dominance :: (PGraph, (String, String), String, Bool) -> IO ()
test_dominance  (g, src, dst, expect) = do
    let res = dominatesStr g src dst
    putStrLn $ "Checking: " ++ (show (src, dst, expect))
    case (expect == res) of
        True -> putStrLn "  =>OK!"
        False -> putStrLn " =>FAIL!"

g4_test_dominance :: IO ()
g4_test_dominance = mapM_ test_dominance tests
    where tests = [ (g4, ("A", "IDONTEXIST"), "X", False)
                  , (g4, ("A", "a"), "X", True)
                  , (g4, ("A", "a"), "O", True)
                  ]

g4 :: PGraph
g4 = U.strToGraph [r|
graph g4 {

    node A { port a[X] }

    node X {
        port a[O_]
        port b[Y]
    }

    node Y {
        port a[O_]
        port b[]
    }

    or O_ {
        port true[O]
        port false[] }

    node O {}
}
|]

g3 :: PGraph
g3 = U.strToGraph [r|
graph g2 {

    node X  { }
    or   X_ { port true[X]
              port false[] }
    node A  { port o[X_] }
    node B  { port o[X_] }

    node S1 {
        spawn start A [predicate "pred(n1,p1)"]
        spawn start B [predicate "pred(n1,p1)"]
    }
}
|]

g2 :: PGraph
g2 = U.strToGraph [r|
graph g2 {

    node X  { }
    or   X_ { port true[X]
              port false[] }
    node A  { port o[X_] }
    node B  { port o[X_] }

    node S1 {
        spawn start A [predicate "and(pred(n1,p1),pred(n2,p2))"]
        spawn start B [predicate "and(pred(n1,p1),pred(n2,p2))"]
    }
}
|]

g1 :: PGraph
g1 = U.strToGraph [r|
graph g1 {
    node SRC {
        port p1[A B C]
        port p2[D]
    }

    node A {
        port true[O1]
        port false[]
    }


    node B {
        port true[O1]
        port false[]
    }

    or O1 {
        port true[O2]
        port false[]
    }

    node C {
        port true[O2]
        port false[]
    }

    or O2 {
        port true[X]
        port false[]
    }

    node X { port o[DST]
             port e[]
    }

    node DST {}
    node D {}
}
|]
