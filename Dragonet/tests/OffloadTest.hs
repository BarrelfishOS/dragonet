{-# LANGUAGE QuasiQuotes #-}

--import qualified Data.Graph.Inductive as DGI

import Dragonet.Predicate
import Dragonet.ProtocolGraph (PGraph, PGNode, PGEdge, Node(..), NPort, Edge(..), NOperator(..), ESAttribute(..))
import Dragonet.ProtocolGraph.Utils (getFNodeByName, getFNodeByName')
import Dragonet.DotGenerator (toDot)
import qualified Dragonet.Unicorn as U
import qualified Util.GraphHelpers as GH

import Control.Monad.State (State, MonadState, gets, modify, runState)
import Control.Monad (Monad)
import Control.Applicative ((<$>))
import Data.Maybe
import Data.List (intercalate)
import Text.Show.Pretty (ppShow)

import Debug.Trace (trace)
import Text.RawString.QQ (r)

import qualified Graphs.LPG as L
import qualified Dragonet.Configuration as C

import TestOptimization (lpgCfg)

------------------------------------------------
-- helpers (to be eventually moved to other files)

traceMessages = True
xtrace = if traceMessages then \a b -> b else trace

-- node type tests
isONode :: PGNode -> Bool
isONode (_, ONode {}) = False
isONode _ = True

isFNode :: PGNode -> Bool
isFNode (_, FNode {}) = True
isFNode _ = False

isCNode :: PGNode -> Bool
isCNode (_, CNode {}) = True
isCNode _ = False

-- simple equality for now
predEquiv :: PredicateExpr -> PredicateExpr -> Bool
predEquiv a b =  a == b

-- state for computing predicate expression:
data PredCompSt = PredCompSt {
      predGraph  :: PGraph
    , predDst    :: PGNode
    , predInPort :: Maybe NPort
} deriving (Show)

-- initialize state
initPredCompSt_ = PredCompSt {
      predGraph  = undefined
    , predDst    = undefined
    , predInPort = Nothing
}

initPredCompSt :: PGraph -> String -> PredCompSt
initPredCompSt gr node = initPredCompSt_ {
      predGraph = gr
    , predDst   = getFNodeByName gr node
}

pgEdgePort :: PGEdge -> NPort
pgEdgePort pedge = case pedge of
    (_, _, ESpawn _ _) -> error "spawn edges do not have ports"
    (_, _, Edge p)     -> p

-- is this a normal (not spawn) edge?
pgIsNormalEdge :: PGEdge -> Bool
pgIsNormalEdge e = case e of
    (_, _, ESpawn _ _) -> False
    (_, _, Edge _)     -> True

pgIsSpawnEdge = not . pgIsNormalEdge

-- normal incomming edges (label of connected node, edge)
pgInEdges :: PGraph -> PGNode -> [(PGNode, PGEdge)]
pgInEdges gr dst = filter (pgIsNormalEdge . snd) $ GH.labLPre gr dst

-- normal incomming edges (label of connected node, edge)
pgInSpawnEdges :: PGraph -> PGNode -> [(PGNode, PGEdge)]
pgInSpawnEdges gr dst = filter (pgIsSpawnEdge . snd) $ GH.labLPre gr dst

-- is the node a traget spawn edges
pgIsSpawnTarget :: PGraph -> PGNode -> Bool
pgIsSpawnTarget g n = not $ null $ pgInSpawnEdges g n

pgSpawnEdgePred :: PGEdge -> PredicateExpr
pgSpawnEdgePred (_, _, ESpawn _ attrs) = expr
    where isPredAttr ::  ESAttribute -> Bool
          isPredAttr (ESAttrPredicate _) = True -- only this one for now

          parse :: ESAttribute -> PredicateExpr
          parse (ESAttrPredicate x) = parseStr $ xtrace ("parsing: " ++ x) x

          predicates = filter isPredAttr attrs
          predicates' = xtrace ("predicates are: " ++ (show predicates)) predicates
          exprs = map parse predicates'
          expr = case length exprs of
                      0 -> PredicateTrue
                      1 -> exprs !! 0
                      otherwise -> PredicateAnd exprs

--predicates from incomming spawn edges (combine them with and)
pgSpawnPreds :: PGraph -> PGNode -> PredicateExpr
pgSpawnPreds pg n = predAnd $ map (pgSpawnEdgePred . snd) $ pgInSpawnEdges pg n


-- Compute a predicate expression for flows (packets, whatever) that start from
--  predSrc and reach predDst, in predGraph
--  (assumes no circles exist)
computePred :: PredCompSt -> PredicateExpr
-- compute predicate of src->dst, where dst is an FNode:
--  predicate of pre(dst) AND predicate of src->pre(dst)
computePred st@(PredCompSt { predDst = (_, FNode {}) } )
    -- we reached an entry node
    | npredecessors == 0 = pgSpawnPreds gr dst
    | npredecessors > 1  = error "F-nodes have at most one predecessor"
    -- npredecessors == 1
    | spawn_target       = error "NYI: both normal and spawn edges" -- combines normal and spawn edges (treat is an OR?)
    -- recurse
    | otherwise          = expr
    where (dst', gr) = (predDst st, predGraph st)
          dst = trace ("Visiting node: " ++ (nLabel $ snd dst')) dst'
          -- predecessors of destintation (going backwards)
          -- (only consider normal edges)
          spawn_target   = pgIsSpawnTarget gr dst
          predecessors   = pgInEdges gr dst
          npredecessors  = length predecessors
          (pnode, pedge) = predecessors !! 0
          port           = pgEdgePort pedge
          nlabel         = nLabel (snd pnode)
          newst          = st { predDst = pnode, predInPort = Just port}
          expr'          = computePred newst
          term           = PredicateTerm nlabel port
          expr           = case pnode of
                             (_, ONode {})            -> expr'
                             (_, FNode {nPorts = np}) -> if length np == 1
                                                             then expr'
                                                             else predAnd [expr', term]
-- compute predicate of src->dst, where dst is an ONode:
--  OP (e.g., AND) [ predicate of src -> pre ] for each pre in pre(dst)
computePred st@(PredCompSt { predDst = (_, ONode { nOperator = op })} ) = expr
    where inport = fromJust $ predInPort st
          -- predecessors of destintation (going backwards)
          predecessors = GH.labLPre (predGraph st) (predDst st)
          -- get predicates for all predecessors
          exprargs :: [PredicateExpr]
          exprargs = map getPred predecessors
          -- here we assume that O-nodes are not connecte in reverse
          -- (true->false, false->true)
          getPred :: (PGNode, PGEdge) -> PredicateExpr
          getPred (n,_) = computePred $ st {predDst = n, predInPort = Just "true"}
          -- predicate operand
          expr = case (inport, op) of
                  ("true", NOpAnd)  -> predAnd exprargs
                  ("true", NOpOr)   -> predOr exprargs
                  ("true", _)       -> error "NYI"
                  ("false", NOpAnd) -> predOr  $ map PredicateNot exprargs
                  ("false", NOpOr)  -> predAnd $ map PredicateNot exprargs
                  ("false", _)      -> error "NYI"
                  (_, _)            -> error $ "Expecting true/false, not:" ++ inport

-- silly hellper
pathPredicate :: PGraph -> String -> PredicateExpr
pathPredicate g n = computePred $ initPredCompSt g n

------------------------------------------------
-- Embedding

-- embed state
data EmbedSt = EmbedSt {
      lpg           :: PGraph
    , prg           :: PGraph
    , lpgSink       :: String
    , prgEntry      :: String
    , embCandidates :: [PGNode] -- nodes that are candidates for embedding
} deriving (Show)

-- embed state initalizer with some defaults
initEmbedSt = EmbedSt {
      lpg           = undefined
    , prg           = undefined
    , lpgSink       = "TxOut"
    , prgEntry      = "TxEntry"
    , embCandidates = []
}

-- state monad for manipulating EmbedSt
newtype EmbedExec a = EmbedExec { doEmbed :: State EmbedSt a }
    deriving (Monad, MonadState EmbedSt, Functor)
--
-- NOTE: let's start with embedding the TX side, and see later if/what applies
-- to the RX side
runEmbeddingTx :: EmbedSt -> (Maybe PGraph, EmbedSt)
runEmbeddingTx st0 = (runState $ doEmbed embedTx) st0

embedTx :: EmbedExec (Maybe PGraph)
embedTx = do
    candidatesInit
    embedNodes
    -- what happens at this point if there are PRG nodes that modify packets
    -- that also exist in the LPG. We probably need to abort.
    return Nothing

-- initialize candidate list
candidatesInit :: EmbedExec ()
candidatesInit = do
    lpg <- gets lpg
    sink <- getFNodeByName lpg <$> gets lpgSink
    modify $ \s -> s { embCandidates = (lpgGetCandidates lpg sink) }
    return ()

-- Get the (single) predecssor.
-- If there are more than one predecessors throw an error. Note that F-nodes
-- have, by definition, a single predecessor.
getSinglePre :: PGraph -> PGNode -> PGNode
getSinglePre g n = if (length ps) == 1 then (ps !! 0) else error "expecting single predecessor"
    where ps = GH.labPre g n

-- Get the embedding candidate list given the sink node.
-- The candidate nodes should be F-nodes and have no other F-nodes in the path
-- to the sink node. If they do, we need to embed this node first. If we cannot,
-- then an ordering dependency will be broken if we embed the successor.
lpgGetCandidates :: PGraph -> PGNode -> [PGNode]
lpgGetCandidates graph sink = filter isFNode $ GH.rdfsStop isONode [start_node] graph
    where start_node = getSinglePre graph sink


-- try to embed node
--  A node can be embedded if:
--    . No other F-node depends on it (this is ensured by choosing the candidates)
--    . We can embed LPG node X if all packets (and only those) that reach both
--      LPG:X and lpgSink reach PRG:X.
--  when done, update candidates as needed
tryEmbedNode :: PGNode -> EmbedExec ()
tryEmbedNode lpg_node = do
    (lpg, prg) <- gets $ \s -> (lpg s, prg s)
    let node_lbl = nLabel $ snd lpg_node
        prg_node = getFNodeByName' prg node_lbl
    case prg_node of
        Nothing -> return ()
        Just prg_node' -> tryEmbedMatchedNode lpg_node prg_node'
    return ()

tryEmbedMatchedNode :: PGNode -> PGNode -> EmbedExec ()
tryEmbedMatchedNode lpg_node prg_node = do
    (lpg, prg) <- gets $ \s -> (lpg s, prg s)

    let pred_prg = computePred $ initPredCompSt_ { predGraph = prg, predDst = prg_node }
        pred_lpg = computePred $ initPredCompSt_ { predGraph = lpg, predDst = lpg_node }

    case predEquiv pred_prg pred_lpg of
        False -> return () -- cannot embed
        True  -> doEmbedNode lpg_node

doEmbedNode :: PGNode -> EmbedExec ()
doEmbedNode lpg_node = do
    -- remove from LPG
    -- add candidates
    return ()

embedNodes :: EmbedExec ()
embedNodes = do
    candidates <- gets embCandidates
    case candidates of
        []     -> return ()
        (x:xs) -> do
            modify $ \s -> s { embCandidates = xs } -- update candidate list
            tryEmbedNode x                          -- try to embed x
            embedNodes                              -- recurse


-- print a path predicate
prPathPredicate :: PGraph -> String -> IO (PredicateExpr)
prPathPredicate g node = do
    let x = pathPredicate g node
    putStrLn $"path to " ++ node  ++ " has a path predicate: " ++ (show x)
    return x

main = do
    --(lpgU,_) <- L.graphH_ "Graphs/LPG/lpgConfImpl-offload.unicorn"
    --let lpgC  = C.applyConfig lpgCfg lpgU
    --writeFile "unicorn-tests/lpg.dot"  $ toDot lpgC

    writeFile "unicorn-tests/g2.dot"  $ toDot g2
    prPathPredicate g2 "X"

    --lpg <- U.fileToGraph "unicorn-tests/offload-tx-lpg.unicorn"
    --prg <- U.fileToGraph "unicorn-tests/offload-tx-prg.unicorn"
    --let (emb, st) = runEmbeddingTx initEmbedSt { lpg = lpg, prg = prg }
    --putStrLn (ppShow $ map (\(_,x) -> nLabel x) (embCandidates st))
    --prPathPredicate lpg "TxEntry" "TxUdpChecksum"
    --prPathPredicate prg "TxEntry" "TxUdpChecksum"
    --prPathPredicate lpg "TxEthernetHdrFill" "TxOut"
    --prPathPredicate lpg "TxUdpChecksum" "TxOut"
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
-- From a reading of the ixgbe linux driver source code, I believe that linux
-- willl set the TX context for every packet.
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

-- another test graph
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

-- a test graph
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

