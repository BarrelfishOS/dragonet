{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Graph.Inductive as DGI

import Dragonet.Predicate
import Dragonet.ProtocolGraph (PGraph, PGNode, PGEdge, Node(..), NPort, Edge(..), NOperator(..), ESAttribute(..), NLabel)
import Dragonet.ProtocolGraph.Utils (getFNodeByName, getFNodeByName', getPGNodeByName)
import Dragonet.DotGenerator (toDot)
import qualified Dragonet.Unicorn as U
import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive.Query.DFS as DFS

import Control.Monad.State (State, MonadState, gets, modify, runState)
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

-- equality based on label: same type and same label
pgEqLabel :: PGNode -> PGNode -> Bool
pgEqLabel (_, FNode {nLabel = x1}) (_, FNode {nLabel = x2}) = x1 == x2
pgEqLabel (_, ONode {nLabel = x1}) (_, ONode {nLabel = x2}) = x1 == x2
pgEqLabel _ _ = False

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

-- normal incoming dependencies (label of connected node, edge)
pgDeps :: PGraph -> PGNode -> [(PGNode, PGEdge)]
pgDeps gr dst = filter (pgIsNormalEdge . snd) $ GH.labLPre gr dst

pgSpawnDeps :: PGraph -> PGNode -> [(PGNode, PGEdge)]
pgSpawnDeps gr dst = filter (pgIsSpawnEdge . snd) $ GH.labLPre gr dst

-- is the node a traget spawn edges
pgIsSpawnTarget :: PGraph -> PGNode -> Bool
pgIsSpawnTarget g n = not $ null $ pgSpawnDeps g n

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

--predicates from incoming spawn edges (combine them with and)
pgSpawnPreds :: PGraph -> PGNode -> PredicateExpr
pgSpawnPreds pg n = case length spawn_edges of
    0 -> PredicateTrue
    1 -> mapfn $ (spawn_edges !! 0)
    otherwise -> predAnd $ map mapfn spawn_edges
    where mapfn = (pgSpawnEdgePred . snd)
          spawn_edges = pgSpawnDeps pg n

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

-- Compute a predicate expression for flows (packets, whatever) that start from
--  predSrc and reach predDst, in predGraph
--  (assumes no circles exist)
computePred :: PredCompSt -> PredicateExpr
-- compute predicate of src->dst, where dst is an FNode:
--  predicate of pre(dst) AND predicate of src->pre(dst)
computePred st@(PredCompSt { predDst = (_, FNode {}) } )
    -- we reached an entry node
    | ndeps == 0   = pgSpawnPreds gr dst
    | ndeps > 1    = error $ "F-nodes have at most one incmming edge" ++ (nLabel $ snd dst)
    -- ndeps == 1
    | spawned = error "NYI: both normal and spawn edges" -- combines normal and spawn edges (treat is an OR?)
    -- recurse
    | otherwise   = expr
    where (dst', gr) = (predDst st, predGraph st)
          dst = dst' --trace ("Visiting node: " ++ (nLabel $ snd dst')) dst'
          -- predecessors of destintation (going backwards)
          -- (only consider normal edges)
          spawned = pgIsSpawnTarget gr dst
          deps    = pgDeps gr dst
          ndeps   = length deps
          (dn,de) = deps !! 0
          port    = pgEdgePort de
          nlabel  = nLabel (snd dn)
          newst   = st { predDst = dn, predInPort = Just port}
          expr'   = computePred newst
          term    = PredicateTerm nlabel port
          expr    = case dn of
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
    , lpgSink       = "TxQueue"
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
    -- that also exist in the LPG? We probably need to abort.
    return Nothing

-- initialize candidate list
candidatesInit :: EmbedExec ()
candidatesInit = do
    lpg <- gets lpg
    sink <- getFNodeByName lpg <$> gets lpgSink
    let candidates = lpgGetCandidates lpg sink
        candidates' =   trace ("\nInitial candidates: " ++ (ppShow $ map (nLabel . snd) candidates)) candidates
    modify $ \s -> s { embCandidates = candidates' }
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


lpgPredRemUnreachable :: PGraph -> String -> PredicateExpr -> PredicateExpr
lpgPredRemUnreachable lpg lpg_sink pred =
    let terms = predGetTerms pred
        -- check all other node ports in the given node. All have to be false
        check_term :: (NLabel, NPort) -> Bool
        check_term (nlbl,p0) = case getFNodeByName' lpg nlbl of
            Nothing -> True
            Just n -> or [check_port (nlbl, p) | p <- filter (/= p0) (nPorts $ snd $ n)]
        -- check a single port
        check_port :: (NLabel, NPort) -> Bool
        check_port x = isReachableFromPort lpg x lpg_sink
        --
        mapfn :: (NLabel, NPort) -> Maybe ((NLabel, NPort), PredicateExpr)
        mapfn term = case (check_term term) of
            False -> Just (term, PredicateTrue)
            True  -> Nothing
        terms' :: [((NLabel,NPort), PredicateExpr)]
        terms' = catMaybes $ map mapfn terms 
    in predEval pred terms'

tryEmbedMatchedNode :: PGNode -> PGNode -> EmbedExec ()
tryEmbedMatchedNode lpg_node prg_node = do
    (lpg, prg) <- gets $ \s -> (lpg s, prg s)
    lpg_sink <- gets lpgSink

    let pred_prg = computePred $ initPredCompSt_ { predGraph = prg, predDst = prg_node }
        pred_lpg' = computePred $ initPredCompSt_ { predGraph = lpg, predDst = lpg_node }
        pred_lpg = lpgPredRemUnreachable lpg lpg_sink pred_lpg'

        pred_prg'' = trace (" pred_prg=" ++ (ppShow pred_prg)) pred_prg
        pred_lpg'' = trace (" pred_lpg=" ++ (ppShow pred_lpg)) pred_lpg

    case predEquiv pred_prg'' pred_lpg'' of
        False -> return () -- cannot embed
        True  -> doEmbedNode lpg_node

modifyLPG :: (PGraph -> PGraph) -> EmbedExec ()
modifyLPG fn = modify $ \s -> s { lpg = (fn $ lpg s) }

doEmbedNode :: PGNode -> EmbedExec ()
doEmbedNode lpg_node@(_, FNode {} ) = do
    -- TODO
    lpg <- gets lpg
    let in_edge = case pgIsSpawnTarget lpg lpg_node of
            True  -> error $ "node " ++ (show lpg_node) ++ "is a spawn target. Cannot embed"
            False -> case pgDeps lpg lpg_node of
                [x] -> x
                x  -> error $ "F-node " ++ (show lpg_node) ++ "has " ++ (show $ length x) ++ "incoming edges"
    let out_edges = case length (nPorts $ snd lpg_node) of
            1 -> GH.labLSucc lpg lpg_node
            x -> error $ "Trying to embed a node with " ++ (show x) ++ " ports"

    -- remove from LPG (not sure if the edge removal is necessary)
    -- TODO: run some tests
    modifyLPG $ DGI.delNode (fst lpg_node)
    modifyLPG $ DGI.delLEdge (snd in_edge)
    modifyLPG $ DGI.delEdges (map (GH.ledgeToEdge . snd) out_edges)
    -- TODO: add new candidates
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

-- is src reachable from dst
isReachable :: PGraph -> NLabel -> NLabel -> Bool
isReachable gr src dst = elem dst reachable
    where src' = getPGNodeByName gr src
          reachable = map (nLabel . snd) $ GH.labReachable gr src'

-- nodes connected to a given port
pgPortNodes :: PGraph -> PGNode -> NPort -> [PGNode]
pgPortNodes gr node port = ret
    where ret = map fst $ filter fn $ GH.labLSucc gr node
          fn (_, (_, _, Edge p)) = p == port
          fn _ = False

-- check whether a node is reachable fro a (node, port)
isReachableFromPort :: PGraph -> (NLabel, NPort) -> NLabel -> Bool
isReachableFromPort gr (sn,sp) dst = or $ map (\s -> isReachable gr s dst) sources
    where sn' = getPGNodeByName gr sn
          sources = map (nLabel . snd) $ pgPortNodes gr sn' sp

dummy_prg :: PGraph
dummy_prg = U.strToGraph [r|
graph dummy_prg {
    node L4Prot {
        port UDP[TxL4UDPFillChecksum]
        port Other[_Out]
    }

    node TxL4UDPFillChecksum {
        port o[_Out]
    }

    or _Out {
        port true[Out]
        port false[]
    }

    node Out {}
}
|]

main = do
    (lpgU,_) <- L.graphH_ "Graphs/LPG/lpgConfImpl-offload.unicorn"
    let lpgC  = C.applyConfig lpgCfg lpgU
    writeFile "unicorn-tests/lpg-offload.dot"  $ toDot lpgC
    writeFile "unicorn-tests/prg-dummy.dot" $ toDot dummy_prg

    let (x,s) = runEmbeddingTx $ initEmbedSt { lpg = lpgC, prg = dummy_prg }

    writeFile "unicorn-tests/lpg-offload-dummy.dot" $ toDot (lpg s)

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

