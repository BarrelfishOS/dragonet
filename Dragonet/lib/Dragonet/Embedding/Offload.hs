{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dragonet.Embedding.Offload (
    embedOffload
) where

import qualified Util.GraphHelpers as GH
import qualified Dragonet.ProtocolGraph as PG
import Dragonet.ProtocolGraph.Utils (getFNodeByNameTag', getPGNodeByName, isSpawnTarget, edgeDeps)
import Dragonet.Predicate (PredicateExpr(..), predGetTerms, predEval, predEquiv)
import Dragonet.Predicate (computePred, initPredCompSt_, PredCompSt(..))
import Dragonet.Conventions (rxQPref, txQPref, qTag)

import qualified Data.Graph.Inductive as DGI
import qualified Data.Graph.Inductive.Query.DFS as GDFS

import Control.Monad.State (State, MonadState, gets, modify, execState)
import Control.Applicative ((<$>))

import Data.Maybe
import qualified Data.List as L

import Text.Show.Pretty (ppShow)

import Debug.Trace (trace)
tr = flip $ trace


-- TODO: Rx side

-- Main embedding function

embedOffload :: PG.PGraph -> String -> PG.PGraph
embedOffload embgraph tag =  embGraph $ execEmbeddingTx st0
    where st0 = initEmbedSt { embGraph  = embgraph
                            , qtag      = qTag tag}

-- Helpers. Some of them we might want to move to different file

-- Get the (single) predecssor.
-- If there are more than one predecessors throw an error. Note that F-nodes
-- have, by definition, a single predecessor.
getSinglePre :: PG.PGraph -> PG.PGNode -> PG.PGNode
getSinglePre g n = if len == 1 then (ps !! 0) else error $ "expecting single predecessor for node " ++ (PG.nLabel $ snd n) ++ " (has: " ++ (show len) ++ ")"
    where len = length ps
          ps = GH.labPre g n

-- find a node reachable from src
findReachable :: PG.PGraph -> PG.PGNode -> (PG.PGNode -> Bool) -> Maybe PG.PGNode
findReachable gr src fn = L.find fn $ GH.labReachable gr src

-- nodes connected to a given port
pgPortNodes :: PG.PGraph -> PG.PGNode -> PG.NPort -> [PG.PGNode]
pgPortNodes gr node port = ret
    where ret = map fst $ filter fn $ GH.labLSucc gr node
          fn (_, (_, _, PG.Edge p)) = p == port
          fn _ = False

-- check whether a node is reachable from a (node, port)
findReachableFromPort :: PG.PGraph ->
                         (PG.PGNode, PG.NPort) ->
                         (PG.PGNode -> Bool) ->
                         Maybe PG.PGNode
findReachableFromPort gr (src, srcPort) fn = L.find fn reachable
    where  reachable = concat $ [GH.labReachable gr src | src <- port_nodes]
           port_nodes = pgPortNodes gr src srcPort


nodesDFS ::  PG.PGraph -> [PG.PGNode] -> [PG.PGNode]
nodesDFS g start = [(n, fromJust $ DGI.lab g n) | n <- GDFS.dfs (map fst start) g ]

--- Embedding

-- embed state
data EmbedSt = EmbedSt {
      embGraph        :: PG.PGraph   -- embedded graph (contains both LPG, PRG)
    , qtag            :: String      -- tag to find lpg nodes
    , txLpgSinkNode   :: PG.PGNode   --
    , txEmbCandidates :: [PG.PGNode] -- nodes that are candidates for embedding
} deriving (Show)

initEmbedSt = EmbedSt {
      embGraph         = undefined
    , qtag             = undefined
    , txLpgSinkNode    = undefined
    , txEmbCandidates  = []
}

-- state monad for manipulating EmbedSt
newtype EmbedExec a = EmbedExec { doEmbed :: State EmbedSt a }
    deriving (Monad, MonadState EmbedSt, Functor)

modifyGraph :: (PG.PGraph -> PG.PGraph) -> EmbedExec ()
modifyGraph fn = modify $ \s -> s { embGraph = (fn $ embGraph s) }

execEmbeddingTx :: EmbedSt -> EmbedSt
execEmbeddingTx st0 = (execState $ doEmbed embedTx) st0

embedTx :: EmbedExec ()
embedTx = do
    txCandidatesInit
    embedTxNodes
    -- what happens at this point if there are PRG nodes that modify packets
    -- that also exist in the LPG? We probably need to abort.
    return ()

-- initialize candidate list
--  candidates are nodes which are on the boundary of O-nodes, i.e., other
--  F-nodes do not depend on them
txCandidatesInit :: EmbedExec ()
txCandidatesInit = do
    graph <- gets embGraph
    qtag  <- gets qtag
    let findFn :: (PG.Node -> Bool)
        findFn n = (PG.nTag n)    == qtag    &&
                   (PG.nLabel n)  == txQPref &&
                   (PG.nOrigin n) == "LPG"

        sink :: PG.PGNode
        sink = case GH.findNodeByL findFn graph of
                Nothing -> error $ "Cannot find LPG node with label " ++ txQPref ++ " and tagged as " ++ qtag
                Just x  -> x
        sink_ = tr sink $ "=======> SINK NODE: " ++ (ppShow sink)

        candidates  = getTxCandidates graph sink
        candidates_ = tr candidates $ ("\nInitial candidates: " ++ (ppShow $ map (PG.nLabel . snd) candidates))

    modify $ \s -> s { txEmbCandidates = candidates, txLpgSinkNode = sink}
    return ()

-- Get the embedding candidate list given the sink node.
-- The candidate nodes should be F-nodes and have no other F-nodes in the path
-- to the sink node. If they do, we need to embed this node first. If we cannot,
-- then an ordering dependency will be broken if we embed the successor.
getTxCandidates :: PG.PGraph -> PG.PGNode -> [PG.PGNode]
getTxCandidates graph sink = filter isFNode $ GH.rdfsStop isFNode [node0] graph
    where node0_ = tr node0 $ "node0=" ++ (ppShow node0)
          node0 = getSinglePre graph sink
          isFNode :: PG.PGNode -> Bool
          isFNode n@(_, PG.FNode {}) = True
          isFNode n                  = False

embedTxNodes :: EmbedExec ()
embedTxNodes = do
    candidates <- gets txEmbCandidates
    case candidates of
        []     -> return ()
        (x:xs) -> do
            modify $ \s -> s { txEmbCandidates = xs } -- update candidate list
            tryEmbedTxNode x                          -- try to embed x
            embedTxNodes                              -- recurse

-- try to embed node
--  A node can be embedded if:
--    . No other F-node depends on it (this is ensured by choosing the candidates)
--    . We can embed LPG node X if all packets (and only those) that reach both
--      LPG:X and lpgTxSink reach PRG:X.
--  when done, update candidates as needed
tryEmbedTxNode :: PG.PGNode -> EmbedExec ()
tryEmbedTxNode lpg_node = do
    graph <- gets embGraph
    sink  <- gets txLpgSinkNode

    let node_lbl = PG.nLabel $ snd lpg_node
        prg_find :: PG.PGNode -> Bool
        prg_find (_,n) = (PG.nLabel n == node_lbl) && (PG.nOrigin n == "PRG")
        prg_node = L.find prg_find $ nodesDFS graph [sink]

    case prg_node of
        Nothing -> return ()
        Just prg_node' -> tryEmbedTxMatchedNode lpg_node prg_node'
    return ()

-- Rationale:
--  To remove offload (remove) an  LPG node to a PRG we need to show that for
--  all packets that reach the LPG sink:
--    if the LPG node was enabled     -> the PRG node will be enabled
--    if the LPG node was not enabled -> the PRG node will not be enabled
--
--  We do that by checking the path predicate of both nodes and make sure that
--  they are equivalent.
--
-- If, however, a term in the path predicate of the LPG node is always true when
-- we reach the sink, we can remove it. An example is tests for dropping
-- packets. We do not have to worry about these checks when comparing the LPG
-- node and the PRG node path predicates, because the packets that do not match
-- it will never go beyond the sink.
--
-- Hence, as a heuristic, we check for these predicate terms by checking if the
-- sink node is rechable from all other ports besides the port that corresponds
-- to the predicate.
--
-- An optiomization here might be to stop after a number of nodes.
lpgPredRemUnreachable :: PredicateExpr -> EmbedExec (PredicateExpr)
lpgPredRemUnreachable pred = do

    graph <- gets embGraph
    sink  <- gets txLpgSinkNode
    qtag  <- gets qtag

    let sink_lbl = PG.nLabel $ snd sink
        -- function to check whether a node is the LPG sink
        is_sink :: PG.PGNode -> Bool
        is_sink (_, n) = (PG.nLabel n == sink_lbl) &&
                         (PG.nTag   n == qtag)
        -- first, find all the terms in the expression
        terms = predGetTerms pred
        -- check_term checks a single term
        -- it goes over all other node ports in the given node.
        -- All have to be unable to reach the sink
        check_term :: (PG.NLabel, PG.NPort) -> Bool
        check_term (nlbl,p0) = case  getFNodeByNameTag' graph nlbl qtag  of
            Nothing -> True
            Just n -> or [check_port (n, p) | p <- filter (/= p0) (PG.nPorts $ snd $ n)]
        -- checks whether the lpg sink is unrechable from a single port
        check_port :: (PG.PGNode, PG.NPort) -> Bool
        check_port (n,p) = case findReachableFromPort graph (n, p) is_sink of
            Just x -> True
            Nothing -> False
        --
        mapfn :: (PG.NLabel, PG.NPort) -> Maybe ((PG.NLabel, PG.NPort), PredicateExpr)
        mapfn term = case (check_term term) of
            False -> Just (term, PredicateTrue)
            True  -> Nothing
        terms' :: [((PG.NLabel,PG.NPort), PredicateExpr)]
        terms' = catMaybes $ map mapfn terms

    return $ predEval pred terms'


tryEmbedTxMatchedNode :: PG.PGNode -> PG.PGNode -> EmbedExec ()
tryEmbedTxMatchedNode lpg_node prg_node = do
    graph <- gets embGraph
    lpg_sink <- gets txLpgSinkNode

    let prg_stopfn :: PG.PGNode -> Bool
        prg_stopfn (_,n) = PG.nOrigin n == "LPG" -- stop when we reach LPG nodes
        pred_prg = computePred $ initPredCompSt_ { predGraph = graph
                                                 , predDst = prg_node
                                                 , compStop = prg_stopfn }
        pred_lpg' = computePred $ initPredCompSt_ { predGraph = graph
                                                  , predDst = lpg_node }

    -- heuristic to simplify predicates
    pred_lpg <- lpgPredRemUnreachable pred_lpg'

    let pred_prg_ = tr pred_prg (" pred_prg=" ++ (ppShow pred_prg))
        pred_lpg_ = tr pred_lpg (" pred_lpg=" ++ (ppShow pred_lpg))
        lpg_node_ = tr lpg_node (" lpg_node=-->" ++ (ppShow lpg_node) ++ "<--" ++ " prg node -->" ++ (ppShow prg_node) ++ "<--")

    case predEquiv pred_prg pred_lpg of
        False -> return () -- cannot embed
        True  -> doEmbedTxNode lpg_node

doEmbedTxNode :: PG.PGNode -> EmbedExec ()
doEmbedTxNode node@(_, n@(PG.FNode {}) ) = do
    graph <- gets embGraph

    -- some sanity (?) checks. They might be too strict.
    let lpg_node = case (PG.nOrigin n == "LPG") of
                    True  -> node
                    False -> error "This is no LPG node!"

    let in_edge = case isSpawnTarget graph lpg_node of
            True  -> error $ "node -->" ++ (ppShow lpg_node) ++ "<-- is a spawn target. Cannot embed"
            False -> case edgeDeps graph lpg_node of
                [x] -> x
                x  -> error $ "F-node --->" ++ (ppShow lpg_node) ++ "<--- is to be embedded but has " ++ (show $ length x) ++ " incoming edges"

    let out_edges = case length (PG.nPorts $ snd lpg_node) of
            1 -> GH.labLSucc graph lpg_node
            x -> error $ "Trying to embed a node with " ++ (show x) ++ " ports"

    -- remove from LPG (not sure if the edge removal is necessary)
    -- TODO: run some tests
    modifyGraph $ DGI.delNode (fst lpg_node)
    modifyGraph $ DGI.delLEdge (snd in_edge)
    modifyGraph $ DGI.delEdges (map (GH.ledgeToEdge . snd) out_edges)
    -- TODO: add new candidates
    return ()

