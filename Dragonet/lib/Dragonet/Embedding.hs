module Dragonet.Embedding(
    embeddingRxTx,
    embeddingRxTx2
) where

import qualified Dragonet.ProtocolGraph       as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Predicate           as PR
import qualified Util.GraphHelpers            as GH
import Dragonet.Embedding.Offload (embedOffload)
import Dragonet.Conventions (rxQPref, txQPref, qTag)

import Data.Maybe
import Data.Function (on)
import qualified Data.Graph.Inductive           as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

import qualified Control.Monad.State as ST
import Control.Applicative ((<$>))

import Debug.Trace (trace)
tr = flip $ trace
trN = \x  _ -> x

--------------------------------------------------------------------------------
-- Embedding for both RX and TX path

tagNodes :: String -> PG.PGraph -> PG.PGraph
tagNodes tag = DGI.nmap tagN
    where tagN n = n { PG.nTag = tag }

setOrigin :: String -> PG.PGraph -> PG.PGraph
setOrigin origin = DGI.nmap f
    where f n@(PG.FNode {}) = n { PG.nOrigin = origin }
          f n               = n

isTxQueueNode :: PG.Node -> Bool
isTxQueueNode node = txQPref `L.isPrefixOf` (PG.nLabel node)

isRxQueueNode :: PG.Node -> Bool
isRxQueueNode node = rxQPref `L.isPrefixOf` (PG.nLabel node)

isQueueNode :: PG.Node -> Bool
isQueueNode node = isTxQueueNode node || isRxQueueNode node

tagPrgQueues :: PG.PGraph -> PG.PGraph
tagPrgQueues = DGI.nmap tagQueue
    where tagQueue n
            | isQueueNode n = n {
                    PG.nTag = qTag $ drop (length rxQPref) $ PG.nLabel n }
            | otherwise = n

-- connect one LPG to the PRG queues identified by rxQ:
-- This also tags the lpg nodes with the queue identifier
addLPG :: PG.PGraph -> PG.PGraph -> String -> PG.PGraph
addLPG lpg prg rxQ = GH.mergeGraphsBy mergeP prg lpg'
    where lpg'  = tagNodes (qTag rxQ) lpg -- tag lpg nodes
          --dbg_lpg   = "\n\nQ=" ++ rxQ ++ "\n\nLPG=" ++ (ppShow lpg') ++ "\n\n"
          mergeP :: PG.Node -> PG.Node -> Bool
          -- NB: it seems that the arguments here need to be reversed (i.e., lpg
          -- node first, and then prg node) compared to the mergeGraphsBy
          -- function
          mergeP lpgN prgN
            | prgL == rxQPref ++ rxQ && lpgL == rxQPref = True -- rx queue match
            | prgL == txQPref ++ rxQ && lpgL == txQPref = True -- tx queue match
            | otherwise = False
            where prgL = PG.nLabel prgN -- prg node label
                  lpgL = PG.nLabel lpgN -- lpg node label
                  --dbg = "trying to merge prg node:\n" ++ prgL ++ " and LPG node "  ++ lpgL ++ ":"
                  --dbg_true = dbg ++ "MERGED"
                  --dbg_false = dbg ++ "NOT MERGED"

embeddingRxTx :: PG.PGraph -> PG.PGraph -> PG.PGraph
embeddingRxTx prg lpg = embg'
    where
        prg' = setOrigin "PRG" $ tagPrgQueues prg
        lpg' = setOrigin "LPG" lpg
        -- List of queue identifiers
        rxQs = [ drop (length rxQPref) l |
                    (_, n)<- DGI.labNodes prg,
                    let l = PG.nLabel n,
                    rxQPref `L.isPrefixOf` l]
        -- PRG with full LPG added for each queue
        -- rxQs contains the name of the queue without the prefix
        embg  = foldl (addLPG lpg') prg' rxQs
        embg' = embg
        --embg' = foldl embedOffload  embg rxQs

-- Embedding
-- Assumptions:
--  Tx and Rx side are connected with spawn edges. That is, traversing the graph
--  from the PRG Rx nodes we will not end up in the PRG Tx nodes

-- TODO:
--  . predicates
--  . multiple queues

embeddingRxTx2 :: PG.PGraph -> PG.PGraph -> PG.PGraph
embeddingRxTx2 prg lpg = curEmb $ ST.execState (doEmbed embedRxTx) st0
    where st0 = initEmbedSt prg lpg

newtype Embed a = QEmbed { doEmbed :: ST.State EmbedSt a }
    deriving (Monad, ST.MonadState EmbedSt, Functor)

-- PRG Rx queues -> Apps => Forward
-- PRG Tx queues -> Apps => Backward
data EmbDirection = EmbRx | EmbTx
    deriving (Show)

-- Embedding state.
data EmbedSt = EmbedSt {
    -- fields set by the caller:
      embPrg        :: PG.PGraph    -- prg graph (does not change)
    , origLpg       :: PG.PGraph    -- lpg graph to embed (does not change)

    -- As we embed nodes, we move nodes from the lpg to the embedded graph
    -- curLpg contains the part of the LPG that is not yet embedded
    , curEmb       :: PG.PGraph
    , curLpg       :: PG.PGraph
    -- a map from original lpg nodes to a list of embedded nodes. There might be
    -- more than one nodes because we might duplicate nodes when there are
    -- multiple endpoints in the PRG
    -- TODO: the value with a [Maybe DGI.Node] to include mappings that are
    -- omitted due to unsatisfiable predicates
    , nodeMap       :: M.Map DGI.Node [DGI.Node]
    -- prg out nodes (Rx)
    , embPrgRx :: [PG.PGNode]
    -- prg in nodes (Tx)
    , embPrgTx :: [PG.PGNode]
    -- lpg Rx/Tx entry/out node
    , embLpgTx :: PG.PGNode
    , embLpgRx :: PG.PGNode
    -- A list of nodes that we need to try to embed next.
    --  They should be a part of the curLpg graph
    --  For EmbTx (EmbRx) all of their predecessors (successors) should be
    --  already embedded
    , lpgGrayNodes :: [PG.PGNode]
    , curDir       :: EmbDirection -- current direction
}

-- start from Rx queue nodes and find the find the sink nodes
prgRxNodes :: PG.PGraph -> [PG.PGNode]
prgRxNodes prg = lnodes
    where start  = map fst $ GH.filterNodesByL isRxQueueNode prg
          nodes  = filter (PGU.isSink_ prg) (DFS.dfs start prg)
          lnodes = [(n, fromJust $ DGI.lab prg n) | n <- nodes]

-- Tx queue nodes (and make sure they are source nodes)
prgTxNodes :: PG.PGraph -> [PG.PGNode]
prgTxNodes prg = nodes
    where nodes = map doCheck $ GH.filterNodesByL isTxQueueNode prg
          doCheck :: PG.PGNode -> PG.PGNode
          doCheck (nid,nlbl) = case PGU.isSource_ prg nid of
            True -> (nid,nlbl)
            False -> error "PRG Tx Queue node is not a source node"

-- lpg should have a single Rx node
lpgRxNode :: PG.PGraph -> PG.PGNode
lpgRxNode lpg = rxnode
    where rxnodes = GH.filterNodesByL isRxQueueNode lpg
          rxnode = case length rxnodes of
                     0 -> error "Did not found an rx queue node in lpg"
                     1 -> rxnodes !! 0
                     _ -> error "More than one rx queue node in lpg"

-- lpg should have a single Tx node
lpgTxNode :: PG.PGraph -> PG.PGNode
lpgTxNode lpg = txnode
    where txnodes = GH.filterNodesByL isTxQueueNode lpg
          txnode = case length txnodes of
                     0 -> error "Did not found an tx queue node in lpg"
                     1 -> txnodes !! 0
                     _ -> error "More than one tx queue node in lpg"

-- initialize embedding state
initEmbedSt prg lpg = EmbedSt {
      embPrg        = prg
    , origLpg       = lpg
    , curLpg        = lpg
    , curEmb        = prg -- embedding starts with the PRG
    , nodeMap       = M.empty
    , embPrgRx      = prgRxNodes prg
    , embPrgTx      = prgTxNodes prg
    , embLpgRx      = lpgRxNode lpg
    , embLpgTx      = lpgTxNode lpg
    , lpgGrayNodes  = undefined
    , curDir        = undefined
}

-- number of duplicates for each LPG node
dupsNr :: EmbedSt -> Int
dupsNr st = case (curDir st) of
              EmbRx -> length $ embPrgRx st
              EmbTx -> length $ embPrgTx st

-- main embedding function
embedRxTx :: Embed ()
embedRxTx = do
    initEmbedDir EmbRx >> embedDir
    initEmbedDir EmbTx >> embedDir
    lpg  <- ST.gets curLpg
    case DGI.isEmpty lpg of
        True -> return ()
        False -> error "Done, but LPG is not empty"
    embedSpawnEdges

-- prepare state for embedding on the given direction
initEmbedDir :: EmbDirection -> Embed ()
initEmbedDir dir = do
    st <- ST.get
    let lpgNodes0 = case dir of
                      EmbRx -> [embLpgRx st]
                      EmbTx -> [embLpgTx st]
        msg = "STARTING Embedding for " ++ (show dir) ++ " lpgNodes0=" ++ (show $ map pgName lpgNodes0)
        lpgNodes0' = tr lpgNodes0 msg
    ST.modify $ \s -> s { lpgGrayNodes = lpgNodes0', curDir = dir }

embedDir :: Embed ()
embedDir = do
    st  <- ST.get
    cur_lpg <- ST.gets curLpg
    orig_lpg <- ST.gets origLpg
    case embFindNext st of
        Nothing                -> return () -- no more nodes to embed
        Just (x@(xid,xlbl),xs) -> do
            ST.modify $ \s -> s { lpgGrayNodes = xs }
            case (DGI.match xid cur_lpg, DGI.match xid orig_lpg) of
                -- TODO: A node might be already be embedded: add a check
                ((Nothing, _), (Nothing, _)) -> error $ "Could not find node:" ++ (PG.nLabel xlbl)
                ((Just _, cur_lpg'), (Just orig_ctx, _)) -> do
                    ST.modify $ \s -> s { curLpg = cur_lpg' }
                    -- NB: we need the full context from the original LPG,
                    -- because the context from the cur_lpg does not contain
                    -- edges to ndoes that are removed
                    ctxEmbed orig_ctx
                    embedDir -- recurse

-- get a list of previous nodes (based on the embedding order)
embGetPrev_ :: EmbedSt -> DGI.Node -> [DGI.Node]
embGetPrev_ st = case (curDir st) of
    EmbRx -> DGI.pre lpg
    EmbTx -> DGI.suc lpg
    where lpg = origLpg st

embGetPrev :: EmbedSt -> PG.PGNode -> [PG.PGNode]
embGetPrev st (nid,_) = [ (pid, fromJust $ DGI.lab lpg pid) | pid <- embGetPrev_ st nid]
    where lpg = origLpg st

-- get a list of the next nodes (based on the embedding order)
embGetNext_ :: EmbedSt -> DGI.Node -> [DGI.Node]
embGetNext_ st = case (curDir st) of
    EmbRx -> DGI.suc lpg
    EmbTx -> DGI.pre lpg
    where lpg = origLpg st

-- check if a node is ready for embedding
-- All of its "previous" nodes should be embedded
readyToEmbed :: EmbedSt -> DGI.Node -> Bool
readyToEmbed st nid = and $ map (isEmbedded st) prevs
    where prevs = embGetPrev_ st nid

-- get next node to embed from the list of gray nodes
embFindNext :: EmbedSt -> Maybe (PG.PGNode, [PG.PGNode])
embFindNext st
    | null grays = Nothing
    | otherwise = ret
    where grays = lpgGrayNodes st
          (ready, rest) = L.partition ((readyToEmbed st) . fst) grays
          ret = case ready of
                  []   -> error $ "NO available nodes for embedding. Gray list: "
                                   ++ (show $ [PG.nLabel n | (_,n) <- grays]) ++ "\n"
                                   ++ "Previous nodes: " ++ (ppShow $ map (embGetPrev st) grays) ++ "\n"
                                   ++ "Node map: " ++ (ppShow (nodeMap st))
                  x:xs -> Just (x, xs++rest)

-- after a particular LPG ctx was embedded, add the appropriate gray nodes
embCtxAddGrays :: PG.PGContext -> Embed ()
embCtxAddGrays lpg_ctx@(ins,nid,_,outs) = do
    curLpg   <- ST.gets curLpg
    oldGrays <- ST.gets lpgGrayNodes
    dir      <- ST.gets curDir
    let in_adj  = filter (PGU.isNormalEdge_ . fst)  ins
        out_adj = filter (PGU.isNormalEdge_ . fst) outs
        newGrays = [(nid, fromJust $ DGI.lab curLpg nid) | (elbl,nid) <- adj]
            where adj = case dir of
                      EmbRx -> out_adj
                      EmbTx -> in_adj
        grays    = (L.nubBy ((==) `on` fst) (newGrays ++ oldGrays))
        msg = "   oldgrays: " ++ (show (map pgName oldGrays)) ++ "\n" ++
              "   newgrays: " ++ (show (map pgName newGrays)) ++ "\n" ++
              "   result  : " ++ (show (map pgName grays))
    ST.modify $ \s -> (tr s msg) {lpgGrayNodes = grays}

-- duplicate a context from the LPG
ctxDuplicate :: EmbedSt -> PG.PGContext -> [PG.PGContext]
ctxDuplicate st orig_ctx@(ins,nid,nlbl,outs) = [dupCtx idx | idx <- [0..size-1]]
    where size    = dupsNr st -- number of new contexts
          in_adj  = filter (PGU.isNormalEdge_ . fst)  ins -- incomming edges
          out_adj = filter (PGU.isNormalEdge_ . fst) outs -- outgoing edges
          emb_ids :: [DGI.Node]
          emb_ids = DGI.newNodes size (curEmb st)         -- get new node ids
          emb_lbls :: [PG.Node]
          emb_lbls = take size $ repeat nlbl              -- copy labels
          -- map the adjacency for the i-th embedding
          map_adj :: PG.PGAdj -> Int -> PG.PGAdj
          map_adj adj idx = [ (elbl, map_fn nid idx) | (elbl, nid) <- adj]
              where map_fn = embGetMapping st
          -- duplicate a context
          dupCtx :: Int -> PG.PGContext
          dupCtx idx = case (curDir st) of
                          EmbRx -> (xins, xnid, nlbl, [])     -- forward
                          EmbTx -> ([],   xnid, nlbl, xouts)  -- backward
            where xnid  = emb_ids !! idx
                  xins  = map_adj in_adj idx
                  xouts = map_adj out_adj idx

embedAddCtxs :: PG.PGContext -> [PG.PGContext] -> Embed ()
embedAddCtxs lpgCtx@(_,nid,nlbl,_) embCtxs = do
    emb  <- ST.gets curEmb
    nmap <- ST.gets nodeMap
    let newEmb = foldl (flip  (DGI.&)) emb embCtxs
        newMap = M.insertWith w nid [xid | (_,xid,_,_) <- embCtxs ] nmap
            where w :: [DGI.Node] -> [DGI.Node] -> [DGI.Node]
                  w _ _ = error $ "Embedded node: " ++ (PG.nLabel nlbl) ++
                                  " already exists in the map"
        ndiff = (DGI.noNodes newEmb) - (DGI.noNodes emb)
        msg = "   Adding " ++ (show ndiff) ++ " nodes to the embedded graph"
    ST.modify $ \s -> (tr s msg) { curEmb = newEmb, nodeMap = newMap }

embedCtxDuplicate :: PG.PGContext -> Embed ()
embedCtxDuplicate ctx = do
    st <- ST.get
    embedAddCtxs ctx (ctxDuplicate st ctx)

embIsQueueNode :: EmbDirection -> PG.Node -> Bool
embIsQueueNode EmbRx = isRxQueueNode
embIsQueueNode EmbTx = isTxQueueNode

-- the given LPG context is a queue node. Do the embedding
embedQueueNode :: PG.PGContext -> Embed ()
embedQueueNode lpg_ctx@(_,nid,nlbl,_) = do
    dir      <- ST.gets curDir
    nmap     <- ST.gets nodeMap
    prgNodes <- case dir of
        EmbRx -> ST.gets embPrgRx
        EmbTx -> ST.gets embPrgTx

    -- verify that PRG/LPG nodes have the same structure
    let verifyPrgNode prgn = case pgMatch nlbl (snd prgn) of
            True  -> prgn
            False -> error $ "PRG node " ++ (pgName prgn) ++ " does not match LPG " ++ (PG.nLabel nlbl) ++ " node's structure"
        prgNodes' = map verifyPrgNode prgNodes

    -- create new map
    let newMap = M.insertWith w nid [ xid | (xid,_) <- prgNodes'] nmap
            where w _ _ = error $ "Embedded node: " ++ (PG.nLabel nlbl) ++
                                  " already exists in the map"
        msg = "    Adding queue nodes:" ++ show (map pgName prgNodes')

    -- update state
    ST.modify $ \s -> (tr s msg) { nodeMap = newMap }

    return ()

ctxEmbed :: PG.PGContext -> Embed ()
ctxEmbed ctx_ = do
    dir <- ST.gets curDir
    let ctx@(ins,nid,nlbl,outs) = tr ctx_ ("\n  Embedding CTX: " ++ (ctxStr ctx_))
    case embIsQueueNode dir nlbl of
        True  -> embedQueueNode ctx
        False -> embedCtxDuplicate ctx
    embCtxAddGrays ctx
    return ()

-- get the ith mapping
embGetMapping :: EmbedSt -> (DGI.Node -> Int -> DGI.Node)
embGetMapping st = fn
    where fn :: DGI.Node -> Int -> DGI.Node
          fn nid idx = case M.lookup nid (nodeMap st) of
            Nothing -> error "node id does not exist"
            Just nodes  -> nodes !! idx

isEmbedded :: EmbedSt -> DGI.Node -> Bool
isEmbedded st xid = M.member xid (nodeMap st)

--embMap :: EmbedSt -> (DGI.Node -> [DGI.Node])
--embMap st = f
--    where f nid = case M.lookup nid (nodeMap st) of
--                    Nothing -> error "node id does not exist"
--                    Just nodes -> nodes

ctxStr :: PG.PGContext -> String
ctxStr (ins,nid,nlbl,outs) = show $ (ins,nid, PG.nLabel nlbl, outs)

ctxNode :: PG.PGContext -> PG.Node
ctxNode (_,_,n,_) = n

pgName :: PG.PGNode -> String
pgName = PG.nLabel . snd

-- check if two nodes have the same structure
pgMatch :: PG.Node -> PG.Node -> Bool
pgMatch (PG.FNode {PG.nPorts = p1}) (PG.FNode {PG.nPorts = p2}) = p1 == p2

checkEmbeddedAdjs :: EmbedSt -> PG.Node -> PG.PGAdj -> PG.PGAdj
checkEmbeddedAdjs st node adjs = adjs'
  where adjs' = map check_emb adjs
        check_emb :: (PG.Edge, DGI.Node) -> (PG.Edge, DGI.Node)
        check_emb x@(_,xid) =
             case isEmbedded st xid of
               True  -> x
               False -> error $ "when trying to embed `" ++
                                 (PG.nLabel node) ++
                                 "' => `" ++
                                 (PG.nLabel $ fromJust $ DGI.lab (origLpg st) xid) ++
                                    "' is not embedded"

getSpawnEdges :: PG.PGraph -> [PG.PGEdge]
getSpawnEdges = (filter PGU.isSpawnEdge) . DGI.labEdges

-- for now is all-to-all. We might want to restrain spawn edges between a
-- particular pair of Tx/Rx queues, though.
embSpawnEdges ::  EmbedSt -> PG.PGEdge -> [PG.PGEdge]
embSpawnEdges st (srcId, dstId, lbl) = ret
    where ret = [(x, y, lbl) | x <- embSrcs, y <- embDsts]
          embSrcs = case M.lookup srcId (nodeMap st) of
                       Nothing -> error $ "source spawn edge mapping does not exist"
                       Just x  -> x
          embDsts = case M.lookup dstId (nodeMap st) of
                       Nothing -> error $ "source spawn edge mapping does not exist"
                       Just x  -> x

embedSpawnEdges :: Embed ()
embedSpawnEdges = do
    st    <- ST.get
    edges <- getSpawnEdges <$> ST.gets origLpg
    emb   <- ST.gets curEmb
    let spawnEdges = concat [ embSpawnEdges st e | e <- edges ]
        newEmb = DGI.insEdges spawnEdges emb
    ST.modify $ \s -> s { curEmb = newEmb }
    return ()
