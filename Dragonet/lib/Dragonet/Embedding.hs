{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dragonet.Embedding(
    embeddingRxTx,
    embeddingRxTx2
) where

import qualified Dragonet.ProtocolGraph       as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Predicate           as PR

import Dragonet.Embedding.Offload (embedOffload)
import Dragonet.Conventions (rxQPref, txQPref, qTag, isTruePort, isFalsePort)

import Data.Maybe
import Data.Function (on)
import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Tuple as T

import qualified Data.Graph.Inductive           as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Util.GraphHelpers            as GH

import qualified Control.Monad.State as ST
import Control.Applicative ((<$>))
import Control.Exception.Base (assert)

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

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
    deriving (Show, Eq)

-- EmbNode:          node mapped at the given id
-- EmbNodeRemoved:   node removed
-- EmbNodeRedundant: node not needed (short-circuited on a particular port)
--                   The active port in the redundant node collapses with
--                   another graph point (node,graph)
-- EmbNodePartial:   node only partially needed (a number of ports are redundant)
-- The difference between the two last cases is that we insert the node in the
-- latter case
data EmbNode = EmbNode DGI.Node |
               EmbNodeRemoved   |
               EmbNodeRedundant PG.NPort (DGI.Node, PG.NPort) |
               EmbNodePartial DGI.Node [PG.NPort]
    deriving (Show)

embNodeId :: EmbNode -> Maybe DGI.Node
embNodeId (EmbNode nid) = Just nid
embNodeId (EmbNodePartial nid _) = Just nid
embNodeId _ = Nothing

-- Embedding state.
data EmbedSt = EmbedSt {
    -- fields set by the caller:
      embPrg        :: PG.PGraph    -- prg graph (does not change)
    , origLpg       :: PG.PGraph    -- lpg graph to embed (does not change)

    -- As we embed nodes, we move nodes from the LPG to the embedded graph
    -- curLpg contains the part of the LPG that is not yet embedded, while
    -- curEmb is the embedded graph.
    , curEmb       :: PG.PGraph
    , curLpg       :: PG.PGraph

    -- We maintain a map from LPG nodes to a list of embedded nodes. There are
    -- more than one nodes when we duplicate nodes for dealing with multiple
    -- endpoints in the PRG. This typically happens only in the Rx side.
    , embNodeMap   :: M.Map DGI.Node [EmbNode]

    -- prg out nodes (Rx)
    , embPrgRx      :: [PG.PGNode]
    , embPrgRxPreds :: [PR.PredExpr]

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
            alse -> error "PRG Tx Queue node is not a source node"

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
    , embNodeMap    = M.empty
    , embPrgRxPreds = rxPreds
    , embPrgRx      = rxNodes
    , embPrgTx      = prgTxNodes prg
    , embLpgRx      = lpgRxNode lpg
    , embLpgTx      = lpgTxNode lpg
    , lpgGrayNodes  = undefined
    , curDir        = undefined
}
    where rxNodes = prgRxNodes prg
          rxPreds = map (PR.nodePred prg) rxNodes

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

embGetPrevL_ :: EmbedSt -> DGI.Node -> [(DGI.Node, PG.Edge)]
embGetPrevL_ st = case (curDir st) of
    EmbRx -> DGI.lpre lpg
    EmbTx -> DGI.lsuc lpg
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
                                   ++ "Node map: " ++ (ppShow (embNodeMap st))
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

-- embed adjacency list
-- PG.PGAdj -> [(PG.Edge, DGI.Node)]
embAdj :: EmbedSt -> Int -> PG.PGAdj -> PG.PGAdj
embAdj st idx adj = concat $ map (embAdj_ st idx) adj

-- embed a single node of the adjacency list
embAdj_ :: EmbedSt ->
           Int ->                   -- embedding mapping index
           (PG.Edge, DGI.Node) ->   -- edge to map
           [(PG.Edge, DGI.Node)]    -- edges to embed (or empty)
embAdj_ st idx (edgeL, edgeN) =
    case M.lookup edgeN (embNodeMap st) of
        Nothing -> error "adjancent node id does not exist in mapping"
        -- decide what edge to insert based on the adjancent node's mapping
        Just xs -> case xs !! idx of
            EmbNode nid           -> [(edgeL, nid)]
            EmbNodeRemoved        -> []
            -- NB: There is a small issue with redundant nodes. If the redundant
            -- node is a boolean FNode (N) connected to an ONode (O), then we
            -- have no way of ensuring that the port name on N's predecessor is
            -- true/false to match what O expects. For now we try to sidestep
            -- this issue in the graph, but the proper solution would be to add
            -- the mapping for true/false in ONode edges in the Edge object,
            -- rather than being implicit based on the destination port. For
            -- example, the Edge with ONode destinations could include a dstPort
            -- as well.
            EmbNodeRedundant port (xnode,xport) -> xret
                where xret  = if (PG.ePort edgeL) == port then [xedge] else []
                      xedge = (PG.Edge { PG.ePort = xport }, xnode)
            EmbNodePartial nid ports -> if (PG.ePort edgeL) `elem` ports
                                        then [(edgeL, nid)]
                                        else []

embIdDummy :: DGI.Node
embIdDummy = -1 --tr undefined "embIdDummy"

embBld = PR.predBuildDNF

embNodePortActive :: EmbNode -> String -> Bool
embNodePortActive (EmbNodeRemoved) _         = False
embNodePortActive (EmbNode _)      _         = True
embNodePortActive (EmbNodeRedundant p _) port  = port == p
embNodePortActive (EmbNodePartial _ ps) port = port `elem` ps

embNodeEdgeActive :: EmbNode -> PG.Edge -> Bool
embNodeEdgeActive node (PG.Edge {PG.ePort = port}) = embNodePortActive node port

type EmbUpdate = (EmbNode, Maybe PG.PGContext)

embRxNode :: EmbedSt -> PG.PGNode -> Int -> (DGI.Node, PG.NPort) -> EmbNode
embRxNode st node@(nid, fnode@(PG.FNode {})) idx (prevId, prevPort)
    | spawnTarget = EmbNode embIdDummy
    | redundant   = EmbNodeRedundant (snd restP0) (prevId, prevPort)
    | otherwise   = EmbNode embIdDummy
 where
    pred = embRxPred st node idx
    portExpr p = tr ret msg
        where portPred = (PR.portPred_ embBld fnode p)
              ret = (PR.buildAND embBld) [pred, portPred]
              msg = "      port expr  for port " ++ (show p) ++ " is:" ++ (show portPred)
    portsExpr = map portExpr $ PG.nPorts fnode
    spawnTarget = PGU.isSpawnTarget (origLpg st) node
    ports = PG.nPorts fnode
    portsExpr' = zip portsExpr ports
    (deadPs,restPs) = L.partition ((==PR.PredFalse) . fst) portsExpr'
    redundant = length restPs == 1 && PR.predEquivHard (fst restP0) pred
    restP0 = head restPs

embRxPredCache :: EmbedSt -> Int -> PR.PredCache
embRxPredCache st idx = M.singleton (fst $ embLpgRx st) ((embPrgRxPreds st) !! idx)

embRxPred :: EmbedSt -> PG.PGNode -> Int -> PR.PredExpr
embRxPred st node idx = tr pred ("    Pred: Idx:" ++ (show idx) ++ " node:" ++ (pgName node) ++ " pred:" ++ (show pred))
    where predCache = embRxPredCache st idx
          pred = PR.nodePredCache (origLpg st) node predCache

embGetNP :: EmbedSt -> (DGI.Node, PG.NPort) -> Int -> Maybe (DGI.Node, PG.NPort)
embGetNP st (nid, nport) idx = case embGetNodeMap st nid idx of
    EmbNodeRemoved -> Nothing
    EmbNode embId  -> Just (embId, nport)
    EmbNodePartial embId ps -> case nport `elem` ps of
        True  -> Just (embId, nport)
        False -> Nothing
    EmbNodeRedundant p (embId,embPort) -> case nport == p of
        True  -> Just (embId, embPort)
        False -> Nothing

doEmbRx :: EmbedSt -> PG.PGContext -> Int -> EmbUpdate
-- embedding an F-node for Rx
-- We need to get:
--  - the predicate for reaching this node
--  - use it to calculate the incomming edge predicate
doEmbRx st orig_ctx@(ins, nid, fnode@PG.FNode {}, outs) idx = tr ret ("    ret:" ++ (show $ fst ret) ++ " ctx:" ++ (mctxStr $ snd ret))
    where
          lpgN = (nid, fnode)
          -- get previous  node id, label, and label from the LPG
          (prevId, PG.Edge {PG.ePort = prevPort}) = case embGetPrevL_ st nid of
              [x] -> x
              otherwise -> error "This is unexpected. Giving up!"
          prevNode = (prevId, fromJust $ DGI.lab (origLpg st) prevId)

          -- get the embedded graph node mapping of the previous node
          prevEmb = embGetNodeMap st prevId idx

          -- check if the port of the previous node is active.
          -- If not, we can omit this node. Otherwise figure out the mapping for
          -- the embedding node
          embN = embRxNode st lpgN  idx (fromJust $ embGetNP st (prevId, prevPort) idx)
          portActive_ = embNodePortActive prevEmb prevPort
          portActive = tr portActive_ ("\n    portactive: " ++ (show portActive_))
          ret = case portActive of
              False -> emptyRet
              True  -> case embN of
                  EmbNodeRemoved      -> error "This is not supposed to happen"
                  EmbNode _           -> (embN, Just newCtxRx)
                  EmbNodePartial _ _  -> (embN, Just newCtxRx)
                  EmbNodeRedundant _ _ -> (embN, Nothing)

          emptyRet = (EmbNodeRemoved, Nothing)
          embId = embIdDummy
          newCtxRx = (embIns, embId, fnode, [])
          embIns = embAdj st idx $ filter (PGU.isNormalEdge_ . fst) ins

-- embedding an O-node for Rx
doEmbRx st orig_ctx@(ins, nid, onode@PG.ONode { PG.nOperator = op}, outs) idx = ret
  where
   lpgN = (nid, onode)
   -- get previous nodes and edges
   prevs :: [(DGI.Node, PG.Edge)]
   prevs = embGetPrevL_ st nid
   (t_prevs, f_prevs) = L.partition (isTruePort . edgePortName_) prevs
   t_pmaps  = [ embGetNodeMap st nid idx | (nid, e) <- t_prevs ]
   f_pmaps  = [ embGetNodeMap st nid idx | (nid, e) <- f_prevs ]
   t_active = [ embNodeEdgeActive nm e | ((_, e), nm) <- zip t_prevs t_pmaps ]
   f_active = [ embNodeEdgeActive nm e | ((_, e), nm) <- zip f_prevs f_pmaps ]
   t_all_inactive = and $ map not t_active
   f_all_inactive = and $ map not f_active
   all_inactive = t_all_inactive && f_all_inactive
   predCache = embRxPredCache st idx

   getDepPred :: (DGI.Node, PG.Edge) -> PR.PredExpr
   getDepPred (nid, PG.Edge { PG.ePort = port }) = ret
       where ret  = PR.depPredCache (origLpg st) ((nid,node), port) predCache
             node = fromJust $ DGI.lab (origLpg st) nid
   t_preds = map getDepPred t_prevs
   f_preds = map getDepPred f_prevs
   pred_port_t  = PR.opPred embBld t_preds (op, "true")
   pred_port_f  = PR.opPred embBld f_preds ((PR.opNot op), "true")
   t_port_false = pred_port_t == PR.PredFalse
   f_port_false = pred_port_f == PR.PredFalse

   msg = "    pred_port_t=" ++ (show pred_port_t) ++ "\n" ++
         "    pred_port_f=" ++ (show pred_port_f) ++ "\n" ++
         "    t_preds=" ++ (show t_preds) ++ "\n" ++
         "    f_preds=" ++ (show f_preds) ++ "\n" ++
         "    embNode=" ++ (show $ fst ret_) ++ "\n"

   ret = tr ret_ msg
   ret_ = if all_inactive then emptyRet
          else if t_port_false && f_port_false then emptyRet
          else if f_port_false then trueRet
          else if t_port_false then falseRet
          else fullRet

   -- only the true (false) port is active
   t_prevX = L.find ((PR.predEquivHard pred_port_t) . snd)  (zip t_prevs t_preds)
   trueRet  = case t_prevX of
              Just x  -> ((EmbNodeRedundant "true" np'), Nothing)
                 where n = (fst . fst) x
                       p = edgePortName_ $ (fst x)
                       np' = fromJust $ embGetNP st (n, p) idx
              Nothing -> (EmbNodePartial embId ["true"], Just newCtxRx)
   falseRet = (EmbNodePartial embId ["false"], Just newCtxRx)
   -- node will not be enabled
   emptyRet = (EmbNodeRemoved, Nothing)

   fullRet  = (EmbNode embId, Just newCtxRx)
   newCtxRx = (embIns, embId, onode, [])
   embId    = embIdDummy
   embIns   = embAdj st idx $ filter (PGU.isNormalEdge_ . fst) ins

embGetNodeMap :: EmbedSt -> DGI.Node -> Int -> EmbNode
embGetNodeMap st nid idx = case M.lookup nid (embNodeMap st) of
    Just ids -> ids !! idx
    Nothing  -> error "Map does not exist"

doEmb :: EmbedSt -> PG.PGContext -> Int -> EmbUpdate
doEmb st orig_ctx@(ins,nid,nlbl,outs) idx
    -- One the Tx side, we always embed the node
    | (curDir st) == EmbTx = (EmbNode embId, Just newCtxTx)
    -- Rx
    | (curDir st) == EmbRx = doEmbRx st orig_ctx idx
    -- | (curDir st) == EmbRx = (EmbNode embId, Just newCtxRx)
    where embId    = embIdDummy
          newCtxTx = ([]    , embId, nlbl, embOuts)
          newCtxRx = (embIns, embId, nlbl, [])
          embOuts  = embAdj st idx $ filter (PGU.isNormalEdge_ . fst) outs
          embIns   = embAdj st idx $ filter (PGU.isNormalEdge_ . fst) ins

edgePortName_ :: (DGI.Node, PG.Edge) -> PG.NPort
edgePortName_ (_, PG.Edge { PG.ePort = eport }) = eport

embMapFillIds :: [EmbNode] -> [DGI.Node] -> [EmbNode]
embMapFillIds [] []                   = []
embMapFillIds ((EmbNode _):ms) (x:xs) = (EmbNode x):(embMapFillIds ms xs)
embMapFillIds ((EmbNodePartial _ ps):ms) (x:xs) = (EmbNodePartial x ps):(embMapFillIds ms xs)
embMapFillIds (m:ms) xs               = m:(embMapFillIds ms xs)


-- update mappings and graph for new embedded nodes
embUpdate :: PG.PGContext -> [EmbUpdate] -> Embed ()
embUpdate lpgCtx@(_,nid,nlbl,_) embUpd_ = do
    emb  <- ST.gets curEmb
    nmap <- ST.gets embNodeMap
    -- NB: get new node ids and update placeholders before changing state
    let embUpd   = tr embUpd_ $ "Update:" ++ (ppShow embUpd_)
        embCtxs  = catMaybes $ map snd embUpd
        embNids  = catMaybes $ map (embNodeId . fst) embUpd
        embIds   = case length embNids == length embCtxs of
            True -> DGI.newNodes (length embCtxs) emb
            False -> error "length of new ctxes does not match length of new ids"
        embCtxs' = [ (xins,xid,xlbl,xouts) | (xid,(xins,_,xlbl,xouts)) <- zip embIds embCtxs ]
        embMaps  = embMapFillIds (map fst embUpd) embIds
        newEmb   = foldl (flip (DGI.&)) emb embCtxs'
        newNMap  = M.insertWith w nid embMaps nmap
           where w _ _ = error $ "Embedded node: " ++ (PG.nLabel nlbl) ++ " already exists in the node map"
        ndiff = (DGI.noNodes newEmb) - (DGI.noNodes emb)
        msg = "  Adding " ++ (show ndiff) ++ " nodes to the embedded graph"
              ++ "\n    embMaps =" ++ (show embMaps)
    ST.modify $ \s -> (tr s msg) { curEmb = newEmb,
                                   embNodeMap = newNMap}
    return ()

embedNode :: PG.PGContext -> Embed ()
embedNode ctx = do
    st <- ST.get
    len <- ST.gets dupsNr
    let u = [doEmb st ctx idx | idx <-  [0..len-1]]
    embUpdate ctx u
    return ()

embIsQueueNode :: EmbDirection -> PG.Node -> Bool
embIsQueueNode EmbRx = isRxQueueNode
embIsQueueNode EmbTx = isTxQueueNode

-- the given LPG context is a queue node. Do the embedding
embedQueueNode :: PG.PGContext -> Embed ()
embedQueueNode lpg_ctx@(_,nid,nlbl,_) = do
    dir  <- ST.gets curDir
    nmap <- ST.gets embNodeMap
    prg  <- ST.gets embPrg
    prgNodes <- case dir of
        EmbRx -> ST.gets embPrgRx
        EmbTx -> ST.gets embPrgTx

    -- verify that PRG/LPG nodes have the same structure
    let verifyPrgNode prgn = case pgMatch nlbl (snd prgn) of
            True  -> prgn
            False -> error $ "PRG node " ++ (pgName prgn) ++ " does not match LPG " ++ (PG.nLabel nlbl) ++ " node's structure"
        prgNodes' = map verifyPrgNode prgNodes
        msg = "    Adding queue nodes:" ++ show (map pgName prgNodes')

    -- create new map
    let newNmap = M.insertWith w nid [EmbNode xid | (xid,_) <- prgNodes'] nmap
            where w _ _ = error $ "Node: " ++ (PG.nLabel nlbl) ++ " already exists in the embedded map"

    -- update state
    ST.modify $ \s -> (tr s msg) {embNodeMap = newNmap}

    return ()

ctxEmbed :: PG.PGContext -> Embed ()
ctxEmbed ctx_ = do
    dir <- ST.gets curDir
    let ctx@(ins,nid,nlbl,outs) = tr ctx_ ("\n  Embedding CTX: " ++ (ctxStr ctx_))
    case embIsQueueNode dir nlbl of
        True  -> embedQueueNode ctx
        False -> embedNode ctx
    embCtxAddGrays ctx
    return ()

isEmbedded :: EmbedSt -> DGI.Node -> Bool
isEmbedded st xid = M.member xid (embNodeMap st)

mctxStr :: Maybe PG.PGContext -> String
mctxStr (Just ctx) = "Just " ++ (ctxStr ctx)
mctxStr Nothing = "NOCTX"

ctxStr :: PG.PGContext -> String
ctxStr (ins,nid,nlbl,outs) = show $ (ins,nid, PG.nLabel nlbl, outs)
--ctxStr (ins,nid,nlbl,outs) = show $ PG.nLabel nlbl

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
    where ret = [(x, y, lbl) | x <- catMaybes embSrcs, y <- catMaybes embDsts]
          embSrcs = case M.lookup srcId (embNodeMap st) of
                       Nothing -> error $ "source spawn edge mapping does not exist"
                       Just x  -> map embNodeId x
          embDsts = case M.lookup dstId (embNodeMap st) of
                       Nothing -> error $ "destination spawn edge mapping does not exist"
                       Just x  -> map embNodeId x

embedSpawnEdges :: Embed ()
embedSpawnEdges = do
    st    <- ST.get
    edges <- getSpawnEdges <$> ST.gets origLpg
    emb   <- ST.gets curEmb
    let spawnEdges = concat [ embSpawnEdges st e | e <- edges ]
        newEmb = DGI.insEdges spawnEdges emb
    ST.modify $ \s -> s { curEmb = newEmb }
    return ()
