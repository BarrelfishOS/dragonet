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

tr = flip trace
trN = \x  _ -> x

--------------------------------------------------------------------------------
-- Embedding for both RX and TX path

-- queue tag (queue identifier)
type QTag = String

qGetTag :: PG.Node -> QTag
qGetTag qnode = if isTxQueueNode qnode      then txTag
                else if isRxQueueNode qnode then rxTag
                else error "qGetTag called on a non-queue node"
    where name  = PG.nLabel qnode
          txTag = qTag $ drop (length txQPref) name
          rxTag = qTag $ drop (length rxQPref) name

tagNodes :: QTag -> PG.PGraph -> PG.PGraph
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
--  . a newtype for LPG/PRG/Emb nodes might be useful

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
--                   another graph point (node,port)
-- EmbNodePartial:   node only partially needed (a number of ports are redundant)
--
-- The difference between the two last cases is that we insert the node in the
-- latter case. An EmbNodePartial that consists of a single active port is not
-- equivalent to a EmbNodeRedundant since the node might perform a necessary
-- computation and (cannot be omitted). In the Rx side, where the nodes
-- typically perform demultiplexing only the port selection matters. In such a
-- case, an EmbNodePartial with a single port is equivalent to an
-- EmbNodeRedundant (check embRxNode).
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

    -- As we embed nodes, we move nodes from the LPG to the embedded graph.
    -- curLpg contains the part of the LPG that is not yet embedded, while
    -- curEmb is the embedded graph.
    -- curEmb is initialized as the PRG.
    , curEmb       :: PG.PGraph
    , curLpg       :: PG.PGraph

    -- We maintain a map from LPG nodes to a list of embedded nodes. There are
    -- more than one nodes when we duplicate nodes for dealing with multiple
    -- endpoints in the PRG.
    , embNodeMap   :: M.Map DGI.Node [EmbNode]

    -- embedded node id -> original lpg node id mapping
    , embRevLpgMap :: M.Map DGI.Node DGI.Node

    -- prg out nodes (Rx)
    , embPrgRx      :: [PG.PGNode]
    , embPrgRxPreds :: [PR.PredExpr]
    , embPrgRxTags  :: [String]

    -- prg in nodes (Tx)
    , embPrgTx     :: [PG.PGNode]
    , embPrgTxOuts :: [PG.PGNode]
    , embPrgTxTags :: [String]

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

-- PRG Rx nodes (i.e., points where the LPG will connect to the PRG's RX side)
-- detection along with their tags
-- start from Rx queue nodes and find the sink nodes
prgRxNodes :: PG.PGraph -> [(PG.PGNode, QTag)]
prgRxNodes prg = L.concat $ map getQSinks rxQs
    where rxQs  = GH.filterNodesByL isRxQueueNode prg
          getN n = (n, fromJust $ DGI.lab prg n)
          getQSinks :: PG.PGNode -> [(PG.PGNode, QTag)]
          getQSinks qn =  [ (getN nid, qGetTag $ (snd  qn))
                            | nid <- DFS.dfs [fst qn] prg
                            , PGU.isSink_ prg nid ]

-- PRG Tx nodes (i.e., points where the LPG will connect to the PRG's TX side)
-- detection along with their corresponding tags.
--
-- For now we assume that onlty TxQueue nodes fill that role.
prgTxNodes :: PG.PGraph -> [(PG.PGNode, QTag)]
prgTxNodes prg = zip nodes tags
    where tags  = map (qGetTag . snd) nodes
          nodes = map doCheck $ GH.filterNodesByL isTxQueueNode prg
          doCheck :: PG.PGNode -> PG.PGNode
          doCheck (nid,nlbl) = case PGU.isSource_ prg nid of
            True -> (nid,nlbl)
            alse -> error "PRG Tx Queue node is not a source node"

-- Tx out nodes: start from prgTxNodes and for each, find the cooresponding out
-- node
prgTxOutNodes :: PG.PGraph -> [PG.PGNode]
prgTxOutNodes prg = map getOutNode txNodes
    where txNodes = map fst $ prgTxNodes prg
          getOutNode :: PG.PGNode -> PG.PGNode
          getOutNode n = case filter (PGU.isSink_ prg) (DFS.dfs [fst n] prg) of
                   []  -> error $ "Cannot find Tx OUT for " ++ (pgName n)
                   [x] -> (x, fromJust $ DGI.lab prg x)
                   _   -> error $ "More than one Tx OUT nodes for " ++ (pgName n)

qTagNodes :: PG.PGraph -> [(PG.PGNode, QTag)] -> PG.PGraph
qTagNodes gr ntags = DGI.gmap mapfn gr
    where ntags' = [ (nid, qtag) | ((nid,nlbl),qtag) <- ntags]
          mapfn ctx@(ins,nid,nlbl,outs) = case L.lookup nid ntags' of
            Just tag -> (ins, nid, nlbl { PG.nTag = tag }, outs)
            Nothing  -> ctx


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
    , origLpg       = lpg'
    , curLpg        = lpg'
    , curEmb        = setOrigin "PRG" taggedPrg -- embedding starts with the PRG
    , embNodeMap    = M.empty
    , embRevLpgMap  = M.empty
    --
    , embPrgRx      = rxNodes
    , embPrgTx      = txNodes
    , embPrgRxTags  = rxTags
    , embPrgTxTags  = txTags
    , embPrgRxPreds = rxPreds
    , embPrgTxOuts  = txOutNodes
    --
    , embLpgRx      = lpgRxNode lpg
    , embLpgTx      = lpgTxNode lpg
    , lpgGrayNodes  = undefined
    , curDir        = undefined
}
    where (rxNodes, rxTags) = unzip $ prgRxNodes prg
          (txNodes, txTags) = unzip $ prgTxNodes prg
          rxPreds = map (PR.nodePred prg) rxNodes
          txOutNodes = prgTxOutNodes prg
          taggedPrg  = qTagNodes prg ((prgRxNodes prg) ++ (prgTxNodes prg))
          lpg'       = setOrigin "LPG" lpg

-- number of duplicates for each LPG node
dupsNr :: EmbedSt -> Int
dupsNr st = case (curDir st) of
              EmbRx -> length $ embPrgRx st
              EmbTx -> length $ embPrgTx st

embCleanup :: Embed ()
embCleanup = do
    emb  <- ST.gets curEmb
    lpg  <- ST.gets origLpg
    rmap <- ST.gets embRevLpgMap
    -- We check the LPG graph to determine whether a node is a sink or a source.
    -- If a reverse mapping does not exist, then this must be a  PRG node, so we
    -- just return True so that cleanupGraphWith does not remove it
    let  clEmb = PGU.cleanupGraphWith isSrc isSink emb
         isSrc (nid,nlbl) = ret --tr ret ("Checking if " ++ (show $ PG.nLabel nlbl) ++ "(" ++ (show nid) ++ ") is a source node: " ++ (show ret))
            where ret = case M.lookup nid rmap of
                          Just x  -> PGU.isSource_ lpg x
                          Nothing -> True
         isSink (nid,nlbl) = ret --tr ret ("Checking if " ++ (show $ PG.nLabel nlbl) ++ "(" ++ (show nid) ++ ") is a sink node: " ++ (show ret))
            where ret = case M.lookup nid rmap of
                          Just x  -> PGU.isSink_ lpg x
                          Nothing -> True
         --clEmb = PGU.cleanupGraph emb
    ST.modify $ \s -> s { curEmb = clEmb }
    return ()

embOffloadTxNode_ :: PG.PGNode -> Int -> Embed ()
embOffloadTxNode_ lpgN idx = do
    nmap   <- ST.gets embNodeMap
    txOuts <- ST.gets embPrgTxOuts
    emb    <- ST.gets curEmb
    let (lpgNid, lpgPGN) = trN lpgN $ "embOffloadTxNode_: " ++ (pgName lpgN) ++ "idx: " ++ (show idx)
        outName  = show $ pgName txOut
        offlName = show $ PG.nLabel lpgPGN
        embN_ = case M.lookup lpgNid nmap of
            Just xs -> xs !! idx
            Nothing -> error $ "embOffloadTxNode_: mapping for node " ++ offlName ++ " does not exist"
        embN  = tr embN_ $ "Trying to offload embedded node: " ++ (show embN_)
        embNid = case embNodeId embN of
            Just x -> x
            Nothing -> error $ "embOffloadTxNode_: node " ++ offlName ++ " is not mapped to a NID"
        -- NB: the embedded graph is initially a copy of the PRG graph, so the
        -- ids for the embedded PRG nodes remain the same
        txOut       = txOuts !! idx
        prOut       = PR.nodePred emb txOut
        newPrOut    = PR.nodePred newEmb txOut
        --equiv       = PR.predEquivHard_ prOut newPrOut
        equiv       = PR.dnfEquiv_ prOut newPrOut
        canOffload  = isNothing equiv
        msg         = "   Trying to offload TX Node:" ++ offlName ++ "\n" ++
                      "   predicate before on " ++ outName ++ ": " ++ (PR.dnetPrShow prOut) ++ "\n\n" ++
                      "   predicate after  on " ++ outName ++ ": " ++ (PR.dnetPrShow newPrOut) ++ "\n"
                      --msg_res

        msg_res     = case equiv of
            Nothing -> "canOffload: YES"
            --Just x  -> "canOffload: NO (offending assignment:\n" ++ (ppShow $ L.sortBy (compare `on` (\(p,_,_)-> p)) x)
            Just x  -> "canOffload: NO (" ++ x ++ ")"

        newEmb      = DGI.delNode embNid emb

    case tr canOffload msg of
        True -> ST.modify $ \s -> s { curEmb = newEmb}
        False -> return ()

embOffloadTxNode :: PG.PGNode -> Embed ()
embOffloadTxNode node = do
    txOuts_ <- ST.gets embPrgTxOuts
    let txOuts = tr txOuts_ $ "embOffloadTxNode: txOuts:" ++ (show txOuts_)
    ST.forM_ [0..(length txOuts)-1] (embOffloadTxNode_ node)

embOffloadTx :: Embed ()
embOffloadTx = do
    emb   <- ST.gets curEmb
    lpg   <- ST.gets origLpg
    prg   <- ST.gets embPrg
    lpgTx <- ST.gets embLpgTx
    let lpgTxNs = filter (not . isTxQueueNode . snd) $  PGU.pgRDfsNEs lpg [lpgTx]
        prgNs = DGI.labNodes prg
        -- NB: intersectBy keeps the element from the first list
        candidates_ = L.intersectBy ((==) `on` pgName) lpgTxNs prgNs
        candidates  = tr candidates_ ("===> Tx Offload candidates:" ++ (show $ map pgName candidates_))
    ST.forM_ candidates embOffloadTxNode
    return ()

prEmbed :: String -> Embed ()
prEmbed prefix = do
    emb <- ST.gets curEmb
    let emb' = trN emb ("\n---" ++ prefix ++ ": Current Embedded Graph--\n" ++ (ppShow emb))
    ST.modify $ \s ->  s { curEmb = emb' }

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
    embOffloadTx
    --prEmbed "Before cleanup"
    embCleanup

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
    st       <- ST.get
    cur_lpg  <- ST.gets curLpg
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
    EmbRx -> PGU.preNE lpg
    EmbTx -> PGU.sucNE lpg
    where lpg = origLpg st

embGetPrevL_ :: EmbedSt -> DGI.Node -> [(DGI.Node, PG.Edge)]
embGetPrevL_ st = case (curDir st) of
    EmbRx -> PGU.lpreNE lpg
    EmbTx -> PGU.lsucNE lpg
    where lpg = origLpg st

embGetPrev :: EmbedSt -> PG.PGNode -> [PG.PGNode]
embGetPrev st (nid,_) = [ (pid, fromJust $ DGI.lab lpg pid) | pid <- embGetPrev_ st nid]
    where lpg = origLpg st

-- get a list of the next nodes (based on the embedding order)
--
--  NB: There is an assumption here: Tx and Rx sides are connected via spawn
--  edges.  Hence, if using sucNE/preNE functions that only consider "normal"
--  edges we cannot reach Tx from Rx and vice versa.
--
embGetNext_ :: EmbedSt -> DGI.Node -> [DGI.Node]
embGetNext_ st = case (curDir st) of
    EmbRx -> PGU.sucNE lpg
    EmbTx -> PGU.preNE lpg
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
    ST.modify $ \s -> (trN s msg) {lpgGrayNodes = grays}

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

embRxNode :: EmbedSt -> PG.PGNode -> Int -> (DGI.Node, PG.NPort) -> EmbNode
embRxNode st node@(nid, fnode@(PG.FNode {})) idx (prevId, prevPort)
    | spawnTarget = EmbNode embIdDummy
    | redundant   = EmbNodeRedundant (snd aliveP0) (prevId, prevPort)
    | partial     = EmbNodePartial embIdDummy $ map snd alivePs
    | otherwise   = EmbNode embIdDummy
 where
    pred = embRxPred st node idx
    portExpr p = tr ret msg
        where portPred = (PR.portPred_ embBld fnode p)
              ret = (PR.buildAND embBld) [pred, portPred]
              msg = "      port:" ++ (show p) ++ "\n\tpred:" ++ (show portPred) ++ "\n\texpr:" ++ (show ret)
    portsExpr = map portExpr $ PG.nPorts fnode
    spawnTarget = PGU.isSpawnTarget (origLpg st) node
    ports = PG.nPorts fnode
    portsExpr' = zip portsExpr ports
    (deadPs,alivePs) = L.partition ((==PR.PredFalse) . fst) portsExpr'
    --redundant = length alivePs == 1 && PR.predEquivHard (fst aliveP0) pred
    redundant = length alivePs == 1 && PR.dnfEquiv (fst aliveP0) pred
    partial = length deadPs > 0
    aliveP0 = head alivePs

embRxPredCache :: EmbedSt -> Int -> PR.PredCache
embRxPredCache st idx = M.singleton (fst $ embLpgRx st) ((embPrgRxPreds st) !! idx)

embRxPred :: EmbedSt -> PG.PGNode -> Int -> PR.PredExpr
embRxPred st node idx = tr pred ("      Pred: Idx:" ++ (show idx) ++ " node:" ++ (pgName node) ++ " pred:" ++ (show pred))
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

-- embedding an F-node for Rx
-- We need to get:
--  - the predicate for reaching this node
--  - use it to calculate the incomming edge predicate
doEmbRx :: EmbedSt -> PG.PGContext -> Int -> EmbNode
doEmbRx st orig_ctx@(ins, nid, fnode@PG.FNode {}, outs) idx = ret
    where
          lpgN = (nid, fnode)
          -- get previous  node id, label, and label from the LPG
          (prevId, PG.Edge {PG.ePort = prevPort}) = case embGetPrevL_ st nid of
              [x] -> x
              []  -> error $ "Node " ++ (pgName lpgN) ++ " has zero predecessors"
              otherwise -> error $ "Node " ++ (pgName lpgN) ++ " has more than one predecessors"
          prevNode = (prevId, fromJust $ DGI.lab (origLpg st) prevId)

          -- get the embedded graph node mapping of the previous node
          prevEmb = embGetNodeMap st prevId idx

          -- check if the port of the previous node is active.
          -- If not, we can omit this node. Otherwise figure out the mapping for
          -- the embedding node
          portActive_ = embNodePortActive prevEmb prevPort
          portActive = tr portActive_ ("\n    "
                                              ++ " [PRG Boundary:"
                                              ++ (pgName $ ((embPrgRx st) !! idx))
                                              ++ "] => "
                                              ++ "previous node port:"
                                              ++ prevPort
                                              ++ " is active: "
                                              ++ (show portActive_)
                                              )
          ret = case portActive of
              False -> EmbNodeRemoved
              True  -> embN
          embN = embRxNode st lpgN  idx (fromJust $ embGetNP st (prevId, prevPort) idx)

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
   getDepPred (nid, PG.Edge { PG.ePort = port }) = p
       where p  = PR.depPredCache (origLpg st) ((nid,node), port) predCache
             node = fromJust $ DGI.lab (origLpg st) nid
   t_preds = map getDepPred t_prevs
   f_preds = map getDepPred f_prevs
   pred_port_t  = PR.opPred embBld t_preds (op, "true")
   pred_port_f  = PR.opPred embBld f_preds ((PR.opNot op), "true")
   t_port_false = pred_port_t == PR.PredFalse
   f_port_false = pred_port_f == PR.PredFalse

   msg = --"    pred_port_t=" ++ (show pred_port_t) ++ "\n" ++
         --"    pred_port_f=" ++ (show pred_port_f) ++ "\n" ++
         --"    t_preds=" ++ (show t_preds) ++ "\n" ++
         --"    f_preds=" ++ (show f_preds) ++ "\n" ++
         "    embNode=" ++ (show $ ret_) ++ "\n"

   ret = tr ret_ msg
   ret_ = if all_inactive then EmbNodeRemoved
          else if t_port_false && f_port_false then EmbNodeRemoved
          else if f_port_false then trueRet
          else if t_port_false then falseRet
          else EmbNode embId

   -- only the true (false) port is active
   --t_prevX = L.find ((PR.predEquivHard pred_port_t) . snd)  (zip t_prevs t_preds)
   t_prevX = L.find ((PR.dnfEquiv pred_port_t) . snd)  (zip t_prevs t_preds)
   trueRet  = case t_prevX of
              Just x  -> EmbNodeRedundant "true" np'
                 where n = (fst . fst) x
                       p = edgePortName_ $ (fst x)
                       np' = fromJust $ embGetNP st (n, p) idx
              Nothing -> EmbNodePartial embId ["true"]
   falseRet = EmbNodePartial embId ["false"]
   -- node will not be enabled
   emptyRet = (EmbNodeRemoved, Nothing)
   embId    = embIdDummy

embGetNodeMap :: EmbedSt -> DGI.Node -> Int -> EmbNode
embGetNodeMap st nid idx = case M.lookup nid (embNodeMap st) of
    Just ids -> ids !! idx
    Nothing  -> error "Map does not exist"

mkEmbCtx :: EmbedSt -> PG.PGContext -> EmbNode -> EmbDirection -> Int -> Maybe PG.PGContext
mkEmbCtx st ctx@(ins,nid,nlbl,outs) embN dir idx  = case embId of
    Nothing -> Nothing
    Just x  -> Just newCtx
    where newCtx = case dir of
                     EmbTx -> ([],     embId', nlbl_tag, embOuts)
                     EmbRx -> (embIns, embId', nlbl_tag, [])
          embOuts  = embAdj st idx $ filter (PGU.isNormalEdge_ . fst) outs
          embIns   = embAdj st idx $ filter (PGU.isNormalEdge_ . fst) ins
          embId    = embNodeId embN
          embId'   = fromJust embId
          qtag     = case dir of
                       EmbTx -> (embPrgTxTags st) !! idx
                       EmbRx -> (embPrgRxTags st) !! idx
          nlbl_tag = nlbl { PG.nTag = qtag }

edgePortName_ :: (DGI.Node, PG.Edge) -> PG.NPort
edgePortName_ (_, PG.Edge { PG.ePort = eport }) = eport

-- add ids to a lst of EmbNodes
embMapFillIds :: [EmbNode] -> [DGI.Node] -> [EmbNode]
embMapFillIds [] []                   = []
embMapFillIds ((EmbNode _):ms) (x:xs) = (EmbNode x):(embMapFillIds ms xs)
embMapFillIds ((EmbNodePartial _ ps):ms) (x:xs) = (EmbNodePartial x ps):(embMapFillIds ms xs)
embMapFillIds (m:ms) xs               = m:(embMapFillIds ms xs)

-- update mappings and graph for new embedded nodes
embUpdate :: PG.PGContext -> [EmbNode] -> Embed ()
embUpdate lpgCtx@(_,lpgNid,nlbl,_) embNs_ = do
    st   <- ST.get
    dir  <- ST.gets curDir
    emb  <- ST.gets curEmb
    nmap <- ST.gets embNodeMap
    rmap <- ST.gets embRevLpgMap
    let -- allocate ids for the embedded graph
        embIds   = DGI.newNodes (length $ catMaybes $ map embNodeId embNs_) emb
        -- update embedded nodes
        embNs   = embMapFillIds embNs_ embIds
        embCtxs = catMaybes [ mkEmbCtx st lpgCtx embN dir idx | (idx, embN) <- zip [0..] embNs ]
        newEmb   = foldl (flip (DGI.&)) emb embCtxs
        -- insert an entry to the reverse map. All entries have the same value:
        -- the node id of the LPG node we are embedding
        insRmap :: M.Map DGI.Node DGI.Node -> DGI.Node -> M.Map DGI.Node DGI.Node
        insRmap m embNid = M.insertWith w embNid lpgNid m
           where w _ _ = error $ "embedded node: " ++ (show embNid) ++ " already exists in the reverse map"
        newRMap = foldl insRmap rmap embIds

        newNMap  = M.insertWith w lpgNid embNs nmap
           where w _ _ = error $ "LPG node: " ++ (PG.nLabel nlbl) ++ " already exists in the node map"
        ndiff = (DGI.noNodes newEmb) - (DGI.noNodes emb)
        msg = "\n  Adding " ++ (show ndiff) ++ " node(s) to the embedded graph"
              ++ "\n    embMaps = " ++ (show embNs)
    ST.modify $ \s -> (tr s msg) { curEmb = newEmb,
                                   embRevLpgMap = newRMap,
                                   embNodeMap = newNMap}
    return ()

embedNode :: PG.PGContext -> Embed ()
embedNode ctx = do
    st  <- ST.get
    len <- ST.gets dupsNr
    embUpdate ctx [doEmb st ctx idx | idx <- [0..len-1]]

doEmb :: EmbedSt -> PG.PGContext -> Int -> EmbNode
doEmb st orig_ctx@(ins,nid,nlbl,outs) idx
    -- One the Tx side, we always embed the node
    | (curDir st) == EmbTx = EmbNode embIdDummy
    -- Rx
    | (curDir st) == EmbRx = doEmbRx st orig_ctx idx

embIsQueueNode :: EmbDirection -> PG.Node -> Bool
embIsQueueNode EmbRx = isRxQueueNode
embIsQueueNode EmbTx = isTxQueueNode

-- check if two nodes have the same structure
pgMatch :: PG.Node -> PG.Node -> Bool
pgMatch (PG.FNode {PG.nPorts = p1}) (PG.FNode {PG.nPorts = p2}) = p1 == p2

-- Provide an embedded mapping for an Rx queue node
--  lpgN: the LPG queue node
--  prgN: The corresponding PRG node
mapRxQueueNode :: PG.PGNode -> PG.PGNode -> EmbNode
mapRxQueueNode lpgN@(lpgNid,lpgNlbl) prgN@(prgNid,prgNlbl)
    | PG.FNode {PG.nPorts = lpgNports} <- lpgNlbl,
      PG.FNode {PG.nPorts = prgNports} <- prgNlbl =
        let commonPorts = L.intersect lpgNports prgNports
            lenCommonPorts = length commonPorts
            errmsg = "PRG node " ++ (pgName prgN) ++
                     " does not have common ports with LPG node " ++
                     (pgName lpgN)
        in if lenCommonPorts == 0 then error errmsg
           else case compare lenCommonPorts (length lpgNports) of
                  EQ -> EmbNode prgNid
                  LT -> EmbNodePartial prgNid commonPorts
                  GT -> error "mapRxQueueNode: this should not happen"
    | otherwise = error "mapRxQueueNode: one of the nodes is not an F-node"

-- Provide an embedded mapping for an Tx queue node
-- no structural check is necessary
mapTxQueueNode :: PG.PGNode -> PG.PGNode -> EmbNode
mapTxQueueNode (_, PG.FNode {}) (prgNid, PG.FNode {}) = EmbNode prgNid
mapTxQueueNode _ _  = error "mapTxQueueNode: one of the nodes is not an F-node"

-- the given LPG context is a queue node. Do the embedding.
--
-- NB: The initial state of the embedded graph is the PRG. Hence, the boundary
-- (queue) nodes we are dealing with here already exist in the embedded graph
-- with the ids and labels of the PRG graph. Hence the mapping id is the id of
-- the matching PRG node.
embedQueueNode :: PG.PGContext -> Embed ()
embedQueueNode lpg_ctx@(_,lpgNid,lpgNlbl,_) = do
    let lpgN = (lpgNid, lpgNlbl)
    dir  <- ST.gets curDir
    nmap <- ST.gets embNodeMap
    prg  <- ST.gets embPrg

    -- prgNodes is the boundary of PRG nodes (sources or sinks)
    prgNodes <- case dir of
        EmbRx -> ST.gets embPrgRx
        EmbTx -> ST.gets embPrgTx
    embNodes <- case dir of
        EmbRx -> return $ map (mapRxQueueNode lpgN) prgNodes
        EmbTx -> return $ map (mapTxQueueNode lpgN) prgNodes

    -- create new map
    let newNmap = M.insertWith w lpgNid embNodes nmap
            where w _ _ = error $ "Node: " ++ (pgName lpgN) ++ " already exists in the embedded map"

    -- update state
    let msg = "    Adding queue nodes:" ++ show (map pgName prgNodes)
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
--ctxStr (ins,nid,nlbl,outs) = show $ (ins,nid, PG.nLabel nlbl, outs)
ctxStr (ins,nid,nlbl,outs) = show $ PG.nLabel nlbl

ctxNode :: PG.PGContext -> PG.Node
ctxNode (_,_,n,_) = n

pgName :: PG.PGNode -> String
pgName = PG.nLabel . snd

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
    where ret = [(x, y, lbl) | x <- catMaybes embSrcs, y <- catMaybes embDsts,
                               validSrcDst(x,y) ]

          getEmbL :: DGI.Node -> PG.Node
          getEmbL nid = case DGI.lab (curEmb st) nid of
                         Just l  -> l
                         Nothing -> error "embSpawnEdges: cannot find label"

          -- create a spawn edge only for nodes that have the same queue tag
          validSrcDst :: (DGI.Node, DGI.Node) -> Bool
          validSrcDst (srcId, dstId) = PG.nTag src == PG.nTag dst
                where src = getEmbL srcId
                      dst = getEmbL dstId

          embSrcs :: [Maybe DGI.Node]
          embSrcs = case M.lookup srcId (embNodeMap st) of
                       Nothing -> error $ "source spawn edge mapping does not exist"
                       Just x  -> map embNodeId x

          embDsts :: [Maybe DGI.Node]
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
