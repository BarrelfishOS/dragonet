{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dragonet.Embedding(
    embeddingRxTx,
    embeddingRxTx2,

    foldAndCleanup, foldAndCleanupAll,
) where

import qualified Dragonet.ProtocolGraph       as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Dragonet.Predicate           as PR

import Dragonet.Embedding.Offload (embedOffload)
import Dragonet.Conventions (rxQPref, txQPref, qTag, isTruePort, isFalsePort,
                             constTrueName, constFalseName)

import Data.Maybe
import Data.Function (on)
import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Tuple as T

import qualified Data.Graph.Inductive           as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Util.GraphHelpers              as GH

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
-- EmbNodePartial:   node mapped at the given id, but only the given ports are
--                   active -- i.e., ports not included will not be enabled.
-- EmbNodeRedundant: node not needed (short-circuited on a particular port)
--                   The active port in the redundant node collapses with
--                   another graph point (node,port). These are nodes that are
--                   offloaded.
-- EmbNodeBoolConst: This is similar to the EmbNodeRedundant -- i.e., the node
--                   is not needed. For cases, however, that the node's
--                   active port is connected to an O-node, we insert this dummy
--                   node so that we can optimize graph after all the nodes are
--                   embedded
--
-- The difference between the two last cases is that we insert the node in the
-- latter case. An EmbNodePartial that consists of a single active port is not
-- equivalent to a EmbNodeRedundant since the node might perform a necessary
-- computation and (cannot be omitted). In the Rx side, where the nodes
-- typically perform demultiplexing only the port selection matters. In such a
-- case, an EmbNodePartial with a single port is equivalent to an
-- EmbNodeRedundant (check embRxNode).
data EmbNode = EmbNode DGI.Node                               |
               EmbNodeRemoved                                 |
               EmbNodePartial   DGI.Node [PG.NPort]           |
               EmbNodeRedundant PG.NPort (DGI.Node, PG.NPort) |
               EmbNodeBoolConst DGI.Node PG.NPort
    deriving (Show)

-- Get the node id of an embedded mapping:
--  - Nothing -> no embedded node exists
--  - Just x  -> id
embNodeId :: EmbNode -> Maybe DGI.Node
embNodeId (EmbNode nid)            = Just nid
embNodeId (EmbNodePartial nid _)   = Just nid
embNodeId (EmbNodeBoolConst nid _) = Just nid
embNodeId EmbNodeRemoved           = Nothing
embNodeId (EmbNodeRedundant _ _)   = Nothing

embMkNode :: EmbNode -> PG.Node ->  PG.Node
embMkNode (EmbNode _) nlbl = nlbl
embMkNode (EmbNodePartial _ _) nlbl = nlbl
embMkNode (EmbNodeBoolConst _ port) _
    | isTruePort  port = PG.baseFNode constTrueName  ["true", "false"]
    | isFalsePort port = PG.baseFNode constFalseName ["true", "false"]
    | otherwise = error $ "Expecting true/false port:" ++ port

pgNodeIsConst :: PG.Node -> Bool
pgNodeIsConst (PG.FNode {PG.nLabel = name}) =
    (name == constTrueName) || (name == constFalseName)
pgNodeIsConst (PG.ONode {}) = False

-- add ids to a lst of EmbNodes
embMapFillIds :: [EmbNode] -> [DGI.Node] -> [EmbNode]
embMapFillIds [] []                              = []
embMapFillIds ((EmbNode _):ms) (x:xs)            = (EmbNode x):rest
    where rest = embMapFillIds ms xs
embMapFillIds ((EmbNodePartial _ ps):ms)  (x:xs) = (EmbNodePartial x ps):rest
    where rest = embMapFillIds ms xs
embMapFillIds ((EmbNodeBoolConst _ p):ms) (x:xs) = (EmbNodeBoolConst x p):rest
    where rest = embMapFillIds ms xs
-- no new node
embMapFillIds (m@(EmbNodeRemoved):ms) xs         = m:(embMapFillIds ms xs)
embMapFillIds (m@(EmbNodeRedundant _ _):ms) xs   = m:(embMapFillIds ms xs)



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
    -- nodes to connect the LPG to
    , embPrgRx            :: [PG.PGNode]
    -- predicate for each node
    , embPrgRxPreds       :: [PR.PredExpr]
    -- queue tag for each node
    , embPrgRxTags        :: [String]
    -- Unused ports for each node (i.e., points to connect the LPG)
    , embPrgRxUnusedPorts :: [[PG.NPort]]

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
-- detection along with their tags. For each queue, we select the nodes which
-- have unconnected ports.
prgRxNodes :: PG.PGraph -> [((PG.PGNode, [PG.NPort]), QTag)]
prgRxNodes prg = L.concat $ map getQSinks rxQs
    where rxQs = GH.filterNodesByL isRxQueueNode prg
          getN n = (n, fromJust $ DGI.lab prg n)
          getQSinks :: PG.PGNode -> [((PG.PGNode, [PG.NPort]), QTag)]
          getQSinks qn =  [ ((node, lports), qtag)
                            | nid <- DFS.dfs [fst qn] prg
                            , let node   = getN nid
                            , let qtag   =  qGetTag $ snd  qn
                            , let lports = PGU.unconnectedPorts prg node
                            , not $ L.null lports]


-- PRG Tx nodes (i.e., points where the LPG will connect to the PRG's TX side)
-- detection along with their corresponding tags.
--
-- For now we assume that only TxQueue nodes fill that role.
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
    , curEmb        = emb0
    , embNodeMap    = M.empty
    , embRevLpgMap  = M.empty
    --
    , embPrgRx      = rxNodes
    , embPrgRxTags  = rxTags
    , embPrgRxPreds = rxPreds
    , embPrgRxUnusedPorts = rxPorts
    --
    , embPrgTx      = txNodes
    , embPrgTxTags  = txTags
    , embPrgTxOuts  = txOutNodes
    --
    , embLpgRx      = lpgRxNode lpg
    , embLpgTx      = lpgTxNode lpg
    , lpgGrayNodes  = undefined
    , curDir        = undefined
}
    where (rxNodesPorts, rxTags) = unzip $ prgRxNodes prg
          (rxNodes, rxPorts)     = unzip rxNodesPorts
          xPrgRxNodes            = zip rxNodes rxTags
          xPrgTxNodes            = prgTxNodes prg
          (txNodes, txTags)      = unzip xPrgTxNodes
          rxPreds                = map (PR.nodePred prg) rxNodes
          txOutNodes             = prgTxOutNodes prg
          taggedPrg              = prg --qTagNodes prg (xPrgRxNodes ++ xPrgTxNodes)
          lpg'                   = setOrigin "LPG" lpg
          -- initial embedded graph: starts with the PRG
          emb0                   = setOrigin "PRG" taggedPrg
          {--
          tfIds                  = DGI.newNodes 2 emb0
          tNode                  = PG.baseFNode constTrueName  ["true", "false"]
          fNode                  = PG.baseFNode constFalseName ["true", "false"]
          tfNodes                = zip tfIds [tNode,fNode]
          emb0'                  = DGI.insNodes tfNodes emb0
          --}

-- number of duplicates for each LPG node
dupsNr :: EmbedSt -> Int
dupsNr st = case (curDir st) of
              EmbRx -> length $ embPrgRx st
              EmbTx -> length $ embPrgTx st
--
-- Folding constant operands of ONodes
-- ===================================
--
-- Currently, all O-nodes can be short-circuted. Hence, each operator is
-- characterized by two parameters:
--  SC_I -> input value that short-circuts the operator
--  SC_O -> output value when operator is short-circuted
-- (see opShortCircuitInOut)
--
--                                      --> (O-node)
-- +-----+-+         +-------+    -----/
-- |     |X|-------->|CONST|A|---/--      +-----+-+     ---> X
-- |Prev +-+         +-------+      \---->|     |T|----/---> Y
-- |     |Y|                              |  OP +-+
-- +-----+-+                          --->|     |F|--------> Z
--                            -------/    +-----+-+    \---> W
--       (Other operands)----/
--
--
-- For each operand (node connected to the O-node) O_i, there is a TRUE (t_i)
-- and a FALSE (f_i) input. Missing edges are interpreted as inputs that cannot
-- be activated.
--
-- The output is (e.g., for OP=AND)
--  SC_O (=False)      : one SC_I (=False) input is activated
-- (NOT SC_O) (=True)  : all (NOT SC_I) (=True) inputs are activated
--
-- There are three cases:
--  - Case 1: Const value does not short-circuit O-node: A = (NOT SC_I)
--    (e.g., for OP=AND, A=TRUE)
--
--    In this case, We can remove the edge from CONST to OP if we can show that
--    activation of any of the other operands implies that the constant node is
--    also activated. Otherwise, it might be the case that the const node is
--    never activated and hence its (constant) value is never passed to the
--    O-node.
--
--    Hence, if (Prev,X) dominates any of the OP's operands, we can remove the
--    (CONST,A) -> OP edge
--
--  - Case 2: Const value short-circuits O-node: A = SC_I
--    (e.g., for OP=AND, A=FALSE)
--
--    Hence, there are two possible outcomes:
--     . OP node is never activated
--     . OP node is activated and its output is SC_O
--
--    In this case we can:
--     . remove all (NOT SC_I) edges since they do not contribute to the result
--     . remove all (NOT SC_O) edges, since they cannot be activated
--     . Potentially connect all nodes connected to OP's SC_O to
--       Prev.X (e..g, for OP=AND, A=False, conect Z and W to Prev.X) and
--       effectively remove OP from the graph.
--
--   Why potentially? We need to avoid the case where the OP node is activated
--   to False by another node, while the CONST node is never activated. Hence,
--   we only do the last part if all OP's operands are dominated by Prev.X
--   because then there is no way for another operand to be activated without
--   the CONST node being activated.
--
--   Depending on the destination type of the nodes connected to OP's SC_O we
--   need to perform a different operation:
--    i)  if it's an F-node, we introduce (Prev.X -> Z) edge
--    ii) if it's an O-node, we introduce (Prev.X -> (CONST SC_O) -> Z) edge
--        (i.e., we add a new CONST node)
--
--  - Case 3: Const is the only input of the O-node
--    In this case we can short-circuit (see case 2) to the appropriate edge
--
graphFoldConstSucc :: (PG.PGNode, PG.NPort) ->
                      PG.PGNode -> PG.PGraph -> PG.PGNode -> PG.PGraph
graphFoldConstSucc prevSrc@((prevNid,prevLbl), prevPort)
                   (constNid,constNlbl) gr (opNid,opNlbl)
    | nArgs == 1       = tryShortCircuit (PG.opSingleVal op constVal)
    | constVal == scIn = tryShortCircuit scOut
    | otherwise        = tryRemoveConstEdge
    where constVal = pgConstNodeVal constNlbl
          op = PG.nOperator opNlbl
          -- short-circuted in/out values for the given operator
          Just (scIn, scOut) = PG.opShortCircuitInOut op
          -- operator argument nodes
          opArgs = DGI.pre gr opNid
          -- operator arguments (operands)
          nArgs = length opArgs
          otherOperands = [(nid, nlbl) | nid <- opArgs
                                       , nid /= constNid
                                       , let nlbl = fromJust $ DGI.lab gr nid]
          -- Case 1:
          -- try remove the Const edge because it does not
          -- contribute to the result
          canRemoveConstEdge = any (PGU.dominates gr prevSrc) otherOperands
          constOutEdge = (constNid, opNid)
          tryRemoveConstEdge = tr ret "graphFoldConst: CASE 1"
            where ret = case canRemoveConstEdge of
                     True  -> DGI.delEdge constOutEdge gr
                     False -> error "NYI: cannot remove const edge"

          -- Case 2:
          canShortCircuit = all (PGU.dominates gr prevSrc) otherOperands
          tryShortCircuit opResult = tr ret "graphFoldConst: CASE 2"
            where
                  ret = case canShortCircuit of
                    True  -> g3
                    False -> g0

                  alivePort = if opResult then "true" else "false"
                  isAliveEdge (_,e) = (PGU.edgePort_ e) == alivePort
                  opOuts = PGU.lsucNE gr opNid
                  (aliveOuts, deadOuts) = L.partition isAliveEdge opOuts

                  -- cannot short-circuit:
                  --  . delete all dead edges from the ONode:
                  g0 = DGI.delEdges [ (opNid, xid) | (xid,_) <- deadOuts ] gr
                  -- can short-circute:
                  --  . delete all edges from ONode
                  --  . insert new edges to F-nodes (2i)
                  --  . insert new const node, and new const node edges (2ii)
                  g1 = DGI.delEdges [(opNid, xid) | (xid,_) <- aliveOuts] g0
                  g2 = DGI.insEdges newEdgesF g1
                  g3 = case length succAliveO of
                         0 -> g2
                         otherwise -> DGI.insEdges newConstEdges
                                      $ DGI.insNode newConstNode g2
                  -- ONode alive successors
                  succAlive = [(nid,nlbl) | (nid,_) <- aliveOuts
                                          , let nlbl = fromJust $ DGI.lab gr nid]
                  -- split successor into F-nodes and O-nodes
                  (succAliveF,succAliveO) = L.partition PGU.isFnode succAlive
                  -- new edges to F-nodes
                  newEdgesF = [(prevNid, nid, PG.Edge prevPort) | (nid,_) <- succAliveF]
                  -- new const node and new const node edges
                  newConstNode = tr (newConstNid, newConstLbl) $ "Inserting new const node: " ++ (show newConstNid)
                  newConstNid = head $ DGI.newNodes 1 gr
                  newConstLbl_ = PG.baseFNode newConstName ["true","false"]
                  newConstLbl  = newConstLbl_ {PG.nTag = PG.nTag prevLbl}
                  newConstName = if alivePort == "true"
                                       then constTrueName
                                       else constFalseName
                  newConstEdges = fromPrev:fromConst
                        where fromPrev  = (prevNid, newConstNid, PG.Edge prevPort)
                              fromConst = [(newConstNid, nid, PG.Edge alivePort)
                                           | (nid,_) <- succAliveO]

pgConstNodeVal :: PG.Node -> Bool
pgConstNodeVal (PG.FNode { PG.nLabel = name })
    | (name == constTrueName)  = True
    | (name == constFalseName) = False

graphFoldConst :: PG.PGraph -> PG.PGNode -> PG.PGraph
graphFoldConst gr constN@(constNid,constNlbl) = tr ret msg
    where ret = foldl (graphFoldConstSucc prev constN) gr lsuc
          msg = "graphFoldConst: " ++ (show (constNid, (pgName constN)))
          alivePort = if pgConstNodeVal constNlbl then "true" else "false"

          -- successor nodes:
          -- verify that they are O-nodes and are connected to the active port
          lsuc :: [PG.PGNode]
          lsuc = assert (all doCheck lsuc_) (map fst lsuc_)
            where doCheck :: (PG.PGNode, PG.Edge) -> Bool
                  doCheck ((_,nlbl), PG.Edge p) =
                        p == alivePort &&
                        PGU.isOnode_ nlbl
                  doCheck _ = False
          lsuc_ :: [(PG.PGNode, PG.Edge)]
          lsuc_ = [((nid,nlbl), edge) | (nid, edge) <- DGI.lsuc gr constNid,
                                        let nlbl = fromJust $ DGI.lab gr nid]
          -- predecessor node
          prev :: (PG.PGNode, PG.NPort)
          prev = PGU.getSinglePrePort gr constN

foldAndCleanup g isSourceF isSinkF = foldAndCleanup_ g isSourceF isSinkF empty
    where empty = S.fromList []

foldAndCleanup_ :: PG.PGraph           -- graph
                -> (PG.PGNode -> Bool) -- isSource
                -> (PG.PGNode -> Bool) -- isSink
                -> S.Set DGI.Node      -- already visited consts
                -> PG.PGraph
foldAndCleanup_ g isSource isSink oldConsts
    | isNothing constNode0 = g_clean
    | otherwise            = recurse
    where g_clean = PGU.cleanupGraphWith isSource isSink g
          findFn (nid,nlbl) = (pgNodeIsConst nlbl)
                              && (nid `S.notMember` oldConsts)
          constNode0 = GH.findNode findFn g_clean
          constNode0'= fromJust constNode0
          g' = graphFoldConst g_clean constNode0'
          oldConsts' = S.insert (fst constNode0') oldConsts
          recurse = foldAndCleanup_ g' isSource isSink oldConsts'

foldAndCleanupAll_ :: PG.PGraph           -- graph
                   -> (PG.PGNode -> Bool) -- isSource
                   -> (PG.PGNode -> Bool) -- isSink
                   -> S.Set DGI.Node      -- already visited consts
                   -> [PG.PGraph]
foldAndCleanupAll_ g isSource isSink oldConsts
    | isNothing constNode0 = [g,g_clean]
    | otherwise            = [g,g_clean,g'] ++ recurse
    where g_clean = PGU.cleanupGraphWith isSource isSink g
          findFn (nid,nlbl) = (pgNodeIsConst nlbl)
                              && (nid `S.notMember` oldConsts)
          constNode0 = GH.findNode findFn g_clean
          constNode0'= fromJust constNode0
          g' = graphFoldConst g_clean constNode0'
          oldConsts' = S.insert (fst constNode0') oldConsts
          recurse = foldAndCleanupAll_ g' isSource isSink oldConsts'

foldAndCleanupAll g isSourceF isSinkF = foldAndCleanupAll_ g isSourceF isSinkF s0
    where s0 = S.fromList []

embCleanup :: Embed ()
embCleanup = do
    emb  <- ST.gets curEmb
    lpg  <- ST.gets origLpg
    rmap <- ST.gets embRevLpgMap
    -- We check the LPG graph to determine whether a node is a sink or a source.
    -- If a reverse mapping does not exist, then this must be a  PRG node, so we
    -- just return True so that cleanupGraphWith does not remove it
    let  clEmb = foldAndCleanup emb isSrc isSink
         isSrc (nid,node) = ret --tr ret ("Checking if " ++ (show name) ++ "(" ++ (show nid) ++ ") is a source node: " ++ (show ret))
            where name = PG.nLabel node
                  ret = if name `elem` [constTrueName, constFalseName]
                        then True
                        else ret_
                  ret_ = case M.lookup nid rmap of
                          Just x  -> PGU.isSource_ lpg x
                          Nothing -> True
         isSink (nid,nlbl) = ret --tr ret ("Checking if " ++ (show $ PG.nLabel nlbl) ++ "(" ++ (show nid) ++ ") is a sink node: " ++ (show ret))
            where ret = case M.lookup nid rmap of
                          Just x  -> PGU.isSink_ lpg x
                          Nothing -> True
         --clEmb = PGU.cleanupGraph emb
    ST.modify $ \s -> s { curEmb = clEmb }
    return ()

doOffloadNode :: PG.PGraph -> PG.PGNode  -> PG.PGraph
doOffloadNode gr node@(nodeNid,nodeLbl) = ret
    where ports = PG.nPorts nodeLbl
          nports = length ports
          ret = case nports of
            1         -> offload
            otherwise -> error $ "Not sure how to offload node with " ++ (show nports) ++ " ports"
          -- TODO: make optimizations?
          offload = newG
          ((prevNid,_), prevPort) = PGU.getSinglePrePort gr node
          next = DGI.lsuc gr nodeNid
          newG =    DGI.delNode nodeNid
                  $ DGI.insEdges [(prevNid,x, PG.Edge prevPort) | (x,e) <- next]
                  $ DGI.delEdges [(nodeNid,x) | (x,_) <- next] gr

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
        embN  = trN embN_ $ "\nTrying to offload embedded node: " ++ (show embN_)
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
        msg         = "\n   Trying to offload TX Node:" ++ offlName ++
                      -- "\n" ++
                      -- "   predicate before on " ++ outName ++ ": " ++ (PR.dnetPrShow prOut) ++ "\n\n" ++
                      -- "   predicate after  on " ++ outName ++ ": " ++ (PR.dnetPrShow newPrOut) ++ "\n" ++
                      msg_res

        msg_res     = case equiv of
            Nothing -> " canOffload: YES"
            --Just x  -> "canOffload: NO (offending assignment:\n" ++ (ppShow $ L.sortBy (compare `on` (\(p,_,_)-> p)) x)
            Just x  -> "canOffload: NO. dnfEquiv said:\n`" ++ x ++ "\n'"

        newEmb = doOffloadNode emb (embNid, fromJust $ DGI.lab emb embNid)

    case tr canOffload msg of
        True -> ST.modify $ \s -> s { curEmb = newEmb}
        False -> return ()

embOffloadTxNode :: PG.PGNode -> Embed ()
embOffloadTxNode node = do
    txOuts_ <- ST.gets embPrgTxOuts
    let txOuts = trN txOuts_ $ "embOffloadTxNode: txOuts:" ++ (show txOuts_)
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
    --embOffloadTx
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
        lpgNodes0' = trN lpgNodes0 msg
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
embAdj :: EmbedSt -> PG.PGContext -> Int -> PG.PGAdj -> PG.PGAdj
embAdj st ctx idx adj = concat $ map (embAdj_ st ctx idx) adj

-- embed a single edge of the adjacency list
--   Rx (Tx): embed the incoming (outgoing) edges of the node we are embedding
embAdj_ :: EmbedSt ->
           PG.PGContext ->          -- context we are embedding
           Int ->                   -- embedding mapping index
           (PG.Edge, DGI.Node) ->   -- edge to map
           [(PG.Edge, DGI.Node)]    -- edges to embed (or empty)
embAdj_ st ctx idx (edgeL, edgeN) =
    case M.lookup edgeN (embNodeMap st) of
        Nothing -> error "adjancent node id does not exist in mapping"
        -- decide what edge to insert based on the adjancent node's mapping
        Just xs -> case xs !! idx of
            EmbNode nid           -> [(edgeL, nid)]
            EmbNodeRemoved        -> []
            EmbNodePartial nid ports -> if (PG.ePort edgeL) `elem` ports
                                        then [(edgeL, nid)]
                                        else []
            -- Redundant node:
            --  if the edge is not on the node's active port: add no edges
            --  otherwise: add an edge from the short-circuited node,port
            EmbNodeRedundant port (xnode,xport) -> ret
                where ret  = if (PG.ePort edgeL) == port then edges else []
                      xedge = (PG.Edge { PG.ePort = xport }, xnode)
                      edges = [xedge]

            EmbNodeBoolConst nid port -> if (PG.ePort edgeL) == port
                                         then [yedge] else []
               where yedge
                      | isTruePort  port = (PG.Edge { PG.ePort = "true" }, nid)
                      | isFalsePort port = (PG.Edge { PG.ePort = "false"}, nid)
                      | otherwise = error $ "port:" ++ port ++ " is not a boolean port"

embIdDummy :: DGI.Node
embIdDummy = -666 --tr undefined "embIdDummy"

embBld = PR.predBuildDNF

embNodePortActive :: EmbNode -> String -> Bool
embNodePortActive (EmbNodeRemoved) _         = False
embNodePortActive (EmbNode _)      _         = True
embNodePortActive (EmbNodeRedundant p _) port  = port == p
embNodePortActive (EmbNodePartial _ ps) port = port `elem` ps
embNodePortActive (EmbNodeBoolConst _ p) port = port == p

embNodeEdgeActive :: EmbNode -> PG.Edge -> Bool
embNodeEdgeActive node (PG.Edge {PG.ePort = port}) = embNodePortActive node port

-- check whether an F-node port is connected to an O-node
nodePortConnectedToONode :: PG.PGraph -> DGI.Node -> PG.NPort -> Bool
nodePortConnectedToONode g nid port = not $ null $ filter filtFn $ PGU.sucPortNE g nid port
    where filtFn xid = case fromJust $ DGI.lab g xid of
                         PG.ONode {} -> True
                         otherwise -> False

embRxNode :: EmbedSt -> PG.PGNode -> Int -> (DGI.Node, PG.NPort) -> EmbNode
embRxNode st node@(nid, fnode@(PG.FNode {})) idx (prevId, prevPort)
    | spawnTarget = EmbNode embIdDummy
    | redundant   = case boolconst of
                     False -> tr embNodeRedundant $ "\nNode: " ++ (pgName node) ++ " found redundant on PRG endpoint " ++ (show idx) ++ " (" ++ prgRxBoundary st idx ++ ")"
                     True  -> tr embNodeBoolConst $ "\nNode: " ++ (pgName node) ++ " found boolconst " ++ (alivePort0) ++ " on PRG endpoint " ++ (show idx) ++ " (" ++ prgRxBoundary st idx ++ ")"
    | partial     = tr embNodePartial $ "\nNode: " ++ (pgName node) ++ " found partial " ++ (show alivePorts) ++ " on PRG endpoint " ++ (show idx) ++ " (" ++ prgRxBoundary st idx ++ ")"

    | otherwise   = EmbNode embIdDummy
 where
    pred = embRxPred st node idx
    portExpr p = trN ret msg
        where portPred = (PR.portPred_ embBld fnode p)
              ret = (PR.buildAND embBld) [pred, portPred]
              msg = "      port:" ++ (show p) ++ "\n\tpred:" ++ (show portPred) ++ "\n\texpr:" ++ (show ret)
    portsExpr = map portExpr $ PG.nPorts fnode
    spawnTarget = PGU.isSpawnTarget (origLpg st) node
    ports = PG.nPorts fnode
    portsExpr' = zip portsExpr ports
    (deadPs,alivePs) = L.partition ((==PR.PredFalse) . fst) portsExpr'
    partial = length deadPs > 0
    alivePorts = map snd alivePs
    embNodePartial = EmbNodePartial embIdDummy alivePorts
    aliveP0 = head alivePs
    (alivePred0, alivePort0) = aliveP0
    -- check for offload
    --redundant = length alivePs == 1 && PR.predEquivHard (fst aliveP0) pred
    redundant = length alivePs == 1 && PR.dnfEquiv alivePred0 pred
    boolconst = nodePortConnectedToONode (origLpg st) nid alivePort0
    embNodeRedundant = EmbNodeRedundant alivePort0 (prevId, prevPort)
    embNodeBoolConst = EmbNodeBoolConst embIdDummy alivePort0

embRxPredCache :: EmbedSt -> Int -> PR.PredCache
embRxPredCache st idx = M.singleton (fst $ embLpgRx st) ((embPrgRxPreds st) !! idx)

embRxPred :: EmbedSt -> PG.PGNode -> Int -> PR.PredExpr
embRxPred st node idx = trN pred ("      Pred: Idx:" ++ (show idx) ++ " node:" ++ (pgName node) ++ " pred:" ++ (show pred))
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
    EmbNodeBoolConst embId p -> case nport == p of
        True -> Just (embId, nport)
        False -> Nothing

-- name of idx-th  PRG boundary
prgRxBoundary :: EmbedSt -> Int -> String
prgRxBoundary st idx = pgName $ (embPrgRx st) !! idx

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
          portActive = trN portActive_ ("\n    "
                                              ++ " [PRG Boundary:"
                                              ++ prgRxBoundary st idx
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

   msg = "    pred_port_t=" ++ (show pred_port_t) ++ "\n" ++
         "    pred_port_f=" ++ (show pred_port_f) ++ "\n" ++
         "    t_preds=" ++ (show t_preds) ++ "\n" ++
         "    f_preds=" ++ (show f_preds) ++ "\n" ++
         "    embNode=" ++ (show $ ret_) ++ "\n"

   ret = trN ret_ msg
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

mkEmbCtx :: EmbedSt -> PG.PGContext -> EmbNode -> Int -> Maybe PG.PGContext
mkEmbCtx st ctx@(ins,nid,nlbl,outs) embN idx  = case embId of
    Nothing -> Nothing
    Just x  -> Just newCtx
    where dir = curDir st
          doEmbAdj = embAdj st ctx idx
          isNormal = PGU.isNormalEdge_ . fst
          newCtx = case dir of
                     EmbTx -> ([], embId', nlblTagged, embOuts)
                            where embOuts = doEmbAdj $ filter isNormal outs
                     EmbRx -> (embIns, embId', nlblTagged, [])
                            where embIns  = doEmbAdj $ filter isNormal ins
          embId    = embNodeId embN
          embId'   = fromJust embId
          qtag     = case dir of
                       EmbTx -> (embPrgTxTags st) !! idx
                       EmbRx -> (embPrgRxTags st) !! idx
          nlblTagged = (embMkNode embN nlbl) {PG.nTag = qtag}

edgePortName_ :: (DGI.Node, PG.Edge) -> PG.NPort
edgePortName_ (_, PG.Edge { PG.ePort = eport }) = eport


-- update mappings and graph for new embedded nodes
embUpdate :: PG.PGContext -> [EmbNode] -> Embed ()
embUpdate lpgCtx@(_,lpgNid,nlbl,_) embNs_ = do
    st   <- ST.get
    emb  <- ST.gets curEmb
    nmap <- ST.gets embNodeMap
    rmap <- ST.gets embRevLpgMap
    let -- allocate ids for the embedded graph
        embIds   = DGI.newNodes (length $ catMaybes $ map embNodeId embNs_) emb
        -- update embedded nodes
        embNs   = embMapFillIds embNs_ embIds
        embCtxs = catMaybes [ mkEmbCtx st lpgCtx embN idx | (idx, embN) <- zip [0..] embNs ]
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
    ST.modify $ \s -> (trN s msg) { curEmb = newEmb,
                                   embRevLpgMap = newRMap,
                                   embNodeMap = newNMap}
    return ()

embedNode :: PG.PGContext -> Embed ()
embedNode ctx = do
    st  <- ST.get
    len <- ST.gets dupsNr
    let embNs :: [EmbNode]
        embNs = [doEmbNodeTrace st ctx idx | idx <- [0..len-1]]
    embUpdate ctx embNs

doEmbNodeTrace :: EmbedSt -> PG.PGContext -> Int -> EmbNode
doEmbNodeTrace st ctx@(_,_,nlbl,_) idx = trN ret msg
    where ret = doEmbNode st ctx idx
          msg = "Node: " ++ (PG.nLabel nlbl) ++ " PRG Endpoint " ++ (show idx)  ++ " mapped to:" ++ (show ret)

doEmbNode :: EmbedSt -> PG.PGContext -> Int -> EmbNode
doEmbNode st orig_ctx@(ins,nid,nlbl,outs) idx
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


-- Provide an embedded mapping for an Tx queue node
-- no structural check is necessary
mapTxQueueNode :: PG.PGNode -> PG.PGNode -> EmbNode
mapTxQueueNode (_, PG.FNode {}) (prgNid, PG.FNode {}) = EmbNode prgNid
mapTxQueueNode _ _  = error "mapTxQueueNode: one of the nodes is not an F-node"

-- Provide an embedded mapping for an Rx queue node
--  lpgN: the LPG queue node
--  prgN: The corresponding PRG node
--  prgNports: the unconnected ports of the PRG node
mapRxQueueNode :: PG.PGNode -> (PG.PGNode, [PG.NPort]) -> EmbNode
mapRxQueueNode lpgN@(lpgNid,lpgNlbl) (prgN@(prgNid,prgNlbl), prgNports)
    | PG.FNode {PG.nPorts = lpgNports} <- lpgNlbl,
      PG.FNode {} <- prgNlbl =
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
       EmbTx -> return $ map (mapTxQueueNode lpgN) prgNodes
       EmbRx -> do
            ts <- ST.gets $ \s -> zip prgNodes (embPrgRxUnusedPorts s)
            return $ map (mapRxQueueNode lpgN) ts

    -- create new map
    let newNmap = M.insertWith w lpgNid embNodes nmap
            where w _ _ = error $ "Node: " ++ (pgName lpgN) ++ " already exists in the embedded map"

    -- update state
    let msg = "    Adding queue nodes:" ++ show (map pgName prgNodes)
    ST.modify $ \s -> (trN s msg) {embNodeMap = newNmap}

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
