module Dragonet.ProtocolGraph.Utils (
    dropSpawnEdges,
    entryNodes,
    orderedSpawns,
    getPGNodeByName,
    getFNodeByName,
    getFNodeByName',
    getFNodeByNameTag',
    getPGNAttr,
    spawnDeps, isSpawnTarget, edgeDeps, edgePort, edgePort_, edgeSucc,
    isSink_, isSource_,
    isSpawnEdge_, isSpawnEdge,
    isNormalEdge_, isNormalEdge,
    cleanupGraph, cleanupGraphWith,
    pgDfsNEs, pgRDfsNEs,
    sucNE, preNE, lpreNE, lsucNE,
    lsucPortNE, sucPortNE,
    unconnectedPorts,
    isOnode_, isOnode, isFnode_, isFnode,
    getSinglePre, getSinglePrePort,
    dominates,

    fromSocketId,toSocketId, toSocketNode, fromSocketNode,
    balanceNode, balanceNodeEndpoint, balanceNodeEndpointId, isBalanceNode,
) where

import Dragonet.ProtocolGraph

import Data.Function
import Data.Maybe
import Data.List (sortBy, find, isPrefixOf)
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Graph.Inductive as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Util.GraphHelpers as GH
import Data.Functor ((<$>))

import Dragonet.DotGenerator (toDot, toDotHighlight)

import Debug.Trace (trace)
tr = flip trace
trN = \x  _ -> x

-- Remove all spawn edges from the graph
dropSpawnEdges :: PGraph -> PGraph
dropSpawnEdges g = flip DGI.gmap g $ \(ins,n,l,outs) ->
    (filter isNSpawn ins, n, l, filter isNSpawn outs)
    where
        isNSpawn (ESpawn {},_) = False
        isNSpawn (_,_) = True

entryNodes :: PGraph -> S.Set DGI.Node
entryNodes g = S.fromList $ mapMaybe getN $ DGI.labEdges g
    where
        -- The if is a very ugly hack
        getN (_,d,ESpawn {}) = Just d
        getN _ = Nothing

-- | Ordered list of spawn-handles (order is used to generate implementation
--   IDs)
orderedSpawns :: PGraph -> DGI.Node -> [(NSpawnHandle,DGI.Node)]
orderedSpawns pg n = sortBy (compare `on` fst)
        [(sh,n) | (n,ESpawn { eIdentifier = sh }) <- DGI.lsuc pg n]


getPGNodeByName :: PGraph -> NLabel -> PGNode
getPGNodeByName graph name = case GH.findNodeByL fn graph of
    Just x -> x
    Nothing -> error $ "Unable to find node with name: " ++ name
    where fn n = (nLabel n == name)

getFNodeByName' :: PGraph -> NLabel -> Maybe PGNode
getFNodeByName' graph name = GH.findNodeByL fn graph
    where fn (FNode {nLabel = label}) = (label == name)
          fn _ = False

getFNodeByNameTag' :: PGraph -> NLabel -> NTag -> Maybe PGNode
getFNodeByNameTag' graph label' tag' = GH.findNodeByL fn graph
    where fn (FNode {nLabel = label, nTag = tag }) = (label == label') && (tag == tag')
          fn _ = False

getFNodeByName :: PGraph -> NLabel -> PGNode
getFNodeByName graph name = case GH.findNodeByL fn graph of
    Just x -> x
    Nothing -> error $ "Unable to find f-node with name: " ++ name
    where fn (FNode label _ _ _ _ _ _ _) = (label == name)
          fn _ = False

-- Get key=value attribute with key == n
getPGNAttr :: Node -> String -> Maybe String
getPGNAttr node n = getVal <$> (find matches $ nAttributes node)
    where
        matches (NAttrCustom s) = (n ++ "=") `isPrefixOf` s
        matches _ = False
        getVal (NAttrCustom s) = drop (length n + 1) s

-- {from,to}Socket/loadBalance Nodes

fromSocketId :: Node -> Maybe String
fromSocketId n = getPGNAttr n fromSocketAttr

toSocketId :: Node -> Maybe String
toSocketId n = getPGNAttr n toSocketAttr

toSocketNode :: Integer -> Node
toSocketNode sid =
    nAttrsAdd [
        NAttrCustom "sink",
        NAttrCustom $ toSocketAttr ++ "=" ++ (show sid) ]
    $ baseFNode ("ToSocket" ++ show sid) ["out","drop"]


fromSocketNode :: Integer -> Node
fromSocketNode sid =
    nAttrsAdd [
        NAttrCustom "source",
        NAttrCustom $ fromSocketAttr ++ "=" ++ (show sid) ]
    $ baseFNode ("FromSocket" ++ show sid) ["true"]


--EndpointBalanceNode :: String -> Integer -> [NPort] -> Node
--EndpointBalanceNode prefix eid ports =
--    balanceNode

-- toSocket/fromSocket nodes contain an attribute of the form
-- tosocket=id, where id is the socket id
toSocketAttr   = "tosocket"
fromSocketAttr = "fromsocket"
-- load balance node attributes
-- (this is used when multiple nodes are connected to the same endpoint)
loadBalanceAttr = "loadbalance"
balanceEpAttr = "balanceEP"

balanceNode :: String -> [NPort] -> Node
balanceNode prefix ports = nAttrAdd attr $ baseFNode name ports
    where name = prefix ++ "::Balance"
          attr = NAttrCustom loadBalanceAttr

isBalanceNode :: Node -> Bool
isBalanceNode = nAttrElem (NAttrCustom loadBalanceAttr)

balanceNodeEndpoint :: String -> Integer -> [NPort] -> Node
balanceNodeEndpoint prefix eid ports =
    let attr = NAttrCustom $ balanceEpAttr ++ "=" ++ (show eid)
    in nAttrAdd attr $ balanceNode prefix ports


balanceNodeEndpointId :: Node -> Maybe Integer
balanceNodeEndpointId n = read <$> getPGNAttr n balanceEpAttr

--

isNormalEdge_ :: Edge -> Bool
isNormalEdge_ e = case e of
    ESpawn _ _ -> False
    Edge _     -> True

isSpawnEdge_ :: Edge -> Bool
isSpawnEdge_ = not . isNormalEdge_

-- is this a normal (not spawn) edge?
isNormalEdge :: PGEdge -> Bool
isNormalEdge (_, _, e) = isNormalEdge_ e

isSpawnEdge = not . isNormalEdge

spawnDeps :: PGraph -> PGNode -> [(PGNode, PGEdge)]
spawnDeps gr dst = filter (isSpawnEdge . snd) $ GH.labLPre gr dst

-- equality based on label: same type and same label
eqLabel :: PGNode -> PGNode -> Bool
eqLabel (_, FNode {nLabel = x1}) (_, FNode {nLabel = x2}) = x1 == x2
eqLabel (_, ONode {nLabel = x1}) (_, ONode {nLabel = x2}) = x1 == x2
eqLabel _ _ = False

edgePort :: PGEdge -> NPort
edgePort pedge = case pedge of
    (_, _, ESpawn _ _) -> error "spawn edges do not have ports"
    (_, _, Edge p)     -> p

edgePort_ :: Edge -> NPort
edgePort_ (Edge p) = p
edgePort_ (ESpawn _ _) = error "spawn edges do not have ports"

-- normal (i.e., not spawn) incoming dependencies
edgeDeps :: PGraph -> PGNode -> [(PGNode, PGEdge)]
edgeDeps gr dst = filter (isNormalEdge . snd) $ GH.labLPre gr dst

isSink_ :: PGraph -> DGI.Node -> Bool
isSink_ gr nid = null $ filter (isNormalEdge_ . snd) (DGI.lsuc gr nid)

unconnectedPorts :: PGraph -> PGNode -> [NPort]
unconnectedPorts gr (nid,nlbl) =  S.toList $ S.difference allPorts allEdgePorts
    where allPorts     = S.fromList $ nPorts nlbl
          allEdgePorts = S.fromList $ [edgePort_ e | (_,e) <- DGI.lsuc gr nid,
                                                     isNormalEdge_ e]

isSource_ :: PGraph -> DGI.Node -> Bool
isSource_ gr nid = null $ filter (isNormalEdge_ . snd) (DGI.lpre gr nid)

-- pre (but only considering normal edges)
preNE :: PGraph -> DGI.Node -> [DGI.Node]
preNE gr n = map fst $ lpreNE gr n

lpreNE :: PGraph -> DGI.Node -> [(DGI.Node, Edge)]
lpreNE gr n = map mapFn $ filter filtFn $ DGI.inn gr n
    where filtFn (src,dst,edge) = isNormalEdge_ edge
          mapFn  (src,dst,edge) = case dst == n of
                   True -> (src,edge)
                   False -> error "lpreNE: I'm doing something wrong!"

sucNE :: PGraph -> DGI.Node -> [DGI.Node]
sucNE gr n = map fst $ lsucNE gr n

lsucNE :: PGraph -> DGI.Node -> [(DGI.Node, Edge)]
lsucNE gr n = map mapFn $ filter filtFn $ DGI.out gr n
    where filtFn (src,dst,edge) = isNormalEdge_ edge
          mapFn  (src,dst,edge) = case src == n of
                    True -> (dst,edge)
                    False -> error "lsucNE: I'm doing something wrong!"

lsucPortNE :: PGraph -> DGI.Node -> NPort -> [(DGI.Node, Edge)]
lsucPortNE g nid port = filter filtFn $ lsucNE g nid
    where filtFn (_,e) = (edgePort_ e) == port

sucPortNE :: PGraph -> DGI.Node -> NPort -> [DGI.Node]
sucPortNE gr n p = map fst $ lsucPortNE gr n p

-- normal (i.e., not spawn) sucessor nodes
edgeSucc :: PGraph -> PGNode -> [(PGNode, PGEdge)]
edgeSucc gr pre = filter (isNormalEdge . snd) $ GH.labLSucc gr pre

-- is the node a traget spawn edges
isSpawnTarget :: PGraph -> PGNode -> Bool
isSpawnTarget g n = not $ null $ spawnDeps g n

cleanupGraphWith :: (PGNode -> Bool) -> (PGNode -> Bool) -> PGraph -> PGraph
cleanupGraphWith isSource isSink g
    | null badNodes = g
    | otherwise = cleanupGraphWith isSource isSink g'
    where
        srcs = filter (\(n,_) -> onlySpawnEs n $ DGI.lpre g n) $ DGI.labNodes g
        snks = filter (\(n,_) -> onlySpawnEs n $ DGI.lsuc g n) $ DGI.labNodes g
        badSrcs = filter (not . isSource ) srcs
        badSnks = filter (not . isSink   ) snks
        badNodes = L.nub $ map fst $ badSrcs ++ badSnks
        g' = DGI.delNodes badNodes g
        onlySpawnEs n = all isSpawnE
            where isSpawnE (m,ESpawn {}) = n == m
                  isSpawnE _ = False

-- Remove sources without "source" attribute and sinks without "sink" attribute
cleanupGraph :: PGraph -> PGraph
cleanupGraph = cleanupGraphWith (hasAttr "source" . snd) (hasAttr "sink" . snd)
    where
        hasAttr a n = elem (NAttrCustom a) $ nAttributes n

-- DFS but only considering normal edges
pgDfsNEs :: PGraph -> [PGNode] -> [PGNode]
pgDfsNEs graph start = DFS.xdfsWith getNext getResult start' graph
    where start' = map fst start
          getNext :: PGContext -> [DGI.Node]
          getNext ctx@(_, _, _, outs) = map snd outs'
              where outs' = filter (isNormalEdge_ . fst) outs
          getResult :: PGContext -> PGNode
          getResult ctx@(_, nid, nlbl, _) = (nid,nlbl)
--
-- reversed DFS but only considering normal edges
pgRDfsNEs :: PGraph -> [PGNode] -> [PGNode]
pgRDfsNEs graph start = DFS.xdfsWith getNext getResult start' graph
    where start' = map fst start
          getNext :: PGContext -> [DGI.Node]
          getNext ctx@(ins, _, _, _) = map snd ins'
              where ins' = filter (isNormalEdge_ . fst) ins
          getResult :: PGContext -> PGNode
          getResult ctx@(_, nid, nlbl, _) = (nid,nlbl)

isOnode_ :: Node -> Bool
isOnode_ (ONode {}) = True
isOnode_ _          = False
isOnode             = isOnode_ . snd

isFnode_ :: Node -> Bool
isFnode_ (FNode {}) = True
isFnode_ _          = False
isFnode             = isFnode_ . snd

-- Get the (single) predecssor.
-- If there are more than one predecessors throw an error. Note that F-nodes
-- have, by definition, a single predecessor.
getSinglePre :: PGraph -> PGNode -> PGNode
getSinglePre g n = if len == 1 then (head ps) else error msg
    where len = length ps
          ps = GH.labPre g n
          msg = "expecting single predecessor for node "
                ++ (nLabel $ snd n) ++ " (has: " ++ (show len) ++ ")"


-- Get the (single) predecssor and the port that it connects.
getSinglePrePort :: PGraph -> PGNode -> (PGNode, NPort)
getSinglePrePort gr (nid,nlbl) = (pre,prePort)
    where lpres = DGI.lpre gr nid
          (preNid, preEdge) = case lpres of
                                [(nid,edge)] -> (nid,edge)
                                otherwise -> error msg
          preNlbl = fromJust $ DGI.lab gr preNid
          pre = (preNid, preNlbl)
          prePort = edgePort_  preEdge
          msg = "expecting single predecessor for node " ++ (nLabel nlbl) ++ " instead got:\n" ++ (show $ length lpres)
                ++ (show lpres)
                ++ "\n-------CUT--------------\n"
                ++ (toDot gr)
                ++ "\n------------------------"

-- domination (does not consider spawn edges)
dominates :: PGraph -> (PGNode, NPort) -> PGNode -> Bool
-- F-node
dominates g (src@(srcId,_), p) dst@(_, FNode {})
    | ndeps == 0 = False -- nowhere to go, cannot find a connection to @dst
    | ndeps > 1  = error $ "F-nodes have at most one incoming edge" ++ (nLabel $ snd dst)
    -- ndeps == 1
    | depNodeId == srcId = (dep_port == p) -- same node
    | otherwise = dominates g (src, p) depNode
    where deps = edgeDeps g dst
          ndeps = length deps
          (depNode@(depNodeId,_),dep_edge) = deps !! 0
          dep_port            = edgePort dep_edge
-- O-Node
dominates g (src@(srcId,_), p) dst@(_, ONode { nOperator = op}) = comb_op $ map check_fn deps
    where deps = edgeDeps g dst
          check_fn :: (PGNode, PGEdge) -> Bool
          check_fn (xnode@(xnodeId,_), xedge)
             | srcId == xnodeId && (edgePort xedge) == p = True
             | otherwise = dominates g (src, p) xnode
          comb_op = case op of
                     NOpAnd -> or   -- we only need one to dominate
                     NOpOr  -> and  -- we need all to dominate
                     otherwise -> error "NYI: not sure about the semantics. We might need to map with not"
