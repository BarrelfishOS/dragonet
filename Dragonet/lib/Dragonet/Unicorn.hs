{-# LANGUAGE TemplateHaskell #-}
module Dragonet.Unicorn(
    constructGraph,
    constructGraph',
    strToGraph,
    fileToGraph,
) where

import Dragonet.Unicorn.Parser

import Data.Maybe
import qualified Data.List as L

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Configuration as CFG
import qualified Dragonet.Semantics as Sem
import qualified Data.Graph.Inductive as DGI

import qualified Text.Show.Pretty as Pr


addImplFun :: Maybe String -> PG.Node -> PG.Node
addImplFun Nothing n = n
addImplFun (Just l) n = n { PG.nImplementation = PG.NImplFunction l }

-- empty implementation for now
node_to_pgnode :: Node -> PG.Node
node_to_pgnode Node {
                nName = name,
                nPorts = ports,
                nAttrs = attrs,
                nPortSems = sems,
                nImplFun = mi } =
    node { PG.nSemantics = sems }
    where
        pnames = map pName ports
        node = addImplFun mi $ PG.nAttrsAdd attrs $ PG.baseFNode name pnames
node_to_pgnode Boolean {
                nName  = name,
                nPortT = pt,
                nPortF = pf,
                nAttrs = attrs,
                nPortSems = sems,
                nImplFun = mi } =
    node { PG.nSemantics = sems,
           PG.nAttributes = a }
    where
        pnames = map pName [pt, pf]
        a = PG.NAttrCustom "Boolean":attrs
        node = addImplFun mi $ PG.baseFNode name pnames
node_to_pgnode And {
                nName  = name,
                nPortT = pt,
                nPortF = pf,
                nAttrs = attrs } =
    PG.nAttrsAdd attrs $ PG.baseONode name pnames PG.NOpAnd
    where pnames = map pName [pt, pf]
node_to_pgnode NAnd {
                nName  = name,
                nPortT = pt,
                nPortF = pf,
                nAttrs = attrs } =
    PG.nAttrsAdd attrs $ PG.baseONode name pnames PG.NOpNAnd
    where pnames = map pName [pt, pf]
node_to_pgnode Or {
                nName  = name,
                nPortT = pt,
                nPortF = pf,
                nAttrs = attrs } =
    PG.nAttrsAdd attrs $ PG.baseONode name pnames PG.NOpOr
    where pnames = map pName [pt, pf]
node_to_pgnode NOr {
                nName  = name,
                nPortT = pt,
                nPortF = pf,
                nAttrs = attrs } =
    PG.nAttrsAdd attrs $ PG.baseONode name pnames PG.NOpNOr
    where pnames = map pName [pt, pf]
node_to_pgnode Config {
                nName     = name,
                nPorts    = ports,
                nAttrs    = attrs,
                nConfType = t } =
    PG.nAttrsAdd attrs $ PG.baseCNode name pnames t unicornSimpleConfig
    where pnames = map pName ports

constructGraph :: Graph -> PG.PGraph
constructGraph (Graph { gName = gname, gRootCluster = cluster }) =
        DGI.mkGraph pg_nodes pg_edges
    where
        -- produce a flatten list of nodes from a (hiearchical) cluster
        cl_flat_nodes :: Cluster -> [Node]
        cl_flat_nodes Cluster {
                        cName     = cl_name,
                        cChildren = cl_clusters,
                        cNodes    = cl_nodes } =
            [ n | n <- cl_nodes ] ++ (concatMap cl_flat_nodes cl_clusters)

        nodes_ids :: [(Int, Node)]
        nodes_ids = zip [1..] $ cl_flat_nodes cluster

        pg_nodes :: [PG.PGNode]
        pg_nodes = [(id, node_to_pgnode(node)) | (id, node) <- nodes_ids]

        node_id :: String -> Int
        node_id name = case node of
            Just n  -> (fst n) -- return id
            Nothing -> error ("Node " ++ name ++ " not found")
            where node = L.find (\x -> (nName (snd x)) == name) nodes_ids

        get_edges_port :: (Int, Port) -> [PG.PGEdge]
        get_edges_port (nid, port) = [(nid, (node_id out), PG.Edge pname) |
                                      out <- pOuts port]
            where pname = pName port

        get_edges_spawn :: (Int, Node) -> [PG.PGEdge]
        get_edges_spawn (nid, node) =
            [(nid,  node_id $ sNode s, PG.ESpawn $ sName s) |
                s <- case node of
                    Node { nSpawns = ss' } -> ss'
                    Config { nSpawns = ss' } -> ss'
                    Boolean { nSpawns = ss' } -> ss'
                    _ -> [] ]

        get_edges_node :: (Int, Node) -> [PG.PGEdge]
        get_edges_node n@(nid, node) =
            concatMap get_edges_port x ++ get_edges_spawn n
            where x = [ (nid, port) | port <- nAllPorts node ]

        pg_edges :: [PG.PGEdge]
        pg_edges = concatMap get_edges_node nodes_ids

-- get a graph from a Unicorn string
strToGraph :: String -> PG.PGraph
strToGraph = constructGraph . parseGraph_

-- load a graph from a file
fileToGraph :: FilePath -> IO (PG.PGraph)
fileToGraph fname = fmap strToGraph $ readFile fname

constructGraph' :: Graph -> (PG.PGraph, Sem.Helpers)
constructGraph' g = (constructGraph g, gSemHelpers g)

nodeClusterMap :: Cluster -> [(String, Node)]
nodeClusterMap Cluster {
                cName     = cn,
                cChildren = cs,
                cNodes    = ns } =
    (map (\n -> (cn, n)) ns) ++ (concatMap nodeClusterMap cs)

clusterMap :: Cluster -> [String] -> [(Node,[String])]
clusterMap Cluster {
                cName     = cn,
                cChildren = cs,
                cNodes    = ns } l =
    map (\n -> (n,l')) ns ++ concatMap (\c -> clusterMap c l') cs
    where l' = if null cn then l else (l ++ [cn])


-----------------------------------------------------------------------------
-- Helper functions

unicornSimpleConfig :: PG.ConfFunction
unicornSimpleConfig n inE outE cval =
    return $ concatMap edge inE
    where
        -- Port name to use
        cfg = PG.nConfType n `CFG.cvEnumName` cval
        -- Pick edges that match config, drop spawn edges
        matchingEdge PG.Edge { PG.ePort = p } = p == cfg
        matchingEdge _ = False
        -- Only the out-endpoints that match the configuration
        outN = map fst $ filter (matchingEdge . snd) outE
        -- Remove labels from node in edge
        unlab ((a,_),(b,_),c) = (a,b,c)
        -- Create edge from (n,p) to all out-endpoints
        edge (n,p) = map unlab $ map (\x -> (n,x,p)) $ outN

