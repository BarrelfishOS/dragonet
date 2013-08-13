module Dragonet.Embedding(
    fullEmbedding,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Util.GraphHelpers as GH

import qualified Data.Graph.Inductive as DGI
import qualified Data.Graph.Inductive.Query.DFS as DGIDFS

import Data.Maybe
import qualified Control.Monad as M
import qualified Data.List as L


subsetLBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
subsetLBy f a b = all (\x -> any (f x) b) a

embMatch :: PG.Node i -> PG.Node i -> Bool
embMatch lA lB = PG.nLabel lA == PG.nLabel lB

-- Test if node l is already embedded in graph g
isEmbeddedIn :: PG.Node i -> PG.PGraph i -> Bool
isEmbeddedIn l g = isJust $ GH.findNodeByL (embMatch l) g

convertL2LN :: PG.Node i -> PG.PGraph i -> Maybe (DGI.LNode (PG.Node i))
convertL2LN l = GH.findNodeByL (embMatch l)

convertN2LN :: DGI.Node -> PG.PGraph i -> PG.PGraph i
                    -> Maybe (DGI.LNode (PG.Node i))
convertN2LN n gFr gTo = do { l <- DGI.lab gFr n ; convertL2LN l gTo }

convertEdge :: PG.PGraph i -> PG.PGraph i -> DGI.LEdge PG.Port
                -> Maybe (DGI.LEdge PG.Port)
convertEdge gFr gTo (a,b,l) = do
    (na,_) <- convN a ; (nb,_) <- convN b
    return (na,nb,l)
    where convN n = convertN2LN n gFr gTo

convertEdges :: PG.PGraph i -> PG.PGraph i -> [DGI.LEdge PG.Port]
                    -> Maybe [DGI.LEdge PG.Port]
convertEdges gFr gTo = mapM (convertEdge gFr gTo)

convInsEdges :: PG.PGraph i -> PG.PGraph i -> [DGI.LEdge PG.Port]
                    -> Maybe (PG.PGraph i)
convInsEdges gFr gTo es = do
    edges <- convertEdges gFr gTo es
    return $ DGI.insEdges edges gTo




-- The actual embedding algorithm for one queue
queueEmbedding :: PG.PGraph i -> PG.PGraph i -> PG.PGraph i
queueEmbedding prg' lpg = embeddingStep DGI.empty lpgNodes
    where
    -- Make sure that we can just add nodes from PRG to LPG without conflicts
    prg = GH.elliminateConflicts prg' lpg

    lpgNodes = map (fromJust . GH.labelNode lpg) $ DGIDFS.topsort lpg

    deps (n,_) g = map (fromJust . GH.labelNode g) $ DGI.pre g n
    lpgDeps n = deps n lpg
    prgDeps n = deps n prg

    embeddingStep emb uem
        | null uem = emb
        | otherwise = embeddingStep emb' uem'
        where
            uem' = L.delete v uem

            embNL = DGI.labNodes emb
            depsSat v' = subsetLBy (\(_,a) (_,b) -> embMatch a b) (lpgDeps v') embNL

            -- Find a node whose LPG dependencies are already embedded
            v = fromJust $ L.find depsSat uem
            (vN,vL) = v

            -- Use PRG embedding unless it is Nothing
            emb' = fromMaybe lpgEmb prgEmb


            -- Try to build a PRG
            prgEmb = do
                pv <- convertL2LN vL prg -- Get corresponding PRG Node (if any)
                pullPRGNodes emb pv      -- Recursively pull in nodes

            -- Add Nodes from PRG to graph and generate edge list
            pullPRGNodes g ln
                | l `isEmbeddedIn` g = Just g -- Node is already embedded
                | otherwise = do
                    -- Add node
                    let withNode = DGI.insNode ln g
                    -- Recursively add other nodes
                    withRec <- M.foldM pullPRGNodes withNode $ prgDeps ln
                    -- Calculate and add incoming edges
                    let newEdges = DGI.inn prg n
                    convInsEdges prg withRec newEdges
                where (n,l) = ln

            -- The LPG embedding is trivial, just add the node and the incoming
            -- edges
            lpgEmb = fromJust $ convInsEdges lpg g $ DGI.inn lpg vN
                where g = DGI.insNode v emb


-- Get list of queue nodes in graph
queues ::  PG.PGraph i -> [PG.PGNode i]
queues g = filter (L.isPrefixOf "Queue" . PG.nLabel . snd) $ DGI.labNodes g

-- Find SW-Entry node for specified queue
findSWE :: String -> PG.PGraph i -> Maybe (DGI.Node,PG.Port)
findSWE q g = do
    (n,_) <- GH.findNodeByL isSWE g
    return (n,"out")
    where
        --isSWE n = (PG.nLabel n == "SoftwareEntry") && (PG.nTag n == q)
        isSWE n = PG.nLabel n == q

-- Serialize graph (introduces SoftwareEntry node for each queue, and fixes
-- edges crossing from PRG-Nodes to LPG-Nodes)
serialize :: PG.PGraph i -> PG.PGraph i
serialize g =
    GH.delDupEdges $ DGI.insEdges fixedEdges $ GH.delLEdges crossingEdges g
    where
        -- List of queue names
        qls = map (PG.nLabel . snd) $ queues g
        -- Generate association tuple for specified queue (q,(node,port))
        genSWE q = (q, fromMaybe err $ findSWE q g)
            where err = error "serialize: SoftwareEntry node not found in LPG"
        -- Association list from queue names to sw entry nodes/ports
        swEMap = map genSWE qls
        -- Map node to respective SW entry node
        swEN n = fromJust $ lookup tag swEMap
            where tag = PG.nTag $ fromJust $ DGI.lab g n

        -- List of edges crossing from HW to SW nodes
        crossingEdges = filter isCrossingEdge $ DGI.labEdges g
        isCrossingEdge (a,b,_) = not (sw a) && sw b
            where sw n = PG.nIsSoftware $ fromJust $ DGI.lab g n

        -- List of fixed edges
        fixedEdges = concatMap fixEdge crossingEdges
        --fixEdge (a,b,l) = [(a,swn,l), (swn,b,swp)]
        fixEdge (a,b,l) = if b /= swn then [(swn,b,swp)] else [(a,b,l)]
            where (swn,swp) = swEN b

-- Get LPG for specified queue (tag nodes and rename "Queue")
lpgQ :: PG.PGNode i -> PG.PGraph i -> PG.PGraph i
lpgQ q = DGI.nmap (lpgQNode q)

-- Relabel node to q if its label is "Queue", and tag the node with q
lpgQNode :: PG.PGNode i -> PG.Node i -> PG.Node i
lpgQNode (_,qn) n = if lbl == "Queue" then n' { PG.nLabel = ql } else n'
    where
        ql = PG.nLabel qn
        n' = n { PG.nTag = ql }
        lbl = PG.nLabel n

-- Get prg for a particular queue (basically go recursively and get all
-- nodes reachable from the queue node. This is necessary to avoid embedding
-- functionality that is not available on a particular queue.
-- We also need to tag software nodes, so they are introduced once for each
-- queue.
prgQ :: PG.PGNode i -> PG.PGraph i -> PG.PGraph i
prgQ (qn,ql) prg = DGI.nmap tagSWN g'
    where
        g' = GH.reduceNodes prg $ DGIDFS.reachable qn $ DGI.grev prg
        qlab = PG.nLabel ql
        tagSWN n = if PG.nIsSoftware n then n { PG.nTag = qlab } else n

fullEmbedding :: PG.PGraph i -> PG.PGraph i -> PG.PGraph i
fullEmbedding prg lpg = serialized
    where
        embedQueue q = queueEmbedding (prgQ q prg) (lpgQ q lpg)
        embeddings = map embedQueue $ queues prg

        merged = foldl1 (GH.mergeGraphsBy (==)) embeddings
        serialized = serialize merged

