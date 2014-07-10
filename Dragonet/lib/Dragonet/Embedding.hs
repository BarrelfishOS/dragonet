module Dragonet.Embedding(
    fullEmbedding,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Util.GraphHelpers as GH
import qualified Util.Misc as UM

import qualified Data.Graph.Inductive as DGI
import qualified Data.Graph.Inductive.Query.DFS as DGIDFS

import Data.Maybe
import qualified Control.Monad as M
import qualified Data.List as L


subsetLBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
subsetLBy f a b = all (\x -> any (f x) b) a

embMatch :: PG.Node -> PG.Node -> Bool
embMatch lA lB = PG.nLabel lA == PG.nLabel lB

-- Test if node l is already embedded in graph g
isEmbeddedIn :: PG.Node -> PG.PGraph -> Bool
isEmbeddedIn l g = isJust $ GH.findNodeByL (embMatch l) g

convertL2LN :: PG.Node -> PG.PGraph -> Maybe (DGI.LNode PG.Node)
convertL2LN l = GH.findNodeByL (embMatch l)

convertN2LN :: DGI.Node -> PG.PGraph -> PG.PGraph
                    -> Maybe (DGI.LNode PG.Node)
convertN2LN n gFr gTo = do { l <- DGI.lab gFr n ; convertL2LN l gTo }

convertEdge :: PG.PGraph -> PG.PGraph -> DGI.LEdge PG.Port
                -> Maybe (DGI.LEdge PG.Port)
convertEdge gFr gTo (a,b,l) = do
    (na,_) <- convN a ; (nb,_) <- convN b
    return (na,nb,l)
    where convN n = convertN2LN n gFr gTo

convertEdges :: PG.PGraph -> PG.PGraph -> [DGI.LEdge PG.Port]
                    -> Maybe [DGI.LEdge PG.Port]
convertEdges gFr gTo = mapM (convertEdge gFr gTo)

convInsEdges :: PG.PGraph -> PG.PGraph -> [DGI.LEdge PG.Port]
                    -> Maybe PG.PGraph 
convInsEdges gFr gTo es = do
    edges <- convertEdges gFr gTo es
    return $ DGI.insEdges edges gTo




-- The actual embedding algorithm for one queue
queueEmbedding :: PG.PGraph -> PG.PGraph -> PG.PGraph
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
            (Just v) = L.find depsSat uem
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
            (Just lpgEmb) = convInsEdges lpg g $ DGI.inn lpg vN
                where g = DGI.insNode v emb


-- Get list of queue nodes in graph
queues ::  PG.PGraph -> [PG.PGNode]
queues g = filter isQueue $ DGI.labNodes g
    where
        isQueue (_,PG.Node { PG.nLabel = l }) =
            "Queue" `L.isPrefixOf` l ||
                "RxQueue" `L.isPrefixOf` l
-- Find SW-Entry node for specified queue
findSWE :: String -> PG.PGraph -> Maybe (DGI.Node,PG.Port)
findSWE q g = do
    (n,_) <- GH.findNodeByL isSWE g
    return (n,"out")
    where
        --isSWE n = (PG.nLabel n == "SoftwareEntry") && (PG.nTag n == q)
        isSWE n = PG.nLabel n == q

-- Serialize graph (introduces SoftwareEntry node for each queue, and fixes
-- edges crossing from PRG-Nodes to LPG-Nodes)
serialize :: PG.PGraph -> PG.PGraph
serialize g =
    GH.delDupEdges $ DGI.insEdges fixedEdges $ GH.delLEdges crossingEdges g
    where
        -- List of queue names
        qls1 = map (PG.nLabel . snd) $ queues g
        qls = if qls1 /= [] then qls1 -- error ("qls list is " ++ show qls1 )
                else error "qls list is empty!"
        -- Generate association tuple for specified queue (q,(node,port))
        genSWE q = (q, fromMaybe err $ findSWE q g)
            where err = error "serialize: SoftwareEntry node not found in LPG"
        -- Association list from queue names to sw entry nodes/ports
        swEMap = map genSWE qls
        -- Map node to respective SW entry node
        swEN n = swe
            where
                (Just tag') = DGI.lab g n
                tag = PG.nTag tag'
                ans = lookup tag swEMap
                (Just swe) = if ans == Nothing then
                    error ("failed lookup tag " ++ (show tag)
                        ++ ", swEMap " ++ (show swEMap) ++
                        ", Most probably, queue-name missing in LPG" )
                    else ans

        -- List of edges crossing from HW to SW nodes
        crossingEdges = filter isCrossingEdge $ DGI.labEdges g
        isCrossingEdge (a,b,_) = not (sw a) && sw b
            where
                sw n = PG.nIsSoftware l
                    where (Just l) = DGI.lab g n

        -- List of fixed edges
        fixedEdges = concatMap fixEdge crossingEdges
        --fixEdge (a,b,l) = [(a,swn,l), (swn,b,swp)]
        fixEdge (a,b,l) = if b /= swn then [(swn,b,swp)] else [(a,b,l)]
            where (swn,swp) = swEN b

-- Get LPG for specified queue (tag nodes and rename "Queue")
lpgQ :: PG.PGNode -> PG.PGraph -> PG.PGraph
lpgQ q = DGI.nmap (lpgQNode q)

-- Relabel node to q if its label is "Queue", and tag the node with q
lpgQNode :: PG.PGNode -> PG.Node -> PG.Node
lpgQNode (_,qn) n =
    if lbl == "Queue" || lbl == "RxQueue" then n' { PG.nLabel = ql } else n'
    where
        ql = PG.nLabel qn
        n' = n { PG.nTag = ql }
        lbl = PG.nLabel n

-- Get prg for a particular queue (basically go recursively and get all
-- nodes reachable from the queue node. This is necessary to avoid embedding
-- functionality that is not available on a particular queue.
-- We also need to tag software nodes, so they are introduced once for each
-- queue.
prgQ :: PG.PGNode -> PG.PGraph -> PG.PGraph
prgQ (qn,ql) prg = DGI.nmap tagSWN g'
    where
        prgNodes q = DGIDFS.reachable q $ DGI.grev prg
        allQNodes = concatMap (prgNodes . fst) $ queues prg
        -- TODO: This is really ulgy. We want to avoid losing software nodes that
        -- only have incoming edges. This is currently done by looking at the
        -- nodes reachable from every queue, and adding the nodes that are not
        -- reachable from any node also for each queue. This could be done
        -- cleaner by taking the graph for each queue, and prune away the nodes
        -- only reachable from each of the other queues.
        nonQNodes = (DGI.nodes prg) `UM.minusL` allQNodes
        g' = GH.reduceNodes prg $ (prgNodes qn ++ nonQNodes)
        qlab = PG.nLabel ql
        tagSWN n = if PG.nIsSoftware n then n { PG.nTag = qlab } else n

fullEmbedding :: PG.PGraph -> PG.PGraph -> PG.PGraph
fullEmbedding prg lpg = serialized
    where
        embedQueue q = queueEmbedding (prgQ q prg) (lpgQ q lpg)
        embeddings = map embedQueue $ queues prg

        merged = foldl1 (GH.mergeGraphsBy (==)) embeddings
        serialized = serialize merged

