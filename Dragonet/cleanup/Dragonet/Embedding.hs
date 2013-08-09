module Dragonet.Embedding(
    fullEmbedding,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Util.GraphHelpers as GH

import qualified Data.Graph.Inductive as DGI
import qualified Data.Graph.Inductive.Query.DFS as DGIDFS

import Data.Maybe
import qualified Control.Monad as M
import qualified Data.Set as S
import qualified Data.List as L

import qualified Debug.Trace as TR


subsetLBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
subsetLBy f a b = all (\x -> any (f x) b) a

embMatch lA lB = PG.nLabel lA == PG.nLabel lB

-- Test if node l is already embedded in graph g
isEmbeddedIn l g = isJust $ GH.findNodeByL (embMatch l) g
convertL2LN l = GH.findNodeByL (embMatch l)
convertN2LN n gFr gTo = do { l <- DGI.lab gFr n ; convertL2LN l gTo }
convertEdge gFr gTo (a,b,l) = do
    (na,_) <- convN a ; (nb,_) <- convN b
    return (na,nb,l)
    where convN n = convertN2LN n gFr gTo
convertEdges gFr gTo es = mapM (convertEdge gFr gTo) es
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
    prgNodes = DGI.labNodes prg'

    deps (n,l) g = map (fromJust . (GH.labelNode g)) $ DGI.pre g n
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





fullEmbedding :: PG.PGraph i -> PG.PGraph i -> PG.PGraph i
fullEmbedding prg lpg = foldl1 (GH.mergeGraphsBy (==)) embeddings
    where
        -- String labels of all queue nodes in PRG
        queues = filter (L.isPrefixOf "Queue") $ map (PG.nLabel . snd) $
                    DGI.labNodes prg

        -- Get LPG for specified queue (tag nodes and rename "Queue")
        lpgQ q = DGI.nmap (lpgQNode q) lpg

        -- Relabel node to q if its label is "Queue", and tag the node with q
        lpgQNode q n = if lbl == "Queue" then n' { PG.nLabel = q } else n'
            where
                n' = n { PG.nTag = q }
                lbl = PG.nLabel n

        embeddings = map (\q -> queueEmbedding prg $ lpgQ q) queues

