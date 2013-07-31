module Dragonet.Configuration(
    applyConfig
) where

import Dragonet.ProtocolGraph
import qualified Data.Graph.Inductive as DGI
import qualified Util.GraphHelpers as GH
import qualified Control.Monad.State as ST
import Data.Maybe
import qualified Debug.Trace as DT


-- Create new node
confMNewNode :: Node i -> ConfMonad i (PGNode i)
confMNewNode n = do
    (m,ns) <- ST.get
    let id = m + 1
    let node = (id,n)
    ST.put (id,node:ns)
    return node

-- Add edges and nodes from configuration function to graph
confMRun :: PGraph i -> ConfMonad i [PGEdge] -> PGraph i
confMRun g m = addEdges $ addNodes g
    where
        (_,maxId) = DGI.nodeRange g
        startSt = (maxId,[])

        (newEdges,endSt) = ST.runState m startSt
        (_,newNodes) = endSt

        -- Add new nodes to graph
        addNodes g' = foldl (flip DGI.insNode) g' newNodes
        -- Add new edges
        addEdges g' = foldl (flip DGI.insEdge) g' newEdges


-- Apply configuration for a single CNode
applyCNode :: [(String,String)] -> PGraph i -> PGContext i -> PGraph i
applyCNode config g ctx = confMRun g' newEM
    where
        node = DGI.lab' ctx
        fun = nConfFun node
        conf = fromJust $ lookup (nLabel node) config

        -- Remove configuration node
        g' = DGI.delNode (DGI.node' ctx) g

        -- Set of incoming and outgoing neighbors
        n = DGI.node' ctx
        inE = map lblPair $ DGI.lpre g n
        outE = map lblPair $ DGI.lsuc g n
        -- Get new edges (in conf monad)
        newEM = fun node inE outE conf

        lblPair (n,a) = ((n,fromJust $ DGI.lab g n), a)


-- Apply specified configuration to graph
applyConfig :: [(String,String)] -> PGraph i -> PGraph i
applyConfig cfg g =
    foldl (applyCNode cfg) g configNodes
    where
        isConfigNode ctx = nIsCNode $ DGI.lab' ctx
        configNodes = GH.filterCtx isConfigNode g

