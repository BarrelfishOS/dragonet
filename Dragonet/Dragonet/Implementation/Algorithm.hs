module Dragonet.Implementation.Algorithm (
    execute, executeNodes
) where

import Dragonet.ProtocolGraph
import Dragonet.Implementation

import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive as DGI

--import Debug.Trace as T
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L



-- Execute graph. First parameter is expected to be a topologically sorted
-- list of the graph nodes, the second list is used to store the enablement
-- indication, so (enabled node, origin, port), and the last argument is the
-- context to start with. Returned is the port name returned by the
-- implementation of the last sink node and the last state.
--
-- Basic idea: use topologically sorted list of nodes, this way all predecessors
-- will have been calculated when arriving at a node (if enabled). The output
-- port taken for a node will be stored in a list, used for implemented AND/OR
-- nodes.
executeNodes :: PGraph Implementation -> [PGNode Implementation]
                -> [(DGI.Node,(PGNode Implementation,Port))]
                -> Context -> Context
executeNodes _ [] _ ctx = ctx
executeNodes g ((n,l):ns) ret ctx =
    --T.trace ("Node " ++ show l) (
        if not $ null inNodes then          -- Node was enabled
            executeNodes g ns ret' ctx'     -- Regular node
        else
            executeNodes g ns ret ctx       -- Skip disabled node
        --)
    where
        inPValues = filter ((== n) . fst) ret
        inValues = map (snd . snd) inPValues
        inNodes = map (fst . fst . snd) inPValues

        -- Check if all predecessor nodes were enabled (for O-Nodes)
        depsMet = DGI.pre g n `lSubset` L.nub inNodes
        lSubset a b = all (`elem` b) a

        successors = DGI.lsuc g n
        nextnodes = filter ((== outport) . snd) $ successors

        -- Enable successors connected to outport
        ret' = ret ++ map (\(n',p) -> (n',((n,l),p))) nextnodes

        -- Execute implementation for this node
        (outport,ctx') = executeNode
        executeNode
            | nIsFNode l = runState (fromJust $ nImplementation l) ctx
            | nIsONode l = executeOpNode
            | otherwise = error "C-Nodes not supported while executing graph"
    
        -- Implementation 
        executeOpNode
            | op == OpAnd =
                ((if L.any (== "false") inValues then "false"
                    else if depsMet then "true" else ""), ctx)
            | op == OpOr =
                ((if L.any (== "true") inValues then "true"
                    else if depsMet then "false" else ""), ctx)
            | otherwise = undefined
            where (ONode op) = nPersonality l


execute :: PGraph Implementation -> Packet -> GlobalState -> GlobalState
execute graph packet gst = ctxState ctx'
    where
        n = fromJust $ GH.findNodeByL ((== "SoftwareRX") . nLabel) graph
        ctx = Context packet M.empty gst
        ctx' = executeNodes graph (GH.topsortLN graph) [(fst n,(n,"in"))] ctx

