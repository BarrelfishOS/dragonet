module Dragonet.Implementation.Algorithm (
    execute, executeNodes
) where

import Dragonet.ProtocolGraph
import Dragonet.Implementation

import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive as DGI

import Util.Misc
import Debug.Trace as T
import Control.Monad.State
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M



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
                -> [(DGI.Node,ContextID,(PGNode Implementation,Port))]
                -> SimState -> SimState
executeNodes _ [] _ ss = ss
executeNodes g ((n,l):ns) ret ss =
        executeNodes g ns ret' ss'
    where
    retN = filter ((== n) . fst3) ret
    contexts = L.nub $ map snd3 retN
    (ret',ss') = foldl execNodeCtx (ret,ss) contexts

    -- Nodes enabled by port p on current node
    nextnodes p = filter ((== p) . snd) $ DGI.lsuc g n

    lSubset a b = all (`elem` b) a

    execNodeCtx (inRet,inSS) ctx =
            if not $ null inNodes then (outRet, outSS') else (inRet, inSS')
        where
            inSS' = inSS { ssCurCtx = ctx }

            -- "Input values" to this node for this context
            inPValues = [(x,y) | (_,x,y) <- retN, x == ctx]
            inValues = map (snd . snd) inPValues
            inNodes = map (fst . fst . snd) inPValues

            -- Check if all predecessor nodes were enabled (for O-Nodes)
            depsMet = DGI.pre g n `lSubset` L.nub inNodes

            -- Enabled port for each active context
            enabledPorts = [(ctx,outport)] ++ M.toList (ssForked outSS)
            -- List of enabled successors for specified port/successors
            cpRets (c,p) = map (\(n',p') -> (n',c,((n,l),p'))) $ nextnodes p
            -- Enable successors connected to outport
            outRet = inRet ++ concatMap cpRets enabledPorts

            -- Execute implementation for this node
            (outport,outSS) = executeNode
            executeNode
                | nIsFNode l = runState (fromJust $ nImplementation l) inSS'
                | nIsONode l = executeOpNode
                | otherwise = error "CNodes not supported while executing graph"
            outSS' = outSS { ssForked = M.empty }
        
            -- Implementation 
            executeOpNode
                | op == OpAnd =
                    ((if L.any (== "false") inValues then "false"
                        else if depsMet then "true" else ""), inSS')
                | op == OpOr =
                    ((if L.any (== "true") inValues then "true"
                        else if depsMet then "false" else ""), inSS')
                | otherwise = undefined
                where (ONode op) = nPersonality l


execute :: PGraph Implementation -> Packet -> GlobalState -> GlobalState
execute graph packet gst = ssGState ss'
    where
        n = fromJust $ GH.findNodeByL ((== "SoftwareRX") . nLabel) graph
        ss = initSimState gst packet
        ss' = executeNodes graph (GH.topsortLN graph) [(fst n,0,(n,"in"))] ss

