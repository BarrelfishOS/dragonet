{-# LANGUAGE ScopedTypeVariables #-}
module Dragonet.Configuration(
    Configuration,

    ConfChange(..),
    foldConfChanges,

    PG.ConfMonad,
    PG.ConfFunction,
    PG.ConfType(..),
    PG.ConfValue(..),

    cvEnumName,

    applyConfig,
    applyConfigInc,

    confMNewNode,
    replaceConfFunctions,
    replaceIncrConfFunctions,
    replaceIncrConf,

    icPartiallyConfigure,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Data.Graph.Inductive as DGI
import qualified Util.GraphHelpers as GH
import qualified Control.Monad.State as ST
import Data.Maybe
import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

type Configuration = [(String,PG.ConfValue)]

{-|
  A class 'ConfChange' provides functionality to apply a changeset of generic
  type 'a' to given configuration and return new configuration.
 -}
class ConfChange a where
    applyConfChange :: Configuration -> a -> Configuration
    --
    -- NB: code below seems too OO, but not sure how to do it better. These are
    -- similar to static methods: the first argument is not used they can be
    -- called as fn undefined::<class_instance>
    --
    -- initial empty configration
    emptyConfig :: a -> Configuration
    -- preety printer
    showConfig  :: a -> Configuration -> String

    -- This can be fed into applyConfigInc
    incrConf :: a -> Configuration

    -- There has to be a better way...
    show :: a -> String

foldConfChanges :: forall a. (ConfChange a) => [a] -> Configuration
foldConfChanges changes = foldl applyConfChange cnf0 changes
    where cnf0 = emptyConfig (undefined::a)

-- Get enumerator name from conf value and its type
cvEnumName :: PG.ConfType -> PG.ConfValue -> String
cvEnumName (PG.CTEnum enums) (PG.CVEnum v) = enums !! v

-- Create new node
confMNewNode :: PG.Node -> PG.ConfMonad PG.PGNode
confMNewNode n = do
    (m,ns) <- ST.get
    let i = m + 1
    let node = (i,n)
    ST.put (i,node:ns)
    return node

-- Add edges and nodes from configuration function to graph
confMRun :: PG.PGraph -> PG.ConfMonad [PG.PGEdge] -> (PG.PGraph, [DGI.Node])
confMRun g m = (addEdges $ addNodes g, map fst newNodes)
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
doApplyCNode :: Bool -> PG.ConfValue -> PG.PGraph -> PG.PGContext
             -> (PG.PGraph, [DGI.Node])
doApplyCNode isIncremental conf g ctx@(_,nid,node,_) = confMRun g' newEM
    where
        -- get NIC-specific configuration function
        fun = case isIncremental of
                    False -> PG.nConfFunction node
                    True  -> PG.nIncrConfFunction node

        -- remove the C-node
        -- NB: The incremental versions are responsible for re-inserting it
        g' = DGI.delNode nid g

        -- Set of incoming and outgoing neighbors
        inE = map lblPair $ DGI.lpre g nid
        outE = map lblPair $ DGI.lsuc g nid
        -- Get new edges (in conf monad)
        newEM :: PG.ConfMonad [PG.PGEdge]
        newEM = fun g (nid,node) inE outE conf

        lblPair (m,a) = ((m,fromJust $ DGI.lab g m), a)

-- Apply configuration for a single CNode
applyCNode_ :: Bool -> Configuration
            -> (PG.PGraph, [DGI.Node])
            -> PG.PGContext
            -> (PG.PGraph, [DGI.Node])
applyCNode_ isIncremental config (g,nodes) ctx@(_,nid,node,_) = ret
    where
        conf = lookup (PG.nLabel node) config
        ret = case conf of
            Just x -> let (g', nodes') = doApplyCNode isIncremental x g ctx
                      in  (g', nodes ++ nodes')
            --Nothing -> error ("ERROR: Configuration node "  ++ (ppShow config)
            --        ++ " was not found.\n"
            --        ++ "   Treating this as an error!!!")
            Nothing -> (g, nodes)

-- Apply specified configuration to graph
applyConfig_ :: Bool -> Configuration -> PG.PGraph -> (PG.PGraph, [DGI.Node])
applyConfig_ isIncremental cfg g =
    foldl (applyCNode_ isIncremental cfg) (g, []) configNodes
    where
        isConfigNode (_,_,PG.CNode {},_) = True
        isConfigNode _ = False
        configNodes = GH.filterCtx isConfigNode g

applyConfig :: Configuration -> PG.PGraph -> PG.PGraph
applyConfig c g = fst $ applyConfig_ False c g

applyConfigInc :: Configuration -> PG.PGraph -> (PG.PGraph, [DGI.Node])
applyConfigInc = applyConfig_ True


-- Add/replace config functions in graph. Calls passed function to get a config
-- function for each CNode.
replaceConfFunctions :: (PG.Node -> PG.ConfFunction) -> PG.PGraph -> PG.PGraph
replaceConfFunctions m = DGI.nmap fixNode
    where
        fixNode n@PG.CNode {} = n { PG.nConfFunction = m n }
        fixNode n = n

-- ditto for incremental functions
replaceIncrConfFunctions :: (PG.Node -> PG.ConfFunction) -> PG.PGraph -> PG.PGraph
replaceIncrConfFunctions m = DGI.nmap fixNode
    where
        fixNode n@PG.CNode {} = n { PG.nIncrConfFunction = m n }
        fixNode n = n

-- this also replaces the incremental counter
replaceIncrConf :: (PG.Node -> Int) -- get counter
                -> (PG.Node -> PG.ConfFunction)  -- get function
                -> PG.PGraph
                -> PG.PGraph
replaceIncrConf cFn mFn = DGI.nmap fixNode
    where
        fixNode n@PG.CNode {} = n { PG.nIncrConfFunction = mFn n
                                  , PG.nIncrCounter = cFn n}
        fixNode n = n

--
-- Incremental configuration
--

type UGraph = PG.PGraph -- unconfigured graph
type CGraph = PG.PGraph -- configured graph

-- The purpose of incremental configuration is to avoid recomputing the
-- information on a graph when we search for an optimal configuration. The
-- information we currently compute is flow maps, but this might change.
--
-- In the traditional configuration scheme we need to recompute everything from
-- scratch, because we always compute the configured graph from the fully
-- unconfigured graph, so any information from previous steps is lost.
--
-- Incremental configuration supports two actions:
-- . applying a partial configuration (represented as a configuration change)
-- . finalizing the graph
--
-- With incremental configuration we allow for partially configured graphs
-- (i.e., some configuration changes have been applied, but configuration nodes
-- still exist) which allows to keep information from the previous steps.
--
-- Hence, contrarily to a full configuration, A partial configuration does _not_
-- remove the C-node. It adds a sub-graph as a number of new nodes and edges.
-- Similarly to the traditional  configuration invariants:
--  . all new edges must have either:
--     . a source and destination in the new nodes (internal edge)
--     . a source node from the predecessors of the configuration node
--       a destination node in the new nodes (in edge)
--     . a source node in the new nodes
--       a destination node that is either the c-node or a successor of the
--       c-node (out edge)
--
-- One way to view this is to consider configuration nodes as boundaries, beyond
-- whcih computing predicate information is impossible. A partial configuration
-- pushes the boundary further. After the partial configuration is done, we can
-- try to compute more things beyond the boundary
--

-- These functions return the new node frontier. The caller will typically start
-- a new search starting from these nodes to update its state. If this is too
-- diffucult to provide, we can always just remove the argument, and let the
-- caller figure it out.

-- partialy configuration:
--  we return the new un-configured graph and the new node frontier
icPartiallyConfigure :: (ConfChange cc) => UGraph -> cc -> (UGraph, [DGI.Node])
icPartiallyConfigure g cc = applyConfigInc (incrConf cc) g

--applyCNodeInc :: (ConfChange cc) =>
-- similar to applyCnode and doApplyCnode for non-incremental configuration
