-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

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

    confMNewNode,
    replaceConfFunctions,
    replaceIncrConfFunctions,
    replaceIncrConf,

    icPartiallyConfigure,
    icReplace,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Data.Graph.Inductive as DGI
import qualified Util.GraphHelpers as GH
import qualified Control.Monad.State as ST
import qualified Data.List as L
import Data.Maybe
import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

tr a b  = trace b a
trN a b = a

type Configuration = [(String,PG.ConfValue)]

{-|
  A class 'ConfChange' provides functionality to apply a changeset of generic
  type 'a' to given configuration and return new configuration.
 -}
class (Ord a, Show a) => ConfChange a where
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

    -- There has to be a better way...
    -- It seems there is... (see above)
    show :: a -> String

    -- Incremental configuration
    -- This can be fed into applyConfigInc
    incrConf :: a -> Configuration

    -- replace (we might want a second class for that)
    ccIsReplace :: a -> Bool
    ccIsReplace _ = False
    -- this is a replace configuration change: it replaces the nodes created
    -- from a previous configuration change
    ccReplace :: a -> [(DGI.Node,PG.Node)] -> [Maybe PG.Node]
    ccReplace _ _ = []
    --
    ccReplacedCc :: a -> Maybe a
    ccReplacedCc _ = Nothing
    --
    ccReplaceNewCc :: a -> a
    ccReplaceNewCc  = error "ccReplaceNewCc: default method"

    ccShow :: a -> String
    ccShow = Prelude.show


foldConfChanges :: forall a. (ConfChange a) => [a] -> Configuration
foldConfChanges changes = trN ret ("foldConfChanges:" ++ ppShow changes)
    where cnf0 = emptyConfig (undefined::a)
          -- NB: reverse is to deal with replace ccs
          ret = foldl applyConfChange cnf0 (L.reverse changes)

-- Get enumerator name from conf value and its type
cvEnumName :: PG.ConfType -> PG.ConfValue -> String
cvEnumName (PG.CTEnum enums) (PG.CVEnum v) = enums !! v

-- Create new node
confMNewNode :: PG.Node -> PG.ConfMonad PG.PGNode
confMNewNode n = do
    cnfSt <- ST.get
    let lastNid  = PG.csLastNid cnfSt
        newNodes = PG.csNewNodes cnfSt
    let i = (PG.csLastNid cnfSt) + 1
    let node = (i,n)
    ST.modify $ \s -> s { PG.csLastNid = i, PG.csNewNodes = node:newNodes }
    return node

-- Add edges and nodes from configuration function to graph
confMRun :: PG.PGraph -> PG.ConfMonad [PG.PGEdge] -> (PG.PGraph, PG.ConfState)
confMRun g m = (addEdges $ addNodes g, endSt)
    where
        (_,maxId) = DGI.nodeRange g
        startSt = PG.initConfState maxId

        (newEdges,endSt) = ST.runState m startSt
        newNodes = PG.csNewNodes endSt

        -- Add new nodes to graph
        addNodes g' = foldl (flip DGI.insNode) g' newNodes
        -- Add new edges
        addEdges g' = foldl (flip DGI.insEdge) g' newEdges

-- Apply configuration for a single CNode
doApplyCNode :: Bool -> PG.ConfValue -> PG.PGraph -> PG.PGContext
             -> (PG.PGraph, PG.ConfState)
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
            -> (PG.PGraph, PG.ConfState)
            -> PG.PGContext
            -> (PG.PGraph, PG.ConfState)
applyCNode_ isIncremental config (g,st) ctx@(_,nid,node,_) = ret
    where
        conf = lookup (PG.nLabel node) config
        ret = case conf of
            -- TODO: use foldM
            Just x -> let (g', st') = doApplyCNode isIncremental x g ctx
                      in  (g', PG.csFold st st')
            --Nothing -> error ("ERROR: Configuration node "  ++ (ppShow config)
            --        ++ " was not found.\n"
            --        ++ "   Treating this as an error!!!")
            Nothing -> (g, st)

-- Apply specified configuration to graph
applyConfig_ :: Bool -> Configuration -> PG.PGraph -> (PG.PGraph, PG.ConfState)
applyConfig_ isIncremental cfg g =
    foldl (applyCNode_ isIncremental cfg) (g, cs0) configNodes
    where
        cs0 = PG.initConfState (snd $ DGI.nodeRange g)
        isConfigNode (_,_,PG.CNode {},_) = True
        isConfigNode _ = False
        configNodes = GH.filterCtx isConfigNode g

applyConfig :: Configuration -> PG.PGraph -> PG.PGraph
applyConfig c g = fst $ applyConfig_ False c g

applyConfigInc :: Configuration -> PG.PGraph -> (PG.PGraph, PG.ConfState)
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

-- | partially configure a node.
-- function returns the new partially configured graph and the new node frontier
icPartiallyConfigure :: (ConfChange cc) => UGraph -> cc -> (UGraph, PG.ConfState)
icPartiallyConfigure g cc = case ccIsReplace cc of
                            False -> applyConfigInc (incrConf cc) g
                            True  -> error "Call icReplace"

-- NOTES:
--  We might want to have a mecnahism to disable the "removed" (to be replaced)
--  filters. This ensures that packets of flows not considered are going to be
--  delivered as expected (e.g., in queue 0). FlowMap should be able to deal
--  with this incrementally.
--
--  Taking this to extreme, we can unfold the *whole* graph and have each node
--  "disabled" and perform incremental operations there. Obviously, there is a
--  tradeoff there in terms of size of the graph -- and what part of it is
--  actually used.
icReplace :: (ConfChange cc)
          => UGraph -> (cc, [(DGI.Node,PG.Node)])
          -> (UGraph, [(DGI.Node,(PG.Node,PG.Node))], [(DGI.Node, PG.Node)])
icReplace g (cc,nodes) = (g', modNodes, newNodes)
    where replacements :: [Maybe PG.Node]
          replacements = ccReplace cc nodes
          newNodes = [ case mlbl' of
                         Nothing    -> (nid, nlbl)
                         Just nlbl' -> (nid, nlbl')
                       | ((nid,nlbl), mlbl') <- zip nodes replacements]
          modNodes = catMaybes $
                     [ case mlbl' of
                         Nothing   -> Nothing
                         Just nlbl' -> Just (nid,(nlbl,nlbl'))
                       | ((nid,nlbl), mlbl') <- zip nodes replacements]
          g' = DGI.gmap mapF g
          mapF ctx@(a1,nid,nlbl,a2) =
                case L.lookup nid modNodes of
                        Nothing -> ctx
                        -- TODO: verify that oldL == nlbl
                        Just (oldL, newL) -> (a1,nid,newL,a2)
