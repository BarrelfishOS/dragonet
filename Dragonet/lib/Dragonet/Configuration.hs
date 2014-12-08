{-# LANGUAGE ScopedTypeVariables #-}
module Dragonet.Configuration(
    Configuration,

    ConfChange(..),
    foldConfChanges,

    ConfType(..),
    ConfValue(..),
    ConfFunction,
    ConfMonad,

    cvEnumName,

    applyConfig,
    confMNewNode,
    replaceConfFunctions
) where

import Dragonet.ProtocolGraph
import qualified Data.Graph.Inductive as DGI
import qualified Util.GraphHelpers as GH
import qualified Control.Monad.State as ST
import Data.Maybe

type Configuration = [(String,ConfValue)]

{-|
  A class 'ConfChange' provides functionality to apply a changeset of generic
  type 'a' to given configuration and return new configuration.
 -}
class ConfChange a where
    applyConfChange :: Configuration -> a -> Configuration
    --
    -- NB: code below seems too OO, but not sure how to do it better These are
    -- similar to static methods: the first argument is not used they can be
    -- called as foo undefined::<class_instance>
    --
    -- initial empty configration
    emptyConfig :: a -> Configuration
    -- preety printer
    showConfig  :: a -> Configuration -> String

foldConfChanges :: forall a. (ConfChange a) => [a] -> Configuration
foldConfChanges changes = foldl applyConfChange cnf0 changes
    where cnf0 = emptyConfig (undefined::a)

-- Get enumerator name from conf value and its type
cvEnumName :: ConfType -> ConfValue -> String
cvEnumName (CTEnum enums) (CVEnum v) = enums !! v

-- Create new node
confMNewNode :: Node -> ConfMonad PGNode
confMNewNode n = do
    (m,ns) <- ST.get
    let i = m + 1
    let node = (i,n)
    ST.put (i,node:ns)
    return node

-- Add edges and nodes from configuration function to graph
confMRun :: PGraph -> ConfMonad [PGEdge] -> PGraph
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
doApplyCNode :: ConfValue -> PGraph -> PGContext -> PGraph
doApplyCNode conf g ctx@(_,nid,node,_) = confMRun g' newEM
    where
        node = DGI.lab' ctx
        fun = nConfFunction node

        -- Remove configuration node
        g' = DGI.delNode (DGI.node' ctx) g

        -- Set of incoming and outgoing neighbors
        n = DGI.node' ctx
        inE = map lblPair $ DGI.lpre g n
        outE = map lblPair $ DGI.lsuc g n
        -- Get new edges (in conf monad)
        newEM = fun g (nid,node) inE outE conf

        lblPair (m,a) = ((m,fromJust $ DGI.lab g m), a)

-- Apply configuration for a single CNode
applyCNode :: Configuration -> PGraph -> PGContext -> PGraph
applyCNode config g ctx@(_,nid,node,_) = ret
    where
        conf = lookup (nLabel node) config
        ret = case conf of
            Just x -> doApplyCNode x g ctx
            Nothing -> g


-- Apply specified configuration to graph
applyConfig :: Configuration -> PGraph -> PGraph
applyConfig cfg g =
    foldl (applyCNode cfg) g configNodes
    where
        isConfigNode (_,_,CNode {},_) = True
        isConfigNode _ = False
        configNodes = GH.filterCtx isConfigNode g

-- Add/replace config functions in graph. Calls passed function to get a config
-- function for each CNode.
replaceConfFunctions :: (Node -> ConfFunction) -> PGraph -> PGraph
replaceConfFunctions m = DGI.nmap fixNode
    where
        fixNode n@CNode {} = n { nConfFunction = m n }
        fixNode n = n

