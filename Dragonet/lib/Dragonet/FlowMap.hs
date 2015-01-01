{-# LANGUAGE ExistentialQuantification #-}
module Dragonet.FlowMap (
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Configuration as C

import qualified Data.Graph.Inductive   as DGI
import qualified Data.Map.Strict        as M
import qualified Control.Monad.State    as ST

import Dragonet.Flows (Flow)

type FlowMap = M.Map DGI.Node [[Flow]] -- flow partition (one set of flows per port)

data FlowMapSt cc = (C.ConfChange cc) => FlowMapSt {
      fmGraph      :: PG.PGraph -- graph
    , fmCnfChanges :: [cc]      -- configuration changes already applied
    , fmFlows      :: [Flow]
    , fmFlowMap    :: FlowMap
}

newtype FlowMapM cc a = FlowMapM { unFlowMapM :: ST.State (FlowMapSt cc) a }
    deriving (Monad, ST.MonadState (FlowMapSt cc), Functor)

initFlowMapSt g = FlowMapSt {
      fmGraph      = g
    , fmCnfChanges = []
    , fmFlows      = []
    , fmFlowMap    = M.empty
}

addFlow :: Flow -> (FlowMapM cc) ()
addFlow fl = error "NYI"

incrConfigure :: (C.ConfChange cc) => cc -> (FlowMapM cc) ()
incrConfigure cc = do
    g <- ST.gets fmGraph
    let (g', nids') = C.icPartiallyConfigure g cc
    -- do a dfs starting from nids', and update all flow maps
    error "NYI!"
