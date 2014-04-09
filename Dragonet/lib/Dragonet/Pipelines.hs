module Dragonet.Pipelines(
    Pipeline(..), PLabel,
    PLGraph, PLNode, PLEdge,
    generatePLG,
) where

import qualified Control.Concurrent.STM.TChan as TC
import qualified Data.Graph.Inductive as DGI
import qualified Dragonet.ProtocolGraph as PG
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Util.GraphHelpers as GH
import qualified Util.GraphMonad as GM
import Data.Function
import Data.Maybe
import Util.Misc


type PLabel = String

data Pipeline = Pipeline {
    plLabel :: PLabel,
    plGraph :: PG.PGraph
} --deriving (Show)

instance Show Pipeline where
    show pl = "Pipeline \"" ++ plLabel pl ++ "\""

type PLGraph = DGI.Gr Pipeline ()
type PLNode = DGI.LNode Pipeline
type PLEdge = DGI.LEdge ()


-- Generate one pipeline, returns list of successor pipeline labels and the
-- pipeline
generatePipeline :: PG.PGraph -> M.Map DGI.Node PLabel -> String
                        -> [DGI.Node] -> (Pipeline, [String])
generatePipeline g nm pll ns = (Pipeline pll pg, suc)
    where
        -- Label a node id
        labN n = PG.nLabel $ fromJust $ DGI.lab g n
        -- Graph obtained by removing all nodes that don't belong to this
        -- pipeline
        pg' = GH.reduceNodes g ns
        nss = S.fromList ns
        -- Edges incoming from other pipelines
        (inE,notInE) = L.partition
                        (\(s,d,_) -> (S.notMember s nss) && (S.member d nss)) $
                        DGI.labEdges g
        inDN = L.nub $ map snd3 $ inE
        -- Edges outgoing to other pipelines
        (outE,_) = L.partition
                        (\(s,d,_) -> (S.member s nss) && (S.notMember d nss)) $
                        notInE
        outN = partListBy snd3 outE
        outPL = partListBy ((nm M.!) . fst) outN

        -- Find nodes without predecessors
        sources' = S.fromList $ filter (\n -> null $ DGI.pre g n) $ DGI.nodes g
        sources = S.toList $ nss `S.intersection` sources'

        -- Adapt graph
        (suc,pg) = flip GM.runOn pg' (do
            -- Demux node for incoming edges
            if null inE then
                return () -- No incoming edges from other pipelines, we're good
            else (do
                let demuxPs = ["_"] ++ map labN inDN
                demuxN <- GM.newNode $
                    PG.baseFNode "Demux" ["source"] demuxPs Nothing
                mapM_ (\n -> GM.newEdge (demuxN, n, labN n)) inDN

                -- Add edges for nodes that were originally without incoming edges
                mapM_ (\n -> GM.newEdge (demuxN, n, "_")) sources)

            -- Destination nodes for outgoing queues
            if null outE then
                return ()
            else
                mapM_ (\(pl,ns') -> do
                    -- Create "To$Pipeline" node
                    let toPLNA = "pipeline=" ++ pl
                    toPLN <- GM.newNode $ PG.baseFNode ("ToPL" ++ pl)
                        ["sink", toPLNA] ["out"] Nothing
                    mapM_ (\(n,es) -> do
                        -- Create "To$Node" node
                        let muxNA = "multiplex=" ++ labN n
                        let muxPL = "muxPL=" ++ pl
                        toNN <- GM.newNode $
                            PG.baseFNode ("ToPL" ++ pl ++ "_" ++ labN n)
                                [muxNA, muxPL] ["out"] Nothing
                        -- Add edge to "To$Pipeline" node
                        GM.newEdge (toNN,toPLN,"out")
                        -- Add edges
                        mapM_ (\(s,_,p) -> GM.newEdge (s,toNN,p)) es
                        ) ns'
                    ) outPL
            return $ map fst outPL)


-- Splits specified graph up into pipelines according to the specified function,
-- that assigns a string representing the pipeline label to each node
generatePLG :: (PG.PGNode -> PLabel) -> PG.PGraph -> PLGraph
generatePLG sf g = DGI.mkGraph nodes edges
    where
        -- Group nodes by pipeline
        plns' = partListBy sf $ DGI.labNodes g
        -- Map from PG node id to pipeline label
        nmp = M.fromList $ map (\ln@(n,_) -> (n, sf ln)) $ DGI.labNodes g

        -- Assignment from pipeline label to node id in PLG
        plids = M.fromList $ flip L.zip [0..] $ map fst plns'
        -- Create pipelines
        pipelines = map (\(pll,ns) ->
            (plids M.! pll,
             generatePipeline g nmp pll $ map fst ns)
            ) plns'

        -- Labeled nodes for pipeline graph
        nodes = map (\(n,(p,_)) -> (n,p)) pipelines
        -- Edges for pipeline graph
        edges = concatMap (\(n,(_,suc)) -> map (\x -> (n,plids M.! x,())) suc)
                    pipelines



