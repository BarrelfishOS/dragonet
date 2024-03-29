-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module Dragonet.DotGenerator(
    toDot,
    toDotWith,
    toDotWith',
    toDotHighlight,
    toDotClustered,
    toDotClusteredWith,
    pipelinesDot,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Pipelines as PL
import qualified Data.Graph.Inductive as DGI
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GA
import qualified Data.GraphViz.Attributes.Colors.X11 as GC
import qualified Data.Text.Lazy as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Control.Arrow as A
import Data.Maybe

t :: String -> T.Text
t = T.pack

-- Edge labels, mainly used to create double edges
data ELabel = SingleEdge String | DoubleEdge | SpawnEdge String

-- Mark edges in adjacency list as double edges (if the case)
fixAdj :: [(PG.Edge,DGI.Node)] -> [(ELabel,DGI.Node)]
fixAdj l = singleEdges ++ doubleEdges
    where
        n = L.nub $ map snd l
        -- Endpoints for double edges
        dblN = filter (\x -> elem (PG.Edge "true",x) l &&
                             elem (PG.Edge "false",x) l) n
        doubleEdges = map (\m -> (DoubleEdge,m)) dblN
        -- Original edges with double edges removed
        lSingle = filter (\(p,m) -> notElem m dblN ||
                                    (p /= PG.Edge "true" &&
                                     p /= PG.Edge "false")) l
        -- Replace label by internal label
        relblEdge PG.Edge { PG.ePort = p } = SingleEdge p
        relblEdge PG.ESpawn { PG.eIdentifier = i } = SpawnEdge i
        singleEdges = map (A.first relblEdge) lSingle

-- Mark double edges in the graph
getELs :: PG.PGraph -> DGI.Gr PG.Node ELabel
getELs = DGI.gmap conv
    where
        conv (i,n,m,o) = (fixAdj i,n,m,fixAdj o)




-- Generate attributes for Edges
formatEdge :: (n,n,ELabel) -> GA.Attributes
formatEdge (_,_,SpawnEdge _) = [tailP,headP,style]
    where
        tailP = GA.TailPort $ GA.CompassPoint GA.North
        headP = GA.HeadPort $ GA.CompassPoint GA.West
        style = GA.Style [ GA.SItem GA.Dashed [] ]
formatEdge (_,_,SingleEdge p) = [tailP,headP]
    where
        tailP = GA.TailPort $ GA.LabelledPort (GA.PN $ t p) (Just GA.East)
        headP = GA.HeadPort $ GA.CompassPoint GA.West
formatEdge (_,_,DoubleEdge) = [tailP,headP,double]
    where
        tailP = GA.TailPort $ GA.CompassPoint GA.East
        headP = GA.HeadPort $ GA.CompassPoint GA.West
        double = GA.Color [GA.WC (GA.X11Color GC.Black) Nothing,
                           GA.WC (GA.X11Color GC.White) Nothing,
                           GA.WC (GA.X11Color GC.Black) Nothing]

opStr :: PG.NOperator -> String
opStr PG.NOpAnd  = "AND"
opStr PG.NOpOr   = "OR"
opStr PG.NOpNAnd = "NAND"
opStr PG.NOpNOr  = "NOR"

-- Get string label for a node
nodeLabel :: PG.Node -> String
nodeLabel n =
    case n of
        PG.ONode { PG.nOperator = o } -> opStr o ++ ":" ++ lbl
        _ -> lbl
    where
        pf = "" {-case PG.nGraphType n of
            PG.GTPrg -> "P:"
            PG.GTLpg -> "L:"
            _ -> ""-}
        lbl = pf ++ PG.nLabel n

-- Build record label for node
nodeRecord :: (String -> String) -> (String -> String) -> PG.Node -> GA.Label
nodeRecord nlf plf n = GA.RecordLabel [GA.FlipFields
                                        [GA.FieldLabel $ t nl,
                                         GA.FlipFields portFields]]
    where
        nl = nlf $ nodeLabel n
        port p = GA.LabelledTarget (GA.PN $ t p) (t $ plf p)
        portFields = map port $ PG.nPorts n

-- Build style attributes for node depending on node personality
nodeStyle :: PG.Node -> GA.Attributes
nodeStyle n@PG.FNode {} = [GA.Style [GA.SItem GA.Dotted []] | isSW]
    where
        isSW = PG.nAttrElem (PG.NAttrCustom "software") n
nodeStyle n@PG.ONode {} =
    [GA.Style [GA.SItem GA.Filled [], GA.SItem GA.Rounded []],
         GA.FillColor [GA.WC (GA.X11Color GC.Gray) Nothing]]
nodeStyle n@PG.CNode {} =
        [GA.Style [GA.SItem GA.Filled [], GA.SItem GA.Diagonals []],
         GA.FillColor [GA.WC (GA.X11Color GC.PaleTurquoise) Nothing]]


-- Generate node attributes
formatNode :: (String -> String) -> (String -> String) -> (n,PG.Node)
                 -> GA.Attributes
formatNode nlf plf (_,n) = [lbl,record] ++ nodeStyle n
    where
        record = GA.Shape GA.Record
        lbl = GA.Label $ nodeRecord nlf plf n



-- Assigns nodes to clusters based on cluster map
clusterByMap :: [(DGI.Node, [String])] -> (DGI.Node, PG.Node) -> GV.LNodeCluster String (PG.Node)
clusterByMap cm (n, nl) = foldr GV.C (GV.N (n,nl)) $ fromMaybe [] $ lookup n cm

-- Assign nodes to clusters based on tags
clusterByTag :: (DGI.Node, PG.Node) -> GV.LNodeCluster String (PG.Node)
clusterByTag (n, nl) = if null tag then cn else GV.C tag cn
    where
        tag = PG.nTag nl
        cn = GV.N (n,nl)

-- Cluster to graph id (dot cluster name)
clusterId :: String -> GV.GraphID
clusterId l = GV.Str $ t l

-- Generate cluster attributes
formatCluster :: String -> [GV.GlobalAttributes]
formatCluster l = [GV.GraphAttrs [GA.Label $ GA.StrLabel $ t l]]

type PgParams = GV.GraphvizParams DGI.Node (PG.Node) ELabel String (PG.Node)

-- Parameters for graphs clustered by node tags
params :: (String -> String) -> (String -> String)
    -> PgParams
params nlf plf = GV.defaultParams {
    GV.fmtEdge = formatEdge,
    GV.fmtNode = formatNode nlf plf,
    GV.fmtCluster = const [GV.GraphAttrs [
                        GA.Label $ GA.StrLabel $ t ""]],
    GV.isDotCluster = const True,
    GV.clusterID = clusterId,
    GV.clusterBy = clusterByTag,
    GV.globalAttributes = [GV.GraphAttrs [GA.RankDir GA.FromLeft]]
}

-- Parameters for clustering by a given cluster map
paramsCluster :: (String -> String) -> (String -> String)
    -> [(DGI.Node, [String])]
    -> PgParams
paramsCluster nlf plf cm = (params nlf plf)
                          { GV.clusterBy = clusterByMap cm,
                            GV.isDotCluster = const True,
                            GV.clusterID = clusterId,
                            GV.fmtCluster = formatCluster }


dotString :: GV.DotGraph DGI.Node -> String
dotString d = T.unpack $ GV.printDotGraph d

-- Get dot source for graph
toDot :: PG.PGraph -> String
toDot = toDotWith id id

hlParamsWith :: (PG.PGNode -> Bool) -> PgParams -> PgParams
hlParamsWith isHl ps = ps'
    where old_fmtN :: (DGI.Node, PG.Node) -> GV.Attributes
          old_fmtN = GV.fmtNode ps
          new_fmtN :: (DGI.Node, PG.Node) -> GV.Attributes
          new_fmtN node = (old_fmtN node) ++ hl
                   where hl = case isHl node of
                            True -> [GV.fillColor GC.Red, GV.style GV.filled]
                            False -> []
          ps' = ps {GV.fmtNode = new_fmtN}

-- same as toDot, but highlight nodes in the first argument
toDotHighlight :: [String] -> PG.PGraph -> String
toDotHighlight hllist = toDotWith' pm
    where hlset = S.fromList hllist
          pm = hlParamsWith hlfn
          hlfn :: PG.PGNode -> Bool
          hlfn (_, node) = (PG.nLabel node) `S.member` hlset

-- Get dot source for graph, applying the mappings to generated node/port labels
toDotWith :: (String -> String) -> (String -> String) -> PG.PGraph -> String
toDotWith nlf plf g = dotString $ GV.graphToDot p $ getELs g
    where p = params nlf plf

-- Get dot source for graph, applying a mapping to the parameters
toDotWith' :: (PgParams -> PgParams)
        -> PG.PGraph -> String
toDotWith' pm g = dotString $ GV.graphToDot (pm p) $ getELs g
    where p = params id id

-- Get dot source for graph and cluster nodes
toDotClustered :: PG.PGraph -> [(DGI.Node, [String])] -> String
toDotClustered = toDotClusteredWith id id

-- Get dot source for graph and cluster nodes, apply mapping to node/port labels
toDotClusteredWith :: (String -> String) -> (String -> String) -> PG.PGraph
                        -> [(DGI.Node, [String])] -> String
toDotClusteredWith nlf plf g cs =
    T.unpack $ GV.printDotGraph $ GV.graphToDot p $ getELs g
    where p = paramsCluster nlf plf cs


--------------------------------------------------------------------------------
-- Code for pipeline graphs

plFormatNode :: Maybe (PL.Pipeline -> String) -> PL.PLNode -> GA.Attributes
plFormatNode lmap (_,p) = [lbl] ++ url
    where
        lbl = GA.Label $ GA.StrLabel $ t $ PL.plLabel p
        url = case lmap of
            Just m -> [GA.URL $ t $ m p]
            Nothing -> []

-- Get graph showing only the pipelines
pipelinesDot :: Maybe (PL.Pipeline -> String) -> PL.PLGraph -> String
pipelinesDot lmap plg = dotString $ GV.graphToDot p plg
    where
        p = GV.nonClusteredParams {
            GV.fmtNode = plFormatNode lmap,
            GV.globalAttributes = [GV.GraphAttrs [GA.RankDir GA.FromLeft]]
        }

