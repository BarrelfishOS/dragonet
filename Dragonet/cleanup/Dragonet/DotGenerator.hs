module Dragonet.DotGenerator(
    toDot,
    toDotClustered,
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Data.Graph.Inductive as DGI
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GA
import qualified Data.GraphViz.Attributes.Colors.X11 as GC
import qualified Data.Text.Lazy as T
import qualified Data.List as L
import qualified Control.Arrow as A
import Data.Maybe

t :: String -> T.Text
t = T.pack

-- Edge labels, mainly used to create double edges
data ELabel = SingleEdge String | DoubleEdge

-- Mark edges in adjacency list as double edges (if the case)
fixAdj :: [(String,DGI.Node)] -> [(ELabel,DGI.Node)]
fixAdj l = singleEdges ++ doubleEdges
    where
        n = L.nub $ map snd l
        -- Endpoints for double edges
        dblN = filter (\x -> elem ("true",x) l && elem ("false",x) l) n
        doubleEdges = map (\m -> (DoubleEdge,m)) dblN
        -- Original edges with double edges removed
        lSingle = filter (\(p,m) -> notElem m dblN ||
                                    (p /= "true" && p /= "false")) l
        singleEdges = map (A.first SingleEdge) lSingle

-- Mark double edges in the graph
getELs :: PG.PGraph i -> DGI.Gr (PG.Node i) ELabel
getELs = DGI.gmap conv
    where
        conv (i,n,m,o) = (fixAdj i,n,m,fixAdj o)




-- Generate attributes for Edges
formatEdge :: (n,n,ELabel) -> GA.Attributes
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

                         
        
-- Get string label for a node
nodeLabel :: PG.Node i -> String
nodeLabel n
    | PG.nIsONode n = PG.opToString op ++ ":" ++ lbl
    | otherwise = lbl
    where
        (PG.ONode op) = PG.nPersonality n
        pf = case PG.nGraphType n of
            PG.GTPrg -> "P:"
            PG.GTLpg -> "L:"
            _ -> ""
        lbl = pf ++ PG.nLabel n

-- Build record label for node
nodeRecord :: PG.Node i -> GA.Label
nodeRecord n = GA.RecordLabel [GA.FlipFields
                                [GA.FieldLabel $ t $ nodeLabel n,
                                 GA.FlipFields portFields]]
    where
        port p = GA.LabelledTarget (GA.PN $ t p) (t p)
        portFields = map port $ PG.nPorts n

-- Build style attributes for node depending on node personality
nodeStyle :: PG.Node i -> GA.Attributes
nodeStyle n
    | PG.nIsFNode n =
        [GA.Style [GA.SItem GA.Dotted []] | isSW]
    | PG.nIsONode n =
        [GA.Style [GA.SItem GA.Filled [], GA.SItem GA.Rounded []],
         GA.FillColor [GA.WC (GA.X11Color GC.Gray) Nothing]]
    | PG.nIsCNode n =
        [GA.Style [GA.SItem GA.Filled [], GA.SItem GA.Diagonals []],
         GA.FillColor [GA.WC (GA.X11Color GC.PaleTurquoise) Nothing]]
    | otherwise = undefined
    where
        isSW = elem "software" $ PG.nAttributes n

-- Generate node attributes
formatNode :: (n,PG.Node i) -> GA.Attributes
formatNode (_,n) = [lbl,record] ++ nodeStyle n
    where
        record = GA.Shape GA.Record
        lbl = GA.Label $ nodeRecord n



-- Assigns nodes to clusters based on cluster map
clusterByMap :: [(DGI.Node, [String])] -> (DGI.Node, PG.Node i) -> GV.LNodeCluster String (PG.Node i)
clusterByMap cm (n, nl) = foldr GV.C (GV.N (n,nl)) $ fromMaybe [] $ lookup n cm

-- Assign nodes to clusters based on tags
clusterByTag :: (DGI.Node, PG.Node i) -> GV.LNodeCluster String (PG.Node i)
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


-- Parameters for graphs clustered by node tags
params :: GV.GraphvizParams DGI.Node (PG.Node i) ELabel String (PG.Node i)
params = GV.defaultParams {
    GV.fmtEdge = formatEdge,
    GV.fmtNode = formatNode,
    GV.fmtCluster = const [],
    GV.isDotCluster = const True,
    GV.clusterID = clusterId,
    GV.clusterBy = clusterByTag,
    GV.globalAttributes = [GV.GraphAttrs [GA.RankDir GA.FromLeft]]
}

-- Parameters for clustering by a given cluster map
paramsCluster :: [(DGI.Node, [String])] -> GV.GraphvizParams DGI.Node (PG.Node i) ELabel String (PG.Node i)
paramsCluster cm = params { GV.clusterBy = clusterByMap cm,
                            GV.isDotCluster = const True,
                            GV.clusterID = clusterId,
                            GV.fmtCluster = formatCluster }


-- Get dot source for graph
toDot :: PG.PGraph i -> String
toDot g = T.unpack $ GV.printDotGraph $ GV.graphToDot params $ getELs g

-- Get dot source for graph and cluster nodes
toDotClustered :: PG.PGraph i -> [(DGI.Node, [String])] -> String
toDotClustered g cs = T.unpack $ GV.printDotGraph $
                        GV.graphToDot (paramsCluster cs) $ getELs g
