
--module Graph (
module Main (
    main
) where

import Data.Array
import qualified Data.Set as Set
import qualified Conditions as CC

type Vertex = Integer
type Table a = Array Vertex a
type Graph e = Table [(e, Vertex)]
type Bounds  = (Vertex, Vertex)
type Edge e = (Vertex, e, Vertex)

type Labeling a = Vertex -> a

-- Labeled graph
data LabGraph n e = LabGraph (Graph e) (Labeling n)

vertices (LabGraph gr _) = indices gr

labels (LabGraph gr l) = map l (indices gr)

-- | Build a graph from a list of edges.
buildG :: Bounds -> [Edge e] -> Graph e
buildG bounds0 edges0 = accumArray (flip (:)) [] bounds0 [(v, (l,w)) | (v,l,w) <- edges0]

-- | The graph obtained by reversing all edges.
transposeG  :: Graph e -> Graph e
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph e -> [Edge e]
reverseE g   = [ (w, l, v) | (v, l, w) <- edges g ]

--showGraphViz :: (LabGraph, Graph e, ) -> String
showGraphViz (LabGraph gr lab)  =
    "digraph name {\n" ++
    "rankdir=LR;\n" ++
    (concatMap showNode $ indices gr) ++
    (concatMap showEdge $ edges gr) ++
    "}\n"
    where showEdge (from, t, to) = show from ++ " -> " ++ show to ++
                   " [label = \"" ++ show t ++ "\"];\n"
          showNode v = show v ++ " [label = " ++ (show $ lab v) ++ "];\n"

edges :: Graph e -> [Edge e]
edges g = [ (v, l, w) | v <- indices g, (l, w) <- g!v ]

type Automaton t = (Vertex, Graph t, Set.Set Vertex) -- ^ Initial, transitions, finals

automatonToGraphviz (i, gr, fs) = showGraphViz (LabGraph gr lab)
    where lab :: Labeling String
          lab v = (if v == i then (">"++) else id) $
                  (if v `Set.member` fs then (++"|") else id) []

aut1 = (1, buildG (1,8) [(1,'a',2),(2,'a',2),(2,'b',2),(2,'c',3),(1,'a',3)], Set.fromList [8])

data Computation = L2ValidLen
        | L2ValidType
        | L2ValidCRC
        | L2ValidDest
        deriving (Show, Eq, Ord)

data Module = Module {
        name :: String
        , computations :: [Computation]
    } deriving (Show, Eq, Ord)



main :: IO()
main = do
        putStrLn ng
        putStrLn lineBreak
--        putStrLn $ show tg
    where
        lineBreak = "\n\n"
        out1 = automatonToGraphviz aut1
        tg = buildG (1,3) [(1,'a',2),(2,'a',2),(2,'b',2),(2,'c',3),(1,'a',3)]
        ng = showGraphViz (LabGraph tg show)

