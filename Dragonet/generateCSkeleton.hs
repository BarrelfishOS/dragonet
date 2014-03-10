import qualified Dragonet.Unicorn.Parser as UP
import qualified Dragonet.Unicorn as Unicorn
import Data.Function
import qualified Data.List as L

import Dragonet.Pipelines

c_enum :: String -> [(String,Maybe Int)] -> String
c_enum name vals = "enum " ++ name ++ " {\n" ++ c_vals ++ "};"
    where
        c_val (l,Nothing) = "    " ++ l ++ ","
        c_val (l,Just i) = "    " ++ l ++ " = " ++ show i ++ ","
        c_vals = unlines $ map c_val vals

graphNodes :: UP.Graph -> [UP.Node]
graphNodes (UP.Graph _ c) = clusterNodes c
    where clusterNodes (UP.Cluster _ sub ns) = ns ++ concatMap clusterNodes sub

generatePortEnum :: String -> UP.Graph -> String
generatePortEnum n g = c_enum n (boolVals ++ nVals)
    where
        boolVals = [("P_false", Just 0), ("P_true", Just 1)]
        nVals = concatMap nodePorts $ graphNodes g
        nodePorts (UP.Node n' ps _) = zip ls $ map Just [0..]
            where ls = map (\(UP.Port p _) -> "P_" ++ n' ++ "_" ++ p) ps
        nodePorts _ = []

generateFNSigs :: UP.Graph -> String
generateFNSigs g = concatMap fsig $ graphNodes g
    where
        nPref = "do_pg__"
        fsig (UP.Node n ps _) =
            "node_out_t " ++ nPref ++ n ++
                "(struct state *state, struct input *in)\n{\n    // " ++
                phs ++ "\n}\n\n"
            where
                phs = L.intercalate ", " $ map ph ps
                ph (UP.Port p _) = "P_" ++ n ++ "_" ++ p
        fsig (UP.Boolean n _ _ _) =
            "node_out_t " ++ nPref ++ n ++
                "(struct state *state, struct input *in)\n{\n    // " ++
                phs ++ "\n}\n\n"
            where phs = "P_true, P_false"
        fsig _ = []


main :: IO ()
main = do
    txt <- readFile "lpgIcmpImpl.unicorn"
    igraph <- UP.parseGraph txt
    putStrLn $ generatePortEnum "out_ports" igraph
    putStrLn "\n"
    putStrLn $ generateFNSigs igraph
    --putStrLn $ generatePortEnum "node_ports" igraph
    --let graph = Unicorn.constructGraph igraph
    return ()

