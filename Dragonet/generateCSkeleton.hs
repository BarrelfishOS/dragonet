import qualified Dragonet.Unicorn.Parser as UP
import qualified Dragonet.Unicorn as Unicorn
import Data.Function
import Data.Maybe
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

generateFNSig :: UP.Node -> Maybe String
generateFNSig node =
    case node of
        (UP.Node n _ _) -> Just $ fsig n
        (UP.Boolean n _ _ _) -> Just $ fsig n
        _ -> Nothing
    where
        nPref = "do_pg__"
        fsig n = "node_out_t " ++ nPref ++ n ++
           "(struct state *state, struct input *in)"

generateFNProtos :: UP.Graph -> String
generateFNProtos g = L.intercalate "\n" $ map (++ ";") $
        mapMaybe generateFNSig $ graphNodes g

generateFNSkels :: UP.Graph -> String
generateFNSkels g = L.intercalate "\n\n" $ mapMaybe proto $ graphNodes g
    where
        ph n (UP.Port p _) = "P_" ++ n ++ "_" ++ p
        phs n ps = L.intercalate ", " $ map (ph n) ps
        proto n = do
            sig <- generateFNSig n
            let body = case n of
                    (UP.Node n' ps _) ->
                        "\n{\n    // " ++ phs n' ps ++ "\n    return 0;\n}"
                    (UP.Boolean _ _ _ _) ->
                        "\n{\n    // P_true, P_false\n    return 0;\n}"
            return (sig ++ body)


main :: IO ()
main = do
    txt <- readFile "lpgIcmpImpl.unicorn"
    igraph <- UP.parseGraph txt
    putStrLn $ generatePortEnum "out_ports" igraph
    putStrLn "\n"
    putStrLn $ generateFNProtos igraph
    putStrLn "\n"
    putStrLn $ generateFNSkels igraph
    --putStrLn $ generatePortEnum "node_ports" igraph
    --let graph = Unicorn.constructGraph igraph
    return ()

