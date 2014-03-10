import Dragonet.ProtocolGraph
import qualified Dragonet.Unicorn.Parser as UP
import qualified Dragonet.Unicorn as Unicorn
import qualified Data.Graph.Inductive as DGI
import qualified Util.GraphHelpers as GH
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



nodeStmts :: PGraph -> PGNode -> [String]
nodeStmts g (n,l) =
    ["if (" ++ guardE ++ ") {", "    " ++ nLabel l ++ " = " ++ callS, dbg, "}"]
    where
        isBoolean l' = (elem "Boolean" $ nAttributes l') || nIsONode l'
        pLabel l' p =
            if isBoolean l'
                then ("P_" ++ p)
                else ("P_" ++ nLabel l' ++ "_" ++ p)
        nfLabel l' = "do_pg__" ++ nLabel l'
        guardE
            | nIsFNode l = if null pre
                then "1"
                else L.intercalate " || " $
                    map (\(n',p) -> (nLabel n' ++ " == " ++ pLabel n' p)) pre
            | nIsONode l = L.intercalate " && " $
                map (\(n',p) -> (nLabel n' ++ " != -1")) pre
            where
                pre = map (\(n',e) -> (fromJust $ DGI.lab g n',e)) $
                    DGI.lpre g n
        callS
            | nIsFNode l = nfLabel l ++ "(st, in);"
            | nIsONode l = "(" ++ onCond ++ " ? P_true : P_false);"
        onCond =
            case op of
                OpAnd -> L.intercalate " && " $
                    map (\l' -> nLabel l' ++ " == P_true") inT
                OpOr -> L.intercalate " || " $
                    map (\l' -> nLabel l' ++ " == P_true") inT
           where
                (ONode op) = nPersonality l
                inT = map (fromJust . DGI.lab g . fst) $
                    filter (\(_,e) -> e == "true") $ DGI.lpre g n
                inF = map (fromJust . DGI.lab g . fst) $
                    filter (\(_,e) -> e == "false") $ DGI.lpre g n
        dbg = "    printf(\"" ++ nLabel l ++ "=%d\\n\", " ++ nLabel l ++ ");"

generateTestFun :: String -> PGraph -> String
generateTestFun name g = sig ++ "\n{\n" ++ body ++ "\n}\n"
    where
        stP = "st"
        inP = "in"
        sig = "void " ++ name ++ "(struct state * " ++ stP ++
            ", struct input *" ++ inP ++ ")"
        body = L.intercalate "\n" $ map ("    " ++) bodyL
        bodyL = [nvDecl] ++ stmts
        nvDecl = "node_out_t " ++
            (L.intercalate ", " $ map (\n -> nLabel n ++ "=-1") $ map snd $
                DGI.labNodes g) ++ ";"
        stmts = concatMap (nodeStmts g) $ GH.topsortLN g



main :: IO ()
main = do
    txt <- readFile "lpgIcmpImpl.unicorn"
    igraph <- UP.parseGraph txt
    putStrLn $ generatePortEnum "out_ports" igraph
    putStrLn "\n"
    putStrLn $ generateFNProtos igraph
    putStrLn "\n"
    putStrLn $ generateFNSkels igraph
    putStrLn "\n"
    let graph = Unicorn.constructGraph igraph
    putStrLn $ generateTestFun "testFun" graph
    return ()

