import Dragonet.ProtocolGraph
import qualified Dragonet.ProtocolGraph.Utils as PGU
import Dragonet.Pipelines
import qualified Dragonet.Unicorn.Parser as UP
import qualified Dragonet.Unicorn as Unicorn
import qualified Data.Graph.Inductive as DGI
import qualified Util.GraphHelpers as GH
import qualified Util.Misc as UM
import Data.Function
import Data.Maybe
import qualified Data.List as L
import Control.Monad (foldM, forM)
import Control.Applicative ((<$>))

import System.Console.GetOpt
import System.Environment (getArgs)


c_enum :: String -> [(String,Maybe Int)] -> String
c_enum name vals = "enum " ++ name ++ " {\n" ++ c_vals ++ "};"
    where
        c_val (l,Nothing) = "    " ++ l ++ ","
        c_val (l,Just i) = "    " ++ l ++ " = " ++ show i ++ ","
        c_vals = unlines $ map c_val vals

c_struct :: String -> [(String,String)] -> String
c_struct name members = "struct " ++ name ++ " {\n" ++ c_members ++ "};"
    where
        c_member (t,l) = "    " ++ t ++ " " ++ l ++ ";"
        c_members = unlines $ map c_member members

graphNodes :: Bool -> String -> UP.Graph -> [UP.Node]
graphNodes swOnly pref (UP.Graph { UP.gRootCluster = c }) =
    map addPrefix $ filter isSWN $ clusterNodes c
    where
        clusterNodes (UP.Cluster _ sub ns) = ns ++ concatMap clusterNodes sub
        isSWN n = not swOnly || NAttrCustom "software" `elem` UP.nAttrs n
        addPrefix n = (addPrefix' n) { UP.nName = pref ++ UP.nName n }

        addPrefix' n@UP.Node { UP.nImplFun = Just l } =
            n { UP.nImplFun = Just (pref ++ l) }
        addPrefix' n@UP.Boolean { UP.nImplFun = Just l } =
            n { UP.nImplFun = Just (pref ++ l) }
        addPrefix' n = n

implName :: UP.Node -> String
implName n
    | UP.Node {} <- n = helper
    | UP.Boolean {} <- n = helper
    | otherwise = UP.nName n
    where
    helper
        | Nothing <- UP.nImplFun n = UP.nName n
        | Just l <- UP.nImplFun n = l

generatePortEnum :: String -> [UP.Node] -> String
generatePortEnum n ns = c_enum n (boolVals ++ nVals)
    where
        boolVals = [("P_false", Just 0), ("P_true", Just 1)]
        nVals = concatMap nodePorts ns
        nodePorts n'@UP.Node { UP.nPorts = ps } =
            zip ls $ map Just [0..]
            where ls = map (\(UP.Port p _) -> "P_" ++ lbl ++ "_" ++ p) ps
                  lbl = implName n'
        nodePorts _ = []

generateSpawnEnum :: String -> [UP.Node] -> String
generateSpawnEnum n ns = c_enum n $ concatMap nodeSpawns ns
    where
        sL n (i, UP.Spawn { UP.sName = sn }) =
            ("S_" ++ implName n ++ "_" ++ sn, Just $ i)
        nS n s = map (sL n) $ zip [0..] $ L.sortBy (compare `on` UP.sName) s
        nodeSpawns n@UP.Node { UP.nSpawns = s } = nS n s
        nodeSpawns n@UP.Boolean { UP.nSpawns = s } = nS n s
        nodeSpawns _ = []


generateFNSig :: UP.Node -> Maybe String
generateFNSig node =
    case node of
        (n@UP.Node {}) -> Just $ fsig $ implName n
        (n@UP.Boolean {}) -> Just $ fsig $ implName n
        _ -> Nothing
    where
        nPref = "do_pg__"
        ctxname n = "ctx_" ++ n
        fsig n = "node_out_t " ++ nPref ++ n ++
           "(struct " ++ ctxname n ++ " *context, struct state *state," ++
            " struct input **in)"

generateFNProtos :: [UP.Node] -> String
generateFNProtos ns = L.intercalate "\n" $ map (++ ";") $
        mapMaybe generateFNSig ns

generateFNSkels :: [UP.Node] -> String
generateFNSkels ns = L.intercalate "\n\n" $ mapMaybe proto ns
    where
        ph n (UP.Port p _) = "P_" ++ n ++ "_" ++ p
        phs n ps = L.intercalate ", " $ map (ph n) ps
        proto n = do
            sig <- generateFNSig n
            let body = case n of
                    (UP.Node { UP.nPorts = ps }) ->
                        "\n{\n    // " ++ phs (implName n) ps
                            ++ "\n    return 0;\n}"
                    (UP.Boolean {}) ->
                        "\n{\n    // P_true, P_false\n    return 0;\n}"
            return (sig ++ body)

generateContextStructs :: [UP.Node] -> String
generateContextStructs nodes = L.intercalate "\n\n" $ map ctxStruct nodes
    where
        ctxStruct n = c_struct ("ctx_" ++ implName n) [
            ("struct ctx_generic", "generic")
            ]

data Flag =
    FPortEnum (Maybe String) |
    FSpawnEnum (Maybe String) |
    FContextStructs |
    FFuncDecls |
    FFuncSkeletons |
    FPRG String |
    FLPG String |
    FGuards String
    deriving (Show,Eq)

data GraphParam =
    GPLPG { gpPrefix :: String,
            gpFile   :: String } |
    GPPRG { gpPrefix :: String,
            gpFile   :: String }
    deriving (Show,Eq)


parseGF :: (String -> String -> Flag) -> String -> Flag
parseGF c s =
    case UM.splitBy ':' s of
        [p,f] -> c p f

options :: [OptDescr Flag]
options = [
    Option ['p'] ["port-enum"] (OptArg FPortEnum "NAME") $
        "Create enum of port identifiers used as return values. " ++
        "NAME defaults to out_ports.",
    Option ['s'] ["spawn-enum"] (OptArg FSpawnEnum "NAME")
        "Create enum of spawn identifiers. NAME defaults to 'out_spawn'.",
    Option ['c'] ["context-structs"] (NoArg FContextStructs)
        "Create structs for node contexts.",
    Option ['f'] ["func-decls"] (NoArg FFuncDecls)
        "Create prototypes for node implementation functions.",
    Option ['k'] ["func-skeletons"] (NoArg FFuncSkeletons)
        "Create skeletons for node implementation functions.",
    Option ['P'] ["prg"] (ReqArg FPRG "[PREFIX:]FILE")
        "Load PRG from Unicorn FILE, prefixing node labels with PREFIX.",
    Option ['L'] ["lpg"] (ReqArg FLPG "[PREFIX:]FILE")
        "Load PRG from Unicorn FILE, prefixing node labels with PREFIX.",
    Option ['g'] ["guards"] (ReqArg FGuards "NAME")
        "Add double-inclusion guards around definitions"
    ]

showUsage errs =
    ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: generateCSkeletons [OPTION...] FILES"

parseOpts :: IO ([Flag], [GraphParam])
parseOpts = do
    argv <- getArgs
    case getOpt Permute options argv of
        (o,[],[]  ) -> foldM parseGPs ([],[]) o
        (_,_,[] ) -> showUsage []
        (_,_,errs) -> showUsage errs
    where
        parseGP s c = case UM.splitByFirst ':' s of
                        (f,[]) -> return $ c "" f
                        (p,f) -> return $ c p f
        parseGPs (fs,gs) (FPRG s) = do
            gp <- parseGP s GPPRG
            return (fs,gs ++ [gp])
        parseGPs (fs,gs) (FLPG s) = do
            gp <- parseGP s GPLPG
            return (fs,gs ++ [gp])
        parseGPs (fs,gs) f = return (fs ++ [f], gs)

getGuards :: [Flag] -> Maybe String
getGuards [] = Nothing
getGuards ((FGuards s):fs) = Just s
getGuards (_:fs) = getGuards fs

runMaybe (Just x) a = a x
runMaybe Nothing _ = return ()

main :: IO ()
main = do
    (flags,files) <- parseOpts
    let mbGrds = getGuards flags

    nodes' <- forM files $ \gp -> do
        s <- readFile $ gpFile gp
        g <- UP.parseGraph s
        let isPrg = case gp of GPLPG {} -> False
                               GPPRG {} -> True
        return $ graphNodes isPrg (gpPrefix gp) g

    let nodes = concat nodes'
        runAction (FPortEnum mbN) =
            generatePortEnum (fromMaybe "out_ports" mbN) nodes
        runAction (FSpawnEnum mbN) =
            generateSpawnEnum (fromMaybe "out_spawns" mbN) nodes
        runAction FContextStructs = generateContextStructs nodes
        runAction FFuncDecls = generateFNProtos nodes
        runAction FFuncSkeletons = generateFNSkels nodes
        runAction _ = ""

    runMaybe mbGrds $ \g -> do
        putStrLn $ "#ifndef " ++ g
        putStrLn $ "#define " ++ g ++ "\n\n"
    putStrLn $ L.intercalate "\n\n\n" $ map runAction flags
    runMaybe mbGrds $ \g -> do
        putStrLn $ "#endif // " ++ g ++ "\n"

