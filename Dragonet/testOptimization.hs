{-# LANGUAGE OverloadedStrings #-}
import qualified Dragonet.Semantics as Sem
import qualified Dragonet.Semantics.Simplify as SS
import qualified Dragonet.Unicorn.Parser as UnicornAST
import qualified Dragonet.Unicorn  as Unicorn
import Dragonet.DotGenerator (toDot, pipelinesDot)
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Embedding as Emb
import Dragonet.Implementation.IPv4 as IP4
import qualified Dragonet.Configuration as C
import qualified Dragonet.Pipelines as PL

import qualified E10k

import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTBV

import qualified Data.Graph.Inductive.Graph as DGI

import Data.Maybe
import qualified Data.List as L
import Data.Functor ((<$>))
import Data.String (fromString)

import Debug.Trace (trace)

-- Parse graph from unicorn file
parseGraph :: PG.GraphType -> FilePath -> IO (PG.PGraph,Sem.Helpers)
parseGraph gt path = do
    b <- readFile path >>= UnicornAST.parseGraph
    let (pg,hs) = Unicorn.constructGraph' b
        pg' = PG.pgSetType gt pg
    return (pg',hs)

--------------------------------------------------------------------------------
-- Graph transformations

-- Remove sources without "source" attribute and sinks without "sink" attribute
-- can also be used to remove hardware parts of the embeded graph
cleanupGraph :: PG.PGraph -> PG.PGraph
cleanupGraph g
    | null badNodes = g
    | otherwise = cleanupGraph g'
    where
        hasAttr a n = elem a $ PG.nAttributes n
        srcs = filter (null . DGI.pre g . fst) $ DGI.labNodes g
        snks = filter (null . DGI.suc g . fst) $ DGI.labNodes g
        badSrcs = filter (not . hasAttr "source" . snd) srcs
        badSnks = filter (not . hasAttr "sink" . snd) snks
        badNodes = L.nub $ map fst $ badSrcs ++ badSnks
        g' = DGI.delNodes badNodes g



pipelineGraph :: PG.PGraph -> PL.PLGraph
pipelineGraph g = PL.generatePLG plAssign g
    where
        plAssign (_,n)
            | take 3 lbl == "App" = lbl
            | take 2 lbl == "Tx" = "Tx" ++ tag
            | otherwise = "Rx" ++ tag
            where
                lbl = PG.nLabel n
                tag = PG.nTag n


--------------------------------------------------------------------------------
-- LPG

configLPGUDPSockets :: PG.ConfFunction
configLPGUDPSockets _ inE outE cfg = concat <$> mapM addSocket tuples
    where
        PG.CVList tuples = cfg
        hasL l PG.Node { PG.nLabel = nl } = l == nl
        findN ps l = (fst . fst) <$> L.find (hasL l . snd . fst) ps
        Just vN = findN inE "RxL4UDPValid"
        Just vhN = findN inE "RxL4UDPValidHeaderLength"
        Just irN = findN outE "TxL4UDPInitiateResponse"
        Just cN  = findN outE "RxL4UDPUnusedPort"
        addSocket (PG.CVTuple [
                    PG.CVInt sid,
                    PG.CVMaybe msIP,
                    PG.CVMaybe msPort,
                    PG.CVMaybe mdIP,
                    PG.CVMaybe mdPort]) = do
            (dxN,_) <- C.confMNewNode $ addFSemantics $ PG.baseFNode filterS []
                        bports Nothing
            (fsN,_) <- C.confMNewNode $ PG.baseFNode ("FromSocket" ++ show sid)
                        [] ["out"] Nothing
            (tsN,_) <- C.confMNewNode $ PG.baseFNode ("ToSocket" ++ show sid)
                        ["sink"] ["out","drop"] Nothing
            (vsN,_) <- C.confMNewNode $ PG.baseONode ("ValidSocket" ++ show sid)
                        [] bports PG.OpAnd Nothing
            return [
                (vhN,dxN,"true"),  -- RxL4UDPValidHeaderLength -> Filter
                (dxN,vsN,"false"), -- Filter -> ValidSocket
                (dxN,vsN,"true"),  -- Filter -> ValidSocket
                (dxN,cN,"false"),  -- Filter -> Collect
                (dxN,cN,"true"),   -- Filter -> Collect
                (vN, vsN,"false"), -- RxL4UDPValid -> ValidSocket
                (vN, vsN,"true"),  -- RxL4UDPValid -> ValidSocket
                (vsN,tsN,"true"),  -- ValidSocket -> ToSocket
                (fsN,irN,"out"),   -- FromSocket -> TxL4UDPInitiateResponse
                (tsN,fsN,"out")]   -- FromSocket -> ToSocket
            where
                tuple@(_,msIP',msP,mdIP',mdP) =
                    (sid, uncvi <$> msIP, uncvi <$> msPort,
                        uncvi <$> mdIP, uncvi <$> mdPort)
                uncvi (PG.CVInt i) = i
                mbIP = maybe "*" (IP4.ipToString . fromIntegral . uncvi)
                mbPort = maybe "*" (show . uncvi)
                filterS = "Filter: UDP(" ++ mbIP msIP ++ ":" ++ mbPort msPort
                            ++ " / " ++ mbIP mdIP ++ ":" ++ mbPort mdPort ++ ")"
                ipSems n i = SMTBV.bv i 32 SMTC.===
                            SMT.app
                                (fromString $ "IP4." ++ n)
                                [SMT.app "pkt" []]
                portSems n i = SMTBV.bv i 16 SMTC.===
                            SMT.app
                                (fromString $ "UDP." ++ n)
                                [SMT.app "pkt" []]
                tSems = foldl1 SMTC.and $ catMaybes [
                        ipSems "src" <$> msIP',
                        portSems "src" <$> msP,
                        ipSems "dst" <$> mdIP',
                        portSems "dst" <$> mdP]
                fSems = SMTC.not tSems
                addFSemantics n = n {
                    PG.nSemantics = [("true",tSems),("false",fSems) ] }
                bports = ["false","true"]

addLpgCfgFun PG.Node { PG.nLabel = l }
    | l == "RxL4UDPCUDPSockets" = configLPGUDPSockets
    | otherwise = error $ "Unknown LPG CNode: '" ++ l ++ "'"

lpgCfg = [
    ("RxL4UDPCUDPSockets", PG.CVList [
            PG.CVTuple [ PG.CVInt 0,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 7 ],
            PG.CVTuple [ PG.CVInt 1,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 8 ],
            PG.CVTuple [ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 1,
                PG.CVMaybe $ Just $ PG.CVInt 1337,
                PG.CVMaybe $ Just $ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 8 ],
            PG.CVTuple [ PG.CVInt 3,
                PG.CVMaybe $ Just $ PG.CVInt 5,
                PG.CVMaybe $ Just $ PG.CVInt 1338,
                PG.CVMaybe $ Just $ PG.CVInt 2,
                PG.CVMaybe $ Just $ PG.CVInt 1234 ],
            PG.CVTuple [ PG.CVInt 4,
                PG.CVMaybe $ Just $ PG.CVInt 6,
                PG.CVMaybe $ Just $ PG.CVInt 2345,
                PG.CVMaybe $ Just $ PG.CVInt 3,
                PG.CVMaybe $ Just $ PG.CVInt 3456 ]
        ])]

--------------------------------------------------------------------------------
-- PRG

addPrgCfgFun PG.Node { PG.nLabel = l }
    | l == "RxC5TupleFilter" = E10k.config5tuple
    | l == "RxCFDirFilter"   = E10k.configFDir
    | otherwise = error $ "Unknown LPG CNode: '" ++ l ++ "'"

prgCfgEmpty = [
    ("RxC5TupleFilter", PG.CVList []),
    ("RxCFDirFilter", PG.CVList [])
    ]

prgCfg = [
    ("RxC5TupleFilter", PG.CVList [
            PG.CVTuple [
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVEnum 1,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 7,
                PG.CVInt 1,
                PG.CVInt 1 ],
            PG.CVTuple [
                PG.CVMaybe Nothing,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVEnum 1,
                PG.CVMaybe Nothing,
                PG.CVMaybe $ Just $ PG.CVInt 8,
                PG.CVInt 1,
                PG.CVInt 2 ]
        ]),
    ("RxCFDirFilter", PG.CVList [])
    ]

--------------------------------------------------------------------------------
-- Cost Function

costFunction :: PL.PLGraph -> Integer
costFunction plg = sum $ map (plCost . snd) $ DGI.labNodes plg

plCost :: PL.Pipeline -> Integer
plCost pl = sum $ map (nodeCost . snd) $ DGI.labNodes $ PL.plGraph pl

nodeCost :: PG.Node -> Integer
nodeCost _ = 1

--------------------------------------------------------------------------------
-- Main


evalGraph helpers prg lpg pref cfg = do
    let write f s = writeFile (pref ++ f) s
    let prgC  = PG.pgSetType PG.GTPrg $ C.applyConfig cfg prg
    -- Dot for configured graphs...
    write "prg_c.dot" $ toDot prgC

    -- Dot for embedded graph...
    let emb = Emb.embeddingRxTx prgC lpg
    write "embed.dot" $ toDot emb

    -- Dot for reduced graph...
    reduced <- SS.reducePG emb helpers
    write "reduced.dot" $ toDot reduced

    -- Dot for cleaned-up graph...
    let cleanedUp = cleanupGraph reduced
    write "cleanup.dot" $ toDot cleanedUp

    -- Dots for pipeline graph...
    let linkMap pl = "pl_" ++ PL.plLabel pl ++ ".svg"
    let plg = pipelineGraph cleanedUp
    write "pipelines.dot" $ pipelinesDot (Just linkMap) plg
    mapM_ (\(_,pl) ->
        write ("pl_" ++ PL.plLabel pl ++ ".dot") $ toDot $ PL.plGraph pl
        ) $ DGI.labNodes plg

    return (costFunction plg, plg)


main = do
    let pref = ""
    let write f s = writeFile (pref ++ f) s

    (prgU,prgH) <- parseGraph PG.GTPrg "prgE10kImpl.unicorn"
    (lpgU,lpgH) <- parseGraph PG.GTLpg "lpgConfImpl.unicorn"
    let helpers = prgH `Sem.mergeHelpers` lpgH

    -- Dot for unconfigured graphs...
    writeFile "prg_u.dot" $ toDot prgU
    writeFile "lpg_u.dot" $ toDot lpgU

    -- Add config functions
    let lpgU' = C.replaceConfFunctions addLpgCfgFun lpgU
        prgU' = C.replaceConfFunctions addPrgCfgFun prgU
    -- Configure LPG
    let lpgC  = PG.pgSetType PG.GTLpg $ C.applyConfig lpgCfg lpgU'
    writeFile "lpg_c.dot" $ toDot lpgC


    -- Evaluate configurations
    let configs = [("cfg_empty_",prgCfgEmpty), ("cfg_test_", prgCfg)]

    -- Evaluate graph
    results <- mapM (uncurry $ evalGraph helpers prgU' lpgC) configs
    putStrLn $ show $ zip (map fst configs) (map fst results)

