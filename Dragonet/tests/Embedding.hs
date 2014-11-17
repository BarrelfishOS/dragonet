{-# LANGUAGE QuasiQuotes #-}

import qualified Dragonet.Unicorn               as U
import qualified Dragonet.ProtocolGraph         as PG
import qualified Dragonet.ProtocolGraph.Utils   as PGU
import qualified Dragonet.Configuration         as C
import qualified Util.GraphHelpers              as GH
import qualified Graphs.E10k                    as E10k
import qualified Graphs.LPG                     as LPG

import Dragonet.Embedding
import Dragonet.DotGenerator (toDot, toDotHighlight)
import Dragonet.Predicate (PredExpr, nodePred)
import qualified Dragonet.Predicate as PR
import Graphs.Cfg (lpgCfg,prgCfg,prgCfgEmpty)

import Data.Maybe
import Data.Function (on)
import qualified Data.Graph.Inductive           as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Set                       as Set
import qualified Data.List                      as L
import Control.Monad (sequence, forM_)
import Control.Applicative ((<$>))

import System.IO.Temp (openTempFile)
import System.IO (hPutStr,hClose)

import Text.RawString.QQ (r)
import Text.Show.Pretty (ppShow)
import Text.Printf (printf)
import Debug.Trace (trace)

import Test.HUnit

tr = flip trace
trN = \x  _ -> x

embeddingX = embeddingRxTx2

-- OneOf is a special case of the OR node
--  only one incoming edge per node (interpreted as true)
--  only outgoing edges on the true output
oRisOneOf :: PG.PGraph -> PG.PGNode -> Bool
oRisOneOf gr n@(_, onode@(PG.ONode { PG.nOperator = op }))
    | op == PG.NOpOr = t1 && t2
    | otherwise = error $ "isOneOf called with operator: " ++ (show onode) ++ " but we expect an OR operator"
    where deps :: [(PG.PGNode, PG.PGEdge)]
          deps = PGU.edgeDeps gr n
          sucs :: [(PG.PGNode, PG.PGEdge)]
          sucs = PGU.edgeSucc gr n
          depNodeId ((nid,_), _) = nid
          dep_groups = L.groupBy ((==) `on` depNodeId) deps
          t1 = and $ [ length l == 1 | l <- dep_groups]
          t2 = and $ [ (PG.ePort e) == "true" | (_,(_,_,e)) <- sucs ]

duplicateOR :: PG.PGraph -> PG.PGNode -> PG.PGraph
duplicateOR gr n@(_, onode@(PG.ONode { PG.nOperator = op }))
    | op == PG.NOpOr = error "NYI!"
    | otherwise = error $ "duplicate OR called with operator: " ++ (show onode)
    where succ_edges = PGU.edgeSucc gr n

------------------------------------------------------------------------------
t1 = embTest "multiple queues (prg1,lpg1)" prg1 lpg1 emb1 "1"
------------------------------------------------------------------------------

myPrgCfg = [ ("RxQueues", PG.CVInt 2),
             ("TxQueues", PG.CVInt 2)]
prgConfig = (C.applyConfig myPrgCfg) . E10k.prepareConf

prg1 :: PG.PGraph
prg1 = prgConfig $ U.strToGraph [r|
graph prg1 {

    node Foo {
        port queues[RxQueues]
        port default[RxQueues]
    }

    config RxQueues {
        attr "software"
        spawn poll RxQueues
        port out[] }

    config TxQueues {
        attr "software" }
}
|]

lpg1 :: PG.PGraph
lpg1 = U.strToGraph [r|
graph lpg1 {
    node TxA { port out[TxQueue] }
    node RxQueue { port out[] }
    node TxQueue {}
}
|]

emb1 :: PG.PGraph
emb1 = U.strToGraph [r|
graph emb1 {
    node Foo {
        port queues[RxQ0Valid RxQ1Valid]
        port default[RxQ0Valid]
    }

    or RxQ0Valid {
        port true[RxQueue0__Q0]
        port false[]
    }

    or RxQ1Valid {
        port true[RxQueue1__Q1]
        port false[]
    }

    node TxA__Q1 { port out[TxQueue1__Q1] }
    node TxA__Q0 { port out[TxQueue0__Q0] }
    node RxQueue1__Q1 { spawn poll RxQueue1__Q1 }
    node RxQueue0__Q0 { spawn poll RxQueue0__Q0 }
    node TxQueue1__Q1 {}
    node TxQueue0__Q0 {}
}
|]


-- note that this does not distinguish between spawn and normal edges
nodeSet :: PG.PGraph -> Set.Set (String, [String], [String])
nodeSet g = Set.fromList [nodeToTuple g n | n <- DGI.labNodes g]
    where nodeName n = case (PG.nTag n) of
                         "" -> (PG.nLabel n)
                         t  -> (PG.nLabel n) ++ "__" ++ t
          nodeToTuple g pn@(_,n) = (nodeName n,
                                   L.sort $ [nodeName x | (_,x) <- GH.labPre g pn],
                                   L.sort $ [nodeName x | (_,x) <- GH.labSuc g pn])

embTest msg prg lpg expect fname_suffix =
 TestLabel ("Embedding: " ++ msg) $ TestCase $ do
    let emb      = embeddingX prg lpg
        embSet   = nodeSet emb
        expectSet = nodeSet expect
        cond      = (DGI.noNodes emb) == (DGI.noNodes expect) && expectSet == embSet
        errmsg_   = "prg:      \n" ++ (ppShow $ nodeSet prg) ++ "\n" ++
                    "lpg:      \n" ++ (ppShow $ nodeSet lpg) ++ "\n" ++
                    "result:   \n" ++ (ppShow embSet) ++ "\n" ++
                    "expected: \n" ++ (ppShow expectSet) ++ "\n" ++
                    "embdump:   "

    writeFile ("tests/lpg" ++ fname_suffix ++ ".dot") $ toDot lpg
    writeFile ("tests/prg" ++ fname_suffix ++ ".dot") $ toDot prg
    writeFile ("tests/emb" ++ fname_suffix ++ ".dot") $ toDot emb
    writeFile ("tests/exp" ++ fname_suffix ++ ".dot") $ toDot expect

    errmsg <- case cond of
        True  -> return "SUCCESS"
        False -> do
            --dumpf <- dumpRandFname ("tests/", "emb-test-.dot") (toDot emb)
            return (errmsg_ ++ "\n")

    assertBool errmsg cond

------------------------------------------------------------------------------
-- t2: UDP checksum for all packets
------------------------------------------------------------------------------

prg2 = U.strToGraph [r|
graph prg2 {

    cluster Tx {

        node Queue { port out[L4Prot] }

        node L4Prot {
            port UDP[L4UDPFillChecksum]
            port Other_true[tmp1]

            predicate UDP "and(pred(EthType,IPv4),pred(IpProt,UDP))"
            predicate Other_true "not(and(pred(EthType,IPv4),pred(IpProt,UDP)))"
        }

        node tmp1 {port true[_Out]}

        node L4UDPFillChecksum {
            port true[_Out]
            // predicate o_true "pred(FOO,YEAHHHHHHHHHHHHHHHHHHHHHHHHHHH)"
        }

       or _Out {
            port true[Out]
            port false[]
        }

        node Out {}
    }

    cluster Rx {
        node In {
            port o[Queue]
        }

        node Queue {
            implementation NoImplementationHere
            attr "software"
            attr "source"
            attr "init"
            spawn poll Queue
            port out[]
            port drop[]
            port init[] }
    }
}
|]

dumpRandFname :: (FilePath, String) -> (String) -> IO (FilePath)
dumpRandFname (dirname,ftmpl) d = do
    (tmpf, tmph) <- openTempFile dirname ftmpl
    hPutStr tmph d
    hClose tmph
    return tmpf

embTests_ (prg, lpg) doCheck errmsg fnameSuffix = do
    let emb = embeddingX prg lpg
        embout  = "tests/emb" ++ fnameSuffix ++ ".dot"
        lpgout  = "tests/lpg" ++ fnameSuffix ++ ".dot"
        prgout  = "tests/prg" ++ fnameSuffix ++ ".dot"
        errmsg' = --"prg:      \n" ++ (ppShow $ prg) ++ "\n" ++
                  --"lpg:      \n" ++ (ppShow $ lpgC) ++ "\n" ++
                  --"result:   \n" ++ (ppShow $ emb) ++ "\n" ++
                  "dump:      " ++ embout  ++ "\n" ++
                  "MSG: " ++ errmsg

    writeFile lpgout $ toDot lpg
    writeFile prgout $ toDot prg
    writeFile embout $ toDot emb

    check <- and <$> sequence [ fn emb | fn <- doCheck ]
    assertBool errmsg' check

embTests :: (PG.PGraph, PG.PGraph)
         -> [(PG.PGraph -> IO Bool)]
         -> String
         -> String
         -> Test
embTests (prg, lpg) doCheck errmsg fnameSuffix =
    TestCase $ embTests_ (prg, lpg) doCheck errmsg fnameSuffix

lpgEmbTest :: PG.PGraph -> [PG.PGraph -> IO Bool] -> String -> String -> Test
lpgEmbTest prg doCheck errmsg fname_suffix = TestCase $ do
    (lpgU,lpgH) <- LPG.graphH_ "Graphs/LPG/lpgConfImpl-offload.unicorn"
    let lpgC    = C.applyConfig lpgCfg lpgU
    embTests_ (prg,lpgC) doCheck errmsg fname_suffix


lpgPredNodeTest :: String -> [String] -> Test
lpgPredNodeTest name preds = TestCase $ do

    lpg <- lpgC
    writeFile "tests/lpg.dot" $ toDot lpg

    let nodePred = nLabelSinglePred lpg name
        checkPreds = map PR.parseStrDNF preds

        doCheck p = case PR.predTrueUnder_ p nodePred of
            Nothing  -> Nothing
            Just as -> Just $ "p=\n" ++ (ppShow p) ++ "\nis not true for nodePred=\n" ++ (ppShow nodePred)
                                     ++ "\nfirst failed assignment:\n" ++ (ppShow $ head as)

        results = map doCheck checkPreds
        ok  = and (map isNothing results)
        errmsg = L.intercalate "\n" $ catMaybes results

    assertBool errmsg ok


nodeExistsOnce :: String -> PG.PGraph -> IO Bool
nodeExistsOnce l g = do
    let nodes = GH.filterNodesByL (\x -> (PG.nLabel x) == l) g
    --putStrLn $ "\n found nodes:" ++ (ppShow nodes) ++ "\n"
    return $ length nodes == 1

nodeOffloaded :: String -> PG.PGraph -> IO Bool
nodeOffloaded l g = do
    let nodes = GH.filterNodesByL (\x -> (PG.nLabel x) == l) g
        (ret, msg) = case length nodes of
                       1 -> case PG.nOrigin $ snd (nodes !! 0) of
                           "PRG" -> (True,  "node " ++ l ++ " was offloaded")
                           "LPG" -> (False, "node " ++ l ++ " is unique, but an LPG node")
                           x     -> (False, "node " ++ l ++ " cannot identifiy origin -->"++ (show x) ++"<--")
                       0 -> (False, "node " ++ l ++ " cannot be found")
                       x -> (False, "node " ++ l ++ " was found " ++ (show x) ++ " times")
    --putStrLn $ "\n" ++ msg ++ "\n"
    return ret

nodeName :: PG.PGNode -> String
nodeName (_, n) = ret''
    where lbl   = PG.nLabel n
          ret   = lbl
          tag   = PG.nTag   n
          ret'  = if tag == "" then ret else (ret ++ ":" ++ tag)
          ori   = PG.nOrigin n
          ret'' = if ori == "" then ret' else (ret' ++ ":" ++ ori)

nLabelPred :: PG.PGraph -> String -> [(String, PredExpr)]
nLabelPred g l = [ (nodeName n, nodePred g n) | n <- nodes ]
    where nodes = GH.filterNodesByL (\x -> (PG.nLabel x) == l) g

nLabelSinglePred :: PG.PGraph -> String -> PredExpr
nLabelSinglePred g l = case nLabelPred g l of
    [x] -> snd x
    []  -> error "nLabelSinglePred: no node found"
    _   -> error "more than one nodes found"

t2 = lpgEmbTest prg2 [nodeOffloaded "TxL4UDPFillChecksum"] "TX UDP Checksum was not offloaded (prg2)" "2"

------------------------------------------------------------------------------
-- t3: closer to the Intel NIC
------------------------------------------------------------------------------
prg3 = U.strToGraph [r|
graph prg3 {

    cluster Tx {
        node Queue {
            attr "software"
            port out[L3Prot] }

        // NB: maybe we want to have the convention for ports named `other' that
        // their predicate is NOT all the other ports of the node
        node L3Prot {
            attr "software"
            port IPv4[L4ProtV4 L3IPv4FillChecksum]
            port other_true[L4Done L3Done]

            predicate IPv4 "pred(EthType,IPv4)"
            predicate other_true "not(pred(EthType,IPv4))"
        }

        node L4ProtV4 {
            port TCP[CtxTCPv4]
            port UDP[CtxUDPv4]
            port other[CtxIPv4]

            predicate UDP "pred(IpProt,UDP)"
            predicate TCP "pred(IpProt,TCP)"
            predicate other "and(not(pred(IpProt,TCP)),not(pred(IpProt,UDP)))"
        }

        // only set up IPv4 checksum
        node CtxIPv4 {
            attr "software"
            port out_true[L4Done]
        }

        // set up IPv4 checksum and UDP checksum
        node CtxUDPv4 {
            attr "software"
            port out[L4UDPFillChecksum]
        }

        // set up IPv4 checksum and TCP checksum
        node CtxTCPv4 {
            attr "software"
            port out[L4UTCPFillChecksum]
        }

        node L3IPv4FillChecksum {
            port true[L3Done]
        }

        node L4UDPFillChecksum  {
            port out_true[L3Done]
        }

        node L4UTCPFillChecksum { port out_true[L4Done] }

        or L4Done {
            port true[PrgDone]
            port false[]
        }

        or L3Done {
            port true[PrgDone]
            port false[]
        }

        and PrgDone {
            port true[Out]
            port false[]
        }

        node Out {}
    }

    cluster Rx {
        node In { port o[Queue] }
        node Queue {
            implementation NoImplementationHere
            attr "software"
            attr "source"
            attr "init"
            spawn poll Queue
            port out[]
            port drop[]
            port init[] }
    }
}
|]

t3 = lpgEmbTest prg3 [nodeOffloaded "TxL4UDPFillChecksum",
                      nodeOffloaded "TxL3IPv4FillChecksum"]
                      "TX UDP and IPv4 Checksum was not offloaded (prg3)"
                       "3"

------------------------------------------------------------------------------
-- t4: putting it all together (first attempt)
-- We consider two simple mock protocols: I (ping, pong) and P (header,csum)
------------------------------------------------------------------------------

lpg4 = U.strToGraph [r|
graph lpg4 {

cluster Ap {
    node ToSock1 {}
    node ToSock2 {}
    node ToSock3 {}

    node FromSock1 {
        port true[.TxP]
        predicate true "pred(prot,p)"
    }

    node FromSock2 {
        port true[.TxP]
        predicate true "pred(prot,p)"
    }

    node FromSock3 {
        port true[.TxP]
        predicate true "pred(prot,p)"
    }
}

cluster Tx {

    node Queue {
        port out[]
    }

    or Q_ {
        port true[Queue]
        port false[]
    }

    node Ipong {
        port true[Q_]
    }

    and P_done {
        port true[Q_]
        port false[]
    }

    node PCsum {
        port true[P_done]
    }

    node PHdr {
        port true[P_done]
    }

    or P {
        port true[PCsum PHdr]
        port false[]
    }
}

cluster Rx {
    node Queue {
        port out[Prot]
    }

    node Prot {
        port I[isPing]
        port P[PDemux PCsum]
        port other[Drop]

        predicate I "pred(prot,i)"
        predicate P "pred(prot,p)"
        predicate other "and(not(pred(prot,i)),not(pred(prot,p)))"
    }

    node Drop {}

    node isPing {
        spawn respose .TxIpong [predicate "pred(prot,i)"]
    }

    boolean PCsum {
        port true [ToS1 ToS2 ToS3]
        port false[PCsumFailed]

        predicate true "pred(RxPCsum,valid)"
        predicate false "pred(RxPCsum,invalid)"
    }

    node PCsumFailed {
    }

    // we use a true suffix for the AND node
    node PDemux {
        port s1_true[ToS1]
        port s2_true[ToS2]
        port s3_true[ToS3]
        port drop[]
    }

    and ToS1 {
        port true[.ApToSock1]
        port false[]
    }

    and ToS2 {
        port true[.ApToSock2]
        port false[]
    }

    and ToS3 {
        port true[.ApToSock3]
        port false[]
    }
}

}
|]

prg4 = U.strToGraph [r|
graph prg4 {

cluster Tx {
    node Queue {
        attr "software"
        port out[isP]
    }

    // again, adding true suffix for the predicate compuation
    node isP {
        attr "software"
        port p_true[PsetCtx]
        port o_true[Out_]

        predicate p_true "pred(prot,p)"
        predicate o_true "not(pred(prot,p))"
    }

    node PsetCtx {
        attr "software"
        port o[PCsum]
    }

    node PCsum {
        port true[Out_]
    }

    or Out_ {
        port true[Out]
        port false[]
    }

    node Out {
        attr "software"
    }
}

cluster Rx {
    node Queue {
        attr "software"
        port out[isP]
    }

    node isP {
        attr "software"
        port p[PCsum]
        port o[Out_]

        predicate p "pred(prot,p)"
        predicate o "not(pred(prot,p))"
    }

    node PCsum {
        attr "software"
        port valid[CsumValid]
        port invalid[CsumInvalid]
    }

    node CsumValid {
        attr "software"
        port out[]
    }
    node CsumInvalid {
        attr "software"
        port out[]
    }
    node Out_ {
        attr "software"
        port out[]
    }

}

}
|]

t4 = embTests (prg4,lpg4) [] "prg4/lpg4 checks" "4"

------------------------------------------------------------------------------
------------------------------------------------------------------------------

tests =
 TestList [
      test0
    , test0'
   , lpgPredNodeTest "TxQueue" ["or(not(and(pred(IpProt,UDP),pred(EthType,IPv4))),pred(TxL4UDPFillChecksum,true))"]
   , lpgPredNodeTest "TxQueue" ["or(not(pred(EthType,IPv4)),pred(TxL3IPv4FillChecksum,true))"]
   , t1
   , t2
   , t3
   , t4
 ]

lpgT = LPG.graphH_ "Graphs/LPG/lpgConfImpl-offload.unicorn"
lpgU = fst <$> lpgT
lpgH = snd <$> lpgT
lpgC = C.applyConfig lpgCfg <$> lpgU

e10kT = E10k.graphH_ "Graphs/E10k/prgE10kImpl.unicorn"
e10kU = fst <$> e10kT
e10kH = snd <$> e10kT

e10kC = (C.applyConfig prgCfg) <$> e10kU

e10kT_simple = E10k.graphH_ "Graphs/E10k/prgE10kImpl-simple.unicorn"
e10kU_simple = fst <$> e10kT_simple
e10kH_simple = snd <$> e10kT_simple
e10kC_simple = (C.applyConfig prgCfg) <$> e10kU_simple

e10kOffloadU = do
    (e10kU, e10kH) <- E10k.graphH_ "Graphs/E10k/prgE10kImpl-minimal.unicorn"
    let --(nQueues, cnf) = (3, prgCfg)
        (nQueues, cnf) = (1, prgCfgEmpty)
        prgQConf = [("RxQueues", PG.CVInt nQueues), ("TxQueues", PG.CVInt nQueues)]
        ret = C.applyConfig prgQConf $  E10k.prepareConf e10kU
        ret' = C.applyConfig cnf ret
        ret'' = PGU.cleanupGraph ret'
    --writeFile "tests/prgE10k-queues.dot"  $ toDot ret'
    -- since some queues might not be used, clean up the PRG graph
    return ret'



remPrefix :: String -> String -> String
remPrefix prefix str = case L.stripPrefix prefix str of
    Nothing -> str
    Just x  -> x

prPred :: PG.PGraph -> String -> IO ()
prPred gr s = do
    putStrLn $ "------>" ++ s
    putStrLn $ ppShow $ nLabelPred gr s
    putStrLn $ "<-------"


main = do

    --runTestTT tests

    lpgU <- lpgU
    lpg <- lpgC
    e10k_prg <- e10kC_simple
    e10k_prg_offload <- e10kOffloadU
    let emb_e10k = embeddingX e10k_prg_offload lpg

   -- writeFile "tests/lpgU.dot"    $ toDot lpgU
    writeFile "tests/lpgC.dot"    $ toDot lpg
    writeFile "tests/prgE10k.dot" $ toDot e10k_prg_offload
    writeFile "tests/embE10k.dot" $ toDot emb_e10k

    {-
    writeFile "tests/prgE10k.dot"   $ toDot e10k_prg
    prPred e10k_prg "RxQueue0"
    prPred e10k_prg "RxQueue1"
    prPred e10k_prg "RxQueue2"
    -}


    {--
    --writeFile "tests/prgE10k-2.dot" $ toDot e10k_prg2
    --writeFile "tests/prg1.dot" $ toDot prg1
    --}

    {--
    writeFile "tests/prg4.dot" $ toDot prg4
    writeFile "tests/lpg4.dot" $ toDot lpg4
    let emb4 = embeddingRxTx2 prg4 lpg4
    writeFile "tests/emb4.dot" $ toDot emb4
    --}

    --prPred emb4 "RxQueue"
    --prPred emb4 "RxisPing"
    --prPred emb4 "TxQueue"

    --putStrLn $ ppShow $ embeddingRxTx prg4 lpg4

    --lpgC <- lpgC
    --writeFile "tests/prg1.dot" $ toDot prg1
    --writeFile "tests/prg3.dot" $ toDot prg3
    --writeFile "tests/lpgYYY.dot" $ toDot lpgC


    {--
    --prPred lpgC "TxL3ARPInitiateResponse"
    prPred lpgC "TxQueue"
    let txQueuePred  = snd $ (nLabelPred lpgC "TxQueue") !! 0
        txQueueAnds  = PR.dnfGetANDs txQueuePred
        txQueueAnds' = [ L.sortBy (compare `on` fst) $ PR.predGetAtoms $ PR.PredAnd l | l <- txQueueAnds ]

    putStrLn $ show $ length txQueueAnds'
    let filter_fn l = True
    --let filter_fn l = isJust $ L.find ((== "ARP") . snd) l
    --let filter_fn l = isJust $ L.find ((== "SPAWN.TxL3ARPLookup_") . fst) l
    putStrLn $ ppShow $ filter filter_fn txQueueAnds'

    forM_  (zip [1..] (filter filter_fn txQueueAnds')) $ \(i,preds) -> do
        let ofile = "tests/lpgYYY-hl-" ++ (show i) ++ ".dot"
        writeFile ofile $ toDotHighlight (map fst preds) lpgC
    --let hl = ["TxL3ARPLookup_","TxL4TCPInitiateResponse","RxL4TCPSocketTCPOutMerge","TxL3IPv4Routing","TxL3ARPSendGratuitous","RxQueue"]
    --writeFile "tests/lpgYYY-hl.dot" $ toDotHighlight hl lpgC
   --}

    return ()

-- just to show the syntax
test0  = TestLabel "not really a test" $ TestCase $ assertEqual "1+1" (1+1) 2
test0' =           "not really a test" ~: "1+2" ~: 3 ~=? (1+2)

