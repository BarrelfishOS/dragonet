{-# LANGUAGE QuasiQuotes #-}

import qualified Dragonet.Unicorn               as U
import qualified Dragonet.ProtocolGraph         as PG
import qualified Dragonet.Configuration         as C
import qualified Util.GraphHelpers              as GH
import qualified Graphs.E10k                    as E10k
import qualified Graphs.LPG                     as LPG

import Dragonet.Embedding (embeddingRxTx)
import Dragonet.DotGenerator (toDot, toDotHighlight)
import Dragonet.ProtocolGraph.Utils (getFNodeByName)
import Dragonet.Predicate (PredExpr, nodePred)
import qualified Dragonet.Predicate as PR
import Graphs.Cfg (lpgCfg)

import Data.Maybe
import Data.Function (on)
import qualified Data.Graph.Inductive           as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Set                       as Set
import qualified Data.List                      as L
import Control.Monad (sequence, forM_)
import Control.Applicative ((<$>))

import Text.RawString.QQ (r)
import Text.Show.Pretty (ppShow)
import Text.Printf (printf)

import Test.HUnit


------------------------------------------------------------------------------
t1 = embTest "multiple queues" prg1 lpg1 emb1
------------------------------------------------------------------------------

myPrgCfg = [ ("RxQueues", PG.CVInt 2),
             ("TxQueues", PG.CVInt 2)]
prgConfig = (C.applyConfig myPrgCfg) . E10k.prepareConf

prg1 :: PG.PGraph
prg1 = prgConfig $ U.strToGraph [r|
graph prg1 {

    config RxQueues {
        attr "software"
        spawn poll RxQueues
        port out[] port drop[] port init[] }

    config TxQueues { attr "software" }
}
|]

lpg1 :: PG.PGraph
lpg1 = U.strToGraph [r|
graph lpg1 {
    node TxA { port out[TxQueue] }
    node RxQueue {}
    node TxQueue {}
}
|]

emb1 :: PG.PGraph
emb1 = U.strToGraph [r|
graph emb1 {
    node TxA__Q1 { port out[TxQueue__Q1] }
    node TxA__Q2 { port out[TxQueue__Q2] }
    node RxQueue__Q1 { spawn poll RxQueue__Q1 }
    node RxQueue__Q2 { spawn poll RxQueue__Q2 }
    node TxQueue__Q1 {}
    node TxQueue__Q2 {}
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

embTest msg prg lpg expect =
 TestLabel ("Embedding: " ++ msg) $ TestCase $ assertBool errmsg cond
    where cond = (DGI.noNodes emb) == (DGI.noNodes expect) && expectSet == embSet
          expectSet = nodeSet expect
          emb      = embeddingRxTx prg lpg
          embSet   = nodeSet emb
          result   = nodeSet $ embeddingRxTx prg lpg
          errmsg = "prg:      \n" ++ (ppShow $ nodeSet prg) ++ "\n" ++
                   "lpg:      \n" ++ (ppShow $ nodeSet lpg) ++ "\n" ++
                   "result:   \n" ++ (ppShow result) ++ "\n" ++
                   "expected: \n" ++ (ppShow expectSet)

------------------------------------------------------------------------------
-- t2: UDP checksum for all packets
------------------------------------------------------------------------------

prg2 = U.strToGraph [r|
graph prg2 {

    cluster Tx {

        node Queue { port o[L4Prot] }

        node L4Prot {
            port UDP[L4UDPFillChecksum]
            port Other[_Out]
        }

        node L4UDPFillChecksum { port o[_Out] } or _Out {
            port true[Out]
            port false[]
        }

        node Out {}
    }

    cluster Rx {
        node In {
            port o[Queue]
        }

        node Queue {}
    }
}
|]

lpgTest :: PG.PGraph -> [(PG.PGraph -> IO Bool)] -> String -> Test
lpgTest prg doCheck errmsg = TestCase $ do
    (lpgU,lpgH) <- LPG.graphH_ "Graphs/LPG/lpgConfImpl-offload.unicorn"
    let lpgC    = C.applyConfig lpgCfg lpgU
        emb     = embeddingRxTx prg lpgC
        embout  = "tests/emb.dot"
        errmsg' = --"prg:      \n" ++ (ppShow $ prg) ++ "\n" ++
                  --"lpg:      \n" ++ (ppShow $ lpgC) ++ "\n" ++
                  --"result:   \n" ++ (ppShow $ emb) ++ "\n" ++
                  "dump:      " ++ embout  ++ "\n" ++
                  "MSG: " ++ errmsg
        dump    = True

    --writeFile "tests/XXXlpg.dot" $ toDot lpgC
    --writeFile "tests/XXXprg.dot" $ toDot prg
    writeFile "tests/XXXemb.dot" $ toDot emb

    check <- or <$> sequence [ fn emb | fn <- doCheck ]
    case (check, dump) of
        (False, True) -> do writeFile "tests/emb.dot" $ toDot emb
        _ ->             do return ()
    assertBool errmsg' check

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
                           _     -> (False, "node " ++ l ++ " cannot be found")
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

nLabelPredicate :: PG.PGraph -> String -> [(String, PredExpr)]
nLabelPredicate g l = [ (nodeName n, nodePred g n) | n <- nodes ]
    where nodes = GH.filterNodesByL (\x -> (PG.nLabel x) == l) g

t2 = lpgTest prg2 [nodeOffloaded "TxL4UDPFillChecksum"] "TX UDP Checksum was not offloaded"

------------------------------------------------------------------------------
-- t3: closer to the Intel NIC
------------------------------------------------------------------------------
prg3 = U.strToGraph [r|
graph prg3 {

    cluster Tx {
        node Queue {
            attr "software"
            port o[L3Prot] }

        // NB: maybe we want to have the convention for ports named other that
        // their predicate is NOT all the other ports of the node
        node L3Prot {
            attr "software"
            port IPv4[L4ProtV4 L3IPv4FillChecksum]
            port other[L4Done L3Done]
        }

        node L4ProtV4 {
            attr "software"
            port UDP[CtxUDPv4]
            port TCP[CtxTCPv4]
            port other[CtxIPv4]
        }

        // only set up IPv4 checksum
        node CtxIPv4 {
            attr "software"
            port out[L4Done]
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

        node L3IPv4FillChecksum { port out[L3Done] }
        node L4UDPFillChecksum  { port out[L4Done] }
        node L4UTCPFillChecksum { port out[L4Done] }

        or L4Done {
            port true[Done]
            port false[]
        }

        or L3Done {
            port true[Done]
            port false[]
        }

        and Done {
            port true[Out]
            port false[]
        }

        node Out {}
    }

    cluster Rx {
        node In { port o[Queue] }
        node Queue {}
    }
}
|]

t3 = lpgTest prg3 [nodeOffloaded "TxL4UDPFillChecksum"] "TX UDP Checksum was not offloaded"

tests =
 TestList [
    test0, test0',
    t1, t2, t3
 ]

lpgT = LPG.graphH_ "Graphs/LPG/lpgConfImpl-offload.unicorn"
lpgU = fst <$> lpgT
lpgH = snd <$>lpgT
lpgC = C.applyConfig lpgCfg <$> lpgU

remPrefix :: String -> String -> String
remPrefix prefix str = case L.stripPrefix prefix str of
    Nothing -> str
    Just x  -> x


main = do
    --writeFile "tests/lpgYYY.dot" $ toDot lpgC
    --writeFile "tests/prg3.dot" $ toDot prg3
    --runTestTT tests
    (lpgU, lpgH) <- lpgT
    lpgC <- lpgC

    let  prPred :: PG.PGraph -> String -> IO ()
         prPred gr s = do
            putStrLn $ "------>" ++ s
            putStrLn $ ppShow $ nLabelPredicate gr s
            putStrLn $ "<-------"

    writeFile "tests/lpgYYY.dot" $ toDot lpgC

    --prPred lpgC "TxL3ARPInitiateResponse"
    prPred lpgC "TxQueue"
    let txQueuePred  = snd $ (nLabelPredicate lpgC "TxQueue") !! 0
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
    {--
   --}

    --let hl = ["TxL3ARPLookup_","TxL4TCPInitiateResponse","RxL4TCPSocketTCPOutMerge","TxL3IPv4Routing","TxL3ARPSendGratuitous","RxQueue"]
    --writeFile "tests/lpgYYY-hl.dot" $ toDotHighlight hl lpgC

    return ()

-- just to show the syntax
test0  = TestLabel "not really a test" $ TestCase $ assertEqual "1+1" (1+1) 2
test0' =           "not really a test" ~: "1+2" ~: 3 ~=? (1+2)

