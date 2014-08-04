{-# LANGUAGE QuasiQuotes #-}

import qualified Dragonet.Unicorn       as U
import qualified Dragonet.ProtocolGraph as PG
import qualified Util.GraphHelpers      as GH
import qualified Dragonet.Configuration as C

import Dragonet.Embedding (embeddingRxTx)
import Dragonet.DotGenerator (toDot)
import Dragonet.ProtocolGraph.Utils (getFNodeByName)

import qualified Graphs.E10k as E10k
import qualified Graphs.LPG as LPG
import Graphs.Cfg (lpgCfg)

import qualified Data.Set as Set
import qualified Data.List as L
import Data.Maybe

import qualified Data.Graph.Inductive as DGI
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Graphs.Helpers as GH

import Text.RawString.QQ (r)
import Test.HUnit
import Text.Show.Pretty (ppShow)


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
-- t2 = embTest ""
------------------------------------------------------------------------------

prg2 = U.strToGraph [r|
graph prg2 {

    cluster Tx {

        node Queue { port o[L4Prot] }

        node L4Prot {
            port UDP[L4UDPFillChecksum]
            port Other[_Out]
        }

        node L4UDPFillChecksum {
            port o[_Out]
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

        node Queue {}
    }
}
|]

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

    check <- doCheck emb
    case (check, dump) of
        (False, True) -> do writeFile "tests/emb.dot" $ toDot emb
        _ ->             do return ()
    assertBool errmsg' check

nodeExistsOnce :: String -> PG.PGraph -> IO Bool
nodeExistsOnce l g = do
    let nodes = GH.filterNodesByL (\x -> (PG.nLabel x) == l) g
    --putStrLn $ "\n found nodes:" ++ (ppShow nodes) ++ "\n"
    return $ length nodes == 1

t2 = lpgTest prg2 (nodeExistsOnce "TxL4UDPFillChecksum") "TX UDP Checksum was not offloaded"


tests =
 TestList [
    test0, test0',
    t1, t2
 ]

main = do
    --(lpgU,lpgH) <- LPG.graphH_ "Graphs/LPG/lpgConfImpl-offload.unicorn"
    --let lpgC  = C.applyConfig lpgCfg lpgU
    runTestTT tests
    return ()

-- just to show the syntax
test0  = TestLabel "not really a test" $ TestCase $ assertEqual "1+1" (1+1) 2
test0' =           "not really a test" ~: "1+2" ~: 3 ~=? (1+2)

