
import Dragonet.Implementation.IPv4 (ipFromString)
import Dragonet.DotGenerator (toDot)

import qualified Dragonet.Configuration       as C
import qualified Dragonet.Optimization        as O
import qualified Dragonet.NetState            as NS
import qualified Dragonet.Implementation.IPv4 as IPv4
import qualified Dragonet.Semantics           as Sem
import qualified Dragonet.Embedding           as Emb
import qualified Dragonet.ProtocolGraph       as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU
import qualified Graphs.ImplTransforms as IT

import Dragonet.Flows (Flow(..))

import qualified Graphs.LPG  as LPG
import qualified Graphs.E10k as E10k

import Data.Maybe
import Data.Map as M


plAssign _ (_,n)
    | Just said <- PGU.getPGNAttr n "appid" = "App" ++ said
    | take 2 lbl == "Tx" = "Tx" ++ tag
    | otherwise = "Rx" ++ tag
    where
        lbl = PG.nLabel n
        tag = PG.nTag n

eps :: [NS.EndpointDesc]
eps = M.elems $ NS.nsEndpoints $ snd
    $ NS.runState0 nsTest

udpLocalPort = 53

nsTest :: NS.NetStateM ()
nsTest = do
    let aid = 1
    (sid,eid) <- NS.udpListen aid (0,udpLocalPort)
    NS.socketSpan aid sid
    NS.socketSpan aid sid
    return ()

testMkFlow rip rport = FlowUDPv4 {
      flSrcIp   = Just $ fromJust $ IPv4.ipFromString rip
    , flSrcPort = Just rport
    , flDstIp   = Nothing
    , flDstPort = Just udpLocalPort
}

conf_ = [
      (("1.1.1.1", 1001), 1)
    , (("1.1.1.2", 1002), 2)
 ]
prgConf = C.foldConfChanges $
          [ E10k.insert5tFromFl fl q | ((i,p),q) <- conf_,
                                     let fl = testMkFlow i p]

debug :: O.DbgFunction ()
debug = O.dbgDotfiles "out/xtest/"

main = do
    (lpgU,lpgH) <- LPG.graphH_ "Graphs/LPG/lpgConfImpl-offload.unicorn"
    (prgU,prgH) <- E10k.graphH
    let lpgC = C.applyConfig (LPG.lpgConfig eps) lpgU
        helpers = prgH `Sem.mergeHelpers` lpgH
        --xforms = [IT.coupleTxSockets, IT.mergeSockets]
        xforms = [IT.balanceAcrossRxQs, IT.coupleTxSockets]
    let prgC = C.applyConfig prgConf prgU
    plg <- O.makeGraph'
        helpers
        prgC
        lpgC
        Emb.embeddingRxTx
        xforms
        (plAssign "")
        (debug "x")

    return ()