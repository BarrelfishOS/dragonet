import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Dynamic as PLD
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Semantics as Sem

import qualified Data.Graph.Inductive as DGI
import Control.Applicative ((<$>))
import Util.GraphHelpers (findNodeByL)
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.Tap as Tap
import qualified Graphs.LPG as LPG

import Runner.Dynamic (createPipeline)

import Control.Concurrent (threadDelay)

lpgCfg = [("RxL4UDPCUDPSockets", PG.CVList [])]

plAssign (_,n)
    | ('R':'x':_) <- PG.nLabel n = "Rx"
    | otherwise = "Tx"

plConnect :: PL.Pipeline -> PL.Pipeline -> (PLI.POutput,PLI.PInput)
plConnect i o = (PLI.POQueue n, PLI.PIQueue n)
    where n = PL.plLabel i ++ "_to_" ++ PL.plLabel o

noCommandInterface :: IO ()
noCommandInterface = do
    threadDelay $ 1000000 * 60
    noCommandInterface

addMuxIdsPG :: PL.PLGraph -> PG.PGraph -> PG.PGraph
addMuxIdsPG plg pg = flip DGI.nmap pg $ \node ->
    case (PGU.getPGNAttr node "multiplex",PGU.getPGNAttr node "muxPL") of
        (Just aDN,Just aDP) ->
            node { PG.nAttributes = PG.nAttributes node ++
                    [PG.NAttrCustom $ "muxid=" ++ (show $ muxId plg aDN aDP)] }
        _ -> node

addMuxIds :: PL.PLGraph -> PL.PLGraph
addMuxIds plg = DGI.nmap fixPL plg
    where fixPL pl = pl { PL.plGraph = addMuxIdsPG plg $ PL.plGraph pl }

muxId :: PL.PLGraph -> String -> String -> Int
muxId plg dNodeL dPL = i
    where
        -- Dest Pipeline
        Just dpg =
            PL.plGraph <$> snd <$> findNodeByL ((==) dPL . PL.plLabel) plg
        -- Demux node
        Just (dxn,dxnL)  = findNodeByL ((==) "Demux" . PG.nLabel) dpg
        -- get map output port -> ID
        dxPorts = flip zip [0..] $ PG.nPorts dxnL
        Just i = lookup dNodeL dxPorts



main = do
    (prgU,prgHelp) <- Tap.graphH
    (lpgU,lpgHelp) <- LPG.graphH
    let helpers = prgHelp `Sem.mergeHelpers` lpgHelp
    let lpgC  = C.applyConfig lpgCfg lpgU
    let dbg = O.dbgDotfiles "graphs-tap" :: O.DbgFunction ()
    plg <- O.makeGraph helpers prgU lpgC plAssign (dbg "") []
    let stackname = "dragonet"
        llvmHelpers = "llvm-helpers"
        createPL = createPipeline plg stackname llvmHelpers
    ctx <- PLD.initialContext stackname
    PLD.run ctx plConnect createPL $ addMuxIds plg
    -- Just for giggles, restart execution with the same graph after a delay
    threadDelay $ 20 * 1000 * 1000
    putStrLn "\n\n\n\n\n-------------------------------------------------------"
    putStrLn "SHOWTIME!"
    PLD.run ctx plConnect createPL $ addMuxIds plg
    noCommandInterface

