import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Dynamic as PLD
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.Pipelines.Applications as PLA
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Semantics as Sem

import qualified Data.Graph.Inductive as DGI
import Control.Applicative ((<$>))
import Util.GraphHelpers (findNodeByL)
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.Tap as Tap
import qualified Graphs.LPG as LPG

import Runner.Dynamic (createPipeline, createPipelineClient)

import qualified Control.Concurrent.STM as STM
import Control.Concurrent (threadDelay)
import qualified Data.Map as M

lpgCfg = [("RxL4UDPCUDPSockets", PG.CVList [])]

data AppDesc = AppDesc {
    adLabel :: String,
    adGraphHandle :: PLI.GraphHandle
}

data SocketDesc = SocketDesc {
    sdAppId :: PLA.AppId
}

data EndpointDesc = EndpointUDPIPv4 {
    edSockets :: [PLA.SocketId],
    edIP4Src :: Maybe PLA.IPv4Addr,
    edIP4Dst :: Maybe PLA.IPv4Addr,
    edUDPSrc :: Maybe PLA.UDPPort,
    edUDPDst :: Maybe PLA.UDPPort
}

data StackState = StackState {
    ssNextAppId :: PLA.AppId,
    ssNextSocketId :: PLA.SocketId,
    ssApplications :: M.Map PLA.AppId AppDesc,
    ssAppChans :: M.Map PLA.ChanHandle PLA.AppId,
    ssSockets :: M.Map PLA.SocketId SocketDesc,
    ssEndpoints :: [EndpointDesc],
    ssUpdateGraphs :: STM.TVar StackState -> IO (),
    ssVersion :: Int
}


plAssign ss (_,n)
    | ('R':'x':_) <- PG.nLabel n = "Rx"
    | Just ssid <- PGU.getPGNAttr n "fromsocket" =
            "App" ++ (show $ appId $ read ssid)
    | Just ssid <- PGU.getPGNAttr n "tosocket" =
            "App" ++ (show $ appId $ read ssid)
    | otherwise = "Tx"
    where
        appId sid = sdAppId socket
            where Just socket = M.lookup sid $ ssSockets ss

plConnect :: PL.Pipeline -> PL.Pipeline -> (PLI.POutput,PLI.PInput)
plConnect i o = (PLI.POQueue n, PLI.PIQueue n)
    where n = PL.plLabel i ++ "_to_" ++ PL.plLabel o

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


--------------------------------------------------------------------------------
-- Handlers for app interface events

eventHandler sstv ch (PLA.EvAppConnected gh) = do
    putStrLn $ "AppConnected: " ++ show ch
    STM.atomically $ do
        ss <- STM.readTVar sstv
        let aid = ssNextAppId ss
            app = AppDesc {
                    adLabel = "",
                    adGraphHandle = gh
                }
        STM.writeTVar sstv $ ss {
                    ssNextAppId = aid + 1,
                    ssAppChans = M.insert ch aid $ ssAppChans ss,
                    ssApplications = M.insert aid app $ ssApplications ss
                }
        return ()

eventHandler sstv ch (PLA.EvAppRegister lbl) = do
    aid <- STM.atomically $ do
        ss <- STM.readTVar sstv
        let Just aid = M.lookup ch $ ssAppChans ss
            Just app = M.lookup aid $ ssApplications ss
        STM.writeTVar sstv $ ss {
                ssApplications = M.insert aid app $ ssApplications ss
            }
        return aid
    putStrLn $ "AppRegister: " ++ lbl ++ "[" ++ show ch ++ "] -> " ++ show aid
    PLA.sendMessage ch $ PLA.MsgWelcome aid

eventHandler sstv ch (PLA.EvSocketUDPListen (ip,port)) = do
    (sid,ss) <- STM.atomically $ do
        ss <- STM.readTVar sstv
        -- TODO: check overlapping
        let Just aid = M.lookup ch $ ssAppChans ss
            sid = ssNextSocketId ss
            ep = EndpointUDPIPv4 {
                    edSockets = [sid],
                    edIP4Src = Nothing,
                    edIP4Dst = if ip == 0 then Nothing else Just ip,
                    edUDPSrc = Nothing,
                    edUDPDst = if port == 0 then Nothing else Just port
                }
            sd = SocketDesc {
                    sdAppId = aid
                }
            ss' = ss {
                ssNextSocketId = sid + 1,
                ssSockets = M.insert sid sd $ ssSockets ss,
                ssEndpoints = ssEndpoints ss ++ [ep]
            }
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLn $ "SocketUDPListen p=" ++ show (ip,port) ++ " -> " ++ show sid
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid
    ssUpdateGraphs ss sstv

eventHandler sstv ch ev = do
    putStrLn $ "eventHandler " ++ show ch ++ " " ++ show ev


--------------------------------------------------------------------------------
-- Main
lpgConfig ss = [("RxL4UDPCUDPSockets", PG.CVList $ cUdpSockets)]
    where
        cUdpSockets = map (PG.CVTuple . cUdpSocket) $ ssEndpoints ss
        cUdpSocket ed = [ PG.CVInt $ fromIntegral sid,
                          PG.CVMaybe msIP,
                          PG.CVMaybe msPort,
                          PG.CVMaybe mdIP,
                          PG.CVMaybe mdPort]
            where
                [sid] = edSockets ed
                msIP = PG.CVInt <$> fromIntegral <$> edIP4Src ed
                mdIP = PG.CVInt <$> fromIntegral <$> edIP4Dst ed
                msPort = PG.CVInt <$> fromIntegral <$> edUDPSrc ed
                mdPort = PG.CVInt <$> fromIntegral <$> edUDPDst ed
main = do
    -- Prepare graphs and so on
    (prgU,prgHelp) <- Tap.graphH
    (lpgU,lpgHelp) <- LPG.graphH
    let helpers = prgHelp `Sem.mergeHelpers` lpgHelp
    let dbg = O.dbgDotfiles $ "graphs-tap" :: O.DbgFunction ()
    let stackname = "dragonet"
        llvmHelpers = "llvm-helpers"
    ctx <- PLD.initialContext stackname

    -- Function to adapt graph to current stack state
    let updateGraph sstv = do
            putStrLn "updateGraph entry"
            ss <- STM.atomically $ do
                ss <- STM.readTVar sstv
                let ss' = ss { ssVersion = ssVersion ss + 1 }
                STM.writeTVar sstv ss'
                return ss'
            -- Configure LPG
            let lpgCfg = lpgConfig ss
                lpgC = C.applyConfig lpgCfg lpgU
            putStrLn $ "LPG config: " ++ show lpgCfg
            plg <- O.makeGraph helpers prgU lpgC (plAssign ss)
                (dbg $ show $ ssVersion ss) []
            let createPL pl@('A':'p':'p':aids) = do
                    putStrLn $ "Creating App pipeline: " ++ pl
                    createPipelineClient agh pl
                    where
                        aid = read aids
                        Just app = M.lookup aid $ ssApplications ss
                        agh = adGraphHandle app
                createPL pl = do
                    putStrLn $ "Creating local pipeline: " ++ pl
                    createPipeline plg stackname llvmHelpers pl
            PLD.run ctx plConnect createPL $ addMuxIds plg
            putStrLn "updateGraph exit"

        initSS = StackState {
                ssNextAppId = 1,
                ssNextSocketId = 1,
                ssApplications = M.empty,
                ssAppChans = M.empty,
                ssSockets = M.empty,
                ssEndpoints = [],
                ssUpdateGraphs = updateGraph,
                ssVersion = 0
            }

    putStrLn "Let's fire her up!"
    sstv <- STM.atomically $ STM.newTVar $ initSS
    updateGraph sstv
    putStrLn "Starting interface thread"
    PLA.interfaceThread stackname (eventHandler sstv)

