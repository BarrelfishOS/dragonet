module Stack (
    instantiate,
    instantiateKK,
    instantiateKKwithSortedFlows,
    startStack,

    StackState(..),
    EndpointDesc(..),
    SocketDesc(..),
    AppDesc(..),
    OracleArgs(..)
) where

import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Dynamic as PLD
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.Pipelines.Applications as PLA
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Semantics as Sem
import Dragonet.Flows  (Flow(..))

import Graphs.Cfg (e10kCfgEmpty)

import qualified Data.Maybe as MB

import qualified Data.Graph.Inductive as DGI
import Control.Applicative ((<$>))
import Util.GraphHelpers (findNodeByL)
import qualified Dragonet.ProtocolGraph.Utils as PGU

import qualified Graphs.Tap as Tap
import qualified Graphs.LPG as LPG
import qualified Graphs.ImplTransforms as IT

import Runner.Dynamic (createPipeline, createPipelineClient)

import qualified Control.Concurrent.STM as STM
import Control.Concurrent (threadDelay)
import qualified Data.Map as M
import qualified Data.Set as S

import Util.XTimeIt (doTimeIt,dontTimeIt)
import Text.Show.Pretty (ppShow)

type EndpointId = Int

data AppDesc = AppDesc {
    adLabel :: String,
    adGraphHandle :: PLI.GraphHandle
}

data SocketDesc = SocketDesc {
    sdAppId :: PLA.AppId,
    sdEpId  :: EndpointId
}

data EndpointDesc = EndpointUDPIPv4 {
    edSockets :: [PLA.SocketId],
    edIP4Src :: Maybe PLA.IPv4Addr,
    edIP4Dst :: Maybe PLA.IPv4Addr,
    edUDPSrc :: Maybe PLA.UDPPort,
    edUDPDst :: Maybe PLA.UDPPort
} deriving (Show, Eq, Ord)

epToFlow EndpointUDPIPv4 {edIP4Src = srcIp,
                          edUDPSrc = srcPort,
                          edIP4Dst = dstIp,
                          edUDPDst = dstPort }
   = FlowUDPv4 {
          flSrcIp = srcIp,
          flDstIp = dstIp,
          flDstPort  = dstPort,
          flSrcPort  = srcPort }

data StackState = StackState {
    ssNextAppId :: PLA.AppId,  -- This ID will be given to the next app that tries to connect
    ssNextSocketId :: PLA.SocketId,
    ssNextEndpointId :: EndpointId,
    ssApplications :: M.Map PLA.AppId AppDesc,
    ssAppChans :: M.Map PLA.ChanHandle PLA.AppId,
    ssSockets :: M.Map PLA.SocketId SocketDesc,
    ssEndpoints :: M.Map EndpointId EndpointDesc,
    -- quick hack to maintain the previous endpoints so that we can find the
    -- difference between the current and the old state. It is probably better
    -- to actually track the changes from the event handlers though.
    ssPrevEndpoints :: M.Map EndpointId EndpointDesc,
    ssUpdateGraphs :: STM.TVar StackState -> IO (),
    ssVersion :: Int
}


-- Oracle related
data OracleArgs =  OracleArgs {
        oracleOldConf           :: (String,C.Configuration)
        , oraclePrg             :: PG.PGraph
        , oracleNewConns        :: [EndpointDesc]
        , oracleSS              :: StackState
    } deriving ()

initOracleArgs :: PG.PGraph -> [EndpointDesc] -> StackState -> OracleArgs
initOracleArgs prg endps ss = OracleArgs  {
    oracleOldConf = getEmptyConf,
    oraclePrg  = prg,
    oracleNewConns  = endps,
    oracleSS = ss
}

-- Force socket nodes in their respective pipeline, for the rest use the
-- function f
plAssign f ss cfg m@(_,n)
    | Just said <- PGU.getPGNAttr n "appid" = "App" ++ said
    | otherwise = f ss cfg m

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
            eid = ssNextEndpointId ss
            ep = EndpointUDPIPv4 {
                    edSockets = [sid],
                    edIP4Src = Nothing,
                    edIP4Dst = if ip == 0 then Nothing else Just ip,
                    edUDPSrc = Nothing,
                    edUDPDst = if port == 0 then Nothing else Just port
                }
            sd = SocketDesc { sdAppId = aid, sdEpId = eid }
            ss' = ss {
                ssNextSocketId = sid + 1,
                ssNextEndpointId = eid + 1,
                ssSockets = M.insert sid sd $ ssSockets ss,
                ssEndpoints = M.insert eid ep $ ssEndpoints ss
            }
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLn $ "SocketUDPListen p=" ++ show (ip,port) ++ " -> " ++ show sid
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid
    ssUpdateGraphs ss sstv

eventHandler sstv ch (PLA.EvSocketUDPFlow (sIp,sP) (dIp,dP)) = do
    (sid,ss) <- STM.atomically $ do
        ss <- STM.readTVar sstv
        -- TODO: check overlapping
        let Just aid = M.lookup ch $ ssAppChans ss
            sid = ssNextSocketId ss
            eid = ssNextEndpointId ss
            ep = EndpointUDPIPv4 {
                    edSockets = [sid],
                    edIP4Src =if sIp == 0 then Nothing else Just sIp,
                    edIP4Dst = if dIp == 0 then Nothing else Just dIp,
                    edUDPSrc = if sP == 0 then Nothing else Just sP,
                    edUDPDst = if dP == 0 then Nothing else Just dP
                }
            sd = SocketDesc { sdAppId = aid, sdEpId = eid }
            ss' = ss {
                ssNextSocketId = sid + 1,
                ssNextEndpointId = eid + 1,
                ssSockets = M.insert sid sd $ ssSockets ss,
                ssEndpoints = M.insert eid ep $ ssEndpoints ss
            }
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLn $ "SocketUDPFlow f=" ++ show (sIp,sP) ++ "/" ++ show (dIp,dP) ++
        " -> " ++ show sid
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid
    ssUpdateGraphs ss sstv

eventHandler sstv ch (PLA.EvSocketSpan oldsid) = do
    (sid,ss) <- STM.atomically $ do
        ss <- STM.readTVar sstv
        let Just aid = M.lookup ch $ ssAppChans ss
            sid = ssNextSocketId ss
            Just oldsd = M.lookup oldsid $ ssSockets ss
            eid = sdEpId oldsd
            Just ep = M.lookup eid $ ssEndpoints ss
            sd = SocketDesc { sdAppId = aid, sdEpId = eid }
            ep' = ep { edSockets = edSockets ep ++ [sid] }
            ss' = ss {
                ssNextSocketId = sid + 1,
                ssSockets = M.insert sid sd $ ssSockets ss,
                ssEndpoints = M.insert eid ep' $ ssEndpoints ss
            }
        STM.writeTVar sstv $ ss'
        return (sid,ss')
    putStrLn $ "SocketSpan existing=" ++ show oldsid ++ " -> " ++ show sid
    PLA.sendMessage ch $ PLA.MsgSocketInfo sid
    ssUpdateGraphs ss sstv

eventHandler sstv ch ev = do
    putStrLn $ "eventHandler " ++ show ch ++ " " ++ show ev


--------------------------------------------------------------------------------
-- Main

-- generates the necessary LPG configuration based on the limited number of
--      endpoints in socket state
lpgConfig' :: [EndpointDesc] -> StackState -> C.Configuration
lpgConfig' ll ss = [("RxL4UDPCUDPSockets", PG.CVList $ cUdpSockets)]
    where
        --eps = ll
        eps = M.elems $ ssEndpoints ss
        cUdpSockets = map (PG.CVTuple . cUdpSocket) $ eps
        cUdpSocket ed = [ PG.CVList $ map (buildSock) sids,
                          PG.CVMaybe msIP,
                          PG.CVMaybe msPort,
                          PG.CVMaybe mdIP,
                          PG.CVMaybe mdPort]
            where
                sids = edSockets ed
                msIP = PG.CVInt <$> fromIntegral <$> edIP4Src ed
                mdIP = PG.CVInt <$> fromIntegral <$> edIP4Dst ed
                msPort = PG.CVInt <$> fromIntegral <$> edUDPSrc ed
                mdPort = PG.CVInt <$> fromIntegral <$> edUDPDst ed
        buildSock sid = PG.CVTuple [PG.CVInt $ fromIntegral sid,
                                    PG.CVInt $ fromIntegral aid]
            where
                Just sd = M.lookup sid $ ssSockets ss
                aid = sdAppId sd


-- generates the necessary LPG configuration based on the stack state
-- (specifically on the state's endpoints and sockets)
lpgConfig :: StackState -> C.Configuration
lpgConfig ss = lpgConfig' allEps ss
    where
        allEps = M.elems $ ssEndpoints ss


instantiate :: (Ord a, Show a) =>
           (PG.PGraph,Sem.Helpers)                     -- | Unconf PRG + helpers
        -> String                                      -- | Name of llvm-helpers
        -> (StackState -> [EndpointDesc] -> O.CostFunction a) -- | Cost Function
        -> (OracleArgs -> [(String,C.Configuration)]) -- | Oracle
        -> (PLI.StateHandle -> C.Configuration -> IO ()) -- | Implement PRG conf
        -> (StackState -> String -> PG.PGNode -> String) -- | Assign nodes to PL
        -> IO ()
instantiate (prgU,prgHelp) llvmH costFun cfgOracle cfgImpl cfgPLA = do
    -- Prepare graphs and so on
    (lpgU,lpgHelp) <- LPG.graphH
    let helpers = prgHelp `Sem.mergeHelpers` lpgHelp
        stackname = "dragonet"
    ctx <- PLD.initialContext stackname
    stackhandle <- PLD.ctxState ctx
    sharedState <- PLI.stackState stackhandle

    -- Function to adapt graph to current stack state
    let updateGraphT x = doTimeIt "updateGraph"  $ updateGraph x
        updateGraph sstv = do
            putStrLn "updateGraph entry"
            ss <- STM.atomically $ do
                ss <- STM.readTVar sstv
                let ss' = ss { ssVersion = ssVersion ss + 1 }
                STM.writeTVar sstv ss'
                return ss'

            -- STEP: we need to create incremantal ss with adding one flow at a time
            let allEps = M.elems $ ssEndpoints ss  -- FIXME: this list should return one flow at a time
                lpgCfg = lpgConfig' allEps ss
                -- Configure LPG
                lpgC = C.applyConfig lpgCfg lpgU
                dbg = O.dbgDotfiles $ "out/graphs-tap/" ++ (show $ ssVersion ss)
                --dbg = O.dbgDummy

                -- STEP: Generate PRG configurations using ORACLE
                pCfgs = cfgOracle $ initOracleArgs lpgC allEps ss

                -- Transformations to be applied to graph before implementing it
                implTransforms = [IT.coupleTxSockets, IT.mergeSockets]

                -- LPG config is essentially all flows in network stack
            putStrLn $ "LPG config: " ++ show lpgCfg

            (plg,(_,pCfg), prgpc) <- O.optimize
                    helpers                    -- | Semantics helpers combined
                    prgU                       -- | Unconfigured PRG
                    lpgC                       -- | Configured LPG
                    implTransforms             -- | Implementation transforms
                    (plAssign cfgPLA ss)       -- | Assign nodes to pipelines
                    dbg                        -- | Debugging function
                    (costFun ss allEps)        -- | Cost function
                    pCfgs                      -- | Configurations to evaluate

            putStrLn $ "Optimization done!"

            -- apply PRG confuguration
            cfgImpl sharedState pCfg
            let createPL pl@('A':'p':'p':aids) = do
                    putStrLn $ "Creating App pipeline: " ++ pl
                    createPipelineClient agh pl
                    where
                        aid = read aids
                        Just app = M.lookup aid $ ssApplications ss
                        agh = adGraphHandle app
                createPL pl = do
                    putStrLn $ "Creating local pipeline: ##### " ++ pl
                    createPipeline plg stackname llvmH pl
            PLD.run ctx plConnect createPL $ addMuxIds plg
            putStrLn "updateGraph exit"
            -- FIXME: Create file here.
            putStrLn "################### calling appendFile with app ready notice"
            appendFile("allAppslist.appready") $ "Application is ready!\n"

        initSS = StackState {
                ssNextAppId = 1,
                ssNextSocketId = 1,
                ssNextEndpointId = 1,
                ssApplications = M.empty,
                ssAppChans = M.empty,
                ssSockets = M.empty,
                ssEndpoints = M.empty,
                ssPrevEndpoints = M.empty,
                ssUpdateGraphs = updateGraph,
                ssVersion = 0
            }

    putStrLn "Let's fire her up!"
    sstv <- STM.atomically $ STM.newTVar $ initSS
    updateGraph sstv
    putStrLn "Starting interface thread"
    PLA.interfaceThread stackname (eventHandler sstv)


--

initStackSt = StackState {
    ssNextAppId = 1,
    ssNextSocketId = 1,
    ssNextEndpointId = 1,
    ssApplications = M.empty,
    ssAppChans = M.empty,
    ssSockets = M.empty,
    ssEndpoints = M.empty,
    ssPrevEndpoints = M.empty,
    ssUpdateGraphs = undefined,
    ssVersion = 0
}

-- TODO: use this in instantiate
ssNewVer :: STM.TVar StackState -> IO (StackState)
ssNewVer sstv = STM.atomically $ do
    ss <- STM.readTVar sstv
    let ss' = ss { ssVersion = ssVersion ss + 1 }
    STM.writeTVar sstv ss'
    return ss'

-- config LPG based on stack state information
ssConfigLPG :: StackState -> PG.PGraph -> PG.PGraph
ssConfigLPG ss lpgU = C.applyConfig (lpgConfig ss) lpgU

-- a simpler version of instantiate:
--  - assumes a configured PRG and does not call optimize
--  - PRG does not change across execution
startStack :: (PG.PGraph, Sem.Helpers) -- | unconfigured LPG
           -> (PG.PGraph, Sem.Helpers) -- | configured PRG
           -> (PG.PGraph -> PG.PGraph -> PG.PGraph) -- | embedding function
           -> String -- | name of llvm-helpers
           -> (StackState -> String -> PG.PGNode -> String) -- | PL assignment
           -> IO ()
startStack (lpgU, lpgH) (prgC, prgH) embed_fn llvmH cfgPLA = do
    let mergeH = prgH `Sem.mergeHelpers` lpgH
        stackname = "dragonet"
    ctx <- PLD.initialContext stackname
    stackH <- PLD.ctxState ctx
    sharedSt <- PLI.stackState stackH

    let initSS = initStackSt { ssUpdateGraphs = updateGraph }
        updateGraph sstv = do
            putStrLn "updateGraph entry"
            ss <- ssNewVer sstv
            let lpgC = ssConfigLPG ss lpgU
                dbg ::O.DbgFunction ()
                dbg = O.dbgDotfiles $ "out/graphs-dummy/" ++ (show $ ssVersion ss)
                debug = dbg "dummy"
                implTransforms = [IT.mergeSockets]
                lbl = "dummy"
                pla = (plAssign cfgPLA ss)

            --debug "prg" $ O.DbgPGraph prgC
            plg <- O.makeGraph' mergeH prgC lpgC embed_fn implTransforms (pla lbl) debug
            error "DONE!"

            let createPL pl@('A':'p':'p':aids) = do
                    putStrLn $ "Creating App pipeline: " ++ pl
                    createPipelineClient agh pl
                    where
                        aid = read aids
                        Just app = M.lookup aid $ ssApplications ss
                        agh = adGraphHandle app
                createPL pl = do
                    putStrLn $ "Creating local pipeline: ##### " ++ pl
                    createPipeline plg stackname llvmH pl
            PLD.run ctx plConnect createPL $ addMuxIds plg
            putStrLn "updateGraph exit"

    putStrLn "Let's fire her up!!"
    sstv <- STM.atomically $ STM.newTVar $ initSS
    updateGraph sstv
    putStrLn "Starting interface thread"
    PLA.interfaceThread stackname (eventHandler sstv)



getEmptyConf = ("EmptyCOnf",
                    [ ("RxCFDirFilter", PG.CVList []),
                      ("RxC5TupleFilter", PG.CVList [])
                    ]
                )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- an update graph is executed:
--  bump the version number
--  update the prevEndpoints
--  return : new stack state, endpoints added, endpoints removed
ssExecUpd :: STM.TVar StackState -> IO (StackState, M.Map EndpointId EndpointDesc)
ssExecUpd sstv = STM.atomically $ do
    ss <- STM.readTVar sstv
    let prevEps = ssPrevEndpoints ss
        newEps  = ssEndpoints ss
        ss' = ss { ssVersion = ssVersion ss + 1, ssPrevEndpoints = newEps }
    STM.writeTVar sstv ss'
    return (ss', prevEps)

epsDiff :: [EndpointDesc] -> [EndpointDesc] -> [EndpointDesc]
epsDiff es1 es2 = S.toList $ S.difference (S.fromList es1) (S.fromList es2)

dummyImpFn :: Flow -> Bool
dummyImpFn _ = True

instantiateKK = instantiateKKwithSortedFlows dummyImpFn

instantiateKKwithSortedFlows ::
           (Flow -> Bool)                              -- | function to tell
                    -- if a flow is gold or not.  if gold, it will be moved to begining
        -> ([Flow] -> C.Configuration)  -- | get configuration
        -> (PG.PGraph,Sem.Helpers)                     -- | Unconf PRG + helpers
        -> String                                      -- | Name of llvm-helpers
        -> (PLI.StateHandle -> C.Configuration -> IO ()) -- | Implement PRG conf
        -> (StackState -> String -> PG.PGNode -> String) -- | Assign nodes to PL
        -> IO ()
instantiateKKwithSortedFlows isImp getConf (prgU,prgHelp) llvmH cfgImpl cfgPLA = do
    -- Prepare graphs and so on
    (lpgU,lpgHelp) <- LPG.graphH
    let mergeH = prgHelp `Sem.mergeHelpers` lpgHelp
        stackname = "dragonet"
    ctx <- PLD.initialContext stackname
    stackhandle <- PLD.ctxState ctx
    sharedState <- PLI.stackState stackhandle

    -- Function to adapt graph to current stack state
    let initSS = initStackSt { ssUpdateGraphs = updateGraph }
        updateGraphT x = doTimeIt "updateGraphKK"  $ updateGraph x
        updateGraph sstv = do
            putStrLn "updateGraphKK entry"
            (ss, prevEpsM) <- ssExecUpd sstv
            -- STEP: we need to create incremantal ss with adding one flow at a time
            let allEps = M.elems $ ssEndpoints ss  -- FIXME: this list should return one flow at a time

                prevEps = M.elems prevEpsM
                newEps = epsDiff allEps prevEps
                rmEps = epsDiff prevEps allEps

                lpgCfg = lpgConfig' allEps ss
                -- Configure LPG
                lpgC = C.applyConfig lpgCfg lpgU
                lbl = "kk"
                debug :: O.DbgFunction ()
                debug = O.dbgDotfiles $ "out/graphs-xxx/" ++ (show $ ssVersion ss)
                dbg = debug lbl
                --dbg = O.dbgDummy
                --
                -- Transformations to be applied to graph before implementing it
                implTransforms = [IT.coupleTxSockets, IT.mergeSockets]
                pla = (plAssign cfgPLA ss)

                -- LPG config is essentially all flows in network stack
            putStrLn $ "=====> REMOVED: " ++ (ppShow rmEps)
            putStrLn $ "=====> ADDED: " ++ (ppShow newEps)
            --putStrLn $ "LPG config: " ++ show lpgCfg

            let allFlows = map epToFlow allEps
                impList = filter isImp $ allFlows
                otherList = filter (not . isImp) $ map epToFlow allEps
                sortedFlows = impList ++  otherList
                prgConf = getConf $ sortedFlows

            --putStrLn $ "All flows are: " ++ show allFlows

            --putStrLn $ "Important flows found: " ++ show impList

            plg <- O.makeGraph mergeH prgU lpgC implTransforms (pla lbl) dbg prgConf

            -- apply PRG confuguration
            cfgImpl sharedState prgConf
            let createPL pl@('A':'p':'p':aids) = do
                    putStrLn $ "Creating App pipeline: " ++ pl
                    createPipelineClient agh pl
                    where
                        aid = read aids
                        Just app = M.lookup aid $ ssApplications ss
                        agh = adGraphHandle app
                createPL pl = do
                    putStrLn $ "Creating local pipeline: ##### " ++ pl
                    createPipeline plg stackname llvmH pl
            PLD.run ctx plConnect createPL $ addMuxIds plg
            putStrLn "updateGraph exit"
            -- FIXME: Create file here.
            putStrLn "################### calling appendFile with app ready notice"
            appendFile("allAppslist.appready") $ "Application is ready!\n"


    putStrLn "Let's fire her up!"
    sstv <- STM.atomically $ STM.newTVar $ initSS
    updateGraph sstv
    putStrLn "Starting interface thread"
    PLA.interfaceThread stackname (eventHandler sstv)
