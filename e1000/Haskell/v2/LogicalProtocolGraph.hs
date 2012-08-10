module LogicalProtocolGraph (
    LogicalProtocolGraph1(..)
    , L4Protocol(..)
    , Application(..)
    , Socket(..)
    , PCB(..)
    , initLPG
    , createApp
    , socket
    , bind
--    , listen
--    , accept
   , connect
--   , close
--   , clone -- clones existing socket/connection into new application (fork)
) where

import qualified NICState as NS
import qualified DecisionTree as DT
import qualified LPGModules as LPGm
import qualified Data.List as DL

type PortNo = Integer
type SocketDes = Integer


data L4Protocol = TCP
        | UDP
        deriving (Show, Eq)

data Application = Application {
        appName :: String
        , appSocks :: [Socket]
    }
    deriving (Show, Eq)

data Socket = Socket {
        socketID :: SocketDes
        , apps :: [Application]
        , protocol :: L4Protocol
    }
    deriving (Show, Eq)


data PCB = PCB {
        sock :: Socket
        , portNo :: PortNo
    }
    deriving (Show, Eq)

data LogicalProtocolGraph1 = LogicalProtocolGraph1 {
        lpg :: DT.Decision
        , socketCount :: SocketDes
    }
    deriving (Show, Eq)


-- Get initial minimal functional graph
-- Process packets till classification in TCP/UDP and then drop them.
initLPG :: LogicalProtocolGraph1
initLPG = (LogicalProtocolGraph1 des 0)
        where
               actionICMP = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mICMP "ICMP")
                    , DT.possibleActions = [DT.Processed]
                  }

               actionTCP = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mTCP "TCP")
                    , DT.possibleActions = [DT.Dropped]
                  }

               actionUDP = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mUDP "UDP")
                    , DT.possibleActions = [DT.Dropped]
                  }

               actionIPv6 = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mIPv6 "IPv6")
                    , DT.possibleActions = [actionTCP,
                            actionUDP, actionICMP]
                  }

               actionIPv4 = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mIPv4 "IPv4")
                    , DT.possibleActions = [actionTCP, actionUDP, actionICMP]
                  }

               actionEthernet = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mEthernet "Ethernet")
                    , DT.possibleActions = [actionIPv4, actionIPv6]
                  }

               des = DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mNIC "NIC")
                    , DT.possibleActions = [actionEthernet]
                  }




-- create new application
createApp :: String -> Application
createApp name =
        Application {
            appName = name
            , appSocks = []
        }


socket :: LogicalProtocolGraph1 -> Application -> L4Protocol ->
                (LogicalProtocolGraph1, Socket)
socket lp app proto = (lp', sock)
            where
            sock = Socket {
                    socketID = socketCount lp
                    , apps = [app]
                    , protocol = proto
                }
            lp' = lp {
                    socketCount = (socketCount lp) + 1
                }

-- #################### Copy Decision tree into new ################

copyAction :: (String, DT.Action) -> DT.Action -> DT.Action
copyAction (modName, modAction) (DT.Error msg) = (DT.Error msg)
copyAction (modName, modAction) (DT.Processed) = (DT.Processed)
copyAction (modName, modAction) (DT.Dropped) = (DT.Dropped)
copyAction (modName, modAction) (DT.InQueue qid) = (DT.InQueue qid)
copyAction (modName, modAction) (DT.ToDecide des) =
                (DT.ToDecide (appendAction des modName modAction))

-- Copy decision data-strucutre with appending the action for given module
appendAction :: DT.Decision -> String -> DT.Action -> DT.Decision
appendAction (DT.Decision (DT.Classifier funptr fname) alist) modName pcbAction =
        if fname == modName
                    then DT.Decision (DT.Classifier funptr fname)
                        (newList ++ [pcbAction])
                    else
                        DT.Decision (DT.Classifier funptr fname) newList
    where
        -- for every decision in alist, replace it with copied decision
        newList = DL.map (copyAction (modName, pcbAction)) alist


-- Create an action entry from an Application
appToAction :: Application -> DT.Action
appToAction app =  DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mAPP (appName app))
--                    , DT.possibleActions = []
                    , DT.possibleActions = [DT.Processed]
                  }


-- Socket to action
socketToAction :: Socket -> DT.Action
socketToAction (Socket id appList proto) =
            DT.ToDecide DT.Decision {
                    -- FIXME: There will be multiple sockets, parameterize it
                    DT.selector = (DT.Classifier LPGm.mSocket
                            ("Socket_" ++ (show id)))
                    , DT.possibleActions = DL.map (appToAction) appList
                  }


decodeProtocol :: Socket -> PortNo -> (PCB, String, DT.Classifier)
decodeProtocol sock portno =
            (pcb, protoName, clasify)
            where
                pcb = (PCB sock portno)
                protoName = show (protocol sock)
                clasify = case (protocol sock) of
                    TCP -> DT.Classifier LPGm.mTCPPCB
                        ("TCPPCB" ++ "_" ++ (show portno))
                    UDP -> DT.Classifier LPGm.mUDPPCB
                        ("UDPPCB" ++ "_" ++ (show portno))

-- Find if socket is TCP or UDP
-- Find the action list for TCP/UDP
-- Create TCP/UDP PCB with port number
-- Add it to action list of TCP/UDP
bind :: LogicalProtocolGraph1 -> Socket -> PortNo -> LogicalProtocolGraph1
bind lp sock portno = lp { lpg = des' }
                       -- (LogicalProtocolGraph1 des' sList)
    where
        (pcb, protoName, classify) = decodeProtocol sock portno
        newAction = DT.ToDecide DT.Decision {
                    DT.selector = classify
                    , DT.possibleActions = [(socketToAction sock)]
                  }
        des' = appendAction (lpg lp) protoName newAction


-- Find more
locateModules :: String -> DT.Action -> [DT.Decision]
locateModules modName (DT.ToDecide des) = listModules modName des
locateModules modName _ = []

myCompare :: String -> String -> Bool
myCompare [] _ = True
myCompare x [] = False
myCompare (x:[]) (y:ys) = if x == y then True else False
myCompare (x:xs) (y:ys) = if x == y then myCompare xs ys else False


-- List all PCB connected to the decision-tree
listModules :: String -> DT.Decision  -> [DT.Decision]
listModules modName des =
    if myCompare modName (DT.funName (DT.selector des))
--    if (funName (selector des)) == modName
        then
            modList ++ [des]
        else
            modList
    where
        modList = DL.concat $
                DL.map (locateModules modName) (DT.possibleActions des)

getPortNo :: DT.Decision -> PortNo
getPortNo des = if fixedPart == "PCB_" then portno
    else
     error "not proper decision block!, only TCPPCB_X or UDPPCB_X are accepted."
    where
        modName = DT.funName (DT.selector des)
        (protoPart, rest) = DL.splitAt 3 modName
        (fixedPart, portStr) =  DL.splitAt 4 rest
        portno = read portStr :: Integer

findPort :: LogicalProtocolGraph1 -> L4Protocol -> PortNo -> Bool
findPort lp proto portno =
            DL.any (==portno) ports
        where
            blockName = if proto == TCP then ( "TCPPCB" ++ "_" )
                        else ( "UDPPCB" ++ "_" )
            pcbDecisions = listModules blockName (lpg lp)
            ports = DL.map getPortNo pcbDecisions

findFreePort :: LogicalProtocolGraph1 -> L4Protocol -> PortNo
findFreePort lp proto = DL.head $
                        DL.dropWhile (findPort lp proto) [1024 .. 65535]

-- connect call
-- Ensure that it is TCP socket
-- Find a free port
-- do everything that is done by bind
-- initiate three way handshake???
connect :: LogicalProtocolGraph1 -> Socket -> LogicalProtocolGraph1
connect lp (Socket id appList (TCP)) =
            bind lp (Socket id appList (TCP)) portno
            where
                portno = findFreePort lp TCP
connect lp (Socket id appList _ ) = error "connect called on non-TCP socket!!"


-- close :: LogicalProtocolGraph1 -> Socket -> LogicalProtocolGraph1

