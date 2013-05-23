module LogicalProtocolGraph (
    LogicalProtocolGraph1(..)
    , L4Protocol(..)
    , Application(..)
    , Socket(..)
    , PCB(..)
    , initLPG
    , initLPGExpand
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
import qualified Data.List as DL
import Debug.Trace

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

socketFixedPart :: String
socketFixedPart = "Socket_"

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
initLPGExpand :: LogicalProtocolGraph1
initLPGExpand = (LogicalProtocolGraph1 des 0)
        where
               actionICMP = DT.Decision {
                    DT.compute = (DT.Computation "ICMP")
                    , DT.possibleActions = [DT.Processed]
                  }

               -- UDP

               actionUDPValHeader = DT.Decision {
                    DT.compute = (DT.Computation "UDP_validate_header")
                    , DT.possibleActions = [DT.Dropped, actionUDPValChecksum]
                  }

               actionUDPValChecksum = DT.Decision {
                    DT.compute = (DT.Computation "UDP_validate_checksum")
                    , DT.possibleActions = [DT.Dropped, actionUDPClassify]
                  }
               actionUDPClassify = DT.Decision {
                    DT.compute = (DT.Computation "UDP_classify")
                    , DT.possibleActions = [DT.Dropped]
                  }

               -- TCP

               actionTCPValHeader = DT.Decision {
                    DT.compute = (DT.Computation "TCP_validate_header")
                    , DT.possibleActions = [DT.Dropped, actionTCPValState]
                  }

               actionTCPValState = DT.Decision {
                    DT.compute = (DT.Computation "TCP_validate_state")
                    , DT.possibleActions = [DT.Dropped, actionTCPClassify]
                  }
               actionTCPClassify = DT.Decision {
                    DT.compute = (DT.Computation "TCP_classify")
                    , DT.possibleActions = [DT.Dropped]
                  }

               actionIPv4 = (ip 4)
               actionIPv6 = (ip 6)

               actionEthernetLen = DT.Decision {
                    DT.compute = (DT.Computation "Ethernet_validate_len")
                    , DT.possibleActions = [actionEthernetHdr]
                  }
               actionEthernetHdr = DT.Decision {
                    DT.compute = (DT.Computation "Ethernet_validate_header")
                    , DT.possibleActions = [actionEthernetMAC]
                  }
               actionEthernetMAC = DT.Decision {
                    DT.compute = (DT.Computation "Ethernet_validate_mac")
                    , DT.possibleActions = [actionEthernetClassify]
                  }
               actionEthernetClassify = DT.Decision {
                    DT.compute = (DT.Computation "Ethernet_classify")
                    , DT.possibleActions = [actionIPv4, actionIPv6]
                  }

               des = DT.Decision {
                    DT.compute = (DT.Computation "NIC")
                    , DT.possibleActions = [actionEthernetLen]
                  }

               -- wrapper for IP versions
               ip = (
                 \v -> DT.Decision {
                   DT.compute = (DT.Computation ((ipstr v) ++ "_validate_header"))
                   , DT.possibleActions = [DT.Dropped,
                      DT.Decision {
                         DT.compute = (DT.Computation ((ipstr v) ++ "_checksum"))
                         , DT.possibleActions = [DT.Dropped,
                                                 DT.Decision {
                                                   DT.compute = (DT.Computation ((ipstr v) ++ "_classify"))
                                                   , DT.possibleActions = [DT.Dropped, actionTCPValHeader, actionUDPValHeader, actionICMP]
                                                   }
                                                ]
                         }
                      ]
                   }

                 )

               ipstr = (\v -> "IPv" ++ (show v))




-- Get initial minimal functional graph
-- Process packets till classification in TCP/UDP and then drop them.
initLPG :: LogicalProtocolGraph1
initLPG = (LogicalProtocolGraph1 des 0)
        where
               actionICMP = DT.Decision {
                    DT.compute = (DT.Computation "ICMP")
                    , DT.possibleActions = [DT.Processed]
                  }

               actionTCP = DT.Decision {
                    DT.compute = (DT.Computation "TCP_classify")
                    , DT.possibleActions = [DT.Dropped]
                  }

               actionUDP = DT.Decision {
                    DT.compute = (DT.Computation "UDP_classify")
                    , DT.possibleActions = [DT.Dropped]
                  }

               actionIPv6 = DT.Decision {
                    DT.compute = (DT.Computation "IPv6")
                    , DT.possibleActions = [actionTCP, actionUDP, actionICMP]
                  }

               actionIPv4 = DT.Decision {
                    DT.compute = (DT.Computation "IPv4")
                    , DT.possibleActions = [actionTCP, actionUDP, actionICMP]
                  }

               actionEthernet = DT.Decision {
                    DT.compute = (DT.Computation "Ethernet")
                    , DT.possibleActions = [actionIPv4, actionIPv6]
                  }

               des = DT.Decision {
                    DT.compute = (DT.Computation "NIC")
                    , DT.possibleActions = [actionEthernet]
                  }



-- create new application
createApp :: String -> Application
createApp name =
        Application {
            appName = name
            , appSocks = []
        }


getSocketID :: DT.Decision -> SocketDes
getSocketID des = if fixedPart == socketFixedPart then sockid
       else error "not proper decision block, only Socket_ is accepted"
    where
        modName = DT.name (DT.compute des)
        (fixedPart, rest) = DL.splitAt (DL.length socketFixedPart) modName
        sockid = read rest :: Integer


findSocket :: LogicalProtocolGraph1 -> SocketDes -> Bool
findSocket lp sockID =
            DL.any (==sockID) valueList
        where
            blockName = socketFixedPart
            decisionList = listModules (beginWith) blockName (lpg lp)
            valueList = DL.map getSocketID decisionList


findFreeSocket :: LogicalProtocolGraph1 -> SocketDes
findFreeSocket lp = DL.head $
                        DL.dropWhile (findSocket lp) [0 .. 1024]

socket :: LogicalProtocolGraph1 -> Application -> L4Protocol ->
                (LogicalProtocolGraph1, Socket)
socket lp app proto = trace ("--socket app:" ++  (show app) ++ "proto" ++  (show proto))
                      (lp', sock)
  where
            sock = Socket {
                    socketID = socketCount lp
--                    socketID = findFreeSocket lp
                    , apps = [app]
                    , protocol = proto
                }
            lp' = lp {
                    socketCount = (socketCount lp) + 1
                }

-- #################### Copy Decision tree into new ################

copyAction :: (String, DT.Decision) -> DT.Decision -> DT.Decision
copyAction (modName, modAction) (DT.Error msg) = (DT.Error msg)
copyAction (modName, modAction) (DT.Processed) = (DT.Processed)
copyAction (modName, modAction) (DT.Dropped) = (DT.Dropped)
copyAction (modName, modAction) (DT.InQueue qid) = (DT.InQueue qid)
copyAction (modName, modAction) (DT.Decision des pa) =
           appendAction (DT.Decision des pa) modName modAction

-- Copy decision data-strucutre with appending the action for given module
appendAction :: DT.Decision -> String -> DT.Decision -> DT.Decision
appendAction (DT.Error msg) _ _= (DT.Error msg)
appendAction (DT.Processed) _ _= (DT.Processed)
appendAction (DT.Dropped) _ _= (DT.Dropped)
appendAction (DT.InQueue qid) _ _= (DT.InQueue qid)
appendAction (DT.Decision (DT.Computation cname) dlist) modName toAdd =
       if cname == modName
--        if cname == (["Classify"] ++ modName)
                    then DT.Decision (DT.Computation cname)
                        (newList ++ [toAdd])
                    else
                        DT.Decision (DT.Computation cname) newList
    where
        -- for every decision in alist, replace it with copied decision
        newList = DL.map (copyAction (modName, toAdd)) dlist


-- Create an action entry from an Application
appToAction :: Application -> DT.Decision
appToAction app =   DT.Decision {
                    DT.compute = (DT.Computation (appName app))
                    , DT.possibleActions = [DT.Processed]
                  }

-- Socket to action
socketToAction :: Socket -> DT.Decision
socketToAction (Socket id appList proto) = trace ("--socketToAction")
            DT.Decision {
                    -- FIXME: There will be multiple sockets, parameterize it
                    DT.compute = (DT.Computation
                            (socketFixedPart ++ (show id)))
                    , DT.possibleActions = DL.map (appToAction) appList
                  }


decodeProtocol :: Socket -> PortNo -> (PCB, String, DT.Computation)
decodeProtocol sock portno = trace ("--decodeProtocol " ++ protoName ++ (show clasify))
            (pcb, protoName, clasify)
            where
                pcb = (PCB sock portno)
                protoName |  protoNameAsStr == "TCP"      = "TCP_classify"
                          |  protoNameAsStr == "UDP"      = "UDP_classify"
                          |  otherwise  = error "wrong protocol name"
                protoNameAsStr = show (protocol sock)
                clasify = case (protocol sock) of
                    TCP -> DT.Computation ("TCPPCB" ++ "_" ++ (show portno))
                    UDP -> DT.Computation ("UDPPCB" ++ "_" ++ (show portno))

-- Find if socket is TCP or UDP
-- Find the action list for TCP/UDP
-- Create TCP/UDP PCB with port number
-- Add it to action list of TCP/UDP
bind :: LogicalProtocolGraph1 -> Socket -> PortNo -> LogicalProtocolGraph1
bind lp sock portno = lp { lpg = des' }
    where
        (pcb, protoName, classify) = decodeProtocol sock portno
        newAction = DT.Decision {
                    DT.compute = classify
                    , DT.possibleActions = [(socketToAction sock)]
                  }
        des' = appendAction (lpg lp) protoName newAction



compareWith :: String -> String -> Bool
compareWith x y = (x == y)

beginWith :: String -> String -> Bool
beginWith [] _ = True
beginWith x [] = False
beginWith (x:[]) (y:ys) = if x == y then True else False
beginWith (x:xs) (y:ys) = if x == y then beginWith xs ys else False

type Comparator = (String -> String -> Bool)

-- Find more
locateModules :: Comparator -> String -> DT.Decision -> [DT.Decision]
locateModules compare modName (DT.Decision (DT.Computation cname) dlist) =
                        listModules compare modName
                                    (DT.Decision (DT.Computation cname) dlist)
locateModules compare modName _ = []


-- List all PCB connected to the decision-tree
listModules :: Comparator -> String -> DT.Decision -> [DT.Decision]
listModules compare modName des =
    if compare modName (DT.name (DT.compute des))
        then
            modList ++ [des]
        else
            modList
    where
        modList = DL.concat $
            DL.map (locateModules compare modName) (DT.possibleActions des)

getPortNo :: DT.Decision -> PortNo
getPortNo des = if fixedPart == "PCB_" then portno
    else
     error "not proper decision block!, only TCPPCB_X or UDPPCB_X are accepted."
    where
        modName = DT.name (DT.compute des)
        (protoPart, rest) = DL.splitAt 3 modName
        (fixedPart, portStr) =  DL.splitAt 4 rest
        portno = read portStr :: Integer

findPort :: LogicalProtocolGraph1 -> L4Protocol -> PortNo -> Bool
findPort lp proto portno =
            DL.any (==portno) ports
        where
            blockName = if proto == TCP then ( "TCPPCB" ++ "_" )
                        else ( "UDPPCB" ++ "_" )
            pcbDecisions = listModules (beginWith) blockName (lpg lp)
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


-- Find appropriate protocol block (TCP/UDP)
-- Find pcb block
-- remove this pcb block from list of available actions in protocol block.
close :: LogicalProtocolGraph1 -> Socket -> LogicalProtocolGraph1
close lp sock = lp


-- Get initial minimal but expanded functional graph
-- Process packets till classification in TCP/UDP and then drop them.
-- SK: What is the difference between processed and actions? Why is there initLPGExpand and initLPG
-- initLPGExpand :: LogicalProtocolGraph1
-- initLPGExpand = (LogicalProtocolGraph1 des 0)
--         where
--                cProcessICMP = DT.Decision {
--                     DT.compute = (DT.Computation "ICMP")
--                     , DT.possibleActions = [DT.Processed]
--                   }

--                cProcessTCP = DT.Decision {
--                     DT.compute = (DT.Computation "TCP_classify")
--                     , DT.possibleActions = [DT.Dropped]
--                   }

--                cProcessUDPHeader = DT.Decision {
--                     DT.compute = (DT.Computation "UDP_validate_headers")
--                     , DT.possibleActions = [DT.Dropped, cProcessUDPChecksum]
--                   }

--                cProcessUDPChecksum = DT.Decision {
--                     DT.compute = (DT.Computation "UDP_validate_checksum")
--                     , DT.possibleActions = [DT.Dropped, cProcessUDPClassify]
--                   }

--                cProcessUDPClassify = DT.Decision {
--                     DT.compute = (DT.Computation "UDP_classify")
--                     , DT.possibleActions = [DT.Dropped]
--                   }

--                cClassifyUDP =  DT.Decision {
--                      DT.compute = (DT.Computation "ClassifyUDP")
--                      , DT.possibleActions = [DT.Dropped, cProcessUDPHeader]
--                 }
--                cClassifyTCP =  DT.Decision {
--                     DT.compute = (DT.Computation "ClassifyTCP")
--                     , DT.possibleActions = [DT.Dropped, cProcessTCP]
--                 }


--                cValidateUDP =  DT.Decision {
--                      DT.compute = (DT.Computation "isValidUDP")
--                      , DT.possibleActions = [DT.Dropped, cClassifyUDP]
--                 }
--                cValidateTCP =  DT.Decision {
--                     DT.compute = (DT.Computation "isValidTCP")
--                     , DT.possibleActions = [DT.Dropped, cClassifyTCP]
--                 }

--                 -- Layer L3 computations
--                cIPv6Processing = DT.Decision {
--                     DT.compute = (DT.Computation "IPv6")
--                     , DT.possibleActions = [cValidateTCP, cValidateUDP,
--                             cProcessICMP]
--                   }

--                cIPv4Processing = DT.Decision {
--                     DT.compute = (DT.Computation "IPv4")
--                     , DT.possibleActions = [cValidateTCP, cValidateUDP,
--                             cProcessICMP]
--                   }

--                cIPv4Checksum =  DT.Decision {
--                     DT.compute = (DT.Computation "checksumIPv4")
--                     , DT.possibleActions = [DT.Dropped, cIPv4Processing]
--                 }

--                cIPv6Checksum  =  DT.Decision {
--                     DT.compute = (DT.Computation "checksumIPv6")
--                     , DT.possibleActions = [DT.Dropped, cIPv6Processing]
--                 }

--                actionEthernet = DT.Decision {
--                     DT.compute = (DT.Computation "classifyL3")
--                     , DT.possibleActions = [cIPv4Checksum, cIPv6Checksum]
--                   }

--                 -- Layer L2 computations
--                actionMulticast =  DT.Decision {
--                     DT.compute = (DT.Computation "isValidMulticast")
--                     , DT.possibleActions = [DT.Dropped, actionEthernet]
--                 }
--                actionBroadcast =  DT.Decision {
--                     DT.compute = (DT.Computation "isValidBroadcast")
--                     , DT.possibleActions = [DT.Dropped, actionEthernet]
--                 }
--                actionUnicast =  DT.Decision {
--                     DT.compute = (DT.Computation "isValidUnicast")
--                     , DT.possibleActions = [DT.Dropped, actionEthernet]
--                 }

--                actionClassifyL2 =  DT.Decision {
--                     DT.compute = (DT.Computation "classifyL2")
--                     , DT.possibleActions = [DT.Dropped, actionUnicast,
--                             actionMulticast, actionBroadcast]
--                 }

--                actionValidateLength =  DT.Decision {
--                     DT.compute = (DT.Computation "isValidLength")
--                     , DT.possibleActions = [DT.Dropped, actionClassifyL2]
--                 }

--                cChecksum = DT.Decision {
--                     DT.compute = (DT.Computation "checksumCRC")
--                     , DT.possibleActions = [DT.Dropped, actionValidateLength]
--                 }

--                 -- NIC hardware representation
--                des = DT.Decision {
--                     DT.compute = (DT.Computation "NIC")
--                     , DT.possibleActions = [cChecksum]
--                   }




