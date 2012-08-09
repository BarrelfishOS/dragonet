module LogicalProtocolGraph (
    Application(..)
    , Socket(..)
    , GenPCB(..)
    , UDPPCB(..)
    , TCPPCB(..)
    , initLPG
    , createApp
    , createTCPSocket
    , createUDPSocket
--    , socket
    , bind
--    , listen
--    , accept
--   , connect
--   , close
) where

import qualified NICState as NS
import qualified DecisionTree as DT
import qualified LPGModules as LPGm
import qualified Data.List as DL

-- Get initial minimal functional graph
-- Process packets till classification in TCP/UDP and then drop them.
initLPG :: DT.Decision
initLPG = des
        where
        {-
               actionTCPPCB= DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mTCPPCB "TCPPCB")
                    , DT.possibleActions = [DT.Dropped]
                  }

               actionUDPPCB= DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mUDPPCB "UDPPCB")
                    , DT.possibleActions = [DT.Dropped]
                  }
-}
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


type PortNo = Integer

data TCPPCB = TCPPCB {
        portNoTCP :: PortNo
    }
    deriving (Show, Eq)

data UDPPCB = UDPPCB {
        portNoUDP :: PortNo
    }
    deriving (Show, Eq)

data GenPCB = TcpPCB  TCPPCB
            | UdpPCB UDPPCB
    deriving (Show, Eq)

data Socket = Socket {
        apps :: [Application]
        , pcb :: GenPCB
    }
    deriving (Show, Eq)

data Application = Application {
        appName :: String
        , appSocks :: [Socket]
    }
    deriving (Show, Eq)

-- create new application
createApp :: String -> Application
createApp name =
        Application {
            appName = name
            , appSocks = []
        }

-- Create a TCP socket, connected to the application
-- NOTE: It does not update the list of sockets inside application
createTCPSocket :: Application -> Socket
createTCPSocket app = Socket {
                    apps = [app]
                    , pcb = TcpPCB $ TCPPCB 0
                }

-- Create a UDP socket, connected to the application
-- NOTE: It does not update the list of sockets inside application
createUDPSocket :: Application -> Socket
createUDPSocket app = Socket {
                    apps = [app]
                    , pcb = UdpPCB $ UDPPCB 0
                }


-- #################### Copy Decision tree into new ################

copyAction :: (String, DT.Action) -> DT.Action -> DT.Action
copyAction (modName, modAction) (DT.Error msg) = (DT.Error msg)
copyAction (modName, modAction) (DT.Processed) = (DT.Processed)
copyAction (modName, modAction) (DT.Dropped) = (DT.Dropped)
copyAction (modName, modAction) (DT.InQueue qid) = (DT.InQueue qid)
copyAction (modName, modAction) (DT.ToDecide des) =
                (DT.ToDecide (copyDT des modName modAction))

-- Copy decision data-strucutre with appending the action for given module
copyDT :: DT.Decision -> String -> DT.Action -> DT.Decision
copyDT (DT.Decision (DT.Classifier funptr fname) alist) modName pcbAction =
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
socketToAction (Socket appList pcb) =
            DT.ToDecide DT.Decision {
                    -- FIXME: There will be multiple sockets, parameterize it
                    DT.selector = (DT.Classifier LPGm.mSocket "Socket")
                    , DT.possibleActions = DL.map (appToAction) appList
                  }

-- Find if socket is TCP or UDP
-- Find the action list for TCP/UDP
-- Create TCP/UDP PCB with port number
-- Add it to action list of TCP/UDP
bind :: DT.Decision -> Socket -> PortNo -> DT.Decision
bind des (Socket appList (UdpPCB udp)) portno = initLPG
bind des (Socket appList (TcpPCB tcp)) portno = des'
    where
        pcb = TcpPCB TCPPCB {portNoTCP = portno}

        des' = copyDT des "TCP" (DT.ToDecide (DT.Decision {
                    DT.selector = (DT.Classifier LPGm.mTCPPCB
                        ("TCPPCB" ++ "_" ++ (show portno)))
                    , DT.possibleActions = [(socketToAction (Socket appList (TcpPCB tcp)))]
                  } ))


