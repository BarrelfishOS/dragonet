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
--    , bind
--    , listen
--    , accept
--   , connect
--   , close
) where

import qualified NICState as NS
import qualified DecisionTree as DT
import qualified LPGModules as LPGm

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


