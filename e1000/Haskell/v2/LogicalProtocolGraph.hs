module LogicalProtocolGraph (
    initLPG
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
-- import qualified ConvertDecision as CD


-- LogicalProtocolGraph

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

