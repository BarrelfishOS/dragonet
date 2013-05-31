
module Main (main) where

import qualified DecisionTree.DecisionTree as DT
import qualified DecisionTree.ConvertDecision as CD


packetHandlingDes :: DT.Decision
packetHandlingDes = des
            where
                actionQSelect =  DT.Decision {
                    DT.compute = (DT.Computation "applyFilter")
                    , DT.possibleActions = [(DT.InQueue 0), (DT.InQueue 1)]
                }
                actionUDP =  DT.Decision {
                     DT.compute = (DT.Computation "isValidUDP")
                     , DT.possibleActions = [DT.Dropped , actionQSelect]
                }
                actionL4 =  DT.Decision {
                    DT.compute = (DT.Computation "isValidTCP")
                    , DT.possibleActions = [actionUDP , actionQSelect]
                }
                actionIPv4 =  DT.Decision {
                    DT.compute = (DT.Computation "checksumIPv4")
                    , DT.possibleActions = [DT.Dropped, actionL4]
                }
                actionIPv6 =  DT.Decision {
                    DT.compute = (DT.Computation "checksumIPv6")
                    , DT.possibleActions = [DT.Dropped, actionL4]
                }
                actionL3 =  DT.Decision {
                    DT.compute = (DT.Computation "classifyL3")
                    , DT.possibleActions = [DT.Dropped, actionIPv4, actionIPv6]
                }
                actionMulticast =  DT.Decision {
                    DT.compute = (DT.Computation "isValidMulticast")
                    , DT.possibleActions = [DT.Dropped, actionL3]
                }
                actionBroadcast =  DT.Decision {
                    DT.compute = (DT.Computation "isValidBroadcast")
                    , DT.possibleActions = [DT.Dropped, actionL3]
                }
                actionUnicast =  DT.Decision {
                    DT.compute = (DT.Computation "isValidUnicast")
                    , DT.possibleActions = [DT.Dropped, actionL3]
                }

                actionClassifyL2 =  DT.Decision {
                    DT.compute = (DT.Computation "classifyL2")
                    -- Ethernet_classify
                    , DT.possibleActions = [DT.Dropped, actionUnicast,
                            actionMulticast, actionBroadcast]
                }

                actionValidateLength =  DT.Decision {
                    DT.compute = (DT.Computation "isValidLength")
                    -- ethernet_validate_len
                    , DT.possibleActions = [DT.Dropped, actionClassifyL2]
                }
                des = DT.Decision {
                    DT.compute = (DT.Computation "checksumCRC")
                    -- Ethernet_validate_header
                    --, DT.compute = (DT.Computation Ethernet_validate_header)
                    , DT.possibleActions = [DT.Dropped, actionValidateLength]
                }

-- #################### Main module ####################

-- main function
main = do
        putStrLn out4
    where
        myTree = CD.convertDT packetHandlingDes
        out4 = CD.printAbstractTree myTree

