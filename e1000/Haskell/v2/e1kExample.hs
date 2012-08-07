
module Main (main) where

import qualified Data.Word as W
import qualified Data.ByteString as BS
import qualified NICState as NS
import qualified DecisionTree as DT
import qualified StandardClassifiers as SC
import qualified ConvertDecision as CD


packetHandlingDes :: DT.Decision
packetHandlingDes = des
            where
                actionQSelect = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.applyFilter "applyFilter")
                    , DT.possibleActions = [(DT.InQueue 0), (DT.InQueue 1)]
                }
                actionUDP = DT.ToDecide DT.Decision {
                     DT.selector = (DT.Classifier SC.isValidUDP "isValidUDP")
                     , DT.possibleActions = [DT.Dropped , actionQSelect]
                }
                actionL4 = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.isValidTCP "isValidTCP")
                    , DT.possibleActions = [actionUDP , actionQSelect]
                }
                actionIPv4 = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.checksumIPv4 "checksumIPv4")
                    , DT.possibleActions = [DT.Dropped, actionL4]
                }
                actionIPv6 = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.checksumIPv6 "checksumIPv6")
                    , DT.possibleActions = [DT.Dropped, actionL4]
                }
                actionL3 = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.classifyL3 "classifyL3")
                    , DT.possibleActions = [DT.Dropped, actionIPv4, actionIPv6]
                }
                actionMulticast = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.isValidMulticast "isValidMulticast")
                    , DT.possibleActions = [DT.Dropped, actionL3]
                }
                actionBroadcast = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.isValidBroadcast "isValidBroadcast")
                    , DT.possibleActions = [DT.Dropped, actionL3]
                }
                actionUnicast = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.isValidUnicast "isValidUnicast")
                    , DT.possibleActions = [DT.Dropped, actionL3]
                }

                actionClassifyL2 = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.classifyL2 "classifyL2")
                    , DT.possibleActions = [DT.Dropped, actionUnicast,
                            actionMulticast, actionBroadcast]
                }

                actionValidateLength = DT.ToDecide DT.Decision {
                    DT.selector = (DT.Classifier SC.isValidLength "isValidLength")
                    , DT.possibleActions = [DT.Dropped, actionClassifyL2]
                }
                des = DT.Decision {
                    DT.selector = (DT.Classifier SC.checksumCRC "checksumCRC")
                    , DT.possibleActions = [DT.Dropped, actionValidateLength]
                }


-- Takes raw packet and returns associated action
classifyPacket :: NS.NICState -> DT.Packet -> DT.Action
classifyPacket nicstate pkt = DT.decide packetHandlingDes nicstate pkt


-- #################### Main module ####################

-- getNextPacket for processing:  Currently it is generated/hardcoded.
-- FIXME: Stupid packet, make it more realasitic
getNextPacket :: DT.Packet
getNextPacket = DT.L3Packet $ DT.RawPacket $ BS.pack ([50..120] :: [W.Word8])

-- main function
main = do
--        putStrLn out1
--        putStrLn out2
--        putStrLn out3
        putStrLn out4
    where
        nicState = NS.updateQueueElement NS.initNICState 6 1 1
--        out4 = show nicState
        out1 = show $ classifyPacket nicState getNextPacket
        out2 = show $ packetHandlingDes
        myTree = CD.convertDT packetHandlingDes
--        myTree = DT.convertDT.DT.Decision packetHandlingDes
        out3 = "\n\n\n\n\n"
        out4 = CD.printAbstractTree myTree


