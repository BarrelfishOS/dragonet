
module Main (main) where

import qualified DecisionTree.DecisionTree as DT
import qualified DecisionTree.ConvertDecision as CD


packetHandlingDes :: DT.Decision
packetHandlingDes = des
            where

                fullMatch =  DT.Decision {
                    DT.compute = (DT.Computation "MAClookup")
                    , DT.possibleActions = [DT.Dropped, (DT.InQueue 0)]
                }

                invertedMatch =  DT.Decision {
                    DT.compute = (DT.Computation "invertedMAClookup")
                    , DT.possibleActions = [(DT.InQueue 0), DT.Dropped]
                }

                hashMatch =  DT.Decision {
                    DT.compute = (DT.Computation "HashMAClookup")
                    , DT.possibleActions = [DT.Dropped, (DT.InQueue 0)]
                }

                actionMulticast =  DT.Decision {
                    DT.compute = (DT.Computation "isValidMulticast")
                    , DT.possibleActions = [hashMatch]
                }
                actionBroadcast =  DT.Decision {
                    DT.compute = (DT.Computation "isValidBroadcast")
                    , DT.possibleActions = [(DT.InQueue 0)]
                }
                actionUnicast =  DT.Decision {
                    DT.compute = (DT.Computation "isValidUnicast")
                    , DT.possibleActions = [fullMatch]
                }

                filterImperfect =  DT.Decision {
                    DT.compute = (DT.Computation "imperfectFilter")
                    , DT.possibleActions = [actionUnicast,
                            actionMulticast, actionBroadcast]
                }


                filterPerfect =  DT.Decision {
                    DT.compute = (DT.Computation "perfectFilter")
                    , DT.possibleActions = [fullMatch]
                }

                filterInverse =  DT.Decision {
                    DT.compute = (DT.Computation "InvertedFilter")
                    , DT.possibleActions = [invertedMatch]
                }

                filterHash =  DT.Decision {
                    DT.compute = (DT.Computation "HashFilter")
                    , DT.possibleActions = [hashMatch]
                }


                confFilterType =  DT.Decision {
                    DT.compute = (DT.Computation "selectFilteringMode")
                    , DT.possibleActions = [filterPerfect, filterImperfect,
                        filterHash, filterInverse]
                }

                actionValidateLength =  DT.Decision {
                    DT.compute = (DT.Computation "isValidLength")
                    , DT.possibleActions = [DT.Dropped, confFilterType]
                }
                des = DT.Decision {
                    DT.compute = (DT.Computation "checksumCRC")
                    , DT.possibleActions = [DT.Dropped, actionValidateLength]
                }

-- #################### Main module ####################

-- main function
main = do
        putStrLn out4
    where
        myTree = CD.convertDT packetHandlingDes
        out4 = CD.printAbstractTree myTree

