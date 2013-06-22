#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Physical Resource Graph (PRG) for the E1k NIC (Intel 82576 1GbE).
 - This PRG only shows the receive side and not the send side.
 - Currently, this graph captures only one of the possible combination
 - of all the possible configurations.
 - In future, this and other possible valid configurations needs to be
 - generated automatically.
 -}

--module Main (
module E1k (
    getE1kPRG
    , applyConfigList
    , purgeFixedOFFConfigs
    , purgeFixedONConfigs
    , purgeFixedConfigs
    , getE1kPRGConfTest
    , getTestcaseConfiguration
    , main
) where


import qualified MyGraph as MG
import qualified Computations as MC
--import qualified Configurations as MConf
import qualified Computations as MConf
import qualified Data.List as DL
--import qualified Debug.Trace as DT


getE1kPRGConfTest ::  IO()
getE1kPRGConfTest =
    do

        putStrLn "#################  basicPRG  ###########################\n"
        putStrLn $ show $ getE1kPRG
        putStrLn "#################  Conf ###########################\n"
        putStrLn $ show $ getTestcaseConfiguration
        putStrLn "#######################################################\n"

getTestcaseConfiguration :: [MC.ConfDecision]
getTestcaseConfiguration = [
            (MC.ConfDecision MC.L2EtherValidCRC MC.ON)
            , (MC.ConfDecision  MC.L3IPv4ValidChecksum MC.OFF)
            , (MC.ConfDecision (MC.ToQueue testQueue) MC.ON)
            , (MC.ConfDecision (MC.IsFlow f1) MC.ON)
            , (MC.ConfDecision (MC.IsPartial tcpChecksumPartial) MC.ON)
            , (MC.ConfDecision (MC.IsPartial hf1) MC.ON)
            ]
        where
        testQueue = MC.Queue 4 1
        f1 = MC.Filter 1 MC.TCP MC.anyIP (MC.toIP "192.168.2.4") MC.anyPort 80
        tcpChecksumPartial = MC.PartialComp MC.L4TCPValidChecksum
            tcpEmulatedPart
        tcpEmulatedPart = MC.IsEmulated (MC.EmulatedComp MC.L4TCPChecksumAdjustment)

        hf1 = MC.PartialComp (MC.IsFlow f4) hf1EmulatedPart
        hf1EmulatedPart = MC.IsEmulated (MC.EmulatedComp (MC.IsHashFilter f4))

        f4' = MC.getDefaultFitlerForID 4

        f4 = MC.Filter 4 MC.TCP (MC.toIP "192.168.2.4")
            (MC.toIP "192.168.2.1") MC.anyPort 23

{-
 - Find all the ConfDecision nodes which are dealing with same Computation Node
 - as input config node.
 -}
matchConfigNode :: MC.Computation -> MG.Gnode MC.Computation ->  Bool
matchConfigNode x ((MC.IsConfSet (MC.ConfDecision c _)), _)  =
    compareComputeNodesForConfig  c x
matchConfigNode _ _ = False

{-
 - Compare computations such that it will tell which two of them are same
 - for purpose of Configuration matching
 -}
compareComputeNodesForConfig :: MC.Computation -> MC.Computation -> Bool
compareComputeNodesForConfig (MC.IsFlow f1) (MC.IsFlow f2) =
    MC.filterID f1 == MC.filterID f2
compareComputeNodesForConfig (MC.IsHashFilter f1) (MC.IsHashFilter f2) =
    MC.filterID f1 == MC.filterID f2
compareComputeNodesForConfig (MC.ToQueue q1) (MC.ToQueue q2) =
    MC.queueId q1 == MC.queueId q2
compareComputeNodesForConfig (MC.IsPartial p1) (MC.IsPartial p2) =
    compareComputeNodesForConfig (MC.pComp p1) (MC.pComp p2)
        -- I am not sure if following part should be there or not.
        && compareComputeNodesForConfig (MC.pNeeds p1) (MC.pNeeds p2)
compareComputeNodesForConfig (MC.IsEmulated ec1) (MC.IsEmulated ec2) =
    compareComputeNodesForConfig (MC.eComp ec1) (MC.eComp ec2)
compareComputeNodesForConfig x y = x == y

{-
getConfigNodesInternal :: MG.Gnode MC.Computation -> [MG.Gnode MC.Computation]
getConfigNodesInternal ((MC.IsConfSet x), deps) = [(MC.IsConfSet x, deps)]
getConfigNodesInternal _ = []

getConfigNodes :: [MG.Gnode MC.Computation] -> [MG.Gnode MC.Computation]
getConfigNodes [] = []
getConfigNodes (x:xs) = (getConfigNodesInternal x) ++ getConfigNodes xs

isConfigPresent :: [MG.Gnode MC.Computation] -> MC.Computation -> [MG.Gnode MC.Computation]
isConfigPresent prg conf = DL.filter (matchConfigNode conf) prg
-}

 {-
 -  for given configuration
 -      check if configuration node is present
 -      if no,
 -          return
 -      if yes,
 -          update configuration node
 -          update its dependencies
 -}
applyConfig :: [MG.Gnode MC.Computation] -> MC.ConfDecision
    -> [MG.Gnode MC.Computation]
applyConfig prg confDes
        | matchingConfNodes == [] = prg
        | otherwise = newPRG
    where
            (MC.ConfDecision conf _) = confDes
            matchingConfNodes =  DL.filter (matchConfigNode conf) prg
            newNode = MC.IsConfSet confDes
            newPRG = DL.foldl (replaceNodesWith  newNode) prg matchingConfNodes


applyConfigList:: [MG.Gnode MC.Computation] -> [MC.ConfDecision]
    -> [MG.Gnode MC.Computation]
applyConfigList prg [] = prg
applyConfigList prg (x:xs) = applyConfigList prg' xs
    where
        prg' = applyConfig prg x


{-
 - for given oldNode and newNode, replace ocurrances of oldNode with NewNode
 - both in "single" (node, dependencies) tuple
 -}
myReplaceFn ::  MC.Computation -> MC.Computation -> MG.Gnode MC.Computation
    -> MG.Gnode MC.Computation
myReplaceFn oldX newX (n, dep) = (n', dep')
    where
        n' = if n == oldX then newX else n
        dep' = DL.map (\x -> if x == oldX then newX else x) dep


{-
 - Replace the oldNode with newNode, for all the tuples in given PRG
 -}
replaceNodesWith :: MC.Computation -> [MG.Gnode MC.Computation]
    -> MG.Gnode  MC.Computation
    -> [MG.Gnode MC.Computation]
replaceNodesWith newNode prg (oldNode, _) = DL.map (myReplaceFn oldNode newNode) prg

{-
 - Replace the given oldNode with internal Computation within the oldNode
 - for whole PRG
 -}
replaceNodeForON ::  [MG.Gnode MC.Computation] -> MG.Gnode MC.Computation
        -> [MG.Gnode MC.Computation]
replaceNodeForON prg ((MC.IsConfSet (MC.ConfDecision comp stat)), _) =
        DL.map (myReplaceFn oldNode newNode) prg
    where
    oldNode = MC.IsConfSet (MC.ConfDecision comp stat)
    newNode = comp
replaceNodeForON _ _ = error "Invalid module cropped in the replaceNodeForON"

{-
 - Get all dependencies of selected node
 -}
getAllDeps :: MC.Computation -> [MG.Gnode MC.Computation] -> [MC.Computation]
getAllDeps chosenOne list = DL.concatMap (\(_, deps) -> deps)
            $ DL.filter (\(node,_) -> node == chosenOne) list

{-
 - Remove the given node from the PRGlist
 -}
removeNode :: MC.Computation -> [MG.Gnode MC.Computation]
    -> [MG.Gnode MC.Computation]
removeNode deleteMe list = DL.filter  (\x -> (fst x) /= deleteMe ) list


{-
 - Remove the given oldNode, and modify all the nodes which were dependent on it
 - with node before oldNode
 -}
replaceNodeForOFF ::  [MG.Gnode MC.Computation] -> MG.Gnode MC.Computation
        -> [MG.Gnode MC.Computation]
replaceNodeForOFF prg ((MC.IsConfSet (MC.ConfDecision comp stat)), _)
    | DL.length allDeps == 0 = prg'
    | otherwise = prg''
    where
    oldNode = MC.IsConfSet (MC.ConfDecision comp stat)
    allDeps = getAllDeps oldNode prg
    prg' = removeNode oldNode prg
    prg'' = replaceOnlyDeps oldNode allDeps prg'
replaceNodeForOFF _ _ = (error ("unexpected node found in the list"
            ++ " which was supposed to have only config nodes"))

replaceOnlyDeps :: MC.Computation -> [MC.Computation]
        -> [MG.Gnode MC.Computation] -> [MG.Gnode MC.Computation]
replaceOnlyDeps _ _ [] = []
replaceOnlyDeps oldNode newDep (x:xs) = dps ++ replaceOnlyDeps oldNode newDep xs
    where
    (node, currentDeps) = x
    dps
        | oldNode `DL.notElem` currentDeps = [x]
        | DL.length currentDeps > 1 = (error ("AND node [" ++ show x ++
            "] is having dependency config node [" ++ show oldNode ++ "]\n"))
        | otherwise = DL.map (\nd -> (node, [nd])) newDep

-- ###############################

selectConfigNode :: MC.ConfStatus -> MG.Gnode MC.Computation ->  Bool
selectConfigNode val ((MC.IsConfSet (MC.ConfDecision _ stat)), _) = stat == val
selectConfigNode _ _ = False


{-
 - Find all the nodes which have fixed config (either ON or OFF)
 - For every node, find a replacement [nodes]
 -}
purgeFixedOFFConfigs :: [MG.Gnode MC.Computation] -> [MG.Gnode MC.Computation]
purgeFixedOFFConfigs prg = newPRG
    where
    --newPRG = prg
    selectedNodes = DL.filter (selectConfigNode MC.OFF) prg
    newPRG = DL.foldl (replaceNodeForOFF) prg selectedNodes
--    newNode = ??
--    newPRG = DL.foldl (replaceNodesWith  newNode) prg selectedNodes

{-
 -
 -}
purgeFixedONConfigs :: [MG.Gnode MC.Computation] -> [MG.Gnode MC.Computation]
purgeFixedONConfigs prg = newPRG
    where
    selectedNodes = DL.filter (selectConfigNode MC.ON) prg
    newPRG = DL.foldl (replaceNodeForON) prg selectedNodes


purgeFixedConfigs :: [MG.Gnode MC.Computation] -> [MG.Gnode MC.Computation]
purgeFixedConfigs prg = purgeFixedOFFConfigs $ purgeFixedONConfigs $ prg


{-
 - Returns list of computations which can happen in the E1k NIC
 - and their dependencies.
 -}
getE1kPRG :: [MG.Gnode MC.Computation]
getE1kPRG = [
        (MC.ClassifiedL2Ethernet, [])
        , (MC.L2EtherValidLen, [MC.ClassifiedL2Ethernet])

        , (etherChecksum, [MC.L2EtherValidLen])

        , (MC.L2EtherValidBroadcast, [etherChecksum])
        , (MC.L2EtherValidMulticast, [etherChecksum])
        , (MC.L2EtherValidUnicast, [etherChecksum])
        , (MC.L2EtherValidDest, [MC.L2EtherValidBroadcast])
        , (MC.L2EtherValidDest, [MC.L2EtherValidMulticast])
        , (MC.L2EtherValidDest, [MC.L2EtherValidUnicast])
        , (MC.L2EtherValidType, [MC.L2EtherValidDest])
        , (MC.ClassifiedL3IPv4, [MC.L2EtherValidType])
        , (ipv4Checksum, [MC.ClassifiedL3IPv4])
        , (MC.L3IPv4ValidProtocol, [ipv4Checksum])
        , (MC.ClassifiedL3IPv6, [MC.L2EtherValidType])
        , (MC.L3IPv6ValidProtocol, [MC.ClassifiedL3IPv6])
        , (MC.ClassifiedL3, [MC.L3IPv4ValidProtocol])
        , (MC.ClassifiedL3, [MC.L3IPv6ValidProtocol])
        , (MC.ClassifiedL4UDP, [MC.ClassifiedL3]) -- UDP classification

        , (tcpChecksum, [MC.ClassifiedL3]) -- TCP checksum
        , (MC.ClassifiedL4TCP, [tcpChecksum]) -- TCP classification
        , (MC.UnclasifiedL4, [MC.ClassifiedL3]) -- all other packets
        , (MC.ClassifiedL4ICMP, [MC.ClassifiedL3]) -- UDP classification


        , (l4ReadyToClassify, [MC.ClassifiedL4TCP])
        , (l4ReadyToClassify, [MC.ClassifiedL4UDP])
        , (l4ReadyToClassify, [MC.ClassifiedL4ICMP])
        , (l4ReadyToClassify, [MC.UnclasifiedL4])

        -- Filtering the packet
        , (generic_filter, [l4ReadyToClassify])

        -- some exaple filters
        , (flow1, [l4ReadyToClassify]) -- sample filter
        , (flow2, [l4ReadyToClassify]) -- sample filter
        , (flow3, [l4ReadyToClassify]) -- sample filter
        , (hf1, [l4ReadyToClassify]) -- sample filter
        , (q4, [flow1]) -- Added just to make show the queues at proper place
        , (q3, [flow2]) -- Added just to make show the queues at proper place
        , (q1, [flow3]) -- Added just to make show the queues at proper place
        , (q3, [hf1]) -- Added just to make show the queues at proper place
        , (q0, [generic_filter])
        ]
    where
        etherChecksum = (MConf.IsConfSet  (MConf.ConfDecision
             MC.L2EtherValidCRC MConf.UnConfigured))
        ipv4Checksum = (MConf.IsConfSet  (MConf.ConfDecision
             MC.L3IPv4ValidChecksum MConf.UnConfigured))


        tcpChecksum = (MConf.IsConfSet (MConf.ConfDecision
            tcpChecksumPartial  MConf.UnConfigured))
        tcpChecksumPartial = MC.IsPartial (MC.PartialComp MC.L4TCPValidChecksum
            tcpEmulatedPart)
        tcpEmulatedPart = MC.IsEmulated (MC.EmulatedComp MC.L4TCPChecksumAdjustment)

        hf1 = (MConf.IsConfSet (MConf.ConfDecision
            partialhf1 MConf.UnConfigured))
        partialhf1  = MC.IsPartial (MC.PartialComp (MC.IsFlow ff1)
            ff1EmulatedPart)
        ff1EmulatedPart = MC.IsEmulated (MC.EmulatedComp (MC.IsHashFilter ff1))
        ff1 = MC.getDefaultFitlerForID 4


        l4ReadyToClassify = MC.L4ReadyToClassify
        --l4ReadyToClassify = (MConf.IsConfSet  (MConf.ConfDecision
        --    MC.L4ReadyToClassify MConf.UnConfigured))


        q0 = MC.ToQueue (MC.Queue 0 0)
        q1 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 1 1))  MConf.UnConfigured))

        q3 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 3 3))  MConf.UnConfigured))

        q4 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 4 4))  MConf.UnConfigured))

        -- sample http server filter
        generic_filter = MC.IsFlow (MC.getDefaultFitlerForID 0)

        flow1 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 1)) MConf.UnConfigured))

        flow2 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 2)) MConf.UnConfigured))

        flow3 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 3)) MConf.UnConfigured))


{-
 - main function: used to test if the PRG generated for E1k is correct or not
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        outDot = MG.showFlowGraph getE1kPRG

