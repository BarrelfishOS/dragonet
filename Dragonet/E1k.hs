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
    , purgeFixedConfigs
    , purgeFixedENABLEConfigs
    , purgeFixedSTOPConfigs
    , purgeFixedSKIPConfigs
    , purgeUnreachableNodes
    , getE1kPRGConfTest
    , getTestcaseConfiguration
    , main
--    , compareComputeNodesForConfig
) where


--import qualified MyGraph as MG
import qualified Computations as MC
--import qualified Configurations as MConf
import qualified Computations as MConf
import qualified Data.List as DL
import qualified Debug.Trace as DT


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
            (MC.ConfDecision MC.L2EtherValidCRC MC.ENABLE)
            , (MC.ConfDecision  MC.L3IPv4ValidChecksum MC.SKIP)
            , (MC.ConfDecision (MC.ToQueue testQueue) MC.ENABLE)
            , (MC.ConfDecision (MC.IsFlow f1) MC.ENABLE)
            , (MC.ConfDecision (MC.IsPartial tcpChecksumPartial) MC.ENABLE)
            , (MC.ConfDecision (MC.IsPartial hf1) MC.ENABLE)
            , (MC.ConfDecision (MC.L2Virtualization) MC.ENABLE)
            , (MC.ConfDecision (MC.L2NOVirtualization) MC.STOP)
            ]
        where
        testQueue = MC.Queue 4 1
        f1 = MC.Filter 1 MC.TCP MC.anyIP (MC.toIP "192.168.2.4") MC.anyPort 80
        tcpChecksumPartial = MC.PartialComp MC.L4TCPValidChecksum
            tcpEmulatedPart
        tcpEmulatedPart = MC.IsEmulated (MC.EmulatedComp MC.L4TCPChecksumAdjustment)

        hf1 = MC.PartialComp (MC.IsFlow f4) hf1EmulatedPart
        hf1EmulatedPart = MC.IsEmulated (MC.EmulatedComp (MC.IsHashFilter f4))

        f4 = MC.Filter 4 MC.TCP (MC.toIP "192.168.2.4")
            (MC.toIP "192.168.2.1") MC.anyPort 23

{-
 - Find all the ConfDecision nodes which are dealing with same Computation Node
 - as input config node.
 -}
matchConfigNode :: MC.Computation -> MC.Gnode MC.Computation ->  Bool
matchConfigNode x ((MC.IsConfSet (MC.ConfDecision c _)), _)  =
    compareComputeNodesForConfig  c x
matchConfigNode x ((MC.InMode (MC.Mode _ c)), _)  = matchConfigNode x (c, [])
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
compareComputeNodesForConfig (MC.IsConfSet conf1) (MC.IsConfSet conf2) =
    compareComputeNodesForConfig (MC.dComp conf1) (MC.dComp conf2)
    && (MC.dStatus conf1) == (MC.dStatus conf1)
compareComputeNodesForConfig x y = x == y

{-
getConfigNodesInternal :: MC.Gnode MC.Computation -> [MC.Gnode MC.Computation]
getConfigNodesInternal ((MC.IsConfSet x), deps) = [(MC.IsConfSet x, deps)]
getConfigNodesInternal _ = []

getConfigNodes :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
getConfigNodes [] = []
getConfigNodes (x:xs) = (getConfigNodesInternal x) ++ getConfigNodes xs

isConfigPresent :: [MC.Gnode MC.Computation] -> MC.Computation -> [MC.Gnode MC.Computation]
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
applyConfig :: [MC.Gnode MC.Computation] -> MC.ConfDecision
    -> [MC.Gnode MC.Computation]
applyConfig prg confDes
        | matchedConfs == [] = prg
        | otherwise = newPRG
    where
            (MC.ConfDecision conf _) = confDes
            matchedConfs = DL.filter (matchConfigNode conf) prg
            newNode = MC.IsConfSet confDes
            newPRG = DL.foldl (replaceNodesWith newNode) prg matchedConfs


applyConfigList:: [MC.Gnode MC.Computation] -> [MC.ConfDecision]
    -> [MC.Gnode MC.Computation]
applyConfigList prg [] = prg
applyConfigList prg (x:xs) = applyConfigList prg' xs
    where
        prg' = applyConfig prg x



{-
 - for given oldNode and newNode, replace ocurrances of oldNode with NewNode
 - both in "single" (node, dependencies) tuple
 -}
myReplaceFn ::  MC.Computation -> MC.Computation -> MC.Gnode MC.Computation
    -> MC.Gnode MC.Computation
myReplaceFn oldX newX (n, dep) = (n', dep')
    where
        n' = if n == oldX then (adaptForMode oldX newX) else n
        dep' = DL.map (\x -> if x == oldX then (adaptForMode oldX newX) else x) dep

{-
 - Changes newNode Mode based on oldNode mode
 - Essentially, final result should always have oldNode mode
 -}
adaptForMode :: MC.Computation -> MC.Computation -> MC.Computation
adaptForMode (MC.InMode (MC.Mode om oc))  (MC.InMode (MC.Mode nm nc)) = (MC.InMode (MC.Mode om nc))
adaptForMode (MC.InMode (MC.Mode om oc)) c = (MC.InMode (MC.Mode om c))
adaptForMode x c = c


{-
 - Replace the oldNode with newNode, for all the tuples in given PRG
 -}
replaceNodesWith :: MC.Computation -> [MC.Gnode MC.Computation]
    -> MC.Gnode  MC.Computation
    -> [MC.Gnode MC.Computation]
replaceNodesWith newNode prg (oldNode, _) = DL.map (myReplaceFn oldNode newNode) prg

{-
 - Replace the given oldNode with internal Computation within the oldNode
 - for whole PRG
 -}
replaceNodeForENABLE ::  [MC.Gnode MC.Computation] -> MC.Gnode MC.Computation
        -> [MC.Gnode MC.Computation]
replaceNodeForENABLE prg ((MC.IsConfSet (MC.ConfDecision comp stat)), _) =
        DL.map (myReplaceFn oldNode newNode) prg
    where
    oldNode = MC.IsConfSet (MC.ConfDecision comp stat)
    newNode = comp
replaceNodeForENABLE prg ((MC.InMode (MC.Mode n c)), _) =
    replaceNodeForENABLE prg (c, [])
replaceNodeForENABLE _ _ = error "Invalid module cropped in the replaceNodeForENABLE"


compareComputeNodesForExchange :: MC.Computation -> MC.Computation -> Bool
compareComputeNodesForExchange (MC.IsFlow f1) (MC.IsFlow f2) =
    MC.protocol f1 == MC.protocol f2
    && MC.srcIP f1 == MC.srcIP f2
    && MC.dstIP f1 == MC.dstIP f2
    && MC.srcPort f1 == MC.srcPort f2
    && MC.dstPort f1 == MC.dstPort f2
compareComputeNodesForExchange (MC.IsHashFilter f1) (MC.IsHashFilter f2) =
    MC.protocol f1 == MC.protocol f2
    && MC.srcIP f1 == MC.srcIP f2
    && MC.dstIP f1 == MC.dstIP f2
    && MC.srcPort f1 == MC.srcPort f2
    && MC.dstPort f1 == MC.dstPort f2
compareComputeNodesForExchange (MC.ToQueue q1) (MC.ToQueue q2) =
    q1 == q2 -- FIXME: maybe I should just compare coreID's and not queueIDs
compareComputeNodesForExchange (MC.IsPartial p1) (MC.IsPartial p2) =
    compareComputeNodesForExchange (MC.pComp p1) (MC.pComp p2)
        -- I am not sure if following part should be there or not.
        && compareComputeNodesForExchange (MC.pNeeds p1) (MC.pNeeds p2)
compareComputeNodesForExchange (MC.IsEmulated ec1) (MC.IsEmulated ec2) =
    compareComputeNodesForExchange (MC.eComp ec1) (MC.eComp ec2)
compareComputeNodesForExchange (MC.IsConfSet conf1) (MC.IsConfSet conf2) =
    compareComputeNodesForExchange (MC.dComp conf1) (MC.dComp conf2)
    && (MC.dStatus conf1) == (MC.dStatus conf1)
compareComputeNodesForExchange (MC.InMode m1) (MC.InMode m2) =
   MC.mName m1 ==  MC.mName m2
   && MC.mComp m1 == MC.mComp m2
compareComputeNodesForExchange x y = x == y



{-
 - Get all dependencies of selected node
 -}
getAllDeps :: MC.Computation -> [MC.Gnode MC.Computation] -> [MC.Computation]
getAllDeps chosenOne list = DL.concatMap (\(_, deps) -> deps)
            $ DL.filter (\(node,_) -> compareComputeNodesForExchange node chosenOne) list

{-
 - Remove the given node from the PRGlist
 -}
removeNode :: MC.Computation -> [MC.Gnode MC.Computation]
    -> [MC.Gnode MC.Computation]
removeNode deleteMe list = DL.filter  (\x -> (fst x) /= deleteMe ) list


{-
 - Remove the given oldNode, and modify all the nodes which were dependent on it
 - with node before oldNode
 -}
replaceNodeForSKIP ::  [MC.Gnode MC.Computation] -> MC.Gnode MC.Computation
        -> [MC.Gnode MC.Computation]
replaceNodeForSKIP prg ((MC.IsConfSet (MC.ConfDecision comp stat)), _)
    | DL.length allDeps == 0 = prg'
    | otherwise = prg''
    where
    oldNode = MC.IsConfSet (MC.ConfDecision comp stat)
    allDeps = getAllDeps oldNode prg
    prg' = removeNode oldNode prg
    prg'' = replaceOnlyDeps oldNode allDeps prg'
replaceNodeForSKIP prg ((MC.InMode (MC.Mode n
    (MC.IsConfSet (MC.ConfDecision cc ss)))), deps)
    | DL.length allDeps == 0 = prg'
    | otherwise = prg''
    where
    origConfNode = (MC.IsConfSet (MC.ConfDecision cc ss))
    origNode = (MC.InMode (MC.Mode n origConfNode))
    allDeps = DT.trace ("replaceNodeForSkip" ++ show origNode) getAllDeps origNode prg
    prg' = removeNode origNode  prg
    prg'' = replaceOnlyDeps origNode allDeps prg'

replaceNodeForSKIP _ _ = (error ("unexpected node found in the list"
            ++ " which was supposed to have only config nodes"))

replaceOnlyDeps :: MC.Computation -> [MC.Computation]
        -> [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
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


{-
 - Find all the nodes which have fixed config (either ON or OFF)
 - For every node, find a replacement [nodes]
 -}
purgeFixedSKIPConfigs :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
purgeFixedSKIPConfigs prg = newPRG
    where
    --newPRG = prg
    selectedNodes = DL.filter (isSpecificConfigNode [MC.SKIP]) prg
    newPRG = DL.foldl (replaceNodeForSKIP) prg selectedNodes
--    newNode = ??
--    newPRG = DL.foldl (replaceNodesWith  newNode) prg selectedNodes

{-
 -
 -}
purgeFixedENABLEConfigs :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
purgeFixedENABLEConfigs prg = newPRG
    where
    selectedNodes = DL.filter (isSpecificConfigNode [MC.ENABLE]) prg
    newPRG = DL.foldl (replaceNodeForENABLE) prg selectedNodes

purgeFixedSTOPConfigs :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
purgeFixedSTOPConfigs prg = newPRG
    where
    newPRG = DL.filter (not . isSpecificConfigNode [MC.STOP]) prg

{-
 - Purge all sort of configs
 -}
purgeFixedConfigs :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
purgeFixedConfigs prg = purgeFixedSTOPConfigs $ purgeFixedSKIPConfigs $ purgeFixedENABLEConfigs $ prg


{-
 - Purges all nodes that are unreachable from given initial state
 - Also, unreachability takes into account the Configuration node.
 -}
purgeUnreachableNodes :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
    -> [MC.Gnode MC.Computation]
purgeUnreachableNodes reachable prg
    | newReachableNodes == reachable = reachable
    | otherwise = purgeUnreachableNodes (newReachableNodes) prg
    where
    newReachableNodes = DL.filter (isReachable reachable) prg


isReachable :: [MC.Gnode MC.Computation] -> MC.Gnode MC.Computation -> Bool
isReachable prg (_, deps) = (notRechableDeps == [])
    where
    notRechableDeps = DL.filter (\x -> x `DL.notElem` reachableNodes) deps
    reachableNodes = getReachableNodesList $ MC.getNodesList prg

{-
 - Get all the nonOFF nodes (nodes which are configured ON, or Undecided,
 -  or are basic nodes.
 -}
getReachableNodesList :: [MC.Computation] -> [MC.Computation]
getReachableNodesList [] = []
getReachableNodesList (x:xs)
    | not (isSpecificConfigNode [MC.STOP, MC.SKIP] (x,[])) = (x:(getReachableNodesList xs))
    | otherwise = getReachableNodesList xs


isSpecificConfigNode :: [MC.ConfStatus] -> MC.Gnode MC.Computation ->  Bool
isSpecificConfigNode val ((MC.IsConfSet (MC.ConfDecision _ stat)), _) =
    stat `DL.elem` val
isSpecificConfigNode val ((MC.InMode (MC.Mode _ c)), _) = isSpecificConfigNode val (c, [])
isSpecificConfigNode val ((MC.IsPartial (MC.PartialComp c _)), _) = isSpecificConfigNode val (c, [])
isSpecificConfigNode _ _ = False




getE1kWithMultipleModes :: [MC.Gnode MC.Computation]
getE1kWithMultipleModes = bootstrap ++ pf' ++ vf1' ++ vf2'

    where
    pfTag = "PF"
    vf1Tag = "VF1"
    vf2Tag = "VF2"
    pf = addTagToAllPRG pfTag $ getE1kVF pfTag
    vf1 = addTagToAllPRG vf1Tag $ getE1kVF vf1Tag
    vf2 = addTagToAllPRG vf2Tag $ getE1kVF vf2Tag

    pf' = [((MC.InMode (MC.Mode pfTag MC.ClassifiedL2Ethernet)) , [pNICConf])] ++ pf
    vf1' = [((MC.InMode (MC.Mode vf1Tag MC.ClassifiedL2Ethernet)) , [vNICConf])] ++ vf1
    vf2' = [((MC.InMode (MC.Mode vf2Tag  MC.ClassifiedL2Ethernet)) , [vNICConf])] ++ vf2

    vNICConf = (MConf.IsConfSet (MConf.ConfDecision
           MC.L2Virtualization MConf.Undecided))

    pNICConf = (MConf.IsConfSet (MConf.ConfDecision
           MC.L2NOVirtualization MConf.Undecided))

    bootstrap = [
        (MC.L0Tag, [])
        , (pNICConf, [MC.L0Tag])
        , (vNICConf, [MC.L0Tag])
        ]



{-
 - Adds specified tag as mode to all the nodes and their dependencies
 -}
addTagToAllPRG :: MC.ModeType -> [MC.Gnode MC.Computation] ->
    [MC.Gnode MC.Computation]
addTagToAllPRG _ [] = []
addTagToAllPRG mode (x:xs) = (currentNode:(addTagToAllPRG mode xs))
    where
    currentNode = (c', deps')
    (c, deps) = x
    c' = (MC.InMode (MC.Mode mode c))
    deps' = DL.map (\p -> (MC.InMode (MC.Mode mode p))) deps



{-
 - Returns list of computations which can happen in the E1k NIC
 - and their dependencies.
 -}
getE1kVF :: MC.ModeType -> [MC.Gnode MC.Computation]
getE1kVF mName = [
        (MC.L2EtherValidLen, [MC.ClassifiedL2Ethernet])

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

        etherChecksum = (MConf.IsConfSet
            (MConf.ConfDecision MC.L2EtherValidCRC MConf.Undecided))
        ipv4Checksum = (MConf.IsConfSet  (MConf.ConfDecision
             MC.L3IPv4ValidChecksum MConf.Undecided))

        tcpChecksum = (MConf.IsConfSet (MConf.ConfDecision
            tcpChecksumPartial  MConf.Undecided))
        tcpChecksumPartial = MC.IsPartial (MC.PartialComp MC.L4TCPValidChecksum
            tcpEmulatedPart)
        tcpEmulatedPart = MC.IsEmulated (MC.EmulatedComp MC.L4TCPChecksumAdjustment)

        hf1 = (MConf.IsConfSet (MConf.ConfDecision
            partialhf1 MConf.Undecided))
        partialhf1  = MC.IsPartial (MC.PartialComp (MC.IsFlow ff1)
            ff1EmulatedPart)
        ff1EmulatedPart = MC.IsEmulated (MC.EmulatedComp (MC.IsHashFilter ff1))
        ff1 = MC.getDefaultFitlerForID 4

        l4ReadyToClassify = MC.L4ReadyToClassify
        --l4ReadyToClassify = (MConf.IsConfSet  (MConf.ConfDecision
        --    MC.L4ReadyToClassify MConf.Undecided))


        q0 = MC.ToQueue (MC.Queue 0 0)
        q1 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 1 1))  MConf.Undecided))

        q3 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 3 3))  MConf.Undecided))

        q4 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 4 4))  MConf.Undecided))

        -- sample http server filter
        generic_filter = MC.IsFlow (MC.getDefaultFitlerForID 0)

        flow1 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 1)) MConf.Undecided))

        flow2 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 2)) MConf.Undecided))

        flow3 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 3)) MConf.Undecided))



{-
 - Returns list of computations which can happen in the E1k NIC
 - and their dependencies.
 -}

getE1kPRG :: [MC.Gnode MC.Computation]
getE1kPRG = getE1kWithMultipleModes
-- getE1kPRG = getE1kPRGV1

getE1kPRGV1 :: [MC.Gnode MC.Computation]
getE1kPRGV1 = [
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
             MC.L2EtherValidCRC MConf.Undecided))
        ipv4Checksum = (MConf.IsConfSet  (MConf.ConfDecision
             MC.L3IPv4ValidChecksum MConf.Undecided))


        tcpChecksum = (MConf.IsConfSet (MConf.ConfDecision
            tcpChecksumPartial  MConf.Undecided))
        tcpChecksumPartial = MC.IsPartial (MC.PartialComp MC.L4TCPValidChecksum
            tcpEmulatedPart)
        tcpEmulatedPart = MC.IsEmulated (MC.EmulatedComp MC.L4TCPChecksumAdjustment)

        hf1 = (MConf.IsConfSet (MConf.ConfDecision
            partialhf1 MConf.Undecided))
        partialhf1  = MC.IsPartial (MC.PartialComp (MC.IsFlow ff1)
            ff1EmulatedPart)
        ff1EmulatedPart = MC.IsEmulated (MC.EmulatedComp (MC.IsHashFilter ff1))
        ff1 = MC.getDefaultFitlerForID 4


        l4ReadyToClassify = MC.L4ReadyToClassify
        --l4ReadyToClassify = (MConf.IsConfSet  (MConf.ConfDecision
        --    MC.L4ReadyToClassify MConf.Undecided))


        q0 = MC.ToQueue (MC.Queue 0 0)
        q1 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 1 1))  MConf.Undecided))

        q3 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 3 3))  MConf.Undecided))

        q4 = (MConf.IsConfSet  (MConf.ConfDecision
           (MC.ToQueue (MC.Queue 4 4))  MConf.Undecided))

        -- sample http server filter
        generic_filter = MC.IsFlow (MC.getDefaultFitlerForID 0)

        flow1 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 1)) MConf.Undecided))

        flow2 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 2)) MConf.Undecided))

        flow3 = (MConf.IsConfSet  (MConf.ConfDecision
            (MC.IsFlow (MC.getDefaultFitlerForID 3)) MConf.Undecided))


{-
 - main function: used to test if the PRG generated for E1k is correct or not
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        outDot = show getE1kPRG

