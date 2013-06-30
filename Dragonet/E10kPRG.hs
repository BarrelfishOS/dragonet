#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Records wiring of E10k to represent PRG graph
 -
 -}

module E10kPRG(
    getE1kPRGminimal
    , getE1kPRG
--    , getTestcaseConfiguration
) where

import qualified Data.List as DL
--import qualified Data.Set as Set

import qualified NetBasics as NB
import qualified Operations as OP
import qualified Data.Maybe as MB

-- Get simplest possible datatype for E10k (for testing purpose only)
getE1kPRGminimal :: OP.Node
getE1kPRGminimal = etherClassified
    where
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet ""
        (OP.BinaryNode (
            [],
            []))

-- Get the datatype for E10k
getE1kPRG :: OP.Node
getE1kPRG = etherClassified
    where

    dropnode = OP.getDropNode
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet ""
        (OP.BinaryNode (
            [etherValidLen],
            [dropnode]))

    etherValidLen = OP.getDecNode NB.L2EtherValidLen ""
        (OP.BinaryNode (
            [etherCRCconf],
            [dropnode]))

    etherCRCconf = OP.getConfNode (MB.Just NB.L2EtherValidCRC) ""
        (OP.BinaryNode (
            [validCRC],
            l2AddrCheckList))

    validCRC = OP.getDecNode NB.L2EtherValidCRC "PF"
        (OP.BinaryNode (
            l2AddrCheckList,
            [dropnode]))

    validAddrOptions = [NB.L2EtherValidBroadcast, NB.L2EtherValidMulticast,
        NB.L2EtherValidUnicast]

    toORop =  OP.BinaryNode ([opORL2EtherValidDest], [opORL2EtherValidDest])
    l2AddrCheckList = DL.map (\ x -> OP.getDecNode x "PF" toORop) validAddrOptions

    opORL2EtherValidDest = OP.getOperatorNode (NB.OR "L2ValidDest") ""
        (OP.BinaryNode (
            [etherValidType],
            [dropnode]))

    etherValidType = OP.getDecNode NB.L2EtherValidType "PF"
        (OP.BinaryNode (
            [classifiedL3IPv4, classifiedL3IPv6],
            [dropnode]))

    classifiedL3IPv4 = OP.getDecNode NB.ClassifiedL3IPv4 "PF"
        (OP.BinaryNode (
            [ipv4ChecksumConf],
            [dropnode]))

    ipv4ChecksumConf = OP.getConfNode (MB.Just NB.L3IPv4ValidChecksum) ""
        (OP.BinaryNode (
            [ipv4Checksum],
            [l3IPv4ValidProto]))

    ipv4Checksum = OP.getDecNode NB.L3IPv4ValidChecksum "PF"
        (OP.BinaryNode (
            [l3IPv4ValidProto],
            [dropnode]))

    l3IPv4ValidProto = OP.getDecNode NB.L3IPv4ValidProtocol "PF"
        (OP.BinaryNode (
            [opORclassifiedL3],
            [opORclassifiedL3]))

    classifiedL3IPv6 = OP.getDecNode NB.ClassifiedL3IPv6 "PF"
        (OP.BinaryNode (
            [l3IPv6ValidProto],
            [dropnode]))

    l3IPv6ValidProto = OP.getDecNode NB.L3IPv6ValidProtocol "PF"
        (OP.BinaryNode (
            [opORclassifiedL3],
            [opORclassifiedL3]))

    opORclassifiedL3 = OP.getOperatorNode (NB.OR "L3Classified") ""
        (OP.BinaryNode (
            [classifiedUDP, classifiedTCP, classifiedICMP, unclassfiedL4],
            [dropnode]))


    classifiedUDP = OP.getDecNode NB.ClassifiedL4UDP "PF"
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))

    classifiedTCP = OP.getDecNode NB.ClassifiedL4TCP "PF"
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))

    classifiedICMP = OP.getDecNode NB.ClassifiedL4ICMP "PF"
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))

    unclassfiedL4 = OP.getDecNode NB.UnclasifiedL4 "PF"
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))

    opORl4ReadyToClassify = OP.getOperatorNode (NB.OR "L4Classified") ""
        (OP.BinaryNode (
--            [genFilter, confFilter1, confFilter2, confFilter3],
--            [genFilter] ++ fList,
            [confSynFilter],
            [dropnode]))

    -- Support for sync filters
    confSynFilter = toGenFilter (NB.SyncFilter q1) queue1 [conf5TupleFilter1]

    ts1 = NB.TupleSelector 0 0 0 0 0
    ts2 = NB.TupleSelector 0 0 0 0 0
    ts3 = NB.TupleSelector 0 0 0 0 0

    conf5TupleFilter1 = toGenFilter (NB.FiveTupleFilter ts1 q1) queue1 [conf5TupleFilter2]
    conf5TupleFilter2 = toGenFilter (NB.FiveTupleFilter ts2 q2) queue2 [confhashFilter]

    confhashFilter = toGenFilter (NB.HashFilter ts3  q3) queue3 [queue0]

    q1 = NB.Queue 1 1 NB.getDefaultBasicQueue
    queue1 = OP.getDecNode (NB.ToQueue q1) ""
        (OP.BinaryNode (
            [],
            []))

    q2 = NB.Queue 2 2 NB.getDefaultBasicQueue
    queue2 = OP.getDecNode (NB.ToQueue q2) ""
        (OP.BinaryNode (
            [],
            []))

    q3 = NB.Queue 3 3 NB.getDefaultBasicQueue
    queue3 = OP.getDecNode (NB.ToQueue q3) ""
        (OP.BinaryNode (
            [],
            []))

    q0 = NB.Queue 0 0 NB.getDefaultBasicQueue
    queue0 = OP.getDecNode (NB.ToQueue q0) ""
        (OP.BinaryNode (
            [],
            []))




{-
    OP.getConfNode  (MB.Just (NB.SyncFilter q1)) "PF"
        (OP.BinaryNode (
            [synFilter],
            []))

    synFilter = OP.getDecNode NB.SyncFilter ""
        (OP.BinaryNode (
            [defaultQueue],
            []))
-}

--  toGenFilter <Type of Queue> <queueNode ToGo when true> <NodeList to go when false>
toGenFilter :: NB.NetOperation -> OP.Node -> [OP.Node] -> OP.Node
toGenFilter filtType qNode ifFalse = OP.getConfNode  (MB.Just filtType) "PF"
        (OP.BinaryNode ( [genFilter], ifFalse))
    where
    genFilter = OP.getDecNode filtType ""
        (OP.BinaryNode ( [qNode], ifFalse))

--    q = case qNode of
--        OP.Des (OP.Decision (OP.GNode (NB.DesLabel (NB.ToQueue x)) _ _ _ _ )) -> x
--        otherwise -> error "E10kPRG.toGenFilter nonQueue element passed to filter True side"

{-
    genFilter = OP.getDecNode (NB.IsFlow (NB.getDefaultFitlerForID 0)) "PF"
        (OP.BinaryNode (
            [defaultQueue],
            []))

    defaultQueue = OP.getDecNode (NB.ToQueue NB.getDefaultQueue) "PF"
        (OP.BinaryNode (
            [],
            []))

    filterConfList = [(1, 1, 1), (2, 2, 2), (3, 3, 3)]
    fList = DL.map (\ (f, q, c) -> getConfFilterQueue f q c) filterConfList
-}

{-
getConfFilterQueue :: NB.FilterID -> NB.Qid -> NB.CoreID -> OP.Node
getConfFilterQueue fid qid cid = confFilter
    where
    confFilter = OP.getConfNode ("filter" ++ show fid) ""
        (OP.BinaryNode (
            [f],
            []))

    f = OP.getDecNode (NB.IsFlow (NB.getDefaultFitlerForID fid)) "PF"
        (OP.BinaryNode (
            [queue],
            []))

    q = NB.Queue qid cid NB.getDefaultBasicQueue
    queue = OP.getDecNode (NB.ToQueue q) "PF"
        (OP.BinaryNode (
            [],
            []))


-}

