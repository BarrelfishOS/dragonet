#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Records wiring of E10k to represent PRG graph
 -
 -}

module E10kPRG(
    getE1kPRGminimal
    , getE1kPRG
    , getE1kPRGSmall
    , getTestcaseConfiguration
    , getTestcaseConfigurationSmall
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
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet "PF"
        (OP.BinaryNode ( [], [])) []


getTestcaseConfigurationSmall :: [OP.ConfWrapperType]
getTestcaseConfigurationSmall = [
        (NB.L3IPv4ValidChecksum, pf1, True)
        , ((NB.FiveTupleFilter ts1 q1), pf1, True)
        , ((NB.FiveTupleFilter ts2 q2), pf1, True)
        , ((NB.HashFilter ts3 q3), pf1, True)
    ]

    where
    pf1 = "PF"
    vf1 = "VF1"
    vf2 = "VF2"

    ts1 = NB.TupleSelector 2 2 2 2 4
    ts2 = NB.TupleSelector 6 3 1 4 2
    ts3 = NB.TupleSelector 5 7 28 13 9

    q1 = NB.Queue 1 1 NB.getDefaultBasicQueue
    q2 = NB.Queue 2 2 NB.getDefaultBasicQueue
    q3 = NB.Queue 3 3 NB.getDefaultBasicQueue
    q0 = NB.Queue 0 0 NB.getDefaultBasicQueue



getTestcaseConfiguration :: [OP.ConfWrapperType]
getTestcaseConfiguration = [
        (NB.L2EtherValidCRC, pf1, True)
        , (NB.L3IPv4ValidChecksum, pf1, True)
        , ((NB.SyncFilter q1), pf1, False)
        , ((NB.FiveTupleFilter ts1 q1), pf1, True)
        , ((NB.FiveTupleFilter ts2 q2), pf1, True)
        , ((NB.HashFilter ts3 q3), pf1, True)
    ]

    where
    pf1 = "PF"
    vf1 = "VF1"
    vf2 = "VF2"

    ts1 = NB.TupleSelector 2 2 2 2 4
    ts2 = NB.TupleSelector 6 3 1 4 2
    ts3 = NB.TupleSelector 5 7 28 13 9

    q1 = NB.Queue 1 1 NB.getDefaultBasicQueue
    q2 = NB.Queue 2 2 NB.getDefaultBasicQueue
    q3 = NB.Queue 3 3 NB.getDefaultBasicQueue
    q0 = NB.Queue 0 0 NB.getDefaultBasicQueue


getIncompletePRGNodeattr :: [NB.DesAttribute]
getIncompletePRGNodeattr = [
        NB.DesAttribute (NB.InSoft True)
        , NB.DesAttribute (NB.ResultSaved False)
        , NB.DesAttribute (NB.NeedAdaptor True)
    ]


-- Get the datatype for E10k
getE1kPRGSmall :: OP.Node
getE1kPRGSmall = etherClassified
    where

    tagname = "PF"
    dropnode = OP.getDropNode
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet tagname
        (OP.BinaryNode (
            [etherValidType],
            [dropnode]))
        []

    etherValidType = OP.getDecNode NB.L2EtherValidType tagname
        (OP.BinaryNode (
            [etherCRCconf],
            [dropnode]))
        []



    etherCRCconf = OP.getConfNode (MB.Just NB.L2EtherValidCRC) tagname
        (OP.BinaryNode (
            [validCRC],
            l2AddrCheckList))

    validCRC = OP.getDecNode NB.L2EtherValidCRC tagname
        (OP.BinaryNode (
            l2AddrCheckList,
            [dropnode]))
        getIncompletePRGNodeattr

    validAddrOptions = [NB.L2EtherValidBroadcast, NB.L2EtherValidMulticast,
        NB.L2EtherValidUnicast]

    toORop =  OP.BinaryNode ([opORL2EtherValidDest], [opORL2EtherValidDest])
    l2AddrCheckList = DL.map (\ x -> OP.getDecNode x tagname toORop []) validAddrOptions

    opORL2EtherValidDest = OP.getOperatorNode NB.OR NB.L2EtherValidDest tagname
        (OP.BinaryNode (
            [confSynFilter],
            [dropnode]))

    -- Support for sync filters
    confSynFilter = toGenFilter (NB.SyncFilter q1) queue1 [conf5TupleFilter1] tagname

    ts1 = NB.TupleSelector 0 0 0 0 0
    ts2 = NB.TupleSelector 0 0 0 0 0
    ts3 = NB.TupleSelector 0 0 0 0 0

    conf5TupleFilter1 = toGenFilter (NB.FiveTupleFilter ts1 q1) queue1 [confhashFilter] tagname

    confhashFilter = toGenFilter (NB.HashFilter ts3  q3) queue3 [queue0] tagname

    q1 = NB.Queue 1 1 NB.getDefaultBasicQueue
    queue1 = OP.getDecNode (NB.ToQueue q1) tagname
        (OP.BinaryNode ([], [])) []

    q2 = NB.Queue 2 2 NB.getDefaultBasicQueue
    queue2 = OP.getDecNode (NB.ToQueue q2) tagname
        (OP.BinaryNode ([], [])) []

    q3 = NB.Queue 3 3 NB.getDefaultBasicQueue
    queue3 = OP.getDecNode (NB.ToQueue q3) tagname
        (OP.BinaryNode ( [], [])) []

    q0 = NB.Queue 0 0 NB.getDefaultBasicQueue
    queue0 = OP.getDecNode (NB.ToQueue q0) tagname
        (OP.BinaryNode ( [], [])) []





-- Get the datatype for E10k
getE1kPRG :: OP.Node
getE1kPRG = etherClassified
    where

    tagname = "PF"
    dropnode = OP.getDropNode
    etherClassified = OP.getDecNode NB.ClassifiedL2Ethernet tagname
        (OP.BinaryNode (
            [etherValidLen],
            [dropnode]))
        [(NB.DesAttribute (NB.NeedAdaptor True))]

    etherValidLen = OP.getDecNode NB.L2EtherValidLen tagname
        (OP.BinaryNode (
            [etherCRCconf],
            [dropnode]))
        []

    etherCRCconf = OP.getConfNode (MB.Just NB.L2EtherValidCRC) tagname
        (OP.BinaryNode (
            [validCRC],
            l2AddrCheckList))

    validCRC = OP.getDecNode NB.L2EtherValidCRC tagname
        (OP.BinaryNode (
            l2AddrCheckList,
            [dropnode]))
        []

    validAddrOptions = [NB.L2EtherValidBroadcast, NB.L2EtherValidMulticast,
        NB.L2EtherValidUnicast]

    toORop =  OP.BinaryNode ([opORL2EtherValidDest], [opORL2EtherValidDest])
    l2AddrCheckList = DL.map (\ x -> OP.getDecNode x tagname toORop []) validAddrOptions

    opORL2EtherValidDest = OP.getOperatorNode NB.OR NB.L2EtherValidDest tagname
        (OP.BinaryNode (
            [etherValidType],
            [dropnode]))

    etherValidType = OP.getDecNode NB.L2EtherValidType tagname
        (OP.BinaryNode (
            [classifiedL3IPv4, classifiedL3IPv6],
            [dropnode]))
        getIncompletePRGNodeattr

    classifiedL3IPv4 = OP.getDecNode NB.ClassifiedL3IPv4 tagname
        (OP.BinaryNode (
            [ipv4ChecksumConf],
            [dropnode]))
        getIncompletePRGNodeattr

    ipv4ChecksumConf = OP.getConfNode (MB.Just NB.L3IPv4ValidChecksum) tagname
        (OP.BinaryNode (
            [ipv4Checksum],
            [l3IPv4ValidProto]))

    ipv4Checksum = OP.getDecNode NB.L3IPv4ValidChecksum tagname
        (OP.BinaryNode (
            [l3IPv4ValidProto],
            [dropnode]))
        []

    l3IPv4ValidProto = OP.getDecNode NB.L3IPv4ValidProtocol tagname
        (OP.BinaryNode (
            [opORclassifiedL3],
            [opORclassifiedL3]))
        []

    classifiedL3IPv6 = OP.getDecNode NB.ClassifiedL3IPv6 tagname
        (OP.BinaryNode (
            [l3IPv6ValidProto],
            [dropnode]))
        getIncompletePRGNodeattr

    l3IPv6ValidProto = OP.getDecNode NB.L3IPv6ValidProtocol tagname
        (OP.BinaryNode (
            [opORclassifiedL3],
            [opORclassifiedL3]))
        []

    opORclassifiedL3 = OP.getOperatorNode NB.OR NB.ClassifiedL3 tagname
        (OP.BinaryNode (
            [classifiedUDP, classifiedTCP, classifiedICMP, unclassfiedL4],
            [dropnode]))


    classifiedUDP = OP.getDecNode NB.ClassifiedL4UDP tagname
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))
        getIncompletePRGNodeattr

    classifiedTCP = OP.getDecNode NB.ClassifiedL4TCP tagname
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))
        getIncompletePRGNodeattr

    classifiedICMP = OP.getDecNode NB.ClassifiedL4ICMP tagname
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))
        getIncompletePRGNodeattr

    unclassfiedL4 = OP.getDecNode NB.UnclassifiedL4 tagname
        (OP.BinaryNode (
            [opORl4ReadyToClassify],
            [opORl4ReadyToClassify]))
        getIncompletePRGNodeattr

    opORl4ReadyToClassify = OP.getOperatorNode NB.OR NB.ClassifiedL4 tagname
        (OP.BinaryNode (
--            [genFilter, confFilter1, confFilter2, confFilter3],
--            [genFilter] ++ fList,
            [confSynFilter],
            [dropnode]))

    -- Support for sync filters
    confSynFilter = toGenFilter (NB.SyncFilter q1) queue1 [conf5TupleFilter1] tagname

    ts1 = NB.TupleSelector 0 0 0 0 0
    ts2 = NB.TupleSelector 0 0 0 0 0
    ts3 = NB.TupleSelector 0 0 0 0 0

    conf5TupleFilter1 = toGenFilter (NB.FiveTupleFilter ts1 q1) queue1 [conf5TupleFilter2] tagname
    conf5TupleFilter2 = toGenFilter (NB.FiveTupleFilter ts2 q2) queue2 [confhashFilter] tagname

    confhashFilter = toGenFilter (NB.HashFilter ts3  q3) queue3 [queue0] tagname

    q1 = NB.Queue 1 1 NB.getDefaultBasicQueue
    queue1 = OP.getDecNode (NB.ToQueue q1) tagname
        (OP.BinaryNode ([], [])) []

    q2 = NB.Queue 2 2 NB.getDefaultBasicQueue
    queue2 = OP.getDecNode (NB.ToQueue q2) tagname
        (OP.BinaryNode ([], [])) []

    q3 = NB.Queue 3 3 NB.getDefaultBasicQueue
    queue3 = OP.getDecNode (NB.ToQueue q3) tagname
        (OP.BinaryNode ( [], [])) []

    q0 = NB.Queue 0 0 NB.getDefaultBasicQueue
    queue0 = OP.getDecNode (NB.ToQueue q0) tagname
        (OP.BinaryNode ( [], [])) []



{-
    OP.getConfNode  (MB.Just (NB.SyncFilter q1)) tagname
        (OP.BinaryNode (
            [synFilter],
            []))

    synFilter = OP.getDecNode NB.SyncFilter tagname
        (OP.BinaryNode (
            [defaultQueue],
            []) [])
-}

--  toGenFilter <Type of Queue> <queueNode ToGo when true> <NodeList to go when false>
toGenFilter :: NB.NetOperation -> OP.Node -> [OP.Node] -> OP.TagType -> OP.Node
toGenFilter filtType qNode ifFalse tagname = OP.getConfNode (MB.Just filtType) tagname
        (OP.BinaryNode ( [genFilter], ifFalse))
    where
    genFilter = OP.getDecNode filtType tagname
        (OP.BinaryNode ( [qNode], ifFalse)) []

--    q = case qNode of
--        OP.Des (OP.Decision (OP.GNode (NB.DesLabel (NB.ToQueue x)) _ _ _ _ )) -> x
--        otherwise -> error "E10kPRG.toGenFilter nonQueue element passed to filter True side"

{-
    genFilter = OP.getDecNode (NB.IsFlow (NB.getDefaultFitlerForID 0)) tagname
        (OP.BinaryNode (
            [defaultQueue],
            []) [] )

    defaultQueue = OP.getDecNode (NB.ToQueue NB.getDefaultQueue) tagname
        (OP.BinaryNode (
            [],
            []) [] )

    filterConfList = [(1, 1, 1), (2, 2, 2), (3, 3, 3)]
    fList = DL.map (\ (f, q, c) -> getConfFilterQueue f q c) filterConfList
-}

{-
getConfFilterQueue :: NB.FilterID -> NB.Qid -> NB.CoreID -> OP.Node
getConfFilterQueue fid qid cid = confFilter
    where
    confFilter = OP.getConfNode ("filter" ++ show fid) tagname
        (OP.BinaryNode (
            [f],
            []))

    f = OP.getDecNode (NB.IsFlow (NB.getDefaultFitlerForID fid)) tagname
        (OP.BinaryNode (
            [queue],
            []) [])

    q = NB.Queue qid cid NB.getDefaultBasicQueue
    queue = OP.getDecNode (NB.ToQueue q) tagname
        (OP.BinaryNode (
            [],
            []) [] )


-}

