#!/usr/bin/env runhaskell

--module Elements (getElementList) where

module Main (main) where

import qualified DecisionTree as DT

{-
 - Create list of all modules which are possible in logical protocol graph
 -}


-- Get NIC hardware emulator module
getNICMod :: DT.Module
getNICMod = DT.Module precond postcond action dependent
        where
            dependent = [[]]
            precond = DT.PreCondition DT.initPrecondition
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.NIC

-- Get Ethernet module
getEthernetMod :: DT.Module
getEthernetMod = DT.Module precond postcond action dependent
        where
            dependent = [[(DT.NT DT.NIC)]]
            precond = DT.PreCondition (DT.testPreCondition dependent)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.Ethernet

-- Get IPv4 capable module
getIPv4Mod :: DT.Module
getIPv4Mod = DT.Module precond postcond action dependent
        where
            dependent = [[(DT.NT DT.NIC), (DT.NT DT.Ethernet)]]
            precond = DT.PreCondition (DT.testPreCondition dependent)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.IPv4

-- Get IPv6 capable module
getIPv6Mod :: DT.Module
getIPv6Mod = DT.Module precond postcond action dependent
        where
            dependent = [[(DT.NT DT.NIC), (DT.NT DT.Ethernet)]]
            precond = DT.PreCondition (DT.testPreCondition dependent)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.IPv6

-- Get ICMP processing module
getICMPMod :: DT.Module
getICMPMod = DT.Module precond postcond action dependent
        where
            dependent = [
                [(DT.NT DT.NIC), (DT.NT DT.Ethernet), (DT.NT DT.IPv4)],
                [(DT.NT DT.NIC), (DT.NT DT.Ethernet), (DT.NT DT.IPv6)]
               ]
            precond = DT.PreCondition (DT.testPreCondition dependent)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.ICMP

-- Get TCP processing module
getTCPMod :: DT.Module
getTCPMod = DT.Module precond postcond action dependent
        where
            dependent = [
                [(DT.NT DT.NIC), (DT.NT DT.Ethernet), (DT.NT DT.IPv4)],
                [(DT.NT DT.NIC), (DT.NT DT.Ethernet), (DT.NT DT.IPv6)]
               ]
            precond = DT.PreCondition (DT.testPreCondition dependent)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.TCP

-- Get UDP processing module
getUDPMod :: DT.Module
getUDPMod = DT.Module precond postcond action dependent
        where
            dependent = [
                [(DT.NT DT.NIC), (DT.NT DT.Ethernet), (DT.NT DT.IPv4)],
                [(DT.NT DT.NIC), (DT.NT DT.Ethernet), (DT.NT DT.IPv6)]
               ]
            precond = DT.PreCondition (DT.testPreCondition dependent)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.UDP


getElementList :: [DT.Module]
getElementList = modList
    where
        modList = [getUDPMod, getTCPMod, getIPv6Mod, getIPv4Mod,
                    getEthernetMod, getNICMod]


-- #################### Main module ####################

main = do
        putStrLn out1
        putStrLn lineBreak
--        putStrLn out2
--        putStrLn lineBreak
    where
        out1 = show getElementList
        lineBreak = "\n\n\n"
-- ############################### EOF #################################

