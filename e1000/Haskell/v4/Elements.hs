#!/usr/bin/env runhaskell

-- module Elements (getElementList) where

module Main (main) where

import qualified DecisionTree as DT

{-
 - Create list of all modules which are possible in logical protocol graph
 -}


-- Get NIC hardware emulator module
getNICMod :: DT.Module
getNICMod = DT.Module precond postcond action
        where
            precond = DT.PreCondition DT.initPrecondition
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.NIC

-- Get Ethernet module
getEthernetMod :: DT.Module
getEthernetMod = DT.Module precond postcond action
        where
            precond = DT.PreCondition DT.initPrecondition
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.Ethernet
            -- post = DT.Or (DT.Or (DT.Condition "IPv4") (DT.Condition "IPv6"))
            --        DT.Error


-- Get IPv4 capable module
getIPv4Mod :: DT.Module
getIPv4Mod = DT.Module precond postcond action
        where
            precond = DT.PreCondition DT.initPrecondition
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.IPv4
            -- post = DT.Or (DT.Or (DT.Or (DT.Condition "ICMP")
            --                (DT.Condition "UDP")) (DT.Condition "TCP"))
            --                DT.Error

-- Get IPv6 capable module
getIPv6Mod :: DT.Module
getIPv6Mod = DT.Module precond postcond action
        where
            precond = DT.PreCondition DT.initPrecondition
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.IPv6
            -- post = DT.Or (DT.Or (DT.Or (DT.Condition "ICMP")
            --                (DT.Condition "UDP")) (DT.Condition "TCP"))
            --                DT.Error

-- Get ICMP processing module
getICMPMod :: DT.Module
getICMPMod = DT.Module precond postcond action
        where
            precond = DT.PreCondition DT.initPrecondition
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.ICMP
            -- post = DT.Empty

-- Get TCP processing module
getTCPMod :: DT.Module
getTCPMod = DT.Module precond postcond action
        where
            precond = DT.PreCondition DT.initPrecondition
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.TCP
            -- post = DT.Empty

-- Get UDP processing module
getUDPMod :: DT.Module
getUDPMod = DT.Module precond postcond action
        where
            precond = DT.PreCondition DT.initPrecondition
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.UDP
            -- actName = DT.Dropped
            -- post = DT.Empty


getElementList :: [DT.Module]
getElementList = modList
    where
        modList = [getUDPMod, getTCPMod, getIPv6Mod, getIPv4Mod,
                    getEthernetMod, getNICMod]


-- #################### Main module ####################

main = do
        putStrLn out1
    where
        out1 = show getElementList

-- ################################## EOF ###################################

