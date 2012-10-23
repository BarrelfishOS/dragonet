#!/usr/bin/env runhaskell

module Elements (getElementList) where

-- module Main (main) where

import qualified DecisionTree as DT

{-
 - Create list of all modules which are possible in logical protocol graph
 -}


-- Get NIC hardware emulator module
getNICMod :: DT.Step
getNICMod = DT.Step pre post (DT.SDecision desName)
        where
            desName = "NIC"
            pre = DT.Empty
            post = DT.Condition "Ethernet"

-- Get Ethernet module
getEthernetMod :: DT.Step
getEthernetMod = DT.Step pre post (DT.SDecision desName)
        where
            desName = "Ethernet"
            pre = DT.Condition "Ethernet"
            post = DT.Or (DT.Or (DT.Condition "IPv4") (DT.Condition "IPv6"))
                    DT.Error


-- Get IPv4 capable module
getIPv4Mod :: DT.Step
getIPv4Mod = DT.Step pre post (DT.SDecision desName)
        where
            desName = "IPv4"
            pre = DT.Condition "IPv4"
            post = DT.Or (DT.Or (DT.Or (DT.Condition "ICMP")
                            (DT.Condition "UDP")) (DT.Condition "TCP"))
                            DT.Error

-- Get IPv6 capable module
getIPv6Mod :: DT.Step
getIPv6Mod = DT.Step pre post (DT.SDecision desName)
        where
            desName = "IPv6"
            pre = DT.Condition "IPv6"
            post = DT.Or (DT.Or (DT.Or (DT.Condition "ICMP")
                            (DT.Condition "UDP")) (DT.Condition "TCP"))
                            DT.Error

-- Get ICMP processing module
getICMPMod :: DT.Step
getICMPMod = DT.Step pre post (DT.SAct actName)
        where
            actName = DT.Processed
            pre = DT.Condition "ICMP"
            post = DT.Empty

-- Get TCP processing module
getTCPMod :: DT.Step
getTCPMod = DT.Step pre post (DT.SAct actName)
        where
            actName = DT.Dropped
            pre = DT.Condition "TCP"
            post = DT.Empty

-- Get UDP processing module
getUDPMod :: DT.Step
getUDPMod = DT.Step pre post (DT.SAct actName)
        where
            actName = DT.Dropped
            pre = DT.Condition "UDP"
            post = DT.Empty


getElementList :: [DT.Step]
getElementList = modList
    where
        modList = [getUDPMod, getTCPMod, getIPv6Mod, getIPv4Mod,
                    getEthernetMod, getNICMod]


-- main function
main = do
        putStrLn out1
    where
        out1 = show getElementList

-- ################################## EOF ###################################

