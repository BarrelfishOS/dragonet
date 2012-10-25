#!/usr/bin/env runhaskell

-- module Elements (getElementList) where

module Main (main) where

import qualified DecisionTree as DT

{-
 - Create list of all modules which are possible in logical protocol graph
 -}

-- check if given element is member of the list
-- Check only if status is still True, otherwise just return False
testMembership :: [DT.Action] -> DT.Action -> Bool -> Bool
testMembership [] _ status = status
testMembership _ _ False = False
testMembership list ele True = elem ele list
--testMembership list ele True = (filter (== ele) list) /= []

-- test subset
-- for given superset and subset list, make sure that all elements
-- of subset list belongs to superset
testListSubset :: [DT.Action] -> [DT.Action] -> Bool
testListSubset superset subset = head $ dropWhile (== True) $
                map (flip elem superset) subset
--testListSubset superset subset = foldr (testMembership subset)
--                                True superset

--
testPreCondition3 :: [DT.Action] -> DT.Module -> [DT.Node] -> Bool
testPreCondition3 mlist mod nlist = testListSubset modlist mlist
    where
        modlist = map (DT.action) $ map (DT.elem) nlist

-- checks over multiple lists of dependencies if anyone of them is
-- satisfied
testPreCondition :: [[DT.Action]] -> DT.Module -> [DT.Node] -> Bool
testPreCondition mlist2 mod nlist = head $ dropWhile (== False) $
                        map (testListSubset superset) mlist2
    where
        superset = map (DT.action) $ map (DT.elem) nlist


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
            dependency = [[(DT.NT DT.NIC)]]
            precond = DT.PreCondition (testPreCondition dependency)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.Ethernet
            -- post = DT.Or (DT.Or (DT.Condition "IPv4") (DT.Condition "IPv6"))
            --        DT.Error


-- Get IPv4 capable module
getIPv4Mod :: DT.Module
getIPv4Mod = DT.Module precond postcond action
        where
            dependency = [[(DT.NT DT.NIC), (DT.NT DT.Ethernet)]]
            precond = DT.PreCondition (testPreCondition dependency)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.IPv4
            -- post = DT.Or (DT.Or (DT.Or (DT.Condition "ICMP")
            --                (DT.Condition "UDP")) (DT.Condition "TCP"))
            --                DT.Error

-- Get IPv6 capable module
getIPv6Mod :: DT.Module
getIPv6Mod = DT.Module precond postcond action
        where
            dependency = [[(DT.NT DT.NIC), (DT.NT DT.Ethernet)]]
            precond = DT.PreCondition (testPreCondition dependency)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.IPv6
            -- post = DT.Or (DT.Or (DT.Or (DT.Condition "ICMP")
            --                (DT.Condition "UDP")) (DT.Condition "TCP"))
            --                DT.Error

-- Get ICMP processing module
getICMPMod :: DT.Module
getICMPMod = DT.Module precond postcond action
        where
            dependency = [[(DT.NT DT.NIC), (DT.NT DT.Ethernet), (DT.NT DT.IPv4)]]
            precond = DT.PreCondition (testPreCondition dependency)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.ICMP
            -- post = DT.Empty

-- Get TCP processing module
getTCPMod :: DT.Module
getTCPMod = DT.Module precond postcond action
        where
            dependency = [[(DT.NT DT.NIC), (DT.NT DT.Ethernet), (DT.NT DT.IPv4)]]
            precond = DT.PreCondition (testPreCondition dependency)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.TCP
            -- post = DT.Empty

-- Get UDP processing module
getUDPMod :: DT.Module
getUDPMod = DT.Module precond postcond action
        where
            dependency = [[(DT.NT DT.NIC), (DT.NT DT.Ethernet), (DT.NT DT.IPv4)]]
            precond = DT.PreCondition (testPreCondition dependency)
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
        putStrLn lineBreak
--        putStrLn out2
--        putStrLn lineBreak
    where
        out1 = show getElementList
        lineBreak = "\n\n\n"
-- ############################### EOF #################################

