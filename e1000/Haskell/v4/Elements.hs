#!/usr/bin/env runhaskell
{-
 - Create list of all modules which are possible in logical protocol graph
-}

module Elements (
    getElementList
    , testPreCondition
    , main
) where

-- module Main (main) where

import qualified DecisionTree as DT


-- test subset
-- for given superset and subset list, make sure that all elements
-- of subset list belongs to superset
testListSubset :: [DT.Action] -> [DT.Action] -> Bool
testListSubset _ [] = True
testListSubset [] _ = False
testListSubset superset subset = nonElements == []
    where
        nonElements = dropWhile (== True) $ map (flip elem superset) subset


-- checks over multiple lists of dependencies if anyone of them is
-- satisfied
testPreCondition :: [[DT.Action]] -> DT.Module -> [DT.Node] -> Bool
testPreCondition [[]] _ _ = True
testPreCondition _ _ [] = False
testPreCondition mlist2 _ nlist = satisfiedDep /= []
    where
        superset = DT.getActLstNode nlist
        satisfiedDep = dropWhile (== False) $
                map (testListSubset superset) mlist2


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
            precond = DT.PreCondition (testPreCondition dependent)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.Ethernet

-- Get IPv4 capable module
getIPv4Mod :: DT.Module
getIPv4Mod = DT.Module precond postcond action dependent
        where
            dependent = [[(DT.NT DT.NIC), (DT.NT DT.Ethernet)]]
            precond = DT.PreCondition (testPreCondition dependent)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.IPv4

-- Get IPv6 capable module
getIPv6Mod :: DT.Module
getIPv6Mod = DT.Module precond postcond action dependent
        where
            dependent = [[(DT.NT DT.NIC), (DT.NT DT.Ethernet)]]
            precond = DT.PreCondition (testPreCondition dependent)
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
            precond = DT.PreCondition (testPreCondition dependent)
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
            precond = DT.PreCondition (testPreCondition dependent)
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
            precond = DT.PreCondition (testPreCondition dependent)
            postcond = DT.PostCondition DT.defaultPostcondition
            action = DT.NT DT.UDP


getElementList :: [DT.Module]
getElementList = modList
    where
        modList = [getUDPMod, getTCPMod, getIPv6Mod, getIPv4Mod,
                    getEthernetMod, getNICMod, getICMPMod]


-- #################### Main module ####################

main :: IO()
main = do
        putStrLn out1
        putStrLn lineBreak
--        putStrLn out2
--        putStrLn lineBreak
    where
        out1 = show getElementList
        lineBreak = "\n\n\n"
-- ############################### EOF #################################

