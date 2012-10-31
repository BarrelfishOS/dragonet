#!/usr/bin/env runhaskell
{-
 - Create list of all modules which are possible in logical protocol graph
-}

module Elements (
    getElementList
    , testPreCondition
    , testExactPrecondition
    , testLoosePreCondition
    , main
) where

-- module Main (main) where

import qualified DecisionTree as DT
import qualified Data.List as DL

-- test that two lists are equal (they can have different order of elements)
testListEqual :: [DT.Action] -> [DT.Action] -> Bool
testListEqual [] [] = True
testListEqual [] _ = False
testListEqual l1 l2 = (DL.sort $ DL.nub l1) == (DL.sort $ DL.nub l2)


-- Test that nodes have exactly the same elements which are in
-- the dependency of module
-- Test that one of the precondition of module matches exactly with
-- given module configuration.
testExactPrecondition :: [[DT.Action]] -> DT.Module -> [DT.Node] -> Bool
testExactPrecondition [[]] _ [] = True
testExactPrecondition _ _ [] = False
testExactPrecondition preCondAList _ nlist = satisfiedDep /= []
    where
        pastActions = DT.getActLstNode nlist
        satisfiedDep = dropWhile (== False) $
                map (testListEqual pastActions) preCondAList


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
testLoosePreCondition :: [[DT.Action]] -> DT.Module -> [DT.Node] -> Bool
testLoosePreCondition [[]] _ _ = True
testLoosePreCondition _ _ [] = False
testLoosePreCondition malist _ nlist = satisfiedDep /= []
    where
        superset = DT.getActLstNode nlist
        satisfiedDep = dropWhile (== False) $
                map (testListSubset superset) malist


-- wrapper to choose between strict or loose function
testPreCondition :: [[DT.Action]] -> DT.Module -> [DT.Node] -> Bool
--testPreCondition malist m nlist = testLoosePreCondition malist m nlist
testPreCondition malist m nlist = testExactPrecondition malist m nlist

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
            --precond = DT.PreCondition (testExactPrecondition dependent)
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

