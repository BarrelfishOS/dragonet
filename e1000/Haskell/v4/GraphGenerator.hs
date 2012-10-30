#!/usr/bin/env runhaskell

-- module Elements (getElementList) where

module Main (main) where

import qualified DecisionTree as DT
import qualified Elements as EL


-- Checks if the action of given module is present in the
-- collective actions by group of modules
myNotElem :: [DT.Module] -> DT.Module -> Bool
myNotElem  mlist m = notElem (DT.action m) (map (DT.action) mlist)


-- applyRound
applyRound :: [DT.Module] -> [DT.Node] -> [DT.Node]
applyRound mlist currentState = newState
    where
        -- Remove modules which perform an action
        -- which is already done in currentState
        usedMods = DT.getModLstNode currentState
        availMods = filter (myNotElem usedMods) mlist

        -- Find all modules which satisfy current state
        sm = filter (DT.applyPrecondition currentState) availMods

        -- Use one of the found module to apply postcondition
        newState = DT.applyPostcondition currentState $ head sm


-- check if all modules are used or not
checkAllModulesUsed :: [DT.Module] -> [DT.Node] -> Bool
checkAllModulesUsed mlist nlist = unusedActions == []
    where
        fromModules = DT.getActLstMod mlist
        fromNodes = DT.getActLstNode nlist
        unusedActions = dropWhile (== True) $
                map (flip elem fromNodes) fromModules


-- loop till you reach desired state
useAllModules :: [DT.Module] -> [DT.Node] -> [DT.Node]
useAllModules mlist nlist
    | checkAllModulesUsed mlist nlist = nlist
    | otherwise = useAllModules mlist nlist'
        where
            nlist' = applyRound mlist nlist


-- check if perticular action is reached
isActionReached :: [DT.Node] -> DT.Action ->  Bool
isActionReached nlist act = elem act fromNodes
    where
        fromNodes = DT.getActLstNode nlist


-- Keep applying modules till you reach the desired state
reachDesiredAction :: [DT.Module] -> [DT.Node] -> DT.Action -> [DT.Node]
reachDesiredAction mlist nlist act
    | isActionReached nlist act = nlist
    | otherwise = reachDesiredAction mlist nlist' act
        where
            nlist' = applyRound mlist nlist

-- Generate graph by connecting given steps
generateGraph :: [DT.Module] -> [DT.Node]
generateGraph ml = useAllModules ml []


-- main function
main :: IO()
main = do
        putStrLn out0
        putStrLn lineBreak
        putStrLn out1
        putStrLn lineBreak
        putStrLn out2
        putStrLn lineBreak
        putStrLn out3
        putStrLn lineBreak
        putStrLn outF
        putStrLn lineBreak
        putStrLn outF2
    where
        lineBreak = "\n\n"
        ml = EL.getElementList
        currentState = []
        out0 = "[Full list] " ++ (show ml)

        currentState1 = applyRound ml currentState
        out1 = "[R1] " ++ (show currentState1)

        currentState2 = applyRound ml currentState1
        out2 = "[R2] " ++ (show currentState2)

        currentState3 = applyRound ml currentState2
        out3 = "[R3] " ++ (show currentState3)

        outF = "[All] " ++ (show $ generateGraph EL.getElementList)
        outF2 = "[Action] " ++ (show $ reachDesiredAction ml [] (DT.NT DT.TCP))

-- ############################# EOF ###################################

