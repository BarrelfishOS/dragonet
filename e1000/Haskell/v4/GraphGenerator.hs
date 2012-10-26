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

-- check if perticular action is reached
isActionReached :: [DT.Node] -> DT.Action ->  Bool
isActionReached nlist act = elem act fromNodes
    where
        fromNodes = DT.getActLstNode nlist


-- loop till you reach desired state
keepWorking :: [DT.Module] -> [DT.Node] -> [DT.Node]
keepWorking mlist nlist
    | checkAllModulesUsed mlist nlist = nlist
    | otherwise = keepWorking mlist nlist'
        where
            nlist' = applyRound mlist nlist



-- Generate graph by connecting given steps
generateGraph :: [DT.Module] -> [DT.Node]
generateGraph ml = keepWorking ml []

-- main function
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
    where
        lineBreak = "\n\n"
        ml = EL.getElementList
        currentState = []
        out0 = "[Full list] " ++ (show ml)

        currentState1 = applyRound ml currentState
        out1 = "[R1] " ++ (show currentState1)

        currentState2 = applyRound ml currentState1
        out2 = "[R1] " ++ (show currentState2)

        currentState3 = applyRound ml currentState2
        out3 = "[R1] " ++ (show currentState3)

        outF = show $ generateGraph EL.getElementList

-- ############################# EOF ###################################

