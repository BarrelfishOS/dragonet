#!/usr/bin/env runhaskell
--module GraphGenerator (
module Main (
    main
    , reachDesiredAction
    , generateGraph
) where

-- module Main (main) where

import qualified DecisionTree as DT
import qualified Elements as EL

-- Current state of the system
type State = [DT.Node]

-- Checks if the action of given module is present in the
-- collective actions by group of modules
myNotElem :: [DT.Module] -> DT.Module -> Bool
myNotElem  mlist m = notElem (DT.action m) (map (DT.action) mlist)

-- filter out modules which are applicable to current state
applicableModules :: State -> [DT.Module] -> [DT.Module]
applicableModules currentState modList = availMods
    where
        -- Remove modules which perform an action
        -- which is already done in currentState
        usedMods = DT.getModLstNode currentState
        availMods = filter (myNotElem usedMods) modList

-- for given modules list and state, return
--  empty state if no module can be applied
--  new state if some module can be applied
xxx :: [DT.Module] -> State -> [State]
xxx mlist currentState = newStateList
    where
        availableMods = applicableModules currentState mlist
        selectedMods = filter(DT.applyPrecondition currentState) availableMods
        newStateList = map (DT.applyPostcondition currentState) selectedMods

-- applyRoundV2 :: available modules -> current state -> future state
applyRoundV2 :: [DT.Module] -> [State] -> [State]
applyRoundV2 mlist currentStateList = mergedStateList
    where
        newStateList = map (xxx  mlist) currentStateList
        mergedStateList = foldr (++) [[]] newStateList


-- applyRound :: available modules -> current state -> future state
applyRound :: [DT.Module] -> State -> State
applyRound mlist currentState = if ( sm == []) then
            error ("no modules matches the pre conditions\n\n" ++
                    (show mlist) ++ ("\n\n\n#####\n\n\n") ++
                    (show currentState))
        else
            -- Use one of the found module to apply postcondition
            DT.applyPostcondition currentState $ head sm
    where
        -- Remove modules which perform an action
        -- which is already done in currentState
        usedMods = DT.getModLstNode currentState
        availMods = filter (myNotElem usedMods) mlist

        -- Find all modules which satisfy current state
        sm = filter (DT.applyPrecondition currentState) availMods


-- check if all modules are used or not
checkAllModulesUsed :: [DT.Module] -> State -> Bool
checkAllModulesUsed mlist nlist = unusedActions == []
    where
        fromModules = DT.getActLstMod mlist
        fromNodes = DT.getActLstNode nlist
        unusedActions = dropWhile (== True) $
                map (flip elem fromNodes) fromModules


-- loop till you reach desired state
useAllModules :: [DT.Module] -> State -> State
useAllModules mlist startState
    | checkAllModulesUsed mlist startState = startState
    | otherwise = useAllModules mlist startState'
        where
            startState' = applyRound mlist startState


-- check if perticular action is reached
isActionReached :: State -> DT.Action ->  Bool
isActionReached currentState act = elem act fromNodes
    where
        fromNodes = DT.getActLstNode currentState

-- check if perticular action is reached
isActionReachedList :: [State] -> DT.Action -> [State]
isActionReachedList currentStateList act = filter
            (flip isActionReached act) currentStateList



-- Keep applying modules till you reach the desired state
reachDesiredAction :: [DT.Module] -> State -> DT.Action -> State
reachDesiredAction mlist nlist act
    | isActionReached nlist act = nlist
    | otherwise = reachDesiredAction mlist nlist' act
        where
            nlist' = applyRound mlist nlist

-- Keep applying modules till you reach the desired state
reachDesiredActionV2 :: [DT.Module] -> [State] -> DT.Action -> [State]
reachDesiredActionV2 mlist slist act
    | reachedAction /= [] = reachedAction
    | otherwise = reachDesiredActionV2 mlist slist' act
        where
            reachedAction = isActionReachedList slist act
            slist' = applyRoundV2 mlist slist



-- Generate graph by connecting given steps
generateGraph :: [DT.Module] -> State
generateGraph ml = useAllModules ml []


-- main function
main :: IO()
main = do
{-
        putStrLn out0
        putStrLn lineBreak
        putStrLn out1
        putStrLn lineBreak
        putStrLn out2
        putStrLn lineBreak
        putStrLn out3
        putStrLn lineBreak
-}
        putStrLn outF3
        putStrLn lineBreak
        putStrLn "Done!!"
        putStrLn lineBreak
--        putStrLn outF2
--        putStrLn lineBreak
--        putStrLn outF
    where
        lineBreak = "\n\n"
        ml = EL.getElementList
{-
        currentState = []
        out0 = "[Full list] " ++ (show ml)

        currentState1 = applyRound ml currentState
        out1 = "[R1] " ++ (show currentState1)

        currentState2 = applyRound ml currentState1
        out2 = "[R2] " ++ (show currentState2)

        currentState3 = applyRound ml currentState2
        out3 = "[R3] " ++ (show currentState3)
-}
        outF3 = "[ActionV2] " ++ (show $ reachDesiredActionV2 ml [[]]
                    (DT.NT DT.TCP))

--        outF = "[Action] " ++ (show $ reachDesiredAction ml [] (DT.NT DT.TCP))
--        outF2 = "[All] " ++ (show $ generateGraph EL.getElementList)


-- ############################# EOF ###################################

