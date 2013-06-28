#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

module PRG (
    applyConfigList
    , purgeFixedConfigs
    , purgeFixedENABLEConfigs
    , purgeFixedSTOPConfigs
    , purgeFixedSKIPConfigs
    , purgeUnreachableNodes
--    , compareComputeNodesForConfig
) where

import qualified Computations as MC
import qualified Data.List as DL
import qualified Debug.Trace as DT


{-
 - Find all the ConfDecision nodes which are dealing with same Computation Node
 - as input config node.
 -}
matchConfigNode :: MC.ModeType -> MC.Computation -> MC.Gnode MC.Computation ->  Bool
matchConfigNode tag1 x ((MC.IsConfSet (MC.ConfDecision c _)), _)  =
    compareComputeNodesForConfig c x && MC.compareModeTags tag1 MC.genericModeTag
matchConfigNode tag1 x ((MC.InMode (MC.Mode tag2 c)), _)  =
    matchConfigNode tag1 x (c, []) && MC.compareModeTags tag1 tag2
matchConfigNode _ _ _ = False


isSpecificConfigNode :: [MC.ConfStatus] -> MC.Gnode MC.Computation ->  Bool
isSpecificConfigNode val ((MC.IsConfSet (MC.ConfDecision _ stat)), _) =
    stat `DL.elem` val
isSpecificConfigNode val ((MC.InMode (MC.Mode _ c)), _) = isSpecificConfigNode val (c, [])
isSpecificConfigNode val ((MC.IsPartial (MC.PartialComp c _)), _) = isSpecificConfigNode val (c, [])
isSpecificConfigNode _ _ = False


{-
 - Compare computations such that it will tell which two of them are same
 - for purpose of Configuration matching
 -}
compareComputeNodesForConfig :: MC.Computation -> MC.Computation -> Bool
compareComputeNodesForConfig (MC.IsFlow f1) (MC.IsFlow f2) =
    MC.filterID f1 == MC.filterID f2
compareComputeNodesForConfig (MC.IsHashFilter f1) (MC.IsHashFilter f2) =
    MC.filterID f1 == MC.filterID f2
compareComputeNodesForConfig (MC.ToQueue q1) (MC.ToQueue q2) =
    MC.queueId q1 == MC.queueId q2
compareComputeNodesForConfig (MC.IsPartial p1) (MC.IsPartial p2) =
    compareComputeNodesForConfig (MC.pComp p1) (MC.pComp p2)
        -- I am not sure if following part should be there or not.
        && compareComputeNodesForConfig (MC.pNeeds p1) (MC.pNeeds p2)
compareComputeNodesForConfig (MC.IsEmulated ec1) (MC.IsEmulated ec2) =
    compareComputeNodesForConfig (MC.eComp ec1) (MC.eComp ec2)
compareComputeNodesForConfig (MC.IsConfSet conf1) (MC.IsConfSet conf2) =
    compareComputeNodesForConfig (MC.dComp conf1) (MC.dComp conf2)
    && (MC.dStatus conf1) == (MC.dStatus conf1)
compareComputeNodesForConfig x y = x == y

{-
getConfigNodesInternal :: MC.Gnode MC.Computation -> [MC.Gnode MC.Computation]
getConfigNodesInternal ((MC.IsConfSet x), deps) = [(MC.IsConfSet x, deps)]
getConfigNodesInternal _ = []

getConfigNodes :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
getConfigNodes [] = []
getConfigNodes (x:xs) = (getConfigNodesInternal x) ++ getConfigNodes xs

isConfigPresent :: [MC.Gnode MC.Computation] -> MC.Computation -> [MC.Gnode MC.Computation]
isConfigPresent prg conf = DL.filter (matchConfigNode conf) prg
-}

 {-
 -  for given configuration
 -      check if configuration node is present
 -      if no,
 -          return
 -      if yes,
 -          update configuration node
 -          update its dependencies
 -}
applyConfig :: [MC.Gnode MC.Computation] -> MC.Computation
    -> [MC.Gnode MC.Computation]
applyConfig prg (MC.InMode (MC.Mode tag (MC.IsConfSet (MC.ConfDecision conf stat)))) = np
    where
    np
        | matchedConfs == [] = prg
        | otherwise = newPRG

    newNode = MC.IsConfSet (MC.ConfDecision conf stat)
    matchedConfs = DL.filter (matchConfigNode tag conf) prg
    newPRG = DL.foldl (replaceNodesWith newNode) prg matchedConfs
applyConfig _ conf = error ("Invalid configuration node given "  ++ show conf)

applyConfigList:: [MC.Gnode MC.Computation] -> [MC.Computation]
    -> [MC.Gnode MC.Computation]
applyConfigList prg [] = prg
applyConfigList prg (x:xs) = applyConfigList prg' xs
    where
        prg' = applyConfig prg x



{-
 - for given oldNode and newNode, replace ocurrances of oldNode with NewNode
 - both in "single" (node, dependencies) tuple
 -}
myReplaceFn ::  MC.Computation -> MC.Computation -> MC.Gnode MC.Computation
    -> MC.Gnode MC.Computation
myReplaceFn oldX newX (n, dep) = (n', dep')
    where
        n' = if n == oldX then (adaptForMode oldX newX) else n
        dep' = DL.map (\x -> if x == oldX then (adaptForMode oldX newX) else x) dep

{-
 - Changes newNode Mode based on oldNode mode
 - Essentially, final result should always have oldNode mode
 -}
adaptForMode :: MC.Computation -> MC.Computation -> MC.Computation
adaptForMode (MC.InMode (MC.Mode om oc))  (MC.InMode (MC.Mode nm nc)) = (MC.InMode (MC.Mode om nc))
adaptForMode (MC.InMode (MC.Mode om oc)) c = (MC.InMode (MC.Mode om c))
adaptForMode x c = c


{-
 - Replace the oldNode with newNode, for all the tuples in given PRG
 -}
replaceNodesWith :: MC.Computation -> [MC.Gnode MC.Computation]
    -> MC.Gnode  MC.Computation
    -> [MC.Gnode MC.Computation]
replaceNodesWith newNode prg (oldNode, _) = DL.map (myReplaceFn oldNode newNode) prg

{-
 - Replace the given oldNode with internal Computation within the oldNode
 - for whole PRG
 -}
replaceNodeForENABLE ::  [MC.Gnode MC.Computation] -> MC.Gnode MC.Computation
        -> [MC.Gnode MC.Computation]
replaceNodeForENABLE prg ((MC.IsConfSet (MC.ConfDecision comp stat)), _) =
    --DT.trace (
    -- "replaceForEnable oldNode " ++ show oldNode ++ " newNode " ++ show newNode )
        DL.map (myReplaceFn oldNode newNode) prg
    where
    oldNode = MC.IsConfSet (MC.ConfDecision comp stat)
    newNode = comp
replaceNodeForENABLE prg ((MC.InMode (MC.Mode n
        (MC.IsConfSet (MC.ConfDecision comp stat)))), _) =
        DL.map (myReplaceFn oldNode newNode) prg
    where
    oldNode = MC.InMode (MC.Mode n (MC.IsConfSet (MC.ConfDecision comp stat)))
    newNode = comp
replaceNodeForENABLE _ _ = error "Invalid module cropped in the replaceNodeForENABLE"


compareComputeNodesForExchange :: MC.Computation -> MC.Computation -> Bool
compareComputeNodesForExchange (MC.IsFlow f1) (MC.IsFlow f2) =
    MC.protocol f1 == MC.protocol f2
    && MC.srcIP f1 == MC.srcIP f2
    && MC.dstIP f1 == MC.dstIP f2
    && MC.srcPort f1 == MC.srcPort f2
    && MC.dstPort f1 == MC.dstPort f2
compareComputeNodesForExchange (MC.IsHashFilter f1) (MC.IsHashFilter f2) =
    MC.protocol f1 == MC.protocol f2
    && MC.srcIP f1 == MC.srcIP f2
    && MC.dstIP f1 == MC.dstIP f2
    && MC.srcPort f1 == MC.srcPort f2
    && MC.dstPort f1 == MC.dstPort f2
compareComputeNodesForExchange (MC.ToQueue q1) (MC.ToQueue q2) =
    q1 == q2 -- FIXME: maybe I should just compare coreID's and not queueIDs
compareComputeNodesForExchange (MC.IsPartial p1) (MC.IsPartial p2) =
    compareComputeNodesForExchange (MC.pComp p1) (MC.pComp p2)
        -- I am not sure if following part should be there or not.
        && compareComputeNodesForExchange (MC.pNeeds p1) (MC.pNeeds p2)
compareComputeNodesForExchange (MC.IsEmulated ec1) (MC.IsEmulated ec2) =
    compareComputeNodesForExchange (MC.eComp ec1) (MC.eComp ec2)
compareComputeNodesForExchange (MC.IsConfSet conf1) (MC.IsConfSet conf2) =
    compareComputeNodesForExchange (MC.dComp conf1) (MC.dComp conf2)
    && (MC.dStatus conf1) == (MC.dStatus conf1)
compareComputeNodesForExchange (MC.InMode m1) (MC.InMode m2) =
   MC.mName m1 ==  MC.mName m2
   && MC.mComp m1 == MC.mComp m2
compareComputeNodesForExchange x y = x == y



{-
 - Get all dependencies of selected node
 -}
getAllDeps :: MC.Computation -> [MC.Gnode MC.Computation] -> [MC.Computation]
getAllDeps chosenOne list = DL.concatMap (\(_, deps) -> deps)
            $ DL.filter (\(node,_) -> compareComputeNodesForExchange node chosenOne) list

{-
 - Remove the given node from the PRGlist
 -}
removeNode :: MC.Computation -> [MC.Gnode MC.Computation]
    -> [MC.Gnode MC.Computation]
removeNode deleteMe list = DL.filter  (\x -> (fst x) /= deleteMe ) list


{-
 - Remove the given oldNode, and modify all the nodes which were dependent on it
 - with node before oldNode
 -}
replaceNodeForSKIP ::  [MC.Gnode MC.Computation] -> MC.Gnode MC.Computation
        -> [MC.Gnode MC.Computation]
replaceNodeForSKIP prg ((MC.IsConfSet (MC.ConfDecision comp stat)), _)
    | DL.length allDeps == 0 = prg'
    | otherwise = prg''
    where
    oldNode = MC.IsConfSet (MC.ConfDecision comp stat)
    allDeps = getAllDeps oldNode prg
    prg' = removeNode oldNode prg
    prg'' = replaceOnlyDeps oldNode allDeps prg'
replaceNodeForSKIP prg ((MC.InMode (MC.Mode n
    (MC.IsConfSet (MC.ConfDecision cc ss)))), deps)
    | DL.length allDeps == 0 = prg'
    | otherwise = prg''
    where
    origConfNode = (MC.IsConfSet (MC.ConfDecision cc ss))
    origNode = (MC.InMode (MC.Mode n origConfNode))
    allDeps =
        -- DT.trace ("replaceNodeForSkip" ++ show origNode)
        getAllDeps origNode prg
    prg' = removeNode origNode  prg
    prg'' = replaceOnlyDeps origNode allDeps prg'

replaceNodeForSKIP _ _ = (error ("unexpected node found in the list"
            ++ " which was supposed to have only config nodes"))

replaceOnlyDeps :: MC.Computation -> [MC.Computation]
        -> [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
replaceOnlyDeps _ _ [] = []
replaceOnlyDeps oldNode newDep (x:xs) = dps ++ replaceOnlyDeps oldNode newDep xs
    where
    (node, currentDeps) = x
    dps
        | oldNode `DL.notElem` currentDeps = [x]
        | DL.length currentDeps > 1 = (error ("AND node [" ++ show x ++
            "] is having dependency config node [" ++ show oldNode ++ "]\n"))
        | otherwise = DL.map (\nd -> (node, [nd])) newDep

-- ###############################


{-
 - Find all the nodes which have fixed config (either ON or OFF)
 - For every node, find a replacement [nodes]
 -}
purgeFixedSKIPConfigs :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
purgeFixedSKIPConfigs prg = newPRG
    where
    --newPRG = prg
    selectedNodes = DL.filter (isSpecificConfigNode [MC.SKIP]) prg
    newPRG = DL.foldl (replaceNodeForSKIP) prg selectedNodes
--    newNode = ??
--    newPRG = DL.foldl (replaceNodesWith  newNode) prg selectedNodes

{-
 -
 -}
purgeFixedENABLEConfigs :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
purgeFixedENABLEConfigs prg = newPRG
    where
    selectedNodes = DL.filter (isSpecificConfigNode [MC.ENABLE]) prg
    newPRG =
        -- DT.trace ("selectedNodes " ++ show selectedNodes )
        DL.foldl (replaceNodeForENABLE) prg selectedNodes

purgeFixedSTOPConfigs :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
purgeFixedSTOPConfigs prg = newPRG
    where
    newPRG = DL.filter (not . isSpecificConfigNode [MC.STOP]) prg

{-
 - Purge all sort of configs
 -}
purgeFixedConfigs :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
purgeFixedConfigs prg = purgeFixedSTOPConfigs $ purgeFixedSKIPConfigs $ purgeFixedENABLEConfigs $ prg


{-
 - Purges all nodes that are unreachable from given initial state
 - Also, unreachability takes into account the Configuration node.
 -}
purgeUnreachableNodes :: [MC.Gnode MC.Computation] -> [MC.Gnode MC.Computation]
    -> [MC.Gnode MC.Computation]
purgeUnreachableNodes reachable prg
    | newReachableNodes == reachable = reachable
    | otherwise = purgeUnreachableNodes (newReachableNodes) prg
    where
    newReachableNodes = DL.filter (isReachable reachable) prg


isReachable :: [MC.Gnode MC.Computation] -> MC.Gnode MC.Computation -> Bool
isReachable prg (_, deps) = (notRechableDeps == [])
    where
    notRechableDeps = DL.filter (\x -> x `DL.notElem` reachableNodes) deps
    reachableNodes = getReachableNodesList $ MC.getNodesList prg

{-
 - Get all the nonOFF nodes (nodes which are configured ON, or Undecided,
 -  or are basic nodes.
 -}
getReachableNodesList :: [MC.Computation] -> [MC.Computation]
getReachableNodesList [] = []
getReachableNodesList (x:xs)
    | not (isSpecificConfigNode [MC.STOP, MC.SKIP] (x,[])) = (x:(getReachableNodesList xs))
    | otherwise = getReachableNodesList xs




