#!/usr/bin/env runhaskell

-- module Elements (getElementList) where

module Main (main) where

import qualified DecisionTree as DT
import qualified Elements as El
import qualified Data.List as DL



{-
 - For given list of elements, it connects them to form a graph:
 -  Needs wrapper data-structure to show it as graph
 - Steps involved:
 -  Find elements with Empty pre-condition list
 -  Make them leaf nodes
 -  For every leaf node, search remaining nodes to see if you can find any
 -      node which matches with post-condition of given node
 -  Repeat this till you use up all nodes.
-}


findNeighbours:: [DT.Step] -> DT.Condition -> [DT.Step]
findNeighbours _ DT.Empty = []
findNeighbours [] cond = []
findNeighbours (x:[]) cond
        | DT.conditionCompare cond (DT.pre x) = [x]
        | otherwise         = []
findNeighbours (x:xs) cond = (findNeighbours [x] cond) ++
                                (findNeighbours xs cond)

{- I have all neighbours of given step.  Now I want to convert
 - all those neighbouring steps into nodes.
 - So, recursively I will have to convert each step into node.
 -
-}

expandStep :: [DT.Step] -> [DT.Step] -> [DT.Node]
expandStep sList [] = []
expandStep sList (x:[]) = [(convertToNode sList x)]
expandStep sList (x:xs) = (expandStep sList [x]) ++
                            (expandStep sList xs)

-- converts given step into graph-node using rest of the steps
convertToNode :: [DT.Step] -> DT.Step -> DT.Node
convertToNode sList cStep = (DT.Node cStep nodes)
    where
        nodes = expandStep sList $ findNeighbours sList $ DT.post cStep


-- Find leave nodes
findLeaves :: [DT.Step] -> [DT.Step]
findLeaves [] = []
findLeaves (x:[]) = case DT.pre x of
           DT.Empty -> [x]
           _ -> []
findLeaves (x:xs) = (findLeaves [x]) ++ (findLeaves xs)

-- Generate graph by connecting given steps
generateGraph :: [DT.Step] -> [DT.Node]
generateGraph elemList = expandStep elemList $ findLeaves elemList

-- main function
main = do
        putStrLn out1
        putStrLn lineBreak
        putStrLn out2
        putStrLn lineBreak
        putStrLn out3
        putStrLn lineBreak
        putStrLn out4
        putStrLn lineBreak
        putStrLn out5
    where
        lineBreak = "\n\n"
        out1 = "[Full list] " ++ (show El.getElementList)
        out2 = "[Leaf nodes] " ++ (show $ findLeaves El.getElementList)
        out3 = "[Leaf pre Condition ] " ++ (show $
            (DT.post $ head $ findLeaves  El.getElementList))
        out4 = "[neighbours] " ++ (show $ findNeighbours El.getElementList
            (DT.post $ head $ findLeaves  El.getElementList))
        out5 = show $ generateGraph El.getElementList

-- ################################## EOF ###################################

