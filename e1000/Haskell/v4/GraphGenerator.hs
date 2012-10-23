#!/usr/bin/env runhaskell

-- module Elements (getElementList) where

module Main (main) where

import qualified DecisionTree as DT
import qualified Elements as El

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


-- Find leave nodes
findLeaves :: [DT.Step] -> [DT.Step]
findLeaves [] = []
findLeaves (x:[]) = case DT.pre x of
           DT.Empty -> [x]
           _ -> []
findLeaves (x:xs) = (findLeaves [x]) ++ (findLeaves xs)

-- Generate graph by connecting given steps
generateGraph :: [DT.Step] -> [DT.Step]
generateGraph elemList = findLeaves elemList

-- main function
main = do
        putStrLn out1
    where
        out1 = show $ generateGraph El.getElementList

-- ################################## EOF ###################################

