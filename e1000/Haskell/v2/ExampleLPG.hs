module Main (main) where

import qualified Data.Word as W
import qualified Data.ByteString as BS
import qualified NICState as NS
import qualified DecisionTree as DT
import qualified ConvertDecision as CD
import qualified LPGModules as LPGm
import qualified LogicalProtocolGraph as LP


-- main function
main = do
--        putStrLn out1
--        putStrLn out2
--        putStrLn out3
        putStrLn out4
    where
        testLP = LP.initLPG
        myTree = CD.convertDT testLP
        out4 = CD.printAbstractTree myTree


