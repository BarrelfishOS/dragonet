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
        putStrLn out2
--        putStrLn out2
--        putStrLn out3
--        putStrLn out4
    where
        lp1 = LP.initLPG
        out1 = CD.printAbstractTree $ CD.convertDT lp1
        app1 = LP.createApp "echoServer"
        soc1 = LP.createTCPSocket app1
        lp2 = LP.bind lp1 soc1 7
        lp3 = LP.bind lp2 soc1 8
        out2 = CD.printAbstractTree $ CD.convertDT lp3


