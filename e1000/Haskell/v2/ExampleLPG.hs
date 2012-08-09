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
        lp1 = LP.initLPG
        out1 = CD.printAbstractTree $ CD.convertDT $ LP.lpg lp1
        app1 = LP.createApp "echoServer"
        (lp1', sock1) = LP.socket lp1 app1 LP.TCP
        (lp1'', sock2) = LP.socket lp1' app1 LP.UDP
        lp2 = LP.bind lp1'' sock1 7
        lp3 = LP.bind lp2 sock2 7
        out2 = CD.printAbstractTree $ CD.convertDT $ LP.lpg lp3
        app2 = LP.createApp "tftpd"
        (lp4, sock3) = LP.socket lp3 app2 LP.UDP
        lp4' = LP.bind lp4 sock3 69
        out3 = CD.printAbstractTree $ CD.convertDT $ LP.lpg lp4'

        lpConnect = LP.connect lp1' sock1
        out4 = CD.printAbstractTree $ CD.convertDT $ LP.lpg lpConnect

