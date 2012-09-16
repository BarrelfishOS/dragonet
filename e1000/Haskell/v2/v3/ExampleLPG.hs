module Main (main) where

import qualified Data.Word as W
import qualified Data.ByteString as BS
import qualified NICState as NS
import qualified DecisionTree as DT
import qualified ConvertDecision as CD
import qualified LogicalProtocolGraph as LP

getGraphAndDS :: DT.Decision -> (String, String)
getGraphAndDS des = (ds, graph)
    where
        ds = ("//") ++ (show des)
        -- starting with // so it will be a comment for click configuration file
        graph = CD.printAbstractTree $ CD.convertDT des

-- main function
main = do
        putStrLn ds1
        putStrLn linebreak
        putStrLn graph1
    where
        -- Initialize the logical protocol graph
        lp1 = LP.initLPG

        -- First application is active now
        -- It will open two sockets and will bind to two ports
        app1 = LP.createApp "echoServer"
        (lp1', sock1) = LP.socket lp1 app1 LP.TCP
        (lp1'', sock2) = LP.socket lp1' app1 LP.UDP
        lp''' = LP.bind lp1'' sock1 7
        lp2 = LP.bind lp''' sock2 7

        -- Second application is active now
        -- It will open one socket and will bind to a port
        app2 = LP.createApp "tftpd"
        (lp2', sock3) = LP.socket lp2 app2 LP.UDP
        lp3 = LP.bind lp2' sock3 69

        -- Application 3 will open a connection
        app3 = LP.createApp "telnetClient"
        (lp3', sock4) = LP.socket lp3 app3 LP.TCP
        lp4 = LP.connect lp3' sock4

        -- For printing the data-structures and graphs
        linebreak = "\n\n\n\n"
        -- (ds1, graph1) = getGraphAndDS $ LP.lpg lp1
        -- (ds1, graph1) = getGraphAndDS $ LP.lpg lp2
        -- (ds1, graph1) = getGraphAndDS $ LP.lpg lp3
        (ds1, graph1) = getGraphAndDS $ LP.lpg lp4

