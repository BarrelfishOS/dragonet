#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Logical Protocol Graph (LPG) support
 -}

--module LPG (
module Main (
    main
) where

import qualified MyGraph as MG
import qualified Computations as MC
import qualified Data.List as DL

--isSocket :: MC.Gnode MC.Computation -> Bool
isSocket :: MC.Computation -> Bool
isSocket (MC.ToSocket _) = True
isSocket _ = False

getSocketID :: MC.Computation -> MC.SocketId
getSocketID (MC.ToSocket (MC.Socket sockid)) = sockid
getSocketID _ = error "Invalid data type"


{-
 - Find all the nodes in the graph which are of type Socket
 -}


getMaxSocketID :: [MG.Gnode MC.Computation] -> MC.SocketId
getMaxSocketID nodesList
    | sockIDList == [] = 0
    | otherwise = DL.maximum sockIDList
    where
       sockIDList = map getSocketID $ filter (isSocket) $ map fst nodesList

{-
 - Opens a new socket in given graph.
 - It also adds this newly created socket into the graph.
 -}
openSocket :: [MG.Gnode MC.Computation] -> ([MG.Gnode MC.Computation], MC.Socket)
openSocket currentGraph = (newGraph, socket)
    where
        maxSockID = getMaxSocketID currentGraph
        socket = MC.Socket $ toInteger (maxSockID + 1)
        newGraph = currentGraph ++ [(MC.ToSocket socket, [])]


{-
 - Bind the given application to given socket
 - FIXME: make sure that I have removed duplicate entries for sock,
 -  app and filter.  Also, if the filter already exist then I should complain
 -}
bind :: [MG.Gnode MC.Computation] -> MC.Application -> MC.Socket -> MC.Filter -> [MG.Gnode MC.Computation]
bind lpg app sock filter = lpgFiltered ++ [
                    (thisFlow, [MC.L4ReadyToClassify])
                    , (tosocket, [thisFlow, MC.VerifiedL4])
                    , (toapp, [MC.ToSocket sock])
                ]
    where
        lpgFiltered = lpg
        tosocket = (MC.ToSocket sock)
        thisFlow = (MC.IsFlow filter)
        toapp = (MC.ToApplication app)
{-
 - main function: used to test if the PRG generated for E1k is correct or not
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        lpg = MC.getNetworkDependency
        (lpg', s) = openSocket lpg
        app = MC.Application "Apache"
        apacheFilter = MC.Filter "TCP" "ANY" "MYIP" "ANY" "80"
        lpg2 = bind lpg' app s apacheFilter

        -- Telnet client
        (lpg2', s2) = openSocket lpg2
        telnetapp = MC.Application "telnet"
        telnetFilter = MC.Filter "TCP" "192.68.2.10" "MYIP" "23" "12210"
        lpg3 = bind lpg2' telnetapp s2 telnetFilter


        outDot = MG.showFlowGraph lpg3

