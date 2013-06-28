#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

{-
 - Logical Protocol Graph (LPG) support
 -}

--module Main (
module LPG (
    getSampleLPG2Apps
    , main
) where

--import qualified MyGraph as MG
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


getMaxSocketID :: [MC.Gnode MC.Computation] -> MC.SocketId
getMaxSocketID nodesList
    | sockIDList == [] = 0
    | otherwise = DL.maximum sockIDList
    where
       sockIDList = map getSocketID $ DL.filter (isSocket) $ map fst nodesList

{-
 - Opens a new socket in given graph.
 - It also adds this newly created socket into the graph.
 -}
openSocket :: [MC.Gnode MC.Computation] -> ([MC.Gnode MC.Computation], MC.Socket)
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
bind :: [MC.Gnode MC.Computation] -> MC.Application -> MC.Socket -> MC.Filter -> [MC.Gnode MC.Computation]
bind lpg app sock fl = lpgFiltered ++ [
                    (thisFlow, [MC.L4ReadyToClassify])
                    , (tosocket, [thisFlow, MC.VerifiedL4])
                    , (toapp, [MC.ToSocket sock])
                ]
    where
        lpgFiltered = lpg
        tosocket = (MC.ToSocket sock)
        thisFlow = (MC.IsFlow fl)
        toapp = (MC.ToApplication app)


{-
 - Returns a sample LPG graph with two applications :
 - apache webserver and tetnet client
 -}
getSampleLPG2Apps :: [MC.Gnode MC.Computation] ->  [MC.Gnode MC.Computation]
getSampleLPG2Apps lpg = finalLPG
    where
        -- Apache server
        (lpg', s) = openSocket lpg
        app = MC.Application "Apache"

        apacheFilter = MC.Filter 1 MC.TCP MC.anyIP (MC.toIP "192.168.2.4")
            MC.anyPort 80
        lpg2 = bind lpg' app s apacheFilter

        -- Telnet client
        (lpg2', s2) = openSocket lpg2
        telnetapp = MC.Application "telnet"

        telnetFilter = MC.Filter 2 MC.TCP (MC.toIP "192.168.2.4")
            (MC.toIP "192.168.2.1") MC.anyPort 23
        finalLPG = bind lpg2' telnetapp s2 telnetFilter

{-
 - main function: used to test if the PRG generated for E1k is correct or not
 -}
main  :: IO()
main = do
         putStrLn outDot
    where
        lpg = MC.getNetworkDependency
        outDot = show $ getSampleLPG2Apps lpg

