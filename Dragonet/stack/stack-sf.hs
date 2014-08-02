import qualified Dragonet.Configuration as C
import qualified Dragonet.Optimization as O
import qualified Dragonet.Pipelines as PL
import qualified Dragonet.Pipelines.Implementation as PLI
import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.ProtocolGraph.Utils as PGU

--import qualified Graphs.E10k as E10k
import qualified Graphs.SF as SF
--import qualified Runner.E10KControl as CTRL
import qualified Runner.SFControl as CTRL

import Stack

import Control.Monad (forever)
import Control.Concurrent(forkIO, ThreadId)
import qualified Control.Concurrent.STM as STM
import Data.Word



-- Does not really matter as we only have one config
costFunction :: StackState -> O.CostFunction Int
costFunction _ _ = 1

-- Start out with a dummy configuration for e10k
oracle :: PG.PGraph -> StackState -> [(String,C.Configuration)]
oracle _ _ = [("default",[
                ("RxCFDirFilter", PG.CVList []),
                ("RxC5TupleFilter", PG.CVList [

{-
 -- ############# For only 1 core #############
                     PG.CVTuple [
                        PG.CVMaybe Nothing, -- $ Just $ PG.CVInt $ fromIntegral 175178809, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 0 ]

  -- FIXME: This is extremely ugly way of configuring the Oracle,
  --    but I am keeping this code in the repo for time being as reference
  --        (in case we need to reproduce current results).
  --    And also as an example of what needs to be done in oracle configuration.
-}

 -- ############# For only 4 core , with port 9000 #############

                        -- queue 3
                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178809, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9000, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 3 ],


                        -- queue 2
                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178772, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9000, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 2 ],

                        -- queue 1
                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178781, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9000, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 1 ],

                        -- queue 0

                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178848, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9000, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888,
                        PG.CVInt 1,
                        PG.CVInt 0 ]

{-
 -- ############# For only 8 cores #############

                     , PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178809, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9001, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 3 ],


                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178772, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9001, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 2 ],

                        -- queue 1
                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178781, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9001, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 1 ],



                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178848, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9001, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888,
                        PG.CVInt 1,
                        PG.CVInt 0 ]

 -- ############# For only 12 cores #############


                     , PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178809, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9002, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 3 ],



                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178772, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9002, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 2 ],



                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178781, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9002, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 1 ],



                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178848, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9002, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888,
                        PG.CVInt 1,
                        PG.CVInt 0 ]


 -- ############# For only 16 cores #############


                     , PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178809, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9003, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 3 ],



                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178772, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9003, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 2 ],



                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178781, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9003, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 1 ],



                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178848, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe $ Just $ PG.CVInt 9003, -- Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888,
                        PG.CVInt 1,
                        PG.CVInt 0 ]
-}

{-
 -- ############# For only 4 core , with wildcards #############

                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178809, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 3 ],

                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178772, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 2 ],



                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178781, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr,
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe Nothing, --  src port
                        PG.CVMaybe $ Just $ PG.CVInt 888, -- dest port
                        PG.CVInt 1, -- priority
                        PG.CVInt 1 ],



                     PG.CVTuple [
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178848, -- srcAddr,
                        PG.CVMaybe $ Just $ PG.CVInt $ fromIntegral 175178947, -- dstAddr
                        PG.CVMaybe $ Just $ PG.CVEnum 1,
                        PG.CVMaybe Nothing,
                        PG.CVMaybe $ Just $ PG.CVInt 888,
                        PG.CVInt 1,
                        PG.CVInt 0 ]

-}


                    ])
                ])]

data CfgAction =
    CfgASet5Tuple Word8 CTRL.FTuple |
    CfgAClear5Tuple Word8
    deriving (Eq,Show)

controlThread :: STM.TChan CfgAction -> PLI.StateHandle -> IO ()
controlThread chan sh = do
    -- Wait for card to be initialized
    CTRL.waitReady sh
    -- Start working on chan
    forever $ do
        act <- STM.atomically $ STM.readTChan chan
        case act of
            CfgASet5Tuple idx ft -> CTRL.ftSet sh idx ft
            CfgAClear5Tuple idx -> CTRL.ftUnset sh idx

-- Nothing to be implemented yet
implCfg :: STM.TMVar ThreadId -> STM.TChan CfgAction -> PLI.StateHandle
            -> C.Configuration -> IO ()
implCfg mtid chan sh _ = do
    mtid' <- STM.atomically $ STM.tryTakeTMVar mtid
    case mtid' of
        Nothing -> do
            tid <- forkIO $ controlThread chan sh
            STM.atomically $ STM.putTMVar mtid tid
        Just _ -> return ()
    return ()




-- Assign all nodes to same pipeline
plAssign :: StackState -> String -> PG.PGNode -> String
plAssign _ _ (_,n)
    | take 2 lbl == "Tx" = "Tx" ++ tag
    | otherwise = "Rx" ++ tag
    where
        lbl = PG.nLabel n
        tag = PG.nTag n


main = do
    -- Channel and MVar with thread id of control thread
    mtid <- STM.newEmptyTMVarIO
    chan <- STM.newTChanIO

{-
 -- ############# For only 1 core #############
    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 0 $ CTRL.FTuple {
                CTRL.ftPriority = 1,
                CTRL.ftQueue = 0,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Nothing, -- Just 175178809, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                --CTRL.ftL4Src = Just 9000, -- sPort, lport
                CTRL.ftL4Src = Nothing, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }
-}

 -- ############# For only 4 cores: port 9000 #############

                    -- queue 3 ----
    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 0 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 3,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178809, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9000, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }


                        -- queue 2
    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 1 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 2,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178772, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9000, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }


                        -- queue 1
    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 2 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 1,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178781, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9000, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }

                        -- queue 0
    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 3 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 0,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src =  Just 175178848, --sAddr, laddr -- burrata
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9000, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }


{-
 -- ############# For only 8 cores #############



    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 4 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 3,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178809, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9001, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }


    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 5 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 2,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178772, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9001, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }




    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 6 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 1,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178781, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9001, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }



    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 7 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 0,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src =  Just 175178848, --sAddr, laddr -- burrata
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9001, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }


        -- ### for 12 cores ###

    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 8 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 3,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178809, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9002, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }



    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 9 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 2,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178772, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9002, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }



    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 10 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 1,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178781, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9002, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }


    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 11 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 0,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src =  Just 175178848, --sAddr, laddr -- burrata
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9002, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }



        -- ### for 16 cores ###

    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 12 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 3,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178809, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9003, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }



    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 13 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 2,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178772, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9003, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }



    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 14 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 1,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178781, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9003, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }


    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 15 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 0,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src =  Just 175178848, --sAddr, laddr -- burrata
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Just 9003, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }
-}

{-
 -- ############# For only 4 core , with wildcards #############
 --
    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 2 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 3,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178809, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                --CTRL.ftL4Src = Just 9000, -- sPort, lport
                CTRL.ftL4Src = Nothing, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }

    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 1 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 2,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178772, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                --CTRL.ftL4Src = Just 9000, -- sPort, lport
                CTRL.ftL4Src = Nothing, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }


    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 0 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 1,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src = Just 175178781, --sAddr, laddr
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Nothing, -- sPort, lport
                --CTRL.ftL4Src = Just 9000, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }



    STM.atomically $ STM.writeTChan chan $
        CfgASet5Tuple 3 $ CTRL.FTuple {
                CTRL.ftPriority = 3,
                CTRL.ftQueue = 0,
                CTRL.ftL3Proto = Just CTRL.L3IPv4,
                CTRL.ftL4Proto = Just CTRL.L4UDP,
                CTRL.ftL3Src =  Just 175178848, --sAddr, laddr -- burrata
                CTRL.ftL3Dst = Just 175178947, --dAddr, raddr
                CTRL.ftL4Src = Nothing, -- sPort, lport
                CTRL.ftL4Dst = Just 888 -- dPort,
            }
-}

{-
# 10.113.4.96  # Burrata # 175178848
# 10.113.4.20  # gruyere # 175178772
# 10.113.4.57  # ziger2  # 175178809
# 10.113.4.29  # sbrinz2 # 175178781
# 10.113.4.95  # Asiago  # 175178847  # Intel
# 10.113.4.195 # Asiago  # 175178947  # solarflare
-}



    -- Prepare graphs and so on
    prgH <- SF.graphH
    instantiate prgH "llvm-helpers-sf" costFunction oracle (implCfg mtid chan)
        plAssign


{-
 -- ############ FIXME: OLD CODE, remove it!!  ##################
-- Does not really matter as we only have one config
costFunction :: StackState -> O.CostFunction Int
costFunction _ _ = 1

-- SF config is trivial
oracle :: PG.PGraph -> StackState -> [(String,C.Configuration)]
oracle _ _ = [("default",[])]

-- So is implementing it
implCfg :: PLI.StateHandle -> C.Configuration -> IO ()
implCfg _ _ = return ()

-- Split Rx and Tx into different pipelines
plAssignSplit :: StackState -> String -> PG.PGNode -> String
plAssignSplit _ _ (_,n)
    | ('R':'x':_) <- PG.nLabel n = "Rx"
    | otherwise = "Tx"

main = do
    -- Prepare graphs and so on
    prgH <- SF.graphH
    --prgH <- Tap.graphH
    instantiate prgH "llvm-helpers-sf" costFunction oracle implCfg plAssignSplit

-}
