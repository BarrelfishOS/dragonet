#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Dragonet.ProtocolGraph
import Dragonet.Unicorn
import Dragonet.Configuration
import Dragonet.DotGenerator
import Dragonet.Embedding
import Dragonet.Constraints

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Implementation as Impl

[unicorn|
graph tcp {

    node TCPQueue {
        port out[L4TCPRxFindSocketByPacket] }

    node EventQueue {
        port out[L4TCPEventFindSocketByContext] }


    cluster L4TCP {

        cluster Event {

            node FindSocketByContext{
                port toSocket[Classify]
                port noSockets[NoSocketEvent]
            }

            node NoSocketEvent{
                // drop the event
            }

            node Classify{
                // get the state based on given event
                port toRetransmissionTimer  [RetransmissionTimer]
                port toPersistTimer         [PersistTimer]
                port toKeepAliveTimer       [KeepAliveTimer]
                port toMsl2Timer            [Msl2Timer]
            }

            node RetransmissionTimer {}
            node PersistTimer        {}
            node KeepAliveTimer      {}
            node Msl2Timer           {}

        } // end cluster: cluster Event


        cluster Rx {

            node FindSocketByPacket{
                port toSocket[PacketForSocket]
                port noSockets[ClosedPortAction]
            }

/*
            node PortClassify {
                port p1[Sock1]
                port p2[Sock2]
                port noSockets[ClosedPortAction]
            }
*/

            node ClosedPortAction{
                // drop packet
                port out[..DropPacket]
            }

            node PacketForSocket{
                port srvSocket[ServerSocket]
                port cliSocket[ClientSocket]
            }

            node ServerSocket{
                port toClose        [.StateClosedStart]
                port toListen       [.StateListen]
                port toSynRecv      [.StateSynSent]
                port toEstablished  [.StateEstablished]
                port toCloseWait    [.StateCloseWait]
                port toLastAck      [.StateLastAck]
            }

            node ClientSocket{
                port toClose        [.StateClosedStart]
                port toSynSent      [.StateSynSent]
                port toEstablished  [.StateEstablished]
                port toFinWait1     [.StateFinWait1]
                port toFinWait2     [.StateFinWait2]
                port toTimeWait     [.StateTimeWait]
                port toClosing      [.StateClosing]
            }

        } /* end cluser : RX */


        cluster State {

            node ClosedStart{
                port toListen[Listen]
                port toSynSent[SynSent]
            }

            node Listen{
                port toSynRecv[SynSent]
            }

            node SynSent{
                port toEstablished [Established]
            }

            node SynReceived{
                port toEstablished [Established]
            }

            node Established{
                port toFinWait1 [FinWait1]
                port toCloseWait [CloseWait]
            }

            node FinWait1{
                port toFinWait2 [FinWait2]
                port toClosing [Closing]
                port toTimeWait [TimeWait]
            }

            node FinWait2{
                port toTimeWait [TimeWait]
            }

            node Closing{
                port toTimeWait [TimeWait]
            }

            node TimeWait{
                port toClosedEnd [ClosedEnd]
            }

            node CloseWait{
                port toLastAck[LastAck]
            }

            node LastAck{
                port toClosedEnd [ClosedEnd]
            }

            node ClosedEnd{
            }

        } // end cluster: cluster State


    } // end cluster: cluster L4TCP


    node DropPacket{
        // drop packet
    }


}
|]

-- The protocol graph


myWriteFile :: String -> String -> IO()
myWriteFile fname contents = do
    putStrLn ("Generating " ++ fname ++ " files...")
    writeFile fname contents

main :: IO ()
main = do

    putStrLn "Generating .dot files..."
    myWriteFile "tcpClustered.dot" $ toDotClustered lpgT tcpClusters

    where
        lpgT = pgSetType GTLpg tcp

