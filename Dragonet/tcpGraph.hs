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

    node TCPPacketIn {
        port out[TCPRxFindSocketByPacket] }

    node EventQueue {
        port out[TCPEventFindSocketByContext TCPEventClassify] }

    cluster TCP {

        cluster Event {

            node FindSocketByContext{
                port toSocket[STestIsTimeWaitState STestIsEstablishedState]
                port noSockets[..DropEvent]
            }

            node Classify{
                // get the state based on given event
                port toRetransmissionTimer  [RetransmissionTimer]
                port toPersistTimer         [PersistTimer]
                port toKeepAliveTimer       [KeepAliveTimer]
                port toMSL2Timer            [MSL2Timer]
            }

            node RetransmissionTimer {}
            node PersistTimer        {}
            boolean KeepAliveTimer      {
                port true false[HandleKeepAliveTimer]
            }

            boolean MSL2Timer {
                port true false[HandleMSL2Timer]
            }

            and HandleKeepAliveTimer {
                port true [.TxSendAck]
                port false[..DropEvent]
            }

            and HandleMSL2Timer {
                port true [.SChangeToClosed]
                port false[..DropEvent]
            }

            cluster STest {
                boolean IsTimeWaitState {
                    port true false[.HandleMSL2Timer]
                }

                boolean IsEstablishedState {
                    port true false[.HandleKeepAliveTimer]
                }

            } // end cluster: STest



        } // end cluster: cluster Event



        cluster Rx {

            node FindSocketByPacket{
                port toSocket[PacketForSocket]
                port noSockets[ClosedPortAction]
            }

            node ClosedPortAction{
                // drop packet
                port out[..DropPacket]
            }

            node PacketForSocket{
                port srvSocket[ServerSocket]
                port cliSocket[ClientSocket]
            }

            node ServerSocket{
                // Selects the next edge based on the current state
                // of socket and without consulting the packet

                port isListen       [IsValidSyn]
                port isSynRecv      [IsValidSynAckS] // for simultaneous open
                port isEstablished  [IsDataPacket IsValidAck IsFin]
                port isLastAck      [IsValidFinAck]
            }

            node ClientSocket{

                port isSynSent      [IsValidSynAck]
                port isEstablished  [IsDataPacket IsValidAck]
                port isFinWait1     [IsValidFinAckOnly1 IsValidFinANDAck IsValidFinOnly1]

                port isFinWait2     [IsValidFinOnly2]
                port isClosing      [IsValidFinAckOnly2]
            }


            boolean IsValidSynAckS{

                port true[.SChangeToEstb]
                port false[..DropPacket]
            }


            boolean IsValidSynAck{
                port true[.TxSendSynAck]
                port false[]
            }


            boolean IsValidSyn{
                port true[.TxSendSyn]
                port false[..DropPacket]
            }

            boolean IsValidAck{
                port true[.ContextAckNo]
                port false[]
            }

            boolean IsDataPacket{
                port true[.TxSendAck CopyData]
                port false[]
            }

            boolean IsFin{
                port true[.TxSendFinAckServer]
                port false[]
            }

            boolean IsValidFinAck{
                port true[.SChangeToClosed]
                port false[]
            }

            boolean IsValidFinAckOnly1{
                // from state FinWait1
                port true[.SChangeToFinWait2]
                port false[]
            }

            boolean IsValidFinAckOnly2{
                // from state closing
                port true[.SChangeToFinWait2]
                port false[]
            }



            boolean IsValidFinOnly1{
                // From FinWait1
                port true[.TxSendFinAckClientC]
                port false[]
            }
            boolean IsValidFinANDAck{
                // From FinWait1
                port true[.TxSendFinAckClient]
                port false[]
            }

            boolean IsValidFinOnly2{
                // From FinWait2
                port true[.TxSendFinAckClient]
                port false[]
            }

            node CopyData{
                // Copy the data into application buffer
                port out[..NotifyApp]
            }

        } /* end cluster : Rx */


        cluster SChange{

            // These are state changes
           node ToSynReceived {
                port out [.ContextGeneric]
                // Update the state to SynReceived
           }

           node ToEstb {
                port out [.ContextGeneric]
                // Update the state to Established
           }

           node ToCloseWait {
                port out [.ContextGeneric]
                // Update the state to SynReceived
           }

           node ToClosed {
                port out [.ContextGeneric]
                // Update the state to close
           }

            // Client state changes /////////
           node ToClosing {
                port out [.ContextGeneric]
                // Update the state to closing for clients
           }

           node ToFinWait2 {
                port out [.ContextGeneric]
                // Update the state to FinWait2 for clients
           }

           node ToTimeWait {
                port out [.ContextGeneric]
                // Update the state to Timewait for clients
           }

        } // end cluster: SChange


        cluster Context{
            // These are context updates

           node AckNo{
                port moved [DropAckData]
                port unMoved []
                // Update the ACKed packets/data
           }

           node DropAckData {
                // Drop the data which is already acked.
           }


           node RecvdSeqNo{
                // Update the next expected sequence number
           }

           node Generic{
                // Can Update any data about socket context
           }


        } // end cluster: Context

        cluster Tx {

           node SendSyn{
                port out [.SChangeToSynReceived ..SendPacket]
           }

           node SendSynAck{
                // send Ack for syn
                port out [.SChangeToEstb ..SendPacket ..TimerResetKeepAlive]
           }

           node SendAck{
                port out[.ContextRecvdSeqNo ..SendPacket ..TimerResetKeepAlive]
           }

           node SendFinAckServer{
                // send Ack for Fin (on server side)
                port out [.SChangeToCloseWait ..SendPacket]
           }

           node SendFinAckClientC{
                // send Ack for Fin from state FinWait1 to closing
                port out [.SChangeToClosing ..SendPacket]
           }

           node SendFinAckClient{
                // send Ack for Fin from state FinWait1 or FinWait2
                port out [.SChangeToTimeWait ..SendPacket ..TimerRegisterMSL2]
           }

           node SendEstablishedData {
               // Create a TCP packet around it with flags for estabilished
               // state, and seq/ack number from current state.
                port out [..SendPacket]
           }

        } // end cluster : Tx


    } // end cluster: cluster L4TCP


    node SendPacket{
        // Send the TCP packet out
    }

    node DropPacket{
        // drop packet
    }

    node DropEvent{
        // drop event
    }

    node NotifyApp {
        // Inform application about the event

        }

    cluster Timer {
    node ResetKeepAlive {
        // Register for callback event/timer
        // to detect (2 * Maximum Life time) of outstanding packets.
    }



    node RegisterMSL2{
        // Register for callback event/timer
        // to detect (2 * Maximum Life time) of outstanding packets.
    }

    }




}
|]

-- The protocol graph

getGraphDirName :: String
getGraphDirName = "./graphsGen/"

myWriteFile :: String -> String -> IO()
myWriteFile fnamefile contents = do
    let fname = (getGraphDirName  ++ fnamefile)
    putStrLn ("Generating " ++ fname ++ " files...")
    writeFile fname contents

main :: IO ()
main = do

    putStrLn "Generating .dot files..."
    myWriteFile "tcpClustered.dot" $ toDotClustered lpgT tcpClusters

    where
        lpgT = pgSetType GTLpg tcp

