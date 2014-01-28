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
        port out[RxL4TCPPortClassify] }

    cluster Rx {

        cluster L4TCP {

            node PortClassify {
                port p1[Sock1]
                port p2[Sock2]
                port noSockets[ClosedPortAction]
            }

            node ClosedPortAction{
                // drop packet
                port out[..DropPacket]
            }

            node Sock1{
                // drop packet
                port srvSocket[ServerSocket]
                port cliSocket[ClientSocket]
            }

            node ServerSocket{
                port toClose[StateClosedStart]
                port toListen[StateListen]
                port toSynRecv[StateSynSent]
                port toEstablished[StateEstablished]
                port toCloseWait[StateCloseWait]
                port toLastAck[StateLastAck]
            }

            node ClientSocket{
                port toClose[StateClosedStart]
                port toSynSent [StateSynSent]
                port toEstablished [StateEstablished]
                port toFinWait1 [StateFinWait1]
                port toFinWait2 [StateFinWait2]
                port toTimeWait [StateTimeWait]
                port toClosing [StateClosing]
            }

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
            }

            node Sock2{
                // drop packet
            }


        } // end cluster: cluster L4TCP

    } /* end cluser : RX */

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

