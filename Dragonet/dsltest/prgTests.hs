#!/usr/bin/env runhaskell
{-# LANGUAGE QuasiQuotes #-}

import Unicorn
import qualified Operations as OP
import DotGenerator as DG
import Embedding as E
import qualified Data.List as L
import Data.Maybe

[unicorn|
graph e10kPRG {
    cluster L2Ether {
        boolean Classified {
            port true[ValidLength]
            port false[] }

        boolean ValidLength {
            port true[ValidCRC]
            port false[] }

        boolean ValidCRC {
            port true[CRCStrip]
            port false[] }

        config CRCStrip {
            function configDropOrDec
            port out[PaddingStrip]
            }

        config PaddingStrip {
            function configDropOrDec
            port out[FilterUnicast]
            }

        cluster Filter {
            config Unicast {
                function configDropOrDec
                port true[Multicast]
                port drop[] }

            config Multicast {
                function configDropOrDec
                port out[VLAN]
                port drop[] }

            config VLAN {
                function configDropOrDec
                port out[Managebility]
                port drop[] }

            config Managebility {
                function configDropOrDec
                port out[..QueueAssignmentEthertype]
                port mng[] }
        }
    }

    cluster QueueAssignment {
        config Ethertype {
            function configDropOrDec
            port queues[.Queues]
            port next[FCoE] }

        config FCoE {
            function configDropOrDec
            port queues[.Queues]
            port next[FiveTuple] }

        config FiveTuple {
            function configDropOrDec
            port queues[.Queues]
            port next[SYN] }

        config SYN {
            function configDropOrDec
            port queues[.Queues]
            port next[FlowDirector] }

        config FlowDirector {
            function configDropOrDec
            port queues[.Queues]
            port next[DCB]
            }

        config DCB {
            function configDropOrDec
            port queues[.Queues]
            port next[RSS] }

        config RSS {
            function configDropOrDec
            port queues[.Queues]
            port next[] }
    }


    node Queues {
        //function queueConfig
        port out[RSC] }

    node RSCTimer {
        port interrupt[RSC] }

    node RSC {
        port drop[]
        port done[L2EtherClassifyL3_] }

    node L2EtherClassifyL3_ {
        port ipv4[L2EtherL3IPv4Classified L3IPv4Checksum_ L3IPv4L4Classify_]
        port ipv6[L2EtherL3IPv6Classified L3IPv6L4Classify_]
        port other[]
        }

    node L3IPv4L4Classify_ {
        port tcp[L3IPv4L4TCPClassified L4TCPChecksum_]
        port udp[L3IPv4L4UDPClassified L4UDPChecksum_]
        port sctp[L3IPv4L4SCTPClassified L4SCTPChecksum_]
        port other[]
        }

    node L3IPv6L4Classify_ {
        port tcp[L3IPv6L4TCPClassified L4TCPChecksum_]
        port udp[L3IPv6L4UDPClassified L4UDPChecksum_]
        port sctp[L3IPv6L4SCTPClassified L4SCTPChecksum_]
        port other[]
        }

    config L2EtherL3IPv4Classified {
        function configDropOrDec
        attr "software"
        port out[] }

    config L2EtherL3IPv6Classified {
        function configDropOrDec
        attr "software"
        port out[] }


    config L3IPv4L4TCPClassified {
        function configDropOrDec
        attr "software"
        port out[] }

    config L3IPv4L4UDPClassified {
        function configDropOrDec
        attr "software"
        port out[] }

    config L3IPv4L4SCTPClassified {
        function configDropOrDec
        attr "software"
        port out[] }


    config L3IPv6L4TCPClassified {
        function configDropOrDec
        attr "software"
        port out[] }

    config L3IPv6L4UDPClassified {
        function configDropOrDec
        attr "software"
        port out[] }

    config L3IPv6L4SCTPClassified {
        function configDropOrDec
        attr "software"
        port out[] }


    node L3IPv4Checksum_ {
        port out[L3IPv4ValidChecksum] }

    config L3IPv4ValidChecksum {
        function configDropOrDec
        attr "software"
        port out[] }

    node L4TCPChecksum_ {
        port out[L4TCPValidChecksum] }

    config L4TCPValidChecksum {
        function configDropOrDec
        attr "software"
        port true false[] }

    node L4UDPChecksum_ {
        port out[L4UDPValidChecksum] }

    config L4UDPValidChecksum {
        function configDropOrDec
        attr "software"
        port true false[] }

    node L4SCTPChecksum_ {
        port out[L4SCTPValidChecksum] }

    config L4SCTPValidChecksum {
        function configDropOrDec
        attr "software"
        port true false[] }

}
|]


[unicorn|
graph lpg {
    /*******************************************/
    /* Ethernet RX                             */

    cluster L2Ether {
        boolean Classified {
            port true[ValidLength]
            port false[] }

        boolean ValidLength {
            port true[ValidType]
            port false[] }

        and Valid {
            port true false[.L3IPAndBelowValid] }

        boolean ValidType {
            port true[ClassifyL3]
            port false[] }

        node ClassifyL3 {
            port out[L3IPv4Classified L3IPv6Classified] }

        node L3IPv4Classified {
            port out[.L3IPv4Classified] }

        node L3IPv6Classified {
            port out[.L3IPv6Classified] }
    }


    /*******************************************/
    /* IPv4 RX                                 */

    cluster L3IPv4 {
        or Classified {
            port true[ValidHeaderLength]
            port false[]
            }

        boolean ValidHeaderLength {
            port true[ValidReassembly ValidVersion ValidLength
                      ValidTTL ValidChecksum ClassifyL4]
            port false[] }

        boolean ValidReassembly {
            port true false[Valid] }

        boolean ValidVersion {
            port true false[Valid] }

        boolean ValidLength {
            port true false[Valid] }

        boolean ValidTTL {
            port true false[Valid] }

        boolean ValidChecksum {
            port true false[Valid] }

        node ClassifyL4 {
            port out[L4TCPClassified L4UDPClassified L3ICMPClassified] }

        boolean L4TCPClassified {
            port true false[.L4TCPClassified] }

         boolean L4UDPClassified {
            port true false[.L4UDPClassified] }
           
         boolean L3ICMPClassified {
            port true false[.L3ICMPClassified] }

        and Valid {
            port true false[.L3IPValid] }
    }


    /*******************************************/
    /* IPv6 RX                                 */

    cluster L3IPv6 {
        or Classified {
            port true[ValidHeaderLength]
            port false[]
            }

        boolean ValidHeaderLength {
            port true false[.L3IPValid] }
    }



    or L3IPValid {
        port true false[L3IPAndBelowValid] }

    and L3IPAndBelowValid {
        port true false[L3ICMPValid L4TCPValid L4UDPValid] }


    /*******************************************/
    /* ICMP RX                                 */

    cluster L3ICMP {
        or Classified {
            port true[ValidHeaderLength]
            port false[] }

        boolean ValidHeaderLength {
            port true false[Valid] }

        and Valid {
            port true[Out]
            port false[] }

        node Out { }
    }

    /*******************************************/
    /* UDP RX                                  */

    cluster L4UDP {
        or Classified {
            port true[ValidHeaderLength]
            port false[] }

        boolean ValidHeaderLength {
            port true[ValidLength ValidChecksum]
            port false[] }

        boolean ValidLength {
            port true false[Valid] }

        boolean ValidChecksum {
            port true false[Valid] }

        and Valid {
            port true[]
            port false[] }

        node Out { }
    }


    /*******************************************/
    /* TCP RX                                  */

    cluster L4TCP {
        or Classified {
            port true[ValidHeaderLength]
            port false[] }

        boolean ValidHeaderLength {
            port true false[Valid] }

        and Valid {
            port true[Out]
            port false[] }

        node Out { }
    }
}

|]


-- If configuration is false just short-circuit, otherwise create decision node
-- with same node and ports as config nodes
configDropOrDec :: OP.Node -> [(OP.Node,String)] -> [(String,OP.Node)] -> String -> [(OP.Node,String,OP.Node)]
configDropOrDec n inE outE cfg =
    if cfg == "true" then
        addNode
    else
        shortCircuit
    where
        -- case: Drop node
        shortCircuit = concatMap (\(a,b) -> map (\(_,d) -> (a,b,d)) outE) inE

        -- case: Add node
        addNode = (map (\(a,b) -> (a,b,queueN)) inE) ++ (map (\(a,b) -> (queueN,a,b)) outE)
        queueN = OP.getDecNode (OP.nLabel n) "" (OP.NaryNode ports) attrs
        attrs = OP.nAttributes n
        ports = map (\p -> (p, (map snd $ filter ((== p) . fst) outE))) $ L.nub $ map fst outE


main = do
    --putStrLn (DG.toDotClustered e10kPRGClusters e10kPRGNodes)
    putStrLn (DG.toDotFromDLP embedded)
    --putStrLn (DG.toDotFromDLP prg)
    --putStrLn ("[" ++ (L.intercalate "\n" $ map strEdge lpgDep) ++ "]")
    where
        embedded = E.testEmbeddingV3 prg lpg

        prgU = E.getDepEdgesP e10kPRGL2EtherClassified
        prg = OP.applyConfig config prgU
        lpg = E.getDepEdgesP lpgL2EtherClassified

        config = [("L2EtherCRCStrip", "true"),
                  ("L2EtherPaddingStrip", "true"),
                  ("L2EtherFilterUnicast", "false"),
                  ("L2EtherFilterMulticast", "false"),
                  ("L2EtherFilterVLAN", "false"),
                  ("L2EtherFilterManagebility", "false"),
                  ("QueueAssignmentEthertype", "false"),
                  ("QueueAssignmentFCoE", "false"),
                  ("QueueAssignmentFiveTuple", "false"),
                  ("QueueAssignmentSYN", "false"),
                  ("QueueAssignmentFlowDirector", "false"),
                  ("QueueAssignmentDCB", "false"),
                  ("QueueAssignmentRSS", "false"),

                  -- Software nodes for classification
                  ("L2EtherL3IPv4Classified", "true"),
                  ("L2EtherL3IPv6Classified", "true"),
                  ("L3IPv4L4TCPClassified", "true"),
                  ("L3IPv4L4UDPClassified", "true"),
                  ("L3IPv4L4SCTPClassified", "true"),
                  ("L3IPv6L4TCPClassified", "true"),
                  ("L3IPv6L4UDPClassified", "true"),
                  ("L3IPv6L4SCTPClassified", "true"),

                  -- Software nodes for checksumming
                  ("L3IPv4ValidChecksum", "false"),
                  ("L4TCPValidChecksum", "true"),
                  ("L4UDPValidChecksum", "true"),
                  ("L4SCTPValidChecksum", "true"),

                  ("Queues","0")]

        {-prgDep = L.nub $ E.removeDroppedNodesP $ E.getDepEdgesP $ prgL2EtherClassified
        lpgDep = L.nub $ E.removeDroppedNodesP $ E.getDepEdgesP $ lpgL2EtherClassified-}
