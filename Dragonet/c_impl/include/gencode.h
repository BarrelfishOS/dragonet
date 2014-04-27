#ifndef GENERATEDCODE_H_

#define GENERATEDCODE_H_

enum out_ports {
    P_false = 0,
    P_true = 1,
    P_Queue_out = 0,
    P_Queue_drop = 1,
    P_RxEchoAPP_out = 0,
    P_RxEchoAPP_drop = 1,
    P_RxDnsAPP_out = 0,
    P_RxDnsAPP_drop = 1,
    P_RxL2EtherClassifyL3_ipv4 = 0,
    P_RxL2EtherClassifyL3_ipv6 = 1,
    P_RxL2EtherClassifyL3_arp = 2,
    P_RxL2EtherClassifyL3_drop = 3,
    P_RxL3ARPClassify_request = 0,
    P_RxL3ARPClassify_response = 1,
    P_RxL3ARPClassify_drop = 2,
    P_RxL3ARPProcessPendingResponse_false = 0,
    P_RxL3ARPProcessPendingResponse_true = 1,
    P_RxL3IPv4Classify_tcp = 0,
    P_RxL3IPv4Classify_udp = 1,
    P_RxL3IPv4Classify_icmp = 2,
    P_RxL3IPv4Classify_drop = 3,
    P_RxL4UDPPortClassifyType_static = 0,
    P_RxL4UDPPortClassifyType_dynamic = 1,
    P_RxL4UDPPortClassifyStatic_appDNS = 0,
    P_RxL4UDPPortClassifyStatic_appEcho = 1,
    P_RxL4UDPPortClassifyStatic_closedPort = 2,
    P_RxL4UDPClosedPortAction_out = 0,
    P_RxL4TCPPortClassifyType_static = 0,
    P_RxL4TCPPortClassifyType_dynamic = 1,
    P_RxL4TCPPortClassifyStatic_toSocket = 0,
    P_RxL4TCPPortClassifyStatic_noSocket = 1,
    P_RxL4TCPClosedPortAction_out = 0,
    P_RxL4TCPSocketClassify_srvSocket = 0,
    P_RxL4TCPSocketClassify_cliSocket = 1,
    P_RxL4TCPSocketServerSide_isListen = 0,
    P_RxL4TCPSocketServerSide_isSynRecv = 1,
    P_RxL4TCPSocketServerSide_isEstablished = 2,
    P_RxL4TCPSocketServerSide_isCloseWait = 3,
    P_RxL4TCPSocketServerSide_isLastAck = 4,
    P_RxL4TCPSocketServerSide_isClosed = 5,
    P_RxL4TCPSocketInClosed_out = 0,
    P_RxL4TCPSocketClientSide_out = 0,
    P_RxL4TCPSocketAddHalfOpenConn_success = 0,
    P_RxL4TCPSocketAddHalfOpenConn_failure = 1,
    P_TxDemux_TCPIR = 0,
    P_TxDemux_UDPIR = 1,
    P_TxDemux_ICMPIR = 2,
    P_TxDemux_ARPLu = 3,
    P_TxDemux_ARPIR = 4,
    P_TxDemux_drop = 5,
    P_TxL4UDPInitiateResponse_out = 0,
    P_TxL4UDPInitiateResponse_drop = 1,
    P_TxL4UDPAllocateHeader_out = 0,
    P_TxL4TCPInitiateResponse_out = 0,
    P_TxL4TCPInitiateResponse_drop = 1,
    P_TxL4TCPAllocateHeader_out = 0,
    P_TxL3ICMPInitiateResponse_out = 0,
    P_TxL3ICMPInitiateResponse_drop = 1,
    P_TxL3ICMPAllocateHeader_out = 0,
    P_TxL3IPv4AllocateHeader_out = 0,
    P_TxL3IPv4FillHeader_out = 0,
    P_TxL3ARPInitiateResponse_false = 0,
    P_TxL3ARPInitiateResponse_true = 1,
    P_TxL3ARPInitiateResponse_drop = 2,
    P_TxL3ARPAllocateHeader_out = 0,
    P_TxL3ARPLookup__false = 0,
    P_TxL3ARPLookup__true = 1,
    P_TxL3ARPLookup__miss = 2,
    P_TxL3ARPSendRequest_false = 0,
    P_TxL3ARPSendRequest_true = 1,
    P_TxL3ARPSendRequest_drop = 2,
    P_TxL2EtherAllocateHeader_out = 0,
    P_TxL2EtherFillHeader_out = 0,
};


node_out_t do_pg__Queue(struct state *state, struct input *in);
node_out_t do_pg__PacketDrop(struct state *state, struct input *in);
node_out_t do_pg__NotSupported(struct state *state, struct input *in);
node_out_t do_pg__RxL3IPv6ValidHeaderLength(struct state *state, struct input *in);
node_out_t do_pg__RxEchoAPP(struct state *state, struct input *in);
node_out_t do_pg__RxDnsAPP(struct state *state, struct input *in);
node_out_t do_pg__RxL2EtherClassified(struct state *state, struct input *in);
node_out_t do_pg__RxL2EtherValidLength(struct state *state, struct input *in);
node_out_t do_pg__RxL2EtherValidUnicast(struct state *state, struct input *in);
node_out_t do_pg__RxL2EtherValidMulticast(struct state *state, struct input *in);
node_out_t do_pg__RxL2EtherValidBroadcast(struct state *state, struct input *in);
node_out_t do_pg__RxL2EtherValidSrc(struct state *state, struct input *in);
node_out_t do_pg__RxL2EtherValidLocalMAC(struct state *state, struct input *in);
node_out_t do_pg__RxL2EtherValidType(struct state *state, struct input *in);
node_out_t do_pg__RxL2EtherClassifyL3(struct state *state, struct input *in);
node_out_t do_pg__RxL3ARPValidHeaderLength(struct state *state, struct input *in);
node_out_t do_pg__RxL3ARPClassify(struct state *state, struct input *in);
node_out_t do_pg__RxL3ARPLocalIPDest(struct state *state, struct input *in);
node_out_t do_pg__RxL3ARPValidRequest(struct state *state, struct input *in);
node_out_t do_pg__RxL3ARPValidResponse(struct state *state, struct input *in);
node_out_t do_pg__RxL3ARPIsPending(struct state *state, struct input *in);
node_out_t do_pg__RxL3ARPProcessPendingResponse(struct state *state, struct input *in);
node_out_t do_pg__RxL3IPv4ValidHeaderLength(struct state *state, struct input *in);
node_out_t do_pg__RxL3IPv4ValidReassembly(struct state *state, struct input *in);
node_out_t do_pg__RxL3IPv4ValidVersion(struct state *state, struct input *in);
node_out_t do_pg__RxL3IPv4ValidLength(struct state *state, struct input *in);
node_out_t do_pg__RxL3IPv4ValidTTL(struct state *state, struct input *in);
node_out_t do_pg__RxL3IPv4ValidChecksum(struct state *state, struct input *in);
node_out_t do_pg__RxL3IPv4ValidLocalIP(struct state *state, struct input *in);
node_out_t do_pg__RxL3IPv4Classify(struct state *state, struct input *in);
node_out_t do_pg__RxL3ICMPValidHeaderLength(struct state *state, struct input *in);
node_out_t do_pg__RxL3ICMPValidChecksum(struct state *state, struct input *in);
node_out_t do_pg__RxL3ICMPIsTypeRequest(struct state *state, struct input *in);
node_out_t do_pg__RxL4UDPValidHeaderLength(struct state *state, struct input *in);
node_out_t do_pg__RxL4UDPValidLength(struct state *state, struct input *in);
node_out_t do_pg__RxL4UDPValidChecksum(struct state *state, struct input *in);
node_out_t do_pg__RxL4UDPPortClassifyType(struct state *state, struct input *in);
node_out_t do_pg__RxL4UDPPortClassifyStatic(struct state *state, struct input *in);
node_out_t do_pg__RxL4UDPPortClassifyDynamic(struct state *state, struct input *in);
node_out_t do_pg__RxL4UDPClosedPortAction(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPValidHeaderLength(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPPortClassifyType(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPPortClassifyStatic(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPPortClassifyDynamic(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPClosedPortAction(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketClassify(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketServerSide(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketInClosed(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketClientSide(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketIsValidSyn(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketAddHalfOpenConn(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketSendSyn(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketIsValidSynAckS(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketIsValidFinAck(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketIsValidFinAck2(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketSChangeEstablished(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketIsDataPacket(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketIsFinSet(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketSChangeLastAck(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketSChangeToClosed(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketSChangeToClosed2(struct state *state, struct input *in);
node_out_t do_pg__RxL4TCPSocketCopyData(struct state *state, struct input *in);
node_out_t do_pg__RxTagTxTCPIR(struct state *state, struct input *in);
node_out_t do_pg__RxTagTxUDPIR(struct state *state, struct input *in);
node_out_t do_pg__RxTagTxARPIR(struct state *state, struct input *in);
node_out_t do_pg__RxTagTxARPLu(struct state *state, struct input *in);
node_out_t do_pg__RxTagTxICMPIR(struct state *state, struct input *in);
node_out_t do_pg__TxDemux(struct state *state, struct input *in);
node_out_t do_pg__TxQueue(struct state *state, struct input *in);
node_out_t do_pg__TxL4UDPInitiateResponse(struct state *state, struct input *in);
node_out_t do_pg__TxL4UDPAllocateHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL4UDPFillHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL4TCPInitiateResponse(struct state *state, struct input *in);
node_out_t do_pg__TxL4TCPAllocateHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL4TCPFillHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL3ICMPInitiateResponse(struct state *state, struct input *in);
node_out_t do_pg__TxL3ICMPAllocateHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL3ICMPFillHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL3IPv4AllocateHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL3IPv4FillHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL3IPv4Routing(struct state *state, struct input *in);
node_out_t do_pg__TxL3ARPInitiateResponse(struct state *state, struct input *in);
node_out_t do_pg__TxL3ARPAllocateHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL3ARPFillHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL3ARPLookupRequestIn(struct state *state, struct input *in);
node_out_t do_pg__TxL3ARPLookup_(struct state *state, struct input *in);
node_out_t do_pg__TxL3ARPSendRequest(struct state *state, struct input *in);
node_out_t do_pg__TxL2EtherAllocateHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL2EtherFillHeader(struct state *state, struct input *in);


static inline void executeGraph(struct state * st, struct input *in)
{
    node_out_t Queue=-1, PacketDrop=-1, NotSupported=-1, RxL3IPv6ValidHeaderLength=-1, RxL3IPValid=-1, RxL3IPAndBelowValid=-1, RxEchoAPP=-1, RxDnsAPP=-1, RxToTx=-1, RxL2EtherClassified=-1, RxL2EtherValidLength=-1, RxL2EtherValidUnicast=-1, RxL2EtherValidMulticast=-1, RxL2EtherValidBroadcast=-1, RxL2EtherValidDest=-1, RxL2EtherValidSrc=-1, RxL2EtherValidLocalMAC=-1, RxL2EtherValid=-1, RxL2EtherValidType=-1, RxL2EtherClassifyL3=-1, RxL3ARPValidHeaderLength=-1, RxL3ARPClassify=-1, RxL3ARPLocalIPDest=-1, RxL3ARPValidRequest=-1, RxL3ARPNeedsResponse=-1, RxL3ARPValidResponse=-1, RxL3ARPIsPending=-1, RxL3ARPValidPendingResponse=-1, RxL3ARPProcessPendingResponse=-1, RxL3IPv4ValidHeaderLength=-1, RxL3IPv4ValidReassembly=-1, RxL3IPv4ValidVersion=-1, RxL3IPv4ValidLength=-1, RxL3IPv4ValidTTL=-1, RxL3IPv4ValidChecksum=-1, RxL3IPv4ValidLocalIP=-1, RxL3IPv4Classify=-1, RxL3IPv4Valid=-1, RxL3ICMPValidHeaderLength=-1, RxL3ICMPValidChecksum=-1, RxL3ICMPValid=-1, RxL3ICMPIsTypeRequest=-1, RxL3ICMPNeedsResponse=-1, RxL4UDPValidHeaderLength=-1, RxL4UDPValidLength=-1, RxL4UDPValidChecksum=-1, RxL4UDPValid=-1, RxL4UDPPortClassifyType=-1, RxL4UDPPortClassifyStatic=-1, RxL4UDPPortClassifyDynamic=-1, RxL4UDPClosedPortAction=-1, RxL4TCPValidHeaderLength=-1, RxL4TCPValid=-1, RxL4TCPPortClassifyType=-1, RxL4TCPPortClassifyStatic=-1, RxL4TCPPortClassifyDynamic=-1, RxL4TCPClosedPortAction=-1, RxL4TCPSocketClassify=-1, RxL4TCPSocketServerSide=-1, RxL4TCPSocketInClosed=-1, RxL4TCPSocketClientSide=-1, RxL4TCPSocketIsValidSyn=-1, RxL4TCPSocketAddHalfOpenConn=-1, RxL4TCPSocketSendSyn=-1, RxL4TCPSocketIsValidSynAckS=-1, RxL4TCPSocketIsValidFinAck=-1, RxL4TCPSocketIsValidFinAck2=-1, RxL4TCPSocketSChangeEstablished=-1, RxL4TCPSocketIsDataPacket=-1, RxL4TCPSocketIsFinSet=-1, RxL4TCPSocketSChangeLastAck=-1, RxL4TCPSocketSChangeToClosed=-1, RxL4TCPSocketSChangeToClosed2=-1, RxL4TCPSocketCopyData=-1, RxL4TCPSocketTCPOutMerge=-1, RxTagTxTCPIR=-1, RxTagTxUDPIR=-1, RxTagTxARPIR=-1, RxTagTxARPLu=-1, RxTagTxICMPIR=-1, TxDemux=-1, TxQueue=-1, TxL4UDPInitiateResponse=-1, TxL4UDPAllocateHeader=-1, TxL4UDPFillHeader=-1, TxL4TCPInitiateResponse=-1, TxL4TCPAllocateHeader=-1, TxL4TCPFillHeader=-1, TxL3ICMPInitiateResponse=-1, TxL3ICMPAllocateHeader=-1, TxL3ICMPFillHeader=-1, TxL3IPv4Prepare=-1, TxL3IPv4AllocateHeader=-1, TxL3IPv4FillHeader=-1, TxL3IPv4Routing=-1, TxL3ARPInitiateResponse=-1, TxL3ARPPrepare=-1, TxL3ARPAllocateHeader=-1, TxL3ARPFillHeader=-1, TxL3ARPLookupRequestIn=-1, TxL3ARPLookup=-1, TxL3ARPLookup_=-1, TxL3ARPSendRequest=-1, TxL2EtherPrepare=-1, TxL2EtherAllocateHeader=-1, TxL2EtherFillHeader=-1;
    if (1) {
        RxL3IPv6ValidHeaderLength = do_pg__RxL3IPv6ValidHeaderLength(st, in);
        ddprint("RxL3IPv6ValidHeaderLength=%d\n", RxL3IPv6ValidHeaderLength);
    }
    if (1) {
        Queue = do_pg__Queue(st, in);
        ddprint("Queue=%d\n", Queue);
    }
    if (Queue == P_Queue_out) {
        RxL2EtherClassified = do_pg__RxL2EtherClassified(st, in);
        ddprint("RxL2EtherClassified=%d\n", RxL2EtherClassified);
    }
    if (RxL2EtherClassified == P_true) {
        RxL2EtherValidLength = do_pg__RxL2EtherValidLength(st, in);
        ddprint("RxL2EtherValidLength=%d\n", RxL2EtherValidLength);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidUnicast = do_pg__RxL2EtherValidUnicast(st, in);
        ddprint("RxL2EtherValidUnicast=%d\n", RxL2EtherValidUnicast);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidMulticast = do_pg__RxL2EtherValidMulticast(st, in);
        ddprint("RxL2EtherValidMulticast=%d\n", RxL2EtherValidMulticast);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidBroadcast = do_pg__RxL2EtherValidBroadcast(st, in);
        ddprint("RxL2EtherValidBroadcast=%d\n", RxL2EtherValidBroadcast);
    }
    if (RxL2EtherValidBroadcast == P_true || RxL2EtherValidMulticast == P_true || RxL2EtherValidUnicast == P_true) {
        RxL2EtherValidDest = P_true;
        ddprint("RxL2EtherValidDest=%d\n", RxL2EtherValidDest);
    }
    if (RxL2EtherValidBroadcast == P_false && RxL2EtherValidMulticast == P_false && RxL2EtherValidUnicast == P_false) {
        RxL2EtherValidDest = P_false;
        ddprint("RxL2EtherValidDest=%d\n", RxL2EtherValidDest);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidSrc = do_pg__RxL2EtherValidSrc(st, in);
        ddprint("RxL2EtherValidSrc=%d\n", RxL2EtherValidSrc);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidLocalMAC = do_pg__RxL2EtherValidLocalMAC(st, in);
        ddprint("RxL2EtherValidLocalMAC=%d\n", RxL2EtherValidLocalMAC);
    }
    if (RxL2EtherValidLocalMAC == P_true && RxL2EtherValidSrc == P_true && RxL2EtherValidDest == P_true) {
        RxL2EtherValid = P_true;
        ddprint("RxL2EtherValid=%d\n", RxL2EtherValid);
    }
    if (RxL2EtherValidLocalMAC == P_false || RxL2EtherValidSrc == P_false || RxL2EtherValidDest == P_false) {
        RxL2EtherValid = P_false;
        ddprint("RxL2EtherValid=%d\n", RxL2EtherValid);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidType = do_pg__RxL2EtherValidType(st, in);
        ddprint("RxL2EtherValidType=%d\n", RxL2EtherValidType);
    }
    if (RxL2EtherValidType == P_true) {
        RxL2EtherClassifyL3 = do_pg__RxL2EtherClassifyL3(st, in);
        ddprint("RxL2EtherClassifyL3=%d\n", RxL2EtherClassifyL3);
    }
    if (RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_ipv6) {
        NotSupported = do_pg__NotSupported(st, in);
        ddprint("NotSupported=%d\n", NotSupported);
    }
    if (RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_arp) {
        RxL3ARPValidHeaderLength = do_pg__RxL3ARPValidHeaderLength(st, in);
        ddprint("RxL3ARPValidHeaderLength=%d\n", RxL3ARPValidHeaderLength);
    }
    if (RxL3ARPValidHeaderLength == P_true) {
        RxL3ARPClassify = do_pg__RxL3ARPClassify(st, in);
        ddprint("RxL3ARPClassify=%d\n", RxL3ARPClassify);
    }
    if (RxL3ARPClassify == P_RxL3ARPClassify_request) {
        RxL3ARPValidRequest = do_pg__RxL3ARPValidRequest(st, in);
        ddprint("RxL3ARPValidRequest=%d\n", RxL3ARPValidRequest);
    }
    if (RxL3ARPClassify == P_RxL3ARPClassify_response) {
        RxL3ARPValidResponse = do_pg__RxL3ARPValidResponse(st, in);
        ddprint("RxL3ARPValidResponse=%d\n", RxL3ARPValidResponse);
    }
    if (RxL3ARPValidResponse == P_true || RxL3ARPValidResponse == P_false) {
        RxL3ARPIsPending = do_pg__RxL3ARPIsPending(st, in);
        ddprint("RxL3ARPIsPending=%d\n", RxL3ARPIsPending);
    }
    if (RxL3ARPValidHeaderLength == P_true) {
        RxL3ARPLocalIPDest = do_pg__RxL3ARPLocalIPDest(st, in);
        ddprint("RxL3ARPLocalIPDest=%d\n", RxL3ARPLocalIPDest);
    }
    if (RxL3ARPValidRequest == P_true && RxL3ARPLocalIPDest == P_true) {
        RxL3ARPNeedsResponse = P_true;
        ddprint("RxL3ARPNeedsResponse=%d\n", RxL3ARPNeedsResponse);
    }
    if (RxL3ARPValidRequest == P_false || RxL3ARPLocalIPDest == P_false) {
        RxL3ARPNeedsResponse = P_false;
        ddprint("RxL3ARPNeedsResponse=%d\n", RxL3ARPNeedsResponse);
    }
    if (RxL3ARPNeedsResponse == P_true) {
        RxTagTxARPIR = do_pg__RxTagTxARPIR(st, in);
        ddprint("RxTagTxARPIR=%d\n", RxTagTxARPIR);
    }
    if (RxL3ARPIsPending == P_true && RxL3ARPValidResponse == P_true && RxL3ARPLocalIPDest == P_true) {
        RxL3ARPValidPendingResponse = P_true;
        ddprint("RxL3ARPValidPendingResponse=%d\n", RxL3ARPValidPendingResponse);
    }
    if (RxL3ARPIsPending == P_false || RxL3ARPValidResponse == P_false || RxL3ARPLocalIPDest == P_false) {
        RxL3ARPValidPendingResponse = P_false;
        ddprint("RxL3ARPValidPendingResponse=%d\n", RxL3ARPValidPendingResponse);
    }
    if (RxL3ARPValidPendingResponse == P_true) {
        RxL3ARPProcessPendingResponse = do_pg__RxL3ARPProcessPendingResponse(st, in);
        ddprint("RxL3ARPProcessPendingResponse=%d\n", RxL3ARPProcessPendingResponse);
    }
    if (RxL3ARPProcessPendingResponse == P_RxL3ARPProcessPendingResponse_true) {
        RxTagTxARPLu = do_pg__RxTagTxARPLu(st, in);
        ddprint("RxTagTxARPLu=%d\n", RxTagTxARPLu);
    }
    if (RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_ipv4) {
        RxL3IPv4ValidHeaderLength = do_pg__RxL3IPv4ValidHeaderLength(st, in);
        ddprint("RxL3IPv4ValidHeaderLength=%d\n", RxL3IPv4ValidHeaderLength);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidReassembly = do_pg__RxL3IPv4ValidReassembly(st, in);
        ddprint("RxL3IPv4ValidReassembly=%d\n", RxL3IPv4ValidReassembly);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidVersion = do_pg__RxL3IPv4ValidVersion(st, in);
        ddprint("RxL3IPv4ValidVersion=%d\n", RxL3IPv4ValidVersion);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidLength = do_pg__RxL3IPv4ValidLength(st, in);
        ddprint("RxL3IPv4ValidLength=%d\n", RxL3IPv4ValidLength);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidTTL = do_pg__RxL3IPv4ValidTTL(st, in);
        ddprint("RxL3IPv4ValidTTL=%d\n", RxL3IPv4ValidTTL);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidChecksum = do_pg__RxL3IPv4ValidChecksum(st, in);
        ddprint("RxL3IPv4ValidChecksum=%d\n", RxL3IPv4ValidChecksum);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidLocalIP = do_pg__RxL3IPv4ValidLocalIP(st, in);
        ddprint("RxL3IPv4ValidLocalIP=%d\n", RxL3IPv4ValidLocalIP);
    }
    if (RxL3IPv4ValidLocalIP == P_true && RxL3IPv4ValidChecksum == P_true && RxL3IPv4ValidTTL == P_true && RxL3IPv4ValidLength == P_true && RxL3IPv4ValidVersion == P_true && RxL3IPv4ValidReassembly == P_true) {
        RxL3IPv4Valid = P_true;
        ddprint("RxL3IPv4Valid=%d\n", RxL3IPv4Valid);
    }
    if (RxL3IPv4ValidLocalIP == P_false || RxL3IPv4ValidChecksum == P_false || RxL3IPv4ValidTTL == P_false || RxL3IPv4ValidLength == P_false || RxL3IPv4ValidVersion == P_false || RxL3IPv4ValidReassembly == P_false) {
        RxL3IPv4Valid = P_false;
        ddprint("RxL3IPv4Valid=%d\n", RxL3IPv4Valid);
    }
    if (RxL3IPv4Valid == P_true || RxL3IPv6ValidHeaderLength == P_true) {
        RxL3IPValid = P_true;
        ddprint("RxL3IPValid=%d\n", RxL3IPValid);
    }
    if (RxL3IPv4Valid == P_false && RxL3IPv6ValidHeaderLength == P_false) {
        RxL3IPValid = P_false;
        ddprint("RxL3IPValid=%d\n", RxL3IPValid);
    }
    if (RxL2EtherValid == P_true && RxL3IPValid == P_true) {
        RxL3IPAndBelowValid = P_true;
        ddprint("RxL3IPAndBelowValid=%d\n", RxL3IPAndBelowValid);
    }
    if (RxL2EtherValid == P_false || RxL3IPValid == P_false) {
        RxL3IPAndBelowValid = P_false;
        ddprint("RxL3IPAndBelowValid=%d\n", RxL3IPAndBelowValid);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4Classify = do_pg__RxL3IPv4Classify(st, in);
        ddprint("RxL3IPv4Classify=%d\n", RxL3IPv4Classify);
    }
    if (RxL3IPv4Classify == P_RxL3IPv4Classify_icmp) {
        RxL3ICMPValidHeaderLength = do_pg__RxL3ICMPValidHeaderLength(st, in);
        ddprint("RxL3ICMPValidHeaderLength=%d\n", RxL3ICMPValidHeaderLength);
    }
    if (RxL3ICMPValidHeaderLength == P_true) {
        RxL3ICMPValidChecksum = do_pg__RxL3ICMPValidChecksum(st, in);
        ddprint("RxL3ICMPValidChecksum=%d\n", RxL3ICMPValidChecksum);
    }
    if (RxL3ICMPValidChecksum == P_true && RxL3IPAndBelowValid == P_true) {
        RxL3ICMPValid = P_true;
        ddprint("RxL3ICMPValid=%d\n", RxL3ICMPValid);
    }
    if (RxL3ICMPValidChecksum == P_false || RxL3IPAndBelowValid == P_false) {
        RxL3ICMPValid = P_false;
        ddprint("RxL3ICMPValid=%d\n", RxL3ICMPValid);
    }
    if (RxL3ICMPValidHeaderLength == P_true) {
        RxL3ICMPIsTypeRequest = do_pg__RxL3ICMPIsTypeRequest(st, in);
        ddprint("RxL3ICMPIsTypeRequest=%d\n", RxL3ICMPIsTypeRequest);
    }
    if (RxL3ICMPIsTypeRequest == P_true && RxL3ICMPValid == P_true) {
        RxL3ICMPNeedsResponse = P_true;
        ddprint("RxL3ICMPNeedsResponse=%d\n", RxL3ICMPNeedsResponse);
    }
    if (RxL3ICMPIsTypeRequest == P_false || RxL3ICMPValid == P_false) {
        RxL3ICMPNeedsResponse = P_false;
        ddprint("RxL3ICMPNeedsResponse=%d\n", RxL3ICMPNeedsResponse);
    }
    if (RxL3ICMPNeedsResponse == P_true) {
        RxTagTxICMPIR = do_pg__RxTagTxICMPIR(st, in);
        ddprint("RxTagTxICMPIR=%d\n", RxTagTxICMPIR);
    }
    if (RxL3IPv4Classify == P_RxL3IPv4Classify_udp) {
        RxL4UDPValidHeaderLength = do_pg__RxL4UDPValidHeaderLength(st, in);
        ddprint("RxL4UDPValidHeaderLength=%d\n", RxL4UDPValidHeaderLength);
    }
    if (RxL4UDPValidHeaderLength == P_true) {
        RxL4UDPValidLength = do_pg__RxL4UDPValidLength(st, in);
        ddprint("RxL4UDPValidLength=%d\n", RxL4UDPValidLength);
    }
    if (RxL4UDPValidHeaderLength == P_true) {
        RxL4UDPValidChecksum = do_pg__RxL4UDPValidChecksum(st, in);
        ddprint("RxL4UDPValidChecksum=%d\n", RxL4UDPValidChecksum);
    }
    if (RxL4UDPValidChecksum == P_true && RxL4UDPValidLength == P_true && RxL3IPAndBelowValid == P_true) {
        RxL4UDPValid = P_true;
        ddprint("RxL4UDPValid=%d\n", RxL4UDPValid);
    }
    if (RxL4UDPValidChecksum == P_false || RxL4UDPValidLength == P_false || RxL3IPAndBelowValid == P_false) {
        RxL4UDPValid = P_false;
        ddprint("RxL4UDPValid=%d\n", RxL4UDPValid);
    }
    if (RxL4UDPValid == P_true) {
        RxL4UDPPortClassifyType = do_pg__RxL4UDPPortClassifyType(st, in);
        ddprint("RxL4UDPPortClassifyType=%d\n", RxL4UDPPortClassifyType);
    }
    if (RxL4UDPPortClassifyType == P_RxL4UDPPortClassifyType_dynamic) {
        RxL4UDPPortClassifyDynamic = do_pg__RxL4UDPPortClassifyDynamic(st, in);
        ddprint("RxL4UDPPortClassifyDynamic=%d\n", RxL4UDPPortClassifyDynamic);
    }
    if (RxL4UDPPortClassifyType == P_RxL4UDPPortClassifyType_static) {
        RxL4UDPPortClassifyStatic = do_pg__RxL4UDPPortClassifyStatic(st, in);
        ddprint("RxL4UDPPortClassifyStatic=%d\n", RxL4UDPPortClassifyStatic);
    }
    if (RxL4UDPPortClassifyStatic == P_RxL4UDPPortClassifyStatic_appDNS) {
        RxDnsAPP = do_pg__RxDnsAPP(st, in);
        ddprint("RxDnsAPP=%d\n", RxDnsAPP);
    }
    if (RxL4UDPPortClassifyStatic == P_RxL4UDPPortClassifyStatic_appEcho) {
        RxEchoAPP = do_pg__RxEchoAPP(st, in);
        ddprint("RxEchoAPP=%d\n", RxEchoAPP);
    }
    if (RxDnsAPP == P_RxDnsAPP_out || RxEchoAPP == P_RxEchoAPP_out) {
        RxTagTxUDPIR = do_pg__RxTagTxUDPIR(st, in);
        ddprint("RxTagTxUDPIR=%d\n", RxTagTxUDPIR);
    }
    if (RxL4UDPPortClassifyStatic == P_RxL4UDPPortClassifyStatic_closedPort) {
        RxL4UDPClosedPortAction = do_pg__RxL4UDPClosedPortAction(st, in);
        ddprint("RxL4UDPClosedPortAction=%d\n", RxL4UDPClosedPortAction);
    }
    if (RxL3IPv4Classify == P_RxL3IPv4Classify_tcp) {
        RxL4TCPValidHeaderLength = do_pg__RxL4TCPValidHeaderLength(st, in);
        ddprint("RxL4TCPValidHeaderLength=%d\n", RxL4TCPValidHeaderLength);
    }
    if (RxL4TCPValidHeaderLength == P_true && RxL3IPAndBelowValid == P_true) {
        RxL4TCPValid = P_true;
        ddprint("RxL4TCPValid=%d\n", RxL4TCPValid);
    }
    if (RxL4TCPValidHeaderLength == P_false || RxL3IPAndBelowValid == P_false) {
        RxL4TCPValid = P_false;
        ddprint("RxL4TCPValid=%d\n", RxL4TCPValid);
    }
    if (RxL4TCPValid == P_true) {
        RxL4TCPPortClassifyType = do_pg__RxL4TCPPortClassifyType(st, in);
        ddprint("RxL4TCPPortClassifyType=%d\n", RxL4TCPPortClassifyType);
    }
    if (RxL4TCPPortClassifyType == P_RxL4TCPPortClassifyType_dynamic) {
        RxL4TCPPortClassifyDynamic = do_pg__RxL4TCPPortClassifyDynamic(st, in);
        ddprint("RxL4TCPPortClassifyDynamic=%d\n", RxL4TCPPortClassifyDynamic);
    }
    if (RxL4TCPPortClassifyType == P_RxL4TCPPortClassifyType_static) {
        RxL4TCPPortClassifyStatic = do_pg__RxL4TCPPortClassifyStatic(st, in);
        ddprint("RxL4TCPPortClassifyStatic=%d\n", RxL4TCPPortClassifyStatic);
    }
    if (RxL4TCPPortClassifyStatic == P_RxL4TCPPortClassifyStatic_noSocket) {
        RxL4TCPClosedPortAction = do_pg__RxL4TCPClosedPortAction(st, in);
        ddprint("RxL4TCPClosedPortAction=%d\n", RxL4TCPClosedPortAction);
    }
    if (RxL4TCPPortClassifyStatic == P_RxL4TCPPortClassifyStatic_toSocket) {
        RxL4TCPSocketClassify = do_pg__RxL4TCPSocketClassify(st, in);
        ddprint("RxL4TCPSocketClassify=%d\n", RxL4TCPSocketClassify);
    }
    if (RxL4TCPSocketClassify == P_RxL4TCPSocketClassify_cliSocket) {
        RxL4TCPSocketClientSide = do_pg__RxL4TCPSocketClientSide(st, in);
        ddprint("RxL4TCPSocketClientSide=%d\n", RxL4TCPSocketClientSide);
    }
    if (RxL4TCPSocketClassify == P_RxL4TCPSocketClassify_srvSocket) {
        RxL4TCPSocketServerSide = do_pg__RxL4TCPSocketServerSide(st, in);
        ddprint("RxL4TCPSocketServerSide=%d\n", RxL4TCPSocketServerSide);
    }
    if (RxL4TCPSocketServerSide == P_RxL4TCPSocketServerSide_isClosed) {
        RxL4TCPSocketInClosed = do_pg__RxL4TCPSocketInClosed(st, in);
        ddprint("RxL4TCPSocketInClosed=%d\n", RxL4TCPSocketInClosed);
    }
    if (RxL4TCPSocketServerSide == P_RxL4TCPSocketServerSide_isListen) {
        RxL4TCPSocketIsValidSyn = do_pg__RxL4TCPSocketIsValidSyn(st, in);
        ddprint("RxL4TCPSocketIsValidSyn=%d\n", RxL4TCPSocketIsValidSyn);
    }
    if (RxL4TCPSocketIsValidSyn == P_true) {
        RxL4TCPSocketAddHalfOpenConn = do_pg__RxL4TCPSocketAddHalfOpenConn(st, in);
        ddprint("RxL4TCPSocketAddHalfOpenConn=%d\n", RxL4TCPSocketAddHalfOpenConn);
    }
    if (RxL4TCPSocketAddHalfOpenConn == P_RxL4TCPSocketAddHalfOpenConn_success) {
        RxL4TCPSocketSendSyn = do_pg__RxL4TCPSocketSendSyn(st, in);
        ddprint("RxL4TCPSocketSendSyn=%d\n", RxL4TCPSocketSendSyn);
    }
    if (RxL4TCPSocketServerSide == P_RxL4TCPSocketServerSide_isSynRecv) {
        RxL4TCPSocketIsValidSynAckS = do_pg__RxL4TCPSocketIsValidSynAckS(st, in);
        ddprint("RxL4TCPSocketIsValidSynAckS=%d\n", RxL4TCPSocketIsValidSynAckS);
    }
    if (RxL4TCPSocketIsValidSynAckS == P_true) {
        RxL4TCPSocketSChangeEstablished = do_pg__RxL4TCPSocketSChangeEstablished(st, in);
        ddprint("RxL4TCPSocketSChangeEstablished=%d\n", RxL4TCPSocketSChangeEstablished);
    }
    if (RxL4TCPSocketIsValidSynAckS == P_false || RxL4TCPSocketIsValidSyn == P_false || RxL4TCPSocketInClosed == P_RxL4TCPSocketInClosed_out || RxL4TCPClosedPortAction == P_RxL4TCPClosedPortAction_out || RxL4TCPValid == P_false || RxL4UDPClosedPortAction == P_RxL4UDPClosedPortAction_out || RxL4UDPValid == P_false || RxL4UDPValidHeaderLength == P_false || RxL3IPv4Classify == P_RxL3IPv4Classify_drop || RxL3IPv4ValidHeaderLength == P_false || RxL3ARPClassify == P_RxL3ARPClassify_drop || RxL3ARPValidHeaderLength == P_false || RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_drop || RxL2EtherValidType == P_false || RxL2EtherValidLength == P_false || RxL2EtherClassified == P_false || RxDnsAPP == P_RxDnsAPP_drop || RxEchoAPP == P_RxEchoAPP_drop) {
        PacketDrop = do_pg__PacketDrop(st, in);
        ddprint("PacketDrop=%d\n", PacketDrop);
    }
    if (RxL4TCPSocketServerSide == P_RxL4TCPSocketServerSide_isCloseWait) {
        RxL4TCPSocketIsValidFinAck = do_pg__RxL4TCPSocketIsValidFinAck(st, in);
        ddprint("RxL4TCPSocketIsValidFinAck=%d\n", RxL4TCPSocketIsValidFinAck);
    }
    if (RxL4TCPSocketIsValidFinAck == P_true) {
        RxL4TCPSocketSChangeToClosed = do_pg__RxL4TCPSocketSChangeToClosed(st, in);
        ddprint("RxL4TCPSocketSChangeToClosed=%d\n", RxL4TCPSocketSChangeToClosed);
    }
    if (RxL4TCPSocketServerSide == P_RxL4TCPSocketServerSide_isLastAck) {
        RxL4TCPSocketIsValidFinAck2 = do_pg__RxL4TCPSocketIsValidFinAck2(st, in);
        ddprint("RxL4TCPSocketIsValidFinAck2=%d\n", RxL4TCPSocketIsValidFinAck2);
    }
    if (RxL4TCPSocketIsValidFinAck2 == P_true) {
        RxL4TCPSocketSChangeToClosed2 = do_pg__RxL4TCPSocketSChangeToClosed2(st, in);
        ddprint("RxL4TCPSocketSChangeToClosed2=%d\n", RxL4TCPSocketSChangeToClosed2);
    }
    if (RxL4TCPSocketServerSide == P_RxL4TCPSocketServerSide_isEstablished) {
        RxL4TCPSocketIsDataPacket = do_pg__RxL4TCPSocketIsDataPacket(st, in);
        ddprint("RxL4TCPSocketIsDataPacket=%d\n", RxL4TCPSocketIsDataPacket);
    }
    if (RxL4TCPSocketIsDataPacket == P_true) {
        RxL4TCPSocketCopyData = do_pg__RxL4TCPSocketCopyData(st, in);
        ddprint("RxL4TCPSocketCopyData=%d\n", RxL4TCPSocketCopyData);
    }
    if (RxL4TCPSocketServerSide == P_RxL4TCPSocketServerSide_isEstablished) {
        RxL4TCPSocketIsFinSet = do_pg__RxL4TCPSocketIsFinSet(st, in);
        ddprint("RxL4TCPSocketIsFinSet=%d\n", RxL4TCPSocketIsFinSet);
    }
    if (RxL4TCPSocketIsFinSet == P_true) {
        RxL4TCPSocketSChangeLastAck = do_pg__RxL4TCPSocketSChangeLastAck(st, in);
        ddprint("RxL4TCPSocketSChangeLastAck=%d\n", RxL4TCPSocketSChangeLastAck);
    }
    if (RxL4TCPSocketCopyData == P_true || RxL4TCPSocketSChangeToClosed == P_true || RxL4TCPSocketSChangeLastAck == P_true || RxL4TCPSocketSendSyn == P_true) {
        RxL4TCPSocketTCPOutMerge = P_true;
        ddprint("RxL4TCPSocketTCPOutMerge=%d\n", RxL4TCPSocketTCPOutMerge);
    }
    if (RxL4TCPSocketCopyData == P_false && RxL4TCPSocketSChangeToClosed == P_false && RxL4TCPSocketSChangeLastAck == P_false && RxL4TCPSocketSendSyn == P_false) {
        RxL4TCPSocketTCPOutMerge = P_false;
        ddprint("RxL4TCPSocketTCPOutMerge=%d\n", RxL4TCPSocketTCPOutMerge);
    }
    if (RxL4TCPSocketTCPOutMerge == P_true) {
        RxTagTxTCPIR = do_pg__RxTagTxTCPIR(st, in);
        ddprint("RxTagTxTCPIR=%d\n", RxTagTxTCPIR);
    }
    if (RxTagTxICMPIR == P_true || RxTagTxARPLu == P_true || RxTagTxARPIR == P_true || RxTagTxUDPIR == P_true || RxTagTxTCPIR == P_true) {
        RxToTx = P_true;
        ddprint("RxToTx=%d\n", RxToTx);
    }
    if (RxTagTxICMPIR == P_false && RxTagTxARPLu == P_false && RxTagTxARPIR == P_false && RxTagTxUDPIR == P_false && RxTagTxTCPIR == P_false) {
        RxToTx = P_false;
        ddprint("RxToTx=%d\n", RxToTx);
    }
    if (RxToTx == P_true) {
        TxDemux = do_pg__TxDemux(st, in);
        ddprint("TxDemux=%d\n", TxDemux);
    }
    if (TxDemux == P_TxDemux_UDPIR) {
        TxL4UDPInitiateResponse = do_pg__TxL4UDPInitiateResponse(st, in);
        ddprint("TxL4UDPInitiateResponse=%d\n", TxL4UDPInitiateResponse);
    }
    if (TxL4UDPInitiateResponse == P_TxL4UDPInitiateResponse_out) {
        TxL4UDPAllocateHeader = do_pg__TxL4UDPAllocateHeader(st, in);
        ddprint("TxL4UDPAllocateHeader=%d\n", TxL4UDPAllocateHeader);
    }
    if (TxL4UDPAllocateHeader == P_TxL4UDPAllocateHeader_out) {
        TxL4UDPFillHeader = do_pg__TxL4UDPFillHeader(st, in);
        ddprint("TxL4UDPFillHeader=%d\n", TxL4UDPFillHeader);
    }
    if (TxDemux == P_TxDemux_TCPIR) {
        TxL4TCPInitiateResponse = do_pg__TxL4TCPInitiateResponse(st, in);
        ddprint("TxL4TCPInitiateResponse=%d\n", TxL4TCPInitiateResponse);
    }
    if (TxL4TCPInitiateResponse == P_TxL4TCPInitiateResponse_out) {
        TxL4TCPAllocateHeader = do_pg__TxL4TCPAllocateHeader(st, in);
        ddprint("TxL4TCPAllocateHeader=%d\n", TxL4TCPAllocateHeader);
    }
    if (TxL4TCPAllocateHeader == P_TxL4TCPAllocateHeader_out) {
        TxL4TCPFillHeader = do_pg__TxL4TCPFillHeader(st, in);
        ddprint("TxL4TCPFillHeader=%d\n", TxL4TCPFillHeader);
    }
    if (TxDemux == P_TxDemux_ICMPIR) {
        TxL3ICMPInitiateResponse = do_pg__TxL3ICMPInitiateResponse(st, in);
        ddprint("TxL3ICMPInitiateResponse=%d\n", TxL3ICMPInitiateResponse);
    }
    if (TxL3ICMPInitiateResponse == P_TxL3ICMPInitiateResponse_out) {
        TxL3ICMPAllocateHeader = do_pg__TxL3ICMPAllocateHeader(st, in);
        ddprint("TxL3ICMPAllocateHeader=%d\n", TxL3ICMPAllocateHeader);
    }
    if (TxL3ICMPAllocateHeader == P_TxL3ICMPAllocateHeader_out) {
        TxL3ICMPFillHeader = do_pg__TxL3ICMPFillHeader(st, in);
        ddprint("TxL3ICMPFillHeader=%d\n", TxL3ICMPFillHeader);
    }
    if (TxL3ICMPFillHeader == P_true || TxL4TCPFillHeader == P_true || TxL4UDPFillHeader == P_true) {
        TxL3IPv4Prepare = P_true;
        ddprint("TxL3IPv4Prepare=%d\n", TxL3IPv4Prepare);
    }
    if (TxL3ICMPFillHeader == P_false && TxL4TCPFillHeader == P_false && TxL4UDPFillHeader == P_false) {
        TxL3IPv4Prepare = P_false;
        ddprint("TxL3IPv4Prepare=%d\n", TxL3IPv4Prepare);
    }
    if (TxL3IPv4Prepare == P_true) {
        TxL3IPv4AllocateHeader = do_pg__TxL3IPv4AllocateHeader(st, in);
        ddprint("TxL3IPv4AllocateHeader=%d\n", TxL3IPv4AllocateHeader);
    }
    if (TxL3IPv4AllocateHeader == P_TxL3IPv4AllocateHeader_out) {
        TxL3IPv4FillHeader = do_pg__TxL3IPv4FillHeader(st, in);
        ddprint("TxL3IPv4FillHeader=%d\n", TxL3IPv4FillHeader);
    }
    if (TxL3IPv4FillHeader == P_TxL3IPv4FillHeader_out) {
        TxL3IPv4Routing = do_pg__TxL3IPv4Routing(st, in);
        ddprint("TxL3IPv4Routing=%d\n", TxL3IPv4Routing);
    }
    if (TxDemux == P_TxDemux_ARPIR) {
        TxL3ARPInitiateResponse = do_pg__TxL3ARPInitiateResponse(st, in);
        ddprint("TxL3ARPInitiateResponse=%d\n", TxL3ARPInitiateResponse);
    }
    if (TxDemux == P_TxDemux_ARPLu) {
        TxL3ARPLookupRequestIn = do_pg__TxL3ARPLookupRequestIn(st, in);
        ddprint("TxL3ARPLookupRequestIn=%d\n", TxL3ARPLookupRequestIn);
    }
    if (TxL3ARPLookupRequestIn == P_true || TxL3IPv4Routing == P_true) {
        TxL3ARPLookup = P_true;
        ddprint("TxL3ARPLookup=%d\n", TxL3ARPLookup);
    }
    if (TxL3ARPLookupRequestIn == P_false && TxL3IPv4Routing == P_false) {
        TxL3ARPLookup = P_false;
        ddprint("TxL3ARPLookup=%d\n", TxL3ARPLookup);
    }
    if (TxL3ARPLookup == P_true) {
        TxL3ARPLookup_ = do_pg__TxL3ARPLookup_(st, in);
        ddprint("TxL3ARPLookup_=%d\n", TxL3ARPLookup_);
    }
    if (TxL3ARPLookup_ == P_TxL3ARPLookup__miss) {
        TxL3ARPSendRequest = do_pg__TxL3ARPSendRequest(st, in);
        ddprint("TxL3ARPSendRequest=%d\n", TxL3ARPSendRequest);
    }
    if (TxL3ARPSendRequest == P_TxL3ARPSendRequest_true || TxL3ARPInitiateResponse == P_TxL3ARPInitiateResponse_true) {
        TxL3ARPPrepare = P_true;
        ddprint("TxL3ARPPrepare=%d\n", TxL3ARPPrepare);
    }
    if (TxL3ARPSendRequest == P_TxL3ARPSendRequest_false && TxL3ARPInitiateResponse == P_TxL3ARPInitiateResponse_false) {
        TxL3ARPPrepare = P_false;
        ddprint("TxL3ARPPrepare=%d\n", TxL3ARPPrepare);
    }
    if (TxL3ARPPrepare == P_true) {
        TxL3ARPAllocateHeader = do_pg__TxL3ARPAllocateHeader(st, in);
        ddprint("TxL3ARPAllocateHeader=%d\n", TxL3ARPAllocateHeader);
    }
    if (TxL3ARPAllocateHeader == P_TxL3ARPAllocateHeader_out) {
        TxL3ARPFillHeader = do_pg__TxL3ARPFillHeader(st, in);
        ddprint("TxL3ARPFillHeader=%d\n", TxL3ARPFillHeader);
    }
    if (TxL3ARPLookup_ == P_TxL3ARPLookup__true || TxL3ARPFillHeader == P_true) {
        TxL2EtherPrepare = P_true;
        ddprint("TxL2EtherPrepare=%d\n", TxL2EtherPrepare);
    }
    if (TxL3ARPLookup_ == P_TxL3ARPLookup__false && TxL3ARPFillHeader == P_false) {
        TxL2EtherPrepare = P_false;
        ddprint("TxL2EtherPrepare=%d\n", TxL2EtherPrepare);
    }
    if (TxL2EtherPrepare == P_true) {
        TxL2EtherAllocateHeader = do_pg__TxL2EtherAllocateHeader(st, in);
        ddprint("TxL2EtherAllocateHeader=%d\n", TxL2EtherAllocateHeader);
    }
    if (TxL2EtherAllocateHeader == P_TxL2EtherAllocateHeader_out) {
        TxL2EtherFillHeader = do_pg__TxL2EtherFillHeader(st, in);
        ddprint("TxL2EtherFillHeader=%d\n", TxL2EtherFillHeader);
    }
    if (TxL2EtherFillHeader == P_TxL2EtherFillHeader_out) {
        TxQueue = do_pg__TxQueue(st, in);
        ddprint("TxQueue=%d\n", TxQueue);
    }
}



#if 0

node_out_t do_pg__Queue(struct state *state, struct input *in)
{
    // P_Queue_out
    return 0;
}

node_out_t do_pg__PacketDrop(struct state *state, struct input *in)
{
    // 
    return 0;
}

node_out_t do_pg__NotSupported(struct state *state, struct input *in)
{
    // 
    return 0;
}

node_out_t do_pg__RxL3IPv6ValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxEchoAPP(struct state *state, struct input *in)
{
    // P_RxEchoAPP_out, P_RxEchoAPP_drop
    return 0;
}

node_out_t do_pg__RxDnsAPP(struct state *state, struct input *in)
{
    // P_RxDnsAPP_out, P_RxDnsAPP_drop
    return 0;
}

node_out_t do_pg__RxL2EtherClassified(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL2EtherValidLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL2EtherValidUnicast(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL2EtherValidMulticast(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL2EtherValidBroadcast(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL2EtherValidSrc(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL2EtherValidLocalMAC(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL2EtherValidType(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL2EtherClassifyL3(struct state *state, struct input *in)
{
    // P_RxL2EtherClassifyL3_ipv4, P_RxL2EtherClassifyL3_ipv6, P_RxL2EtherClassifyL3_arp, P_RxL2EtherClassifyL3_drop
    return 0;
}

node_out_t do_pg__RxL3ARPValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPClassify(struct state *state, struct input *in)
{
    // P_RxL3ARPClassify_request, P_RxL3ARPClassify_response, P_RxL3ARPClassify_drop
    return 0;
}

node_out_t do_pg__RxL3ARPLocalIPDest(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPValidRequest(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPValidResponse(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPIsPending(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPProcessPendingResponse(struct state *state, struct input *in)
{
    // P_RxL3ARPProcessPendingResponse_true, P_RxL3ARPProcessPendingResponse_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidReassembly(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidVersion(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidTTL(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidChecksum(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidLocalIP(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4Classify(struct state *state, struct input *in)
{
    // P_RxL3IPv4Classify_tcp, P_RxL3IPv4Classify_udp, P_RxL3IPv4Classify_icmp, P_RxL3IPv4Classify_drop
    return 0;
}

node_out_t do_pg__RxL3ICMPValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ICMPValidChecksum(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ICMPIsTypeRequest(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4UDPValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4UDPValidLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4UDPValidChecksum(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4UDPPortClassifyType(struct state *state, struct input *in)
{
    // P_RxL4UDPPortClassifyType_static, P_RxL4UDPPortClassifyType_dynamic
    return 0;
}

node_out_t do_pg__RxL4UDPPortClassifyStatic(struct state *state, struct input *in)
{
    // P_RxL4UDPPortClassifyStatic_appDNS, P_RxL4UDPPortClassifyStatic_appEcho, P_RxL4UDPPortClassifyStatic_closedPort
    return 0;
}

node_out_t do_pg__RxL4UDPPortClassifyDynamic(struct state *state, struct input *in)
{
    // 
    return 0;
}

node_out_t do_pg__RxL4UDPClosedPortAction(struct state *state, struct input *in)
{
    // P_RxL4UDPClosedPortAction_out
    return 0;
}

node_out_t do_pg__RxL4TCPValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPPortClassifyType(struct state *state, struct input *in)
{
    // P_RxL4TCPPortClassifyType_static, P_RxL4TCPPortClassifyType_dynamic
    return 0;
}

node_out_t do_pg__RxL4TCPPortClassifyStatic(struct state *state, struct input *in)
{
    // P_RxL4TCPPortClassifyStatic_toSocket, P_RxL4TCPPortClassifyStatic_noSocket
    return 0;
}

node_out_t do_pg__RxL4TCPPortClassifyDynamic(struct state *state, struct input *in)
{
    // 
    return 0;
}

node_out_t do_pg__RxL4TCPClosedPortAction(struct state *state, struct input *in)
{
    // P_RxL4TCPClosedPortAction_out
    return 0;
}

node_out_t do_pg__RxL4TCPSocketClassify(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketClassify_srvSocket, P_RxL4TCPSocketClassify_cliSocket
    return 0;
}

node_out_t do_pg__RxL4TCPSocketServerSide(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketServerSide_isListen, P_RxL4TCPSocketServerSide_isSynRecv, P_RxL4TCPSocketServerSide_isEstablished, P_RxL4TCPSocketServerSide_isCloseWait, P_RxL4TCPSocketServerSide_isLastAck, P_RxL4TCPSocketServerSide_isClosed
    return 0;
}

node_out_t do_pg__RxL4TCPSocketInClosed(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketInClosed_out
    return 0;
}

node_out_t do_pg__RxL4TCPSocketClientSide(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketClientSide_out
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidSyn(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketAddHalfOpenConn(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketAddHalfOpenConn_success, P_RxL4TCPSocketAddHalfOpenConn_failure
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSendSyn(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidSynAckS(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidFinAck(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidFinAck2(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeEstablished(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsDataPacket(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsFinSet(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeLastAck(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeToClosed(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeToClosed2(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketCopyData(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxTagTxTCPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxTagTxUDPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxTagTxARPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxTagTxARPLu(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxTagTxICMPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxDemux(struct state *state, struct input *in)
{
    // P_TxDemux_TCPIR, P_TxDemux_UDPIR, P_TxDemux_ICMPIR, P_TxDemux_ARPLu, P_TxDemux_ARPIR, P_TxDemux_drop
    return 0;
}

node_out_t do_pg__TxQueue(struct state *state, struct input *in)
{
    // 
    return 0;
}

node_out_t do_pg__TxL4UDPInitiateResponse(struct state *state, struct input *in)
{
    // P_TxL4UDPInitiateResponse_out, P_TxL4UDPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL4UDPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL4UDPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL4UDPFillHeader(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxL4TCPInitiateResponse(struct state *state, struct input *in)
{
    // P_TxL4TCPInitiateResponse_out, P_TxL4TCPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL4TCPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL4TCPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL4TCPFillHeader(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxL3ICMPInitiateResponse(struct state *state, struct input *in)
{
    // P_TxL3ICMPInitiateResponse_out, P_TxL3ICMPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL3ICMPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL3ICMPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL3ICMPFillHeader(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxL3IPv4AllocateHeader(struct state *state, struct input *in)
{
    // P_TxL3IPv4AllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL3IPv4FillHeader(struct state *state, struct input *in)
{
    // P_TxL3IPv4FillHeader_out
    return 0;
}

node_out_t do_pg__TxL3IPv4Routing(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxL3ARPInitiateResponse(struct state *state, struct input *in)
{
    // P_TxL3ARPInitiateResponse_true, P_TxL3ARPInitiateResponse_false, P_TxL3ARPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL3ARPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL3ARPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL3ARPFillHeader(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxL3ARPLookupRequestIn(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxL3ARPLookup_(struct state *state, struct input *in)
{
    // P_TxL3ARPLookup__true, P_TxL3ARPLookup__false, P_TxL3ARPLookup__miss
    return 0;
}

node_out_t do_pg__TxL3ARPSendRequest(struct state *state, struct input *in)
{
    // P_TxL3ARPSendRequest_true, P_TxL3ARPSendRequest_false, P_TxL3ARPSendRequest_drop
    return 0;
}

node_out_t do_pg__TxL2EtherAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL2EtherAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL2EtherFillHeader(struct state *state, struct input *in)
{
    // P_TxL2EtherFillHeader_out
    return 0;
}
#endif // 0



#endif // GENERATEDCODE_H_

