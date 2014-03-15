#ifndef GENERATEDCODE_H_

#define GENERATEDCODE_H_

enum out_ports {
    P_false = 0,
    P_true = 1,
    P_Queue_out = 0,
    P_RxL2EtherClassifyL3_ipv4 = 0,
    P_RxL2EtherClassifyL3_ipv6 = 1,
    P_RxL2EtherClassifyL3_arp = 2,
    P_RxL2EtherClassifyL3_drop = 3,
    P_RxL3ARPClassify_request = 0,
    P_RxL3ARPClassify_response = 1,
    P_RxL3ARPClassify_drop = 2,
    P_RxL3ARPProcessPendingResponse_true = 0,
    P_RxL3ARPProcessPendingResponse_false = 1,
    P_RxL3ARPProcessPendingResponse_drop = 2,
    P_RxL3IPv4Classify_udp = 0,
    P_RxL3IPv4Classify_icmp = 1,
    P_RxL3IPv4Classify_drop = 2,
    P_TxDemux_ICMPIR = 0,
    P_TxDemux_ARPLu = 1,
    P_TxDemux_ARPIR = 2,
    P_TxDemux_drop = 3,
    P_TxL3ICMPInitiateResponse_out = 0,
    P_TxL3ICMPInitiateResponse_drop = 1,
    P_TxL3ICMPAllocateHeader_out = 0,
    P_TxL3IPv4AllocateHeader_out = 0,
    P_TxL3IPv4FillHeader_out = 0,
    P_TxL3ARPInitiateResponse_true = 0,
    P_TxL3ARPInitiateResponse_false = 1,
    P_TxL3ARPInitiateResponse_drop = 2,
    P_TxL3ARPAllocateHeader_out = 0,
    P_TxL3ARPLookup__true = 0,
    P_TxL3ARPLookup__false = 1,
    P_TxL3ARPLookup__miss = 2,
    P_TxL3ARPSendRequest_true = 0,
    P_TxL3ARPSendRequest_false = 1,
    P_TxL3ARPSendRequest_drop = 2,
    P_TxL2EtherAllocateHeader_out = 0,
    P_TxL2EtherFillHeader_out = 0,
};


node_out_t do_pg__Queue(struct state *state, struct input *in);
node_out_t do_pg__PacketDrop(struct state *state, struct input *in);
node_out_t do_pg__NotSupported(struct state *state, struct input *in);
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
node_out_t do_pg__RxTagTxARPIR(struct state *state, struct input *in);
node_out_t do_pg__RxTagTxARPLu(struct state *state, struct input *in);
node_out_t do_pg__RxTagTxICMPIR(struct state *state, struct input *in);
node_out_t do_pg__TxDemux(struct state *state, struct input *in);
node_out_t do_pg__TxQueue(struct state *state, struct input *in);
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
    node_out_t Queue=-1, PacketDrop=-1, NotSupported=-1, RxL3IPValid=-1, RxL3IPAndBelowValid=-1, RxToTx=-1, RxL2EtherClassified=-1, RxL2EtherValidLength=-1, RxL2EtherValidUnicast=-1, RxL2EtherValidMulticast=-1, RxL2EtherValidBroadcast=-1, RxL2EtherValidDest=-1, RxL2EtherValidSrc=-1, RxL2EtherValidLocalMAC=-1, RxL2EtherValid=-1, RxL2EtherValidType=-1, RxL2EtherClassifyL3=-1, RxL3ARPValidHeaderLength=-1, RxL3ARPClassify=-1, RxL3ARPLocalIPDest=-1, RxL3ARPValidRequest=-1, RxL3ARPNeedsResponse=-1, RxL3ARPValidResponse=-1, RxL3ARPIsPending=-1, RxL3ARPValidPendingResponse=-1, RxL3ARPProcessPendingResponse=-1, RxL3IPv4ValidHeaderLength=-1, RxL3IPv4ValidReassembly=-1, RxL3IPv4ValidVersion=-1, RxL3IPv4ValidLength=-1, RxL3IPv4ValidTTL=-1, RxL3IPv4ValidChecksum=-1, RxL3IPv4ValidLocalIP=-1, RxL3IPv4Classify=-1, RxL3IPv4Valid=-1, RxL3ICMPValidHeaderLength=-1, RxL3ICMPValidChecksum=-1, RxL3ICMPValid=-1, RxL3ICMPIsTypeRequest=-1, RxL3ICMPNeedsResponse=-1, RxTagTxARPIR=-1, RxTagTxARPLu=-1, RxTagTxICMPIR=-1, TxDemux=-1, TxQueue=-1, TxL3ICMPInitiateResponse=-1, TxL3ICMPAllocateHeader=-1, TxL3ICMPFillHeader=-1, TxL3IPv4Prepare=-1, TxL3IPv4AllocateHeader=-1, TxL3IPv4FillHeader=-1, TxL3IPv4Routing=-1, TxL3ARPInitiateResponse=-1, TxL3ARPPrepare=-1, TxL3ARPAllocateHeader=-1, TxL3ARPFillHeader=-1, TxL3ARPLookupRequestIn=-1, TxL3ARPLookup=-1, TxL3ARPLookup_=-1, TxL3ARPSendRequest=-1, TxL2EtherPrepare=-1, TxL2EtherAllocateHeader=-1, TxL2EtherFillHeader=-1;
    if (1) {
        Queue = do_pg__Queue(st, in);
        dprint("Queue=%d\n", Queue);
    }
    if (Queue == P_Queue_out) {
        RxL2EtherClassified = do_pg__RxL2EtherClassified(st, in);
        dprint("RxL2EtherClassified=%d\n", RxL2EtherClassified);
    }
    if (RxL2EtherClassified == P_true) {
        RxL2EtherValidLength = do_pg__RxL2EtherValidLength(st, in);
        dprint("RxL2EtherValidLength=%d\n", RxL2EtherValidLength);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidUnicast = do_pg__RxL2EtherValidUnicast(st, in);
        dprint("RxL2EtherValidUnicast=%d\n", RxL2EtherValidUnicast);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidMulticast = do_pg__RxL2EtherValidMulticast(st, in);
        dprint("RxL2EtherValidMulticast=%d\n", RxL2EtherValidMulticast);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidBroadcast = do_pg__RxL2EtherValidBroadcast(st, in);
        dprint("RxL2EtherValidBroadcast=%d\n", RxL2EtherValidBroadcast);
    }
    if (RxL2EtherValidBroadcast == P_true || RxL2EtherValidMulticast == P_true || RxL2EtherValidUnicast == P_true) {
        RxL2EtherValidDest = P_true;
        dprint("RxL2EtherValidDest=%d\n", RxL2EtherValidDest);
    }
    if (RxL2EtherValidBroadcast == P_false && RxL2EtherValidMulticast == P_false && RxL2EtherValidUnicast == P_false) {
        RxL2EtherValidDest = P_false;
        dprint("RxL2EtherValidDest=%d\n", RxL2EtherValidDest);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidSrc = do_pg__RxL2EtherValidSrc(st, in);
        dprint("RxL2EtherValidSrc=%d\n", RxL2EtherValidSrc);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidLocalMAC = do_pg__RxL2EtherValidLocalMAC(st, in);
        dprint("RxL2EtherValidLocalMAC=%d\n", RxL2EtherValidLocalMAC);
    }
    if (RxL2EtherValidLocalMAC == P_true && RxL2EtherValidSrc == P_true && RxL2EtherValidDest == P_true) {
        RxL2EtherValid = P_true;
        dprint("RxL2EtherValid=%d\n", RxL2EtherValid);
    }
    if (RxL2EtherValidLocalMAC == P_false || RxL2EtherValidSrc == P_false || RxL2EtherValidDest == P_false) {
        RxL2EtherValid = P_false;
        dprint("RxL2EtherValid=%d\n", RxL2EtherValid);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidType = do_pg__RxL2EtherValidType(st, in);
        dprint("RxL2EtherValidType=%d\n", RxL2EtherValidType);
    }
    if (RxL2EtherValidType == P_true) {
        RxL2EtherClassifyL3 = do_pg__RxL2EtherClassifyL3(st, in);
        dprint("RxL2EtherClassifyL3=%d\n", RxL2EtherClassifyL3);
    }
    if (RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_arp) {
        RxL3ARPValidHeaderLength = do_pg__RxL3ARPValidHeaderLength(st, in);
        dprint("RxL3ARPValidHeaderLength=%d\n", RxL3ARPValidHeaderLength);
    }
    if (RxL3ARPValidHeaderLength == P_true) {
        RxL3ARPClassify = do_pg__RxL3ARPClassify(st, in);
        dprint("RxL3ARPClassify=%d\n", RxL3ARPClassify);
    }
    if (RxL3ARPClassify == P_RxL3ARPClassify_request) {
        RxL3ARPValidRequest = do_pg__RxL3ARPValidRequest(st, in);
        dprint("RxL3ARPValidRequest=%d\n", RxL3ARPValidRequest);
    }
    if (RxL3ARPClassify == P_RxL3ARPClassify_response) {
        RxL3ARPValidResponse = do_pg__RxL3ARPValidResponse(st, in);
        dprint("RxL3ARPValidResponse=%d\n", RxL3ARPValidResponse);
    }
    if (RxL3ARPValidResponse == P_true || RxL3ARPValidResponse == P_false) {
        RxL3ARPIsPending = do_pg__RxL3ARPIsPending(st, in);
        dprint("RxL3ARPIsPending=%d\n", RxL3ARPIsPending);
    }
    if (RxL3ARPValidHeaderLength == P_true) {
        RxL3ARPLocalIPDest = do_pg__RxL3ARPLocalIPDest(st, in);
        dprint("RxL3ARPLocalIPDest=%d\n", RxL3ARPLocalIPDest);
    }
    if (RxL3ARPValidRequest == P_true && RxL3ARPLocalIPDest == P_true) {
        RxL3ARPNeedsResponse = P_true;
        dprint("RxL3ARPNeedsResponse=%d\n", RxL3ARPNeedsResponse);
    }
    if (RxL3ARPValidRequest == P_false || RxL3ARPLocalIPDest == P_false) {
        RxL3ARPNeedsResponse = P_false;
        dprint("RxL3ARPNeedsResponse=%d\n", RxL3ARPNeedsResponse);
    }
    if (RxL3ARPNeedsResponse == P_true) {
        RxTagTxARPIR = do_pg__RxTagTxARPIR(st, in);
        dprint("RxTagTxARPIR=%d\n", RxTagTxARPIR);
    }
    if (RxL3ARPIsPending == P_true && RxL3ARPValidResponse == P_true && RxL3ARPLocalIPDest == P_true) {
        RxL3ARPValidPendingResponse = P_true;
        dprint("RxL3ARPValidPendingResponse=%d\n", RxL3ARPValidPendingResponse);
    }
    if (RxL3ARPIsPending == P_false || RxL3ARPValidResponse == P_false || RxL3ARPLocalIPDest == P_false) {
        RxL3ARPValidPendingResponse = P_false;
        dprint("RxL3ARPValidPendingResponse=%d\n", RxL3ARPValidPendingResponse);
    }
    if (RxL3ARPValidPendingResponse == P_true) {
        RxL3ARPProcessPendingResponse = do_pg__RxL3ARPProcessPendingResponse(st, in);
        dprint("RxL3ARPProcessPendingResponse=%d\n", RxL3ARPProcessPendingResponse);
    }
    if (RxL3ARPProcessPendingResponse == P_RxL3ARPProcessPendingResponse_true) {
        RxTagTxARPLu = do_pg__RxTagTxARPLu(st, in);
        dprint("RxTagTxARPLu=%d\n", RxTagTxARPLu);
    }
    if (RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_ipv4) {
        RxL3IPv4ValidHeaderLength = do_pg__RxL3IPv4ValidHeaderLength(st, in);
        dprint("RxL3IPv4ValidHeaderLength=%d\n", RxL3IPv4ValidHeaderLength);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidReassembly = do_pg__RxL3IPv4ValidReassembly(st, in);
        dprint("RxL3IPv4ValidReassembly=%d\n", RxL3IPv4ValidReassembly);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidVersion = do_pg__RxL3IPv4ValidVersion(st, in);
        dprint("RxL3IPv4ValidVersion=%d\n", RxL3IPv4ValidVersion);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidLength = do_pg__RxL3IPv4ValidLength(st, in);
        dprint("RxL3IPv4ValidLength=%d\n", RxL3IPv4ValidLength);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidTTL = do_pg__RxL3IPv4ValidTTL(st, in);
        dprint("RxL3IPv4ValidTTL=%d\n", RxL3IPv4ValidTTL);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidChecksum = do_pg__RxL3IPv4ValidChecksum(st, in);
        dprint("RxL3IPv4ValidChecksum=%d\n", RxL3IPv4ValidChecksum);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidLocalIP = do_pg__RxL3IPv4ValidLocalIP(st, in);
        dprint("RxL3IPv4ValidLocalIP=%d\n", RxL3IPv4ValidLocalIP);
    }
    if (RxL3IPv4ValidLocalIP == P_true && RxL3IPv4ValidChecksum == P_true && RxL3IPv4ValidTTL == P_true && RxL3IPv4ValidLength == P_true && RxL3IPv4ValidVersion == P_true && RxL3IPv4ValidReassembly == P_true) {
        RxL3IPv4Valid = P_true;
        dprint("RxL3IPv4Valid=%d\n", RxL3IPv4Valid);
    }
    if (RxL3IPv4ValidLocalIP == P_false || RxL3IPv4ValidChecksum == P_false || RxL3IPv4ValidTTL == P_false || RxL3IPv4ValidLength == P_false || RxL3IPv4ValidVersion == P_false || RxL3IPv4ValidReassembly == P_false) {
        RxL3IPv4Valid = P_false;
        dprint("RxL3IPv4Valid=%d\n", RxL3IPv4Valid);
    }
    if (RxL3IPv4Valid == P_true) {
        RxL3IPValid = P_true;
        dprint("RxL3IPValid=%d\n", RxL3IPValid);
    }
    if (RxL3IPv4Valid == P_false) {
        RxL3IPValid = P_false;
        dprint("RxL3IPValid=%d\n", RxL3IPValid);
    }
    if (RxL2EtherValid == P_true && RxL3IPValid == P_true) {
        RxL3IPAndBelowValid = P_true;
        dprint("RxL3IPAndBelowValid=%d\n", RxL3IPAndBelowValid);
    }
    if (RxL2EtherValid == P_false || RxL3IPValid == P_false) {
        RxL3IPAndBelowValid = P_false;
        dprint("RxL3IPAndBelowValid=%d\n", RxL3IPAndBelowValid);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4Classify = do_pg__RxL3IPv4Classify(st, in);
        dprint("RxL3IPv4Classify=%d\n", RxL3IPv4Classify);
    }
    if (RxL3IPv4Classify == P_RxL3IPv4Classify_udp || RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_ipv6) {
        NotSupported = do_pg__NotSupported(st, in);
        dprint("NotSupported=%d\n", NotSupported);
    }
    if (RxL3IPv4Classify == P_RxL3IPv4Classify_drop || RxL3IPv4ValidHeaderLength == P_false || RxL3ARPClassify == P_RxL3ARPClassify_drop || RxL3ARPValidHeaderLength == P_false || RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_drop || RxL2EtherValidType == P_false || RxL2EtherValidLength == P_false || RxL2EtherClassified == P_false) {
        PacketDrop = do_pg__PacketDrop(st, in);
        dprint("PacketDrop=%d\n", PacketDrop);
    }
    if (RxL3IPv4Classify == P_RxL3IPv4Classify_icmp) {
        RxL3ICMPValidHeaderLength = do_pg__RxL3ICMPValidHeaderLength(st, in);
        dprint("RxL3ICMPValidHeaderLength=%d\n", RxL3ICMPValidHeaderLength);
    }
    if (RxL3ICMPValidHeaderLength == P_true) {
        RxL3ICMPValidChecksum = do_pg__RxL3ICMPValidChecksum(st, in);
        dprint("RxL3ICMPValidChecksum=%d\n", RxL3ICMPValidChecksum);
    }
    if (RxL3ICMPValidChecksum == P_true && RxL3IPAndBelowValid == P_true) {
        RxL3ICMPValid = P_true;
        dprint("RxL3ICMPValid=%d\n", RxL3ICMPValid);
    }
    if (RxL3ICMPValidChecksum == P_false || RxL3IPAndBelowValid == P_false) {
        RxL3ICMPValid = P_false;
        dprint("RxL3ICMPValid=%d\n", RxL3ICMPValid);
    }
    if (RxL3ICMPValidHeaderLength == P_true) {
        RxL3ICMPIsTypeRequest = do_pg__RxL3ICMPIsTypeRequest(st, in);
        dprint("RxL3ICMPIsTypeRequest=%d\n", RxL3ICMPIsTypeRequest);
    }
    if (RxL3ICMPIsTypeRequest == P_true && RxL3ICMPValid == P_true) {
        RxL3ICMPNeedsResponse = P_true;
        dprint("RxL3ICMPNeedsResponse=%d\n", RxL3ICMPNeedsResponse);
    }
    if (RxL3ICMPIsTypeRequest == P_false || RxL3ICMPValid == P_false) {
        RxL3ICMPNeedsResponse = P_false;
        dprint("RxL3ICMPNeedsResponse=%d\n", RxL3ICMPNeedsResponse);
    }
    if (RxL3ICMPNeedsResponse == P_true) {
        RxTagTxICMPIR = do_pg__RxTagTxICMPIR(st, in);
        dprint("RxTagTxICMPIR=%d\n", RxTagTxICMPIR);
    }
    if (RxTagTxICMPIR == P_true || RxTagTxARPLu == P_true || RxTagTxARPIR == P_true) {
        RxToTx = P_true;
        dprint("RxToTx=%d\n", RxToTx);
    }
    if (RxTagTxICMPIR == P_false && RxTagTxARPLu == P_false && RxTagTxARPIR == P_false) {
        RxToTx = P_false;
        dprint("RxToTx=%d\n", RxToTx);
    }
    if (RxToTx == P_true) {
        TxDemux = do_pg__TxDemux(st, in);
        dprint("TxDemux=%d\n", TxDemux);
    }
    if (TxDemux == P_TxDemux_ICMPIR) {
        TxL3ICMPInitiateResponse = do_pg__TxL3ICMPInitiateResponse(st, in);
        dprint("TxL3ICMPInitiateResponse=%d\n", TxL3ICMPInitiateResponse);
    }
    if (TxL3ICMPInitiateResponse == P_TxL3ICMPInitiateResponse_out) {
        TxL3ICMPAllocateHeader = do_pg__TxL3ICMPAllocateHeader(st, in);
        dprint("TxL3ICMPAllocateHeader=%d\n", TxL3ICMPAllocateHeader);
    }
    if (TxL3ICMPAllocateHeader == P_TxL3ICMPAllocateHeader_out) {
        TxL3ICMPFillHeader = do_pg__TxL3ICMPFillHeader(st, in);
        dprint("TxL3ICMPFillHeader=%d\n", TxL3ICMPFillHeader);
    }
    if (TxL3ICMPFillHeader == P_true) {
        TxL3IPv4Prepare = P_true;
        dprint("TxL3IPv4Prepare=%d\n", TxL3IPv4Prepare);
    }
    if (TxL3ICMPFillHeader == P_false) {
        TxL3IPv4Prepare = P_false;
        dprint("TxL3IPv4Prepare=%d\n", TxL3IPv4Prepare);
    }
    if (TxL3IPv4Prepare == P_true) {
        TxL3IPv4AllocateHeader = do_pg__TxL3IPv4AllocateHeader(st, in);
        dprint("TxL3IPv4AllocateHeader=%d\n", TxL3IPv4AllocateHeader);
    }
    if (TxL3IPv4AllocateHeader == P_TxL3IPv4AllocateHeader_out) {
        TxL3IPv4FillHeader = do_pg__TxL3IPv4FillHeader(st, in);
        dprint("TxL3IPv4FillHeader=%d\n", TxL3IPv4FillHeader);
    }
    if (TxL3IPv4FillHeader == P_TxL3IPv4FillHeader_out) {
        TxL3IPv4Routing = do_pg__TxL3IPv4Routing(st, in);
        dprint("TxL3IPv4Routing=%d\n", TxL3IPv4Routing);
    }
    if (TxDemux == P_TxDemux_ARPIR) {
        TxL3ARPInitiateResponse = do_pg__TxL3ARPInitiateResponse(st, in);
        dprint("TxL3ARPInitiateResponse=%d\n", TxL3ARPInitiateResponse);
    }
    if (TxDemux == P_TxDemux_ARPLu) {
        TxL3ARPLookupRequestIn = do_pg__TxL3ARPLookupRequestIn(st, in);
        dprint("TxL3ARPLookupRequestIn=%d\n", TxL3ARPLookupRequestIn);
    }
    if (TxL3ARPLookupRequestIn == P_true || TxL3IPv4Routing == P_true) {
        TxL3ARPLookup = P_true;
        dprint("TxL3ARPLookup=%d\n", TxL3ARPLookup);
    }
    if (TxL3ARPLookupRequestIn == P_false && TxL3IPv4Routing == P_false) {
        TxL3ARPLookup = P_false;
        dprint("TxL3ARPLookup=%d\n", TxL3ARPLookup);
    }
    if (TxL3ARPLookup == P_true) {
        TxL3ARPLookup_ = do_pg__TxL3ARPLookup_(st, in);
        dprint("TxL3ARPLookup_=%d\n", TxL3ARPLookup_);
    }
    if (TxL3ARPLookup_ == P_TxL3ARPLookup__miss) {
        TxL3ARPSendRequest = do_pg__TxL3ARPSendRequest(st, in);
        dprint("TxL3ARPSendRequest=%d\n", TxL3ARPSendRequest);
    }
    if (TxL3ARPSendRequest == P_TxL3ARPSendRequest_true || TxL3ARPInitiateResponse == P_TxL3ARPInitiateResponse_true) {
        TxL3ARPPrepare = P_true;
        dprint("TxL3ARPPrepare=%d\n", TxL3ARPPrepare);
    }
    if (TxL3ARPSendRequest == P_TxL3ARPSendRequest_false && TxL3ARPInitiateResponse == P_TxL3ARPInitiateResponse_false) {
        TxL3ARPPrepare = P_false;
        dprint("TxL3ARPPrepare=%d\n", TxL3ARPPrepare);
    }
    if (TxL3ARPPrepare == P_true) {
        TxL3ARPAllocateHeader = do_pg__TxL3ARPAllocateHeader(st, in);
        dprint("TxL3ARPAllocateHeader=%d\n", TxL3ARPAllocateHeader);
    }
    if (TxL3ARPAllocateHeader == P_TxL3ARPAllocateHeader_out) {
        TxL3ARPFillHeader = do_pg__TxL3ARPFillHeader(st, in);
        dprint("TxL3ARPFillHeader=%d\n", TxL3ARPFillHeader);
    }
    if (TxL3ARPLookup_ == P_TxL3ARPLookup__true || TxL3ARPFillHeader == P_true) {
        TxL2EtherPrepare = P_true;
        dprint("TxL2EtherPrepare=%d\n", TxL2EtherPrepare);
    }
    if (TxL3ARPLookup_ == P_TxL3ARPLookup__false && TxL3ARPFillHeader == P_false) {
        TxL2EtherPrepare = P_false;
        dprint("TxL2EtherPrepare=%d\n", TxL2EtherPrepare);
    }
    if (TxL2EtherPrepare == P_true) {
        TxL2EtherAllocateHeader = do_pg__TxL2EtherAllocateHeader(st, in);
        dprint("TxL2EtherAllocateHeader=%d\n", TxL2EtherAllocateHeader);
    }
    if (TxL2EtherAllocateHeader == P_TxL2EtherAllocateHeader_out) {
        TxL2EtherFillHeader = do_pg__TxL2EtherFillHeader(st, in);
        dprint("TxL2EtherFillHeader=%d\n", TxL2EtherFillHeader);
    }
    if (TxL2EtherFillHeader == P_TxL2EtherFillHeader_out) {
        TxQueue = do_pg__TxQueue(st, in);
        dprint("TxQueue=%d\n", TxQueue);
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
    // P_RxL3ARPProcessPendingResponse_true, P_RxL3ARPProcessPendingResponse_false, P_RxL3ARPProcessPendingResponse_drop
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
    // P_RxL3IPv4Classify_udp, P_RxL3IPv4Classify_icmp, P_RxL3IPv4Classify_drop
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
    // P_TxDemux_ICMPIR, P_TxDemux_ARPLu, P_TxDemux_ARPIR, P_TxDemux_drop
    return 0;
}

node_out_t do_pg__TxQueue(struct state *state, struct input *in)
{
    // 
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

