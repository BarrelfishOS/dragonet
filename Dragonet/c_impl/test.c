#include <implementation.h>
#include <stddef.h>
#include <stdio.h>

#include <packet_access.h>

void testFun(struct state * st, struct input *in);

int main(int argc, char *argv[])
{
    testFun(NULL, NULL);
    return 0;
}

void testFun(struct state * st, struct input *in)
{
    node_out_t Queue=-1, PacketDrop=-1, NotSupported=-1, RxL3IPValid=-1, RxL3IPAndBelowValid=-1, RxToTx=-1, RxL2EtherClassified=-1, RxL2EtherValidLength=-1, RxL2EtherValidUnicast=-1, RxL2EtherValidMulticast=-1, RxL2EtherValidBroadcast=-1, RxL2EtherValidDest=-1, RxL2EtherValidSrc=-1, RxL2EtherValidLocalMAC=-1, RxL2EtherValid=-1, RxL2EtherValidType=-1, RxL2EtherClassifyL3=-1, RxL3ARPValidHeaderLength=-1, RxL3ARPClassify=-1, RxL3ARPLocalIPDest=-1, RxL3ARPValidRequest=-1, RxL3ARPNeedsResponse=-1, RxL3ARPValidResponse=-1, RxL3ARPIsPending=-1, RxL3ARPValidPendingResponse=-1, RxL3ARPProcessPendingResponse=-1, RxL3IPv4ValidHeaderLength=-1, RxL3IPv4ValidReassembly=-1, RxL3IPv4ValidVersion=-1, RxL3IPv4ValidLength=-1, RxL3IPv4ValidTTL=-1, RxL3IPv4ValidChecksum=-1, RxL3IPv4ValidLocalIP=-1, RxL3IPv4Classify=-1, RxL3IPv4Valid=-1, RxL3ICMPValidHeaderLength=-1, RxL3ICMPValidChecksum=-1, RxL3ICMPValid=-1, RxL3ICMPIsTypeRequest=-1, RxL3ICMPNeedsResponse=-1, RxTagTxARPIR=-1, RxTagTxARPLu=-1, RxTagTxICMPIR=-1, TxDemux=-1, TxQueue=-1, TxL3ICMPInitiateResponse=-1, TxL3ICMPAllocateHeader=-1, TxL3ICMPFillHeader=-1, TxL3IPv4Prepare=-1, TxL3IPv4AllocateHeader=-1, TxL3IPv4FillHeader=-1, TxL3IPv4Routing=-1, TxL3ARPInitiateResponse=-1, TxL3ARPPrepare=-1, TxL3ARPAllocateHeader=-1, TxL3ARPFillHeader=-1, TxL3ARPLookup=-1, TxL3ARPLookup_=-1, TxL3ARPSendRequest=-1, TxL2EtherPrepare=-1, TxL2EtherAllocateHeader=-1, TxL2EtherFillHeader=-1;
    if (1) {
        Queue = do_pg__Queue(st, in);
        printf("Queue=%d\n", Queue);
    }
    if (Queue == P_Queue_out) {
        RxL2EtherClassified = do_pg__RxL2EtherClassified(st, in);
        printf("RxL2EtherClassified=%d\n", RxL2EtherClassified);
    }
    if (RxL2EtherClassified == P_true) {
        RxL2EtherValidLength = do_pg__RxL2EtherValidLength(st, in);
        printf("RxL2EtherValidLength=%d\n", RxL2EtherValidLength);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidUnicast = do_pg__RxL2EtherValidUnicast(st, in);
        printf("RxL2EtherValidUnicast=%d\n", RxL2EtherValidUnicast);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidMulticast = do_pg__RxL2EtherValidMulticast(st, in);
        printf("RxL2EtherValidMulticast=%d\n", RxL2EtherValidMulticast);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidBroadcast = do_pg__RxL2EtherValidBroadcast(st, in);
        printf("RxL2EtherValidBroadcast=%d\n", RxL2EtherValidBroadcast);
    }
    if (RxL2EtherValidBroadcast != -1 && RxL2EtherValidBroadcast != -1 && RxL2EtherValidMulticast != -1 && RxL2EtherValidMulticast != -1 && RxL2EtherValidUnicast != -1 && RxL2EtherValidUnicast != -1) {
        RxL2EtherValidDest = (RxL2EtherValidBroadcast == P_true || RxL2EtherValidMulticast == P_true || RxL2EtherValidUnicast == P_true ? P_true : P_false);
        printf("RxL2EtherValidDest=%d\n", RxL2EtherValidDest);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidSrc = do_pg__RxL2EtherValidSrc(st, in);
        printf("RxL2EtherValidSrc=%d\n", RxL2EtherValidSrc);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidLocalMAC = do_pg__RxL2EtherValidLocalMAC(st, in);
        printf("RxL2EtherValidLocalMAC=%d\n", RxL2EtherValidLocalMAC);
    }
    if (RxL2EtherValidLocalMAC != -1 && RxL2EtherValidLocalMAC != -1 && RxL2EtherValidSrc != -1 && RxL2EtherValidSrc != -1 && RxL2EtherValidDest != -1 && RxL2EtherValidDest != -1) {
        RxL2EtherValid = (RxL2EtherValidLocalMAC == P_true && RxL2EtherValidSrc == P_true && RxL2EtherValidDest == P_true ? P_true : P_false);
        printf("RxL2EtherValid=%d\n", RxL2EtherValid);
    }
    if (RxL2EtherValidLength == P_true) {
        RxL2EtherValidType = do_pg__RxL2EtherValidType(st, in);
        printf("RxL2EtherValidType=%d\n", RxL2EtherValidType);
    }
    if (RxL2EtherValidType == P_true) {
        RxL2EtherClassifyL3 = do_pg__RxL2EtherClassifyL3(st, in);
        printf("RxL2EtherClassifyL3=%d\n", RxL2EtherClassifyL3);
    }
    if (RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_arp) {
        RxL3ARPValidHeaderLength = do_pg__RxL3ARPValidHeaderLength(st, in);
        printf("RxL3ARPValidHeaderLength=%d\n", RxL3ARPValidHeaderLength);
    }
    if (RxL3ARPValidHeaderLength == P_true) {
        RxL3ARPClassify = do_pg__RxL3ARPClassify(st, in);
        printf("RxL3ARPClassify=%d\n", RxL3ARPClassify);
    }
    if (RxL3ARPClassify == P_RxL3ARPClassify_request) {
        RxL3ARPValidRequest = do_pg__RxL3ARPValidRequest(st, in);
        printf("RxL3ARPValidRequest=%d\n", RxL3ARPValidRequest);
    }
    if (RxL3ARPClassify == P_RxL3ARPClassify_response) {
        RxL3ARPValidResponse = do_pg__RxL3ARPValidResponse(st, in);
        printf("RxL3ARPValidResponse=%d\n", RxL3ARPValidResponse);
    }
    if (RxL3ARPValidResponse == P_true || RxL3ARPValidResponse == P_false) {
        RxL3ARPIsPending = do_pg__RxL3ARPIsPending(st, in);
        printf("RxL3ARPIsPending=%d\n", RxL3ARPIsPending);
    }
    if (RxL3ARPValidHeaderLength == P_true) {
        RxL3ARPLocalIPDest = do_pg__RxL3ARPLocalIPDest(st, in);
        printf("RxL3ARPLocalIPDest=%d\n", RxL3ARPLocalIPDest);
    }
    if (RxL3ARPValidRequest != -1 && RxL3ARPValidRequest != -1 && RxL3ARPLocalIPDest != -1 && RxL3ARPLocalIPDest != -1) {
        RxL3ARPNeedsResponse = (RxL3ARPValidRequest == P_true && RxL3ARPLocalIPDest == P_true ? P_true : P_false);
        printf("RxL3ARPNeedsResponse=%d\n", RxL3ARPNeedsResponse);
    }
    if (RxL3ARPNeedsResponse == P_true) {
        RxTagTxARPIR = do_pg__RxTagTxARPIR(st, in);
        printf("RxTagTxARPIR=%d\n", RxTagTxARPIR);
    }
    if (RxL3ARPIsPending != -1 && RxL3ARPIsPending != -1 && RxL3ARPValidResponse != -1 && RxL3ARPValidResponse != -1 && RxL3ARPLocalIPDest != -1 && RxL3ARPLocalIPDest != -1) {
        RxL3ARPValidPendingResponse = (RxL3ARPIsPending == P_true && RxL3ARPValidResponse == P_true && RxL3ARPLocalIPDest == P_true ? P_true : P_false);
        printf("RxL3ARPValidPendingResponse=%d\n", RxL3ARPValidPendingResponse);
    }
    if (RxL3ARPValidPendingResponse == P_true) {
        RxL3ARPProcessPendingResponse = do_pg__RxL3ARPProcessPendingResponse(st, in);
        printf("RxL3ARPProcessPendingResponse=%d\n", RxL3ARPProcessPendingResponse);
    }
    if (RxL3ARPProcessPendingResponse == P_RxL3ARPProcessPendingResponse_true || RxL3ARPProcessPendingResponse == P_RxL3ARPProcessPendingResponse_false) {
        RxTagTxARPLu = do_pg__RxTagTxARPLu(st, in);
        printf("RxTagTxARPLu=%d\n", RxTagTxARPLu);
    }
    if (RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_ipv4) {
        RxL3IPv4ValidHeaderLength = do_pg__RxL3IPv4ValidHeaderLength(st, in);
        printf("RxL3IPv4ValidHeaderLength=%d\n", RxL3IPv4ValidHeaderLength);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidReassembly = do_pg__RxL3IPv4ValidReassembly(st, in);
        printf("RxL3IPv4ValidReassembly=%d\n", RxL3IPv4ValidReassembly);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidVersion = do_pg__RxL3IPv4ValidVersion(st, in);
        printf("RxL3IPv4ValidVersion=%d\n", RxL3IPv4ValidVersion);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidLength = do_pg__RxL3IPv4ValidLength(st, in);
        printf("RxL3IPv4ValidLength=%d\n", RxL3IPv4ValidLength);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidTTL = do_pg__RxL3IPv4ValidTTL(st, in);
        printf("RxL3IPv4ValidTTL=%d\n", RxL3IPv4ValidTTL);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidChecksum = do_pg__RxL3IPv4ValidChecksum(st, in);
        printf("RxL3IPv4ValidChecksum=%d\n", RxL3IPv4ValidChecksum);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4ValidLocalIP = do_pg__RxL3IPv4ValidLocalIP(st, in);
        printf("RxL3IPv4ValidLocalIP=%d\n", RxL3IPv4ValidLocalIP);
    }
    if (RxL3IPv4ValidLocalIP != -1 && RxL3IPv4ValidLocalIP != -1 && RxL3IPv4ValidChecksum != -1 && RxL3IPv4ValidChecksum != -1 && RxL3IPv4ValidTTL != -1 && RxL3IPv4ValidTTL != -1 && RxL3IPv4ValidLength != -1 && RxL3IPv4ValidLength != -1 && RxL3IPv4ValidVersion != -1 && RxL3IPv4ValidVersion != -1 && RxL3IPv4ValidReassembly != -1 && RxL3IPv4ValidReassembly != -1) {
        RxL3IPv4Valid = (RxL3IPv4ValidLocalIP == P_true && RxL3IPv4ValidChecksum == P_true && RxL3IPv4ValidTTL == P_true && RxL3IPv4ValidLength == P_true && RxL3IPv4ValidVersion == P_true && RxL3IPv4ValidReassembly == P_true ? P_true : P_false);
        printf("RxL3IPv4Valid=%d\n", RxL3IPv4Valid);
    }
    if (RxL3IPv4Valid != -1 && RxL3IPv4Valid != -1) {
        RxL3IPValid = (RxL3IPv4Valid == P_true ? P_true : P_false);
        printf("RxL3IPValid=%d\n", RxL3IPValid);
    }
    if (RxL2EtherValid != -1 && RxL2EtherValid != -1 && RxL3IPValid != -1 && RxL3IPValid != -1) {
        RxL3IPAndBelowValid = (RxL2EtherValid == P_true && RxL3IPValid == P_true ? P_true : P_false);
        printf("RxL3IPAndBelowValid=%d\n", RxL3IPAndBelowValid);
    }
    if (RxL3IPv4ValidHeaderLength == P_true) {
        RxL3IPv4Classify = do_pg__RxL3IPv4Classify(st, in);
        printf("RxL3IPv4Classify=%d\n", RxL3IPv4Classify);
    }
    if (RxL3IPv4Classify == P_RxL3IPv4Classify_udp || RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_ipv6) {
        NotSupported = do_pg__NotSupported(st, in);
        printf("NotSupported=%d\n", NotSupported);
    }
    if (RxL3IPv4Classify == P_RxL3IPv4Classify_drop || RxL3IPv4ValidHeaderLength == P_false || RxL3ARPClassify == P_RxL3ARPClassify_drop || RxL3ARPValidHeaderLength == P_false || RxL2EtherClassifyL3 == P_RxL2EtherClassifyL3_drop || RxL2EtherValidType == P_false || RxL2EtherValidLength == P_false || RxL2EtherClassified == P_false) {
        PacketDrop = do_pg__PacketDrop(st, in);
        printf("PacketDrop=%d\n", PacketDrop);
    }
    if (RxL3IPv4Classify == P_RxL3IPv4Classify_icmp) {
        RxL3ICMPValidHeaderLength = do_pg__RxL3ICMPValidHeaderLength(st, in);
        printf("RxL3ICMPValidHeaderLength=%d\n", RxL3ICMPValidHeaderLength);
    }
    if (RxL3ICMPValidHeaderLength == P_true) {
        RxL3ICMPValidChecksum = do_pg__RxL3ICMPValidChecksum(st, in);
        printf("RxL3ICMPValidChecksum=%d\n", RxL3ICMPValidChecksum);
    }
    if (RxL3ICMPValidChecksum != -1 && RxL3ICMPValidChecksum != -1 && RxL3IPAndBelowValid != -1 && RxL3IPAndBelowValid != -1) {
        RxL3ICMPValid = (RxL3ICMPValidChecksum == P_true && RxL3IPAndBelowValid == P_true ? P_true : P_false);
        printf("RxL3ICMPValid=%d\n", RxL3ICMPValid);
    }
    if (RxL3ICMPValidHeaderLength == P_true) {
        RxL3ICMPIsTypeRequest = do_pg__RxL3ICMPIsTypeRequest(st, in);
        printf("RxL3ICMPIsTypeRequest=%d\n", RxL3ICMPIsTypeRequest);
    }
    if (RxL3ICMPIsTypeRequest != -1 && RxL3ICMPIsTypeRequest != -1 && RxL3ICMPValid != -1 && RxL3ICMPValid != -1) {
        RxL3ICMPNeedsResponse = (RxL3ICMPIsTypeRequest == P_true && RxL3ICMPValid == P_true ? P_true : P_false);
        printf("RxL3ICMPNeedsResponse=%d\n", RxL3ICMPNeedsResponse);
    }
    if (RxL3ICMPNeedsResponse == P_true) {
        RxTagTxICMPIR = do_pg__RxTagTxICMPIR(st, in);
        printf("RxTagTxICMPIR=%d\n", RxTagTxICMPIR);
    }
    if (RxTagTxICMPIR != -1 && RxTagTxARPLu != -1 && RxTagTxARPIR != -1) {
        RxToTx = (RxTagTxICMPIR == P_true || RxTagTxARPLu == P_true || RxTagTxARPIR == P_true ? P_true : P_false);
        printf("RxToTx=%d\n", RxToTx);
    }
    if (RxToTx == P_true) {
        TxDemux = do_pg__TxDemux(st, in);
        printf("TxDemux=%d\n", TxDemux);
    }
    if (TxDemux == P_TxDemux_ICMPIR) {
        TxL3ICMPInitiateResponse = do_pg__TxL3ICMPInitiateResponse(st, in);
        printf("TxL3ICMPInitiateResponse=%d\n", TxL3ICMPInitiateResponse);
    }
    if (TxL3ICMPInitiateResponse == P_TxL3ICMPInitiateResponse_out) {
        TxL3ICMPAllocateHeader = do_pg__TxL3ICMPAllocateHeader(st, in);
        printf("TxL3ICMPAllocateHeader=%d\n", TxL3ICMPAllocateHeader);
    }
    if (TxL3ICMPAllocateHeader == P_TxL3ICMPAllocateHeader_out) {
        TxL3ICMPFillHeader = do_pg__TxL3ICMPFillHeader(st, in);
        printf("TxL3ICMPFillHeader=%d\n", TxL3ICMPFillHeader);
    }
    if (TxL3ICMPFillHeader != -1 && TxL3ICMPFillHeader != -1) {
        TxL3IPv4Prepare = (TxL3ICMPFillHeader == P_true ? P_true : P_false);
        printf("TxL3IPv4Prepare=%d\n", TxL3IPv4Prepare);
    }
    if (TxL3IPv4Prepare == P_true) {
        TxL3IPv4AllocateHeader = do_pg__TxL3IPv4AllocateHeader(st, in);
        printf("TxL3IPv4AllocateHeader=%d\n", TxL3IPv4AllocateHeader);
    }
    if (TxL3IPv4AllocateHeader == P_TxL3IPv4AllocateHeader_out) {
        TxL3IPv4FillHeader = do_pg__TxL3IPv4FillHeader(st, in);
        printf("TxL3IPv4FillHeader=%d\n", TxL3IPv4FillHeader);
    }
    if (TxL3IPv4FillHeader == P_TxL3IPv4FillHeader_out) {
        TxL3IPv4Routing = do_pg__TxL3IPv4Routing(st, in);
        printf("TxL3IPv4Routing=%d\n", TxL3IPv4Routing);
    }
    if (TxDemux == P_TxDemux_ARPIR) {
        TxL3ARPInitiateResponse = do_pg__TxL3ARPInitiateResponse(st, in);
        printf("TxL3ARPInitiateResponse=%d\n", TxL3ARPInitiateResponse);
    }
    if (TxL3IPv4Routing != -1 && TxL3IPv4Routing != -1 && TxDemux != -1) {
        TxL3ARPLookup = (TxL3IPv4Routing == P_true ? P_true : P_false);
        printf("TxL3ARPLookup=%d\n", TxL3ARPLookup);
    }
    if (TxL3ARPLookup == P_true) {
        TxL3ARPLookup_ = do_pg__TxL3ARPLookup_(st, in);
        printf("TxL3ARPLookup_=%d\n", TxL3ARPLookup_);
    }
    if (TxL3ARPLookup_ == P_TxL3ARPLookup__miss) {
        TxL3ARPSendRequest = do_pg__TxL3ARPSendRequest(st, in);
        printf("TxL3ARPSendRequest=%d\n", TxL3ARPSendRequest);
    }
    if (TxL3ARPSendRequest != -1 && TxL3ARPSendRequest != -1 && TxL3ARPInitiateResponse != -1 && TxL3ARPInitiateResponse != -1) {
        TxL3ARPPrepare = (TxL3ARPSendRequest == P_true || TxL3ARPInitiateResponse == P_true ? P_true : P_false);
        printf("TxL3ARPPrepare=%d\n", TxL3ARPPrepare);
    }
    if (TxL3ARPPrepare == P_true) {
        TxL3ARPAllocateHeader = do_pg__TxL3ARPAllocateHeader(st, in);
        printf("TxL3ARPAllocateHeader=%d\n", TxL3ARPAllocateHeader);
    }
    if (TxL3ARPAllocateHeader == P_TxL3ARPAllocateHeader_out) {
        TxL3ARPFillHeader = do_pg__TxL3ARPFillHeader(st, in);
        printf("TxL3ARPFillHeader=%d\n", TxL3ARPFillHeader);
    }
    if (TxL3ARPLookup_ != -1 && TxL3ARPLookup_ != -1 && TxL3ARPFillHeader != -1 && TxL3ARPFillHeader != -1) {
        TxL2EtherPrepare = (TxL3ARPLookup_ == P_true || TxL3ARPFillHeader == P_true ? P_true : P_false);
        printf("TxL2EtherPrepare=%d\n", TxL2EtherPrepare);
    }
    if (TxL2EtherPrepare == P_true) {
        TxL2EtherAllocateHeader = do_pg__TxL2EtherAllocateHeader(st, in);
        printf("TxL2EtherAllocateHeader=%d\n", TxL2EtherAllocateHeader);
    }
    if (TxL2EtherAllocateHeader == P_TxL2EtherAllocateHeader_out) {
        TxL2EtherFillHeader = do_pg__TxL2EtherFillHeader(st, in);
        printf("TxL2EtherFillHeader=%d\n", TxL2EtherFillHeader);
    }
    if (TxL2EtherFillHeader == P_TxL2EtherFillHeader_out) {
        TxQueue = do_pg__TxQueue(st, in);
        printf("TxQueue=%d\n", TxQueue);
    }
}

