#ifndef GENERATEDCODE_H_
#define GENERATEDCODE_H_


enum out_ports {
    P_false = 0,
    P_true = 1,
    P_RxQueue_out = 0,
    P_RxQueue_drop = 1,
    P_RxQueue_init = 2,
    P_RxL2EtherClassifyL3_ipv4 = 0,
    P_RxL2EtherClassifyL3_ipv6 = 1,
    P_RxL2EtherClassifyL3_arp = 2,
    P_RxL2EtherClassifyL3_drop = 3,
    P_RxL3ARPClassify_request = 0,
    P_RxL3ARPClassify_response = 1,
    P_RxL3ARPClassify_drop = 2,
    P_RxL3IPv4Classify_tcp = 0,
    P_RxL3IPv4Classify_udp = 1,
    P_RxL3IPv4Classify_icmp = 2,
    P_RxL3IPv4Classify_drop = 3,
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
    P_TxL3ARPSendGratuitous_false = 0,
    P_TxL3ARPSendGratuitous_true = 1,
    P_TxL3ARPProcessPendingResponse_out = 0,
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
    P_E10kRxQueue0_out = 0,
    P_E10kRxQueue0_drop = 1,
    P_E10kRxQueue0_init = 2,
    P_E10kRxQueue1_out = 0,
    P_E10kRxQueue1_drop = 1,
    P_E10kRxQueue1_init = 2,
    P_E10kRxQueue2_out = 0,
    P_E10kRxQueue2_drop = 1,
    P_E10kRxQueue2_init = 2,
    P_E10kRxQueue3_out = 0,
    P_E10kRxQueue3_drop = 1,
    P_E10kRxQueue3_init = 2,
    P_TapRxQueue_out = 0,
    P_TapRxQueue_drop = 1,
    P_TapRxQueue_init = 2,
};


enum out_spawns {
    S_TxL3ARPProcessPendingResponse_restart = 0,
    S_TxL3ARPLookup__miss = 0,
    S_E10kInit_q0 = 0,
    S_E10kInit_q1 = 1,
    S_E10kInit_q2 = 2,
    S_E10kInit_q3 = 3,
    S_E10kRxQueue0_poll = 0,
    S_E10kRxQueue1_poll = 0,
    S_E10kRxQueue2_poll = 0,
    S_E10kRxQueue3_poll = 0,
    S_TapRxQueue_poll = 0,
};


struct ctx_RxQueue {
    struct ctx_generic generic;
};

struct ctx_RxL3IPValid {
    struct ctx_generic generic;
};

struct ctx_RxL3IPAndBelowValid {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherClassified {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherValidLength {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherValidUnicast {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherValidMulticast {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherValidBroadcast {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherValidDest {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherValidSrc {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherValidLocalMAC {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherValid {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherValidType {
    struct ctx_generic generic;
};

struct ctx_RxL2EtherClassifyL3 {
    struct ctx_generic generic;
};

struct ctx_RxL3ARPValidHeaderLength {
    struct ctx_generic generic;
};

struct ctx_RxL3ARPClassify {
    struct ctx_generic generic;
};

struct ctx_RxL3ARPLocalIPDest {
    struct ctx_generic generic;
};

struct ctx_RxL3ARPValidRequest {
    struct ctx_generic generic;
};

struct ctx_RxL3ARPNeedsResponse {
    struct ctx_generic generic;
};

struct ctx_RxL3ARPValidResponse {
    struct ctx_generic generic;
};

struct ctx_RxL3ARPIsPending {
    struct ctx_generic generic;
};

struct ctx_RxL3ARPValidPendingResponse {
    struct ctx_generic generic;
};

struct ctx_RxL3IPv4ValidHeaderLength {
    struct ctx_generic generic;
};

struct ctx_RxL3IPv4ValidReassembly {
    struct ctx_generic generic;
};

struct ctx_RxL3IPv4ValidVersion {
    struct ctx_generic generic;
};

struct ctx_RxL3IPv4ValidLength {
    struct ctx_generic generic;
};

struct ctx_RxL3IPv4ValidTTL {
    struct ctx_generic generic;
};

struct ctx_RxL3IPv4ValidChecksum {
    struct ctx_generic generic;
};

struct ctx_RxL3IPv4ValidLocalIP {
    struct ctx_generic generic;
};

struct ctx_RxL3IPv4Classify {
    struct ctx_generic generic;
};

struct ctx_RxL3IPv4Valid {
    struct ctx_generic generic;
};

struct ctx_RxL3ICMPValidHeaderLength {
    struct ctx_generic generic;
};

struct ctx_RxL3ICMPValidChecksum {
    struct ctx_generic generic;
};

struct ctx_RxL3ICMPValid {
    struct ctx_generic generic;
};

struct ctx_RxL3ICMPIsTypeRequest {
    struct ctx_generic generic;
};

struct ctx_RxL3ICMPNeedsResponse {
    struct ctx_generic generic;
};

struct ctx_RxL4UDPValidHeaderLength {
    struct ctx_generic generic;
};

struct ctx_RxL4UDPValidLength {
    struct ctx_generic generic;
};

struct ctx_RxL4UDPValidChecksum {
    struct ctx_generic generic;
};

struct ctx_RxL4UDPValid {
    struct ctx_generic generic;
};

struct ctx_RxL4UDPCUDPSockets {
    struct ctx_generic generic;
};

struct ctx_RxL4UDPUnusedPort {
    struct ctx_generic generic;
};

struct ctx_RxL4UDPClosedPortAction {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPValidHeaderLength {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPValid {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPPortClassifyType {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPPortClassifyStatic {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPPortClassifyDynamic {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPClosedPortAction {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketClassify {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketServerSide {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketInClosed {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketClientSide {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketIsValidSyn {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketAddHalfOpenConn {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketSendSyn {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketIsValidSynAckS {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketIsValidFinAck {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketIsValidFinAck2 {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketSChangeEstablished {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketIsDataPacket {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketIsFinSet {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketSChangeLastAck {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketSChangeToClosed {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketSChangeToClosed2 {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketCopyData {
    struct ctx_generic generic;
};

struct ctx_RxL4TCPSocketTCPOutMerge {
    struct ctx_generic generic;
};

struct ctx_TxQueue {
    struct ctx_generic generic;
};

struct ctx_TxL4UDPInitiateResponse {
    struct ctx_generic generic;
};

struct ctx_TxL4UDPAllocateHeader {
    struct ctx_generic generic;
};

struct ctx_TxL4UDPFillHeader {
    struct ctx_generic generic;
};

struct ctx_TxL4TCPInitiateResponse {
    struct ctx_generic generic;
};

struct ctx_TxL4TCPAllocateHeader {
    struct ctx_generic generic;
};

struct ctx_TxL4TCPFillHeader {
    struct ctx_generic generic;
};

struct ctx_TxL3ICMPInitiateResponse {
    struct ctx_generic generic;
};

struct ctx_TxL3ICMPAllocateHeader {
    struct ctx_generic generic;
};

struct ctx_TxL3ICMPFillHeader {
    struct ctx_generic generic;
};

struct ctx_TxL3IPv4Prepare {
    struct ctx_generic generic;
};

struct ctx_TxL3IPv4AllocateHeader {
    struct ctx_generic generic;
};

struct ctx_TxL3IPv4FillHeader {
    struct ctx_generic generic;
};

struct ctx_TxL3IPv4Routing {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPSendGratuitous {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPProcessPendingResponse {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPInitiateResponse {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPPrepare {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPAllocateHeader {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPFillHeader {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPLookupRequestIn {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPLookup {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPLookup_ {
    struct ctx_generic generic;
};

struct ctx_TxL3ARPSendRequest {
    struct ctx_generic generic;
};

struct ctx_TxL2EtherPrepare {
    struct ctx_generic generic;
};

struct ctx_TxL2EtherAllocateHeader {
    struct ctx_generic generic;
};

struct ctx_TxL2EtherFillHeader {
    struct ctx_generic generic;
};

struct ctx_E10kInit {
    struct ctx_generic generic;
};

struct ctx_E10kRxQueue0 {
    struct ctx_generic generic;
};

struct ctx_E10kRxQueue1 {
    struct ctx_generic generic;
};

struct ctx_E10kRxQueue2 {
    struct ctx_generic generic;
};

struct ctx_E10kRxQueue3 {
    struct ctx_generic generic;
};

struct ctx_E10kL3IPv4Classified {
    struct ctx_generic generic;
};

struct ctx_E10kL3IPv4ValidChecksum {
    struct ctx_generic generic;
};

struct ctx_E10kTxQueue0 {
    struct ctx_generic generic;
};

struct ctx_E10kTxQueue1 {
    struct ctx_generic generic;
};

struct ctx_E10kTxQueue2 {
    struct ctx_generic generic;
};

struct ctx_E10kTxQueue3 {
    struct ctx_generic generic;
};

struct ctx_TapRxQueue {
    struct ctx_generic generic;
};

struct ctx_TapTxQueue {
    struct ctx_generic generic;
};


node_out_t do_pg__RxQueue(struct ctx_RxQueue *context, struct state *state, struct input **in);
node_out_t do_pg__RxL2EtherClassified(struct ctx_RxL2EtherClassified *context, struct state *state, struct input **in);
node_out_t do_pg__RxL2EtherValidLength(struct ctx_RxL2EtherValidLength *context, struct state *state, struct input **in);
node_out_t do_pg__RxL2EtherValidUnicast(struct ctx_RxL2EtherValidUnicast *context, struct state *state, struct input **in);
node_out_t do_pg__RxL2EtherValidMulticast(struct ctx_RxL2EtherValidMulticast *context, struct state *state, struct input **in);
node_out_t do_pg__RxL2EtherValidBroadcast(struct ctx_RxL2EtherValidBroadcast *context, struct state *state, struct input **in);
node_out_t do_pg__RxL2EtherValidSrc(struct ctx_RxL2EtherValidSrc *context, struct state *state, struct input **in);
node_out_t do_pg__RxL2EtherValidLocalMAC(struct ctx_RxL2EtherValidLocalMAC *context, struct state *state, struct input **in);
node_out_t do_pg__RxL2EtherValidType(struct ctx_RxL2EtherValidType *context, struct state *state, struct input **in);
node_out_t do_pg__RxL2EtherClassifyL3(struct ctx_RxL2EtherClassifyL3 *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3ARPValidHeaderLength(struct ctx_RxL3ARPValidHeaderLength *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3ARPClassify(struct ctx_RxL3ARPClassify *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3ARPLocalIPDest(struct ctx_RxL3ARPLocalIPDest *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3ARPValidRequest(struct ctx_RxL3ARPValidRequest *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3ARPValidResponse(struct ctx_RxL3ARPValidResponse *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3ARPIsPending(struct ctx_RxL3ARPIsPending *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3IPv4ValidHeaderLength(struct ctx_RxL3IPv4ValidHeaderLength *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3IPv4ValidReassembly(struct ctx_RxL3IPv4ValidReassembly *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3IPv4ValidVersion(struct ctx_RxL3IPv4ValidVersion *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3IPv4ValidLength(struct ctx_RxL3IPv4ValidLength *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3IPv4ValidTTL(struct ctx_RxL3IPv4ValidTTL *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3IPv4ValidChecksum(struct ctx_RxL3IPv4ValidChecksum *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3IPv4ValidLocalIP(struct ctx_RxL3IPv4ValidLocalIP *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3IPv4Classify(struct ctx_RxL3IPv4Classify *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3ICMPValidHeaderLength(struct ctx_RxL3ICMPValidHeaderLength *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3ICMPValidChecksum(struct ctx_RxL3ICMPValidChecksum *context, struct state *state, struct input **in);
node_out_t do_pg__RxL3ICMPIsTypeRequest(struct ctx_RxL3ICMPIsTypeRequest *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4UDPValidHeaderLength(struct ctx_RxL4UDPValidHeaderLength *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4UDPValidLength(struct ctx_RxL4UDPValidLength *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4UDPValidChecksum(struct ctx_RxL4UDPValidChecksum *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4UDPClosedPortAction(struct ctx_RxL4UDPClosedPortAction *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPValidHeaderLength(struct ctx_RxL4TCPValidHeaderLength *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPPortClassifyType(struct ctx_RxL4TCPPortClassifyType *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPPortClassifyStatic(struct ctx_RxL4TCPPortClassifyStatic *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPPortClassifyDynamic(struct ctx_RxL4TCPPortClassifyDynamic *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPClosedPortAction(struct ctx_RxL4TCPClosedPortAction *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketClassify(struct ctx_RxL4TCPSocketClassify *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketServerSide(struct ctx_RxL4TCPSocketServerSide *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketInClosed(struct ctx_RxL4TCPSocketInClosed *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketClientSide(struct ctx_RxL4TCPSocketClientSide *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketIsValidSyn(struct ctx_RxL4TCPSocketIsValidSyn *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketAddHalfOpenConn(struct ctx_RxL4TCPSocketAddHalfOpenConn *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketSendSyn(struct ctx_RxL4TCPSocketSendSyn *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketIsValidSynAckS(struct ctx_RxL4TCPSocketIsValidSynAckS *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketIsValidFinAck(struct ctx_RxL4TCPSocketIsValidFinAck *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketIsValidFinAck2(struct ctx_RxL4TCPSocketIsValidFinAck2 *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketSChangeEstablished(struct ctx_RxL4TCPSocketSChangeEstablished *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketIsDataPacket(struct ctx_RxL4TCPSocketIsDataPacket *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketIsFinSet(struct ctx_RxL4TCPSocketIsFinSet *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketSChangeLastAck(struct ctx_RxL4TCPSocketSChangeLastAck *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketSChangeToClosed(struct ctx_RxL4TCPSocketSChangeToClosed *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketSChangeToClosed2(struct ctx_RxL4TCPSocketSChangeToClosed2 *context, struct state *state, struct input **in);
node_out_t do_pg__RxL4TCPSocketCopyData(struct ctx_RxL4TCPSocketCopyData *context, struct state *state, struct input **in);
node_out_t do_pg__TxQueue(struct ctx_TxQueue *context, struct state *state, struct input **in);
node_out_t do_pg__TxL4UDPInitiateResponse(struct ctx_TxL4UDPInitiateResponse *context, struct state *state, struct input **in);
node_out_t do_pg__TxL4UDPAllocateHeader(struct ctx_TxL4UDPAllocateHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL4UDPFillHeader(struct ctx_TxL4UDPFillHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL4TCPInitiateResponse(struct ctx_TxL4TCPInitiateResponse *context, struct state *state, struct input **in);
node_out_t do_pg__TxL4TCPAllocateHeader(struct ctx_TxL4TCPAllocateHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL4TCPFillHeader(struct ctx_TxL4TCPFillHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ICMPInitiateResponse(struct ctx_TxL3ICMPInitiateResponse *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ICMPAllocateHeader(struct ctx_TxL3ICMPAllocateHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ICMPFillHeader(struct ctx_TxL3ICMPFillHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3IPv4AllocateHeader(struct ctx_TxL3IPv4AllocateHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3IPv4FillHeader(struct ctx_TxL3IPv4FillHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3IPv4Routing(struct ctx_TxL3IPv4Routing *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ARPSendGratuitous(struct ctx_TxL3ARPSendGratuitous *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ARPProcessPendingResponse(struct ctx_TxL3ARPProcessPendingResponse *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ARPInitiateResponse(struct ctx_TxL3ARPInitiateResponse *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ARPAllocateHeader(struct ctx_TxL3ARPAllocateHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ARPFillHeader(struct ctx_TxL3ARPFillHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ARPLookupRequestIn(struct ctx_TxL3ARPLookupRequestIn *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ARPLookup_(struct ctx_TxL3ARPLookup_ *context, struct state *state, struct input **in);
node_out_t do_pg__TxL3ARPSendRequest(struct ctx_TxL3ARPSendRequest *context, struct state *state, struct input **in);
node_out_t do_pg__TxL2EtherAllocateHeader(struct ctx_TxL2EtherAllocateHeader *context, struct state *state, struct input **in);
node_out_t do_pg__TxL2EtherFillHeader(struct ctx_TxL2EtherFillHeader *context, struct state *state, struct input **in);
node_out_t do_pg__E10kInit(struct ctx_E10kInit *context, struct state *state, struct input **in);
node_out_t do_pg__E10kRxQueue0(struct ctx_E10kRxQueue0 *context, struct state *state, struct input **in);
node_out_t do_pg__E10kRxQueue1(struct ctx_E10kRxQueue1 *context, struct state *state, struct input **in);
node_out_t do_pg__E10kRxQueue2(struct ctx_E10kRxQueue2 *context, struct state *state, struct input **in);
node_out_t do_pg__E10kRxQueue3(struct ctx_E10kRxQueue3 *context, struct state *state, struct input **in);
node_out_t do_pg__E10kL3IPv4Classified(struct ctx_E10kL3IPv4Classified *context, struct state *state, struct input **in);
node_out_t do_pg__E10kL3IPv4ValidChecksum(struct ctx_E10kL3IPv4ValidChecksum *context, struct state *state, struct input **in);
node_out_t do_pg__E10kTxQueue0(struct ctx_E10kTxQueue0 *context, struct state *state, struct input **in);
node_out_t do_pg__E10kTxQueue1(struct ctx_E10kTxQueue1 *context, struct state *state, struct input **in);
node_out_t do_pg__E10kTxQueue2(struct ctx_E10kTxQueue2 *context, struct state *state, struct input **in);
node_out_t do_pg__E10kTxQueue3(struct ctx_E10kTxQueue3 *context, struct state *state, struct input **in);
node_out_t do_pg__TapRxQueue(struct ctx_TapRxQueue *context, struct state *state, struct input **in);
node_out_t do_pg__TapTxQueue(struct ctx_TapTxQueue *context, struct state *state, struct input **in);



#endif // GENERATEDCODE_H_

