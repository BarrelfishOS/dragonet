#include <implementation.h>
#include <stddef.h>
#include <stdio.h>
#include <packet_access.h>
#include "config.h"

void testFun(struct state * st, struct input *in);

static uint8_t arp_request_rx[] = {
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xd6, 0xc8, 0x7f, 0xdd,
        0xe3, 0xab, 0x8, 0x6, 0x0, 0x1, 0x8, 0x0, 0x6, 0x4, 0x0, 0x1, 0xd6,
        0xc8, 0x7f, 0xdd, 0xe3, 0xab, 0xc0, 0xa8, 0x7b, 0x64, 0x0, 0x0, 0x0,
        0x0, 0x0, 0x0, 0xc0, 0xa8, 0x7b, 0x1
    };

//static  // FIXME: commenting out to avoid warning of unused variable
uint8_t arp_response_tx[] = {
         0xd6, 0xc8, 0x7f, 0xdd, 0xe3,
         0xab, 0x0, 0x1b, 0x22, 0x54, 0x69, 0xf8, 0x8, 0x6, 0x0, 0x1, 0x8, 0x0,
         0x6, 0x4, 0x0, 0x2, 0x0, 0x1b, 0x22, 0x54, 0x69, 0xf8, 0xc0, 0xa8,
         0x7b, 0x1, 0xd6, 0xc8, 0x7f, 0xdd, 0xe3, 0xab, 0xc0, 0xa8, 0x7b, 0x64
    };

static uint8_t pkt_icmp_echo_rx[] = {
         0x0, 0x1b, 0x22, 0x54, 0x69, 0xf8, 0xd6, 0xc8, 0x7f, 0xdd,
         0xe3, 0xab, 0x8, 0x0, 0x45, 0x0, 0x0, 0x54, 0x2b, 0xdd, 0x40, 0x0,
         0x40, 0x1, 0x97, 0x15, 0xc0, 0xa8, 0x7b, 0x64, 0xc0, 0xa8, 0x7b, 0x1,
         0x8, 0x0, 0x28, 0x59, 0xd, 0x94, 0x0, 0x1, 0x9, 0x5a, 0x1f, 0x53, 0x0,
         0x0, 0x0, 0x0, 0xd1, 0x91, 0x9, 0x0, 0x0, 0x0, 0x0, 0x0, 0x10, 0x11,
         0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
         0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29,
         0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35,
         0x36, 0x37
    };

//static  // FIXME: commenting out to avoid warning of unused variable
uint8_t pkt_icmp_echo_response_tx[] = {
         0xd6, 0xc8, 0x7f, 0xdd, 0xe3, 0xab, 0x0, 0x1b, 0x22, 0x54,
         0x69, 0xf8, 0x8, 0x0, 0x45, 0x0, 0x0, 0x54, 0x0, 0x0, 0x40, 0x0, 0x40,
         0x1, 0xc2, 0xf2, 0xc0, 0xa8, 0x7b, 0x1, 0xc0, 0xa8, 0x7b, 0x64, 0x0,
         0x0, 0x30, 0x59, 0xd, 0x94, 0x0, 0x1, 0x9, 0x5a, 0x1f, 0x53, 0x0, 0x0,
         0x0, 0x0, 0xd1, 0x91, 0x9, 0x0, 0x0, 0x0, 0x0, 0x0, 0x10, 0x11, 0x12,
         0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e,
         0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a,
         0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36,
         0x37
    };


static uint8_t pkt_unsupported_proto_rx[] = {
         0x1, 0x0, 0x5e, 0x0, 0x0, 0xfb, 0xd6, 0xc8, 0x7f, 0xdd, 0xe3,
         0xab, 0x8, 0x0, 0x45, 0x0, 0x0, 0x49, 0xa9, 0x40, 0x40, 0x0, 0xff,
         0x11, 0xb5, 0x5a, 0xc0, 0xa8, 0x7b, 0x64, 0xe0, 0x0, 0x0, 0xfb, 0x14,
         0xe9, 0x14, 0xe9, 0x0, 0x35, 0xb7, 0x2e, 0x0, 0x0, 0x0, 0x0, 0x0, 0x2,
         0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x5, 0x5f, 0x69, 0x70, 0x70, 0x73, 0x4,
         0x5f, 0x74, 0x63, 0x70, 0x5, 0x6c, 0x6f, 0x63, 0x61, 0x6c, 0x0, 0x0,
         0xc, 0x0, 0x1, 0x4, 0x5f, 0x69, 0x70, 0x70, 0xc0, 0x12, 0x0, 0xc, 0x0,
         0x1
    };


static void show_hex_dump(void *data, size_t len)
{
    int i = 0;
    printf("[");
    uint8_t *ptr = data;
    for (i = 0; i < len; ++i) {
        printf("0x%x,", ptr[i]);
    }
    printf("]\n");
}

static void run_packet(struct state *st, void *buffer, size_t len)
{
    struct input *in = input_alloc();
    input_copy_packet(in, buffer, len);
    testFun(st, in);
    input_free(in);
}



struct arp_cache hardcoded_cache = {
    .ip = 0xc0a87b64,
    .mac = 0x1d366fc109a2ULL,
    .next = NULL,
};


void pg_state_init(struct state *st);

int main_loop(struct driver *drv)
{
    struct state st;

    pg_state_init(&st);
    st.driver_handler = drv;

    if (st.driver_handler == NULL
            || st.driver_handler->drv_init == NULL
            || st.driver_handler->drv_rx == NULL
            || st.driver_handler->drv_tx == NULL
            || st.driver_handler->drv_mac_read == NULL
            || st.driver_handler->drv_ip_read == NULL
          ) {
        panic("No driver handler configured, quitting\n");
        return -1;
    }

    st.driver_handler->drv_handle = st.driver_handler->drv_init(NULL);

    if (st.driver_handler->drv_handle == NULL) {
        panic("driver initialization failed, quitting\n");
        return -1;
    }

    st.local_mac = st.driver_handler->drv_mac_read(st.driver_handler->drv_handle);
    st.local_ip = st.driver_handler->drv_ip_read(st.driver_handler->drv_handle);

    uint8_t buf[4096];
    int pktsize;

    // looping with tuntap device
    for (;;) {
        //pktsize = tap_read(tap_dn, buf, sizeof(buf));
        pktsize =  st.driver_handler->drv_rx(st.driver_handler->drv_handle, buf, sizeof(buf));
        if (pktsize < 0) {
            panic("drv_rx failed and returned %d\n", pktsize);
            return -1;
        }
        run_packet(&st, buf, pktsize);
    }
    return 0;

    // Testing incoming arp request packet
    printf("\nTesting:arp_request_rx\n");
    run_packet(&st, arp_request_rx, sizeof(arp_request_rx));
    printf("\nVerify: Should match with following expected packet\n");
    show_hex_dump(arp_response_tx, sizeof(arp_response_tx));

    // Testing incoming icmp packet
    printf("\nTesting:ICMP packet\n");
    run_packet(&st, pkt_icmp_echo_rx, sizeof(pkt_icmp_echo_rx));
    printf("\nVerify: Should match with following expected packet\n");
    show_hex_dump(pkt_icmp_echo_response_tx, sizeof(pkt_icmp_echo_response_tx));

    // Testing incoming packet for unsupported protocol
    printf("\nTesting incoming packet for unsupported protocol\n");
    run_packet(&st, pkt_unsupported_proto_rx, sizeof(pkt_unsupported_proto_rx));

    return 0;
}

void testFun(struct state * st, struct input *in)
{
    node_out_t Queue=-1, PacketDrop=-1, NotSupported=-1, RxL3IPValid=-1,
               RxL3IPAndBelowValid=-1, RxToTx=-1, RxL2EtherClassified=-1,
               RxL2EtherValidLength=-1, RxL2EtherValidUnicast=-1,
               RxL2EtherValidMulticast=-1, RxL2EtherValidBroadcast=-1,
               RxL2EtherValidDest=-1, RxL2EtherValidSrc=-1,
               RxL2EtherValidLocalMAC=-1, RxL2EtherValid=-1,
               RxL2EtherValidType=-1, RxL2EtherClassifyL3=-1,
               RxL3ARPValidHeaderLength=-1, RxL3ARPClassify=-1,
               RxL3ARPLocalIPDest=-1, RxL3ARPValidRequest=-1,
               RxL3ARPNeedsResponse=-1, RxL3ARPValidResponse=-1,
               RxL3ARPIsPending=-1, RxL3ARPValidPendingResponse=-1,
               RxL3ARPProcessPendingResponse=-1, RxL3IPv4ValidHeaderLength=-1,
               RxL3IPv4ValidReassembly=-1, RxL3IPv4ValidVersion=-1,
               RxL3IPv4ValidLength=-1, RxL3IPv4ValidTTL=-1,
               RxL3IPv4ValidChecksum=-1, RxL3IPv4ValidLocalIP=-1,
               RxL3IPv4Classify=-1, RxL3IPv4Valid=-1,
               RxL3ICMPValidHeaderLength=-1, RxL3ICMPValidChecksum=-1,
               RxL3ICMPValid=-1, RxL3ICMPIsTypeRequest=-1,
               RxL3ICMPNeedsResponse=-1, RxTagTxARPIR=-1, RxTagTxARPLu=-1,
               RxTagTxICMPIR=-1, TxDemux=-1, TxQueue=-1,
               TxL3ICMPInitiateResponse=-1, TxL3ICMPAllocateHeader=-1,
               TxL3ICMPFillHeader=-1, TxL3IPv4Prepare=-1,
               TxL3IPv4AllocateHeader=-1, TxL3IPv4FillHeader=-1,
               TxL3IPv4Routing=-1, TxL3ARPInitiateResponse=-1,
               TxL3ARPPrepare=-1, TxL3ARPAllocateHeader=-1,
               TxL3ARPFillHeader=-1, TxL3ARPLookupRequestIn=-1,
               TxL3ARPLookup=-1, TxL3ARPLookup_=-1, TxL3ARPSendRequest=-1,
               TxL2EtherPrepare=-1, TxL2EtherAllocateHeader=-1,
               TxL2EtherFillHeader=-1;
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
//        dprint("PacketContents\n");
//        show_hex_dump(in->data, in->len);
    }
}



