#ifndef IMPLEMENTATION_H_
#define IMPLEMENTATION_H_

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

typedef int node_out_t;
typedef uint16_t pktoff_t;

struct arp_pending;
struct arp_cache;
struct input;
struct state {
    uint32_t local_ip;
    uint64_t local_mac;

    struct arp_pending *arp_pending;
    struct arp_cache   *arp_cache;

    uint64_t pkt_counter;
};

struct arp_pending {
    uint32_t ip;
    struct input *input;

    struct arp_pending *next;
};

struct arp_cache {
    uint32_t ip;
    uint64_t mac;

    struct arp_cache *next;
};

struct input {
    // Buffer
    void  *data;
    size_t len;
    size_t space_before;
    size_t space_after;

// Attributes ----------------------------------------

    // Offset for headers on different layers
    pktoff_t offset_l2;
    pktoff_t offset_l3;
    pktoff_t offset_l4;
    pktoff_t offset_l5;

    // Ethernet
    uint64_t eth_dst_mac;
    uint64_t eth_src_mac;
    uint16_t eth_type;

    // IPv4
    uint16_t ip4_proto;
    uint32_t ip4_dst;
    uint32_t ip4_src;

    // ARP
    uint64_t arp_src_mac;
    uint64_t arp_dst_mac;
    uint32_t arp_src_ip;
    uint32_t arp_dst_ip;
    uint8_t  arp_oper;

    // Misc
    uint8_t mux_id;
};

enum attr_mux_id {
    ATTR_MUX_ARPIR,
    ATTR_MUX_ARPLU,
    ATTR_MUX_ICMPIR,
};


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

#define PORT_BOOL(b) ((b) ? P_true : P_false)

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
node_out_t do_pg__TxL3ARPLookup_(struct state *state, struct input *in);
node_out_t do_pg__TxL3ARPSendRequest(struct state *state, struct input *in);
node_out_t do_pg__TxL2EtherAllocateHeader(struct state *state, struct input *in);
node_out_t do_pg__TxL2EtherFillHeader(struct state *state, struct input *in);


//#define panic(x...) do { printf(#__FILE__":"#__LINE__": "x); abort(); } while (0)
#define panic(x...) panic_(__FILE__,__LINE__,x);

static inline void panic_(const char *file, int line, const char *fmt,...)
    __attribute__((noreturn));
static inline void panic_(const char *file, int line, const char *fmt,...)
{
    va_list va;
    fprintf(stderr, "%s:%d: ", file, line);
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
    abort();
}

/** Allocate/initialize a new input structure including a buffer */
struct input *input_alloc(void);
void input_free(struct input *in);
/** Keep the data in the input struct as is, but reinitialize attributes */
void input_clean_attrs(struct input *in);

#define MYDEBUG     1
#ifdef MYDEBUG
#define dprint(x...)    printf("debug:" x)
#else
#define dprint(x...)   ((void)0)
#endif // MYDEBUG

#endif

