#ifndef IMPLEMENTATION_H_
#define IMPLEMENTATION_H_

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <inttypes.h>
#include <pthread.h>

#include <pipelines.h>


#if 0
//uint64_t get_tsc(void);
__inline__ static uint64_t
get_tsc(void) {
    uint32_t lo, hi;
    __asm__ __volatile__ ( /* serialize */
            "xorl %%eax,%%eax \n cpuid"
            ::: "%rax", "%rbx", "%rcx", "%rdx");
    /* We cannot use "=A", since this would use %rax on x86_64 and
     * return only the lower 32bits of the TSC
     */
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return (uint64_t)hi << 32 | lo;
}
#endif // 0

/*
//  output with cpu cycles
#define dprint(x...)    do { printf("TID:%d:Cycle:%"PRIu64":", \
(int)pthread_self(), get_tsc()); printf(":debug:" x); } while(0)
*/
#define dbg_printf(x...) do { printf("TID:%d:%s:%s:%d: ", (int)pthread_self(),   \
            __BASE_FILE__, __FUNCTION__, __LINE__); printf(":" x);          \
    } while(0)


//#define MYDEBUG     1
#ifdef MYDEBUG

#define dprint(x...)  dbg_printf(x)

#else
#define dprint(x...)   ((void)0)
#endif // MYDEBUG

//#define MYDEBUGV     1
#ifdef MYDEBUGV
#define ddprint(x...)    dprint(x)
#else
#define ddprint(x...)   ((void)0)
#endif // MYDEBUGV


// Shows packet classification after every 'INTERVAL_STAT_FREQUENCY' packet.
//  This is for debugging purpose to show where exactly packets are going
//  Currently supported by E10k queues,  sf queues, fancyecho
//  memecached has its own copy of these variables in dnet_interface.h
//#define SHOW_INTERVAL_STATS  1
//#define INTERVAL_STAT_FREQUENCY     (1)
#define INTERVAL_STAT_FREQUENCY     (1000)

/** Shows the type of each IPv4 packet.  Helpful in seeing which packets
 *      are going where.  When used with "INTERVAL_STAT_FREQUENCY      1"
 *      it helps to work out which queue, which core packet is being processed.
 */
//#define PACKET_CLASIFY_DEBUG      1
#ifdef PACKET_CLASIFY_DEBUG
#define clasify_msg(x...)       printf(x)
#else
#define clasify_msg(x...)       dprint(x)
#endif // PACKET_CLASIFY_DEBUG

#define DEFAULT_BUFFER_SIZE             (2048)


#include "../../lib/Util/tap.h"

typedef int node_out_t;
typedef uint16_t pktoff_t;

typedef uint16_t portno_t;

struct arp_pending;
struct arp_cache;
struct input;

typedef void* device_t;

typedef device_t (*drv_init_ft)(char *);
typedef pktoff_t (*drv_rx_pkt_ft)(device_t, uint8_t *, pktoff_t);
typedef int (*drv_tx_pkt_ft)(device_t, uint8_t *, pktoff_t);
typedef uint64_t (*drv_mac_read_ft)(device_t );
typedef uint32_t (*drv_ip_read_ft)(device_t );

struct driver {
    device_t drv_handle;
    drv_init_ft drv_init;
    drv_rx_pkt_ft drv_rx;
    drv_tx_pkt_ft drv_tx;
    drv_mac_read_ft drv_mac_read;
    drv_ip_read_ft drv_ip_read;
};

struct state {
    uint32_t local_ip;
    uint64_t local_mac;

    struct arp_pending *arp_pending;
    struct arp_cache   *arp_cache;

    uint64_t pkt_counter;
    struct driver *driver_handler;

    // XXX: HACK
    struct tap_handler *tap_handler;

    void *udp_lock;
    void *udp_flow_ht;
    void *udp_listen_ht;
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


struct input_attributes {
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

    // ICMP
    uint32_t icmp_id;

    // UDP
    portno_t udp_sport;
    portno_t udp_dport;

    // Misc
    int32_t mux_id;
    int32_t socket_id;
};

//  Hopefully, this is correct -AKK
//
// (Pointers)                    (Lengths)
//
//  addr+bsize  +-----------+ +
//              |           | |
//              |           | |---- space before
// data ------> +-----------+ +
//              |-----------| |
//              |-----------| |------ len
//              |-----------| |
//              +-----------+ +
//              |           | |
//              |           | |------ space after
//   addr       +-----------+ +


struct input {
    // Buffer
    void  *data;
    struct input_attributes *attr;
    uint64_t phys;
    int      qid;
    pktoff_t len;
    pktoff_t space_before;
    pktoff_t space_after;
    struct input *next;

    buffer_handle_t data_buffer;
    buffer_handle_t attr_buffer;
};

enum attr_mux_id {
    ATTR_MUX_ARPIR,
    ATTR_MUX_ARPLU,
    ATTR_MUX_ICMPIR,
    ATTR_MUX_UDPIR,
    ATTR_MUX_TCPIR,
};

struct ctx_generic {
    void *implementation;
};

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
struct input *input_alloc_plh(pipeline_handle_t plh);
void input_free_plh(pipeline_handle_t plh, struct input *in);
void input_copy_packet(struct input *in, unsigned char *buff, size_t len);
/** Keep the data in the input struct as is, but reinitialize attributes */
void input_clean_attrs(struct input *in);
void input_clean_packet(struct input *in);
void input_dump(struct input *in);
int32_t input_muxid(struct input *in);
void input_set_muxid(struct input *in, int32_t mux);
void input_xchg(struct input *a, struct input *b);


bool ip_from_string(const char *ip, uint32_t *dst);

//void testFun(struct state * st, struct input *in);

// does the continuous packet processing by fetching packets from driver
int main_loop(struct driver *drv);


// This is a way to declare the dragonet stack is initialied
#define DN_READY_FNAME       "stack.dnready"
#define APP_READY_FNAME       ".appready"
void declare_dragonet_initialized(char *fname, char *msg);

#include "gencode.h"

#define PORT_BOOL(b) ((b) ? P_true : P_false)




enum spawn_priority {
    SPAWNPRIO_HIGH,
    SPAWNPRIO_LOW,
};

bool spawn_impl(struct ctx_generic *ctx, struct input *in, enum out_spawns s,
        enum spawn_priority p);
#define spawn(nctx,in,sid,prio) spawn_impl(&(nctx)->generic, in, sid, prio)


// For stats debugging
struct pkt_stats {
    uint64_t rx_eth;
    uint64_t rx_ethv;
    uint64_t rx_arp;
    uint64_t rx_arpv;
    uint64_t rx_ipv4;
    uint64_t rx_ipv6;
    uint64_t rx_icmp;
    uint64_t rx_icmpv;
    uint64_t rx_udp;
    uint64_t rx_drop;
    uint64_t tx_eth;
    uint64_t tx_ethv;
    uint64_t tx_arp;
    uint64_t tx_arpv;
    uint64_t tx_ipv4;
    uint64_t tx_ipv6;
    uint64_t tx_icmp;
    uint64_t tx_icmpv;
    uint64_t tx_udp;
    uint64_t tx_drop;
};
extern struct pkt_stats debug_pkt_stats;
void show_pkt_stats(struct pkt_stats *stats);


#endif // IMPLEMENTATION_H_
