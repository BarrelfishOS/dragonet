#ifndef IMPLEMENTATION_H_
#define IMPLEMENTATION_H_

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

#include <pipelines.h>
#include "../../lib/Util/tap.h"

//#define MYDEBUG     1
#ifdef MYDEBUG
#define dprint(x...)    printf("debug:" x)
#else
#define dprint(x...)   ((void)0)
#endif // MYDEBUG

//#define MYDEBUGV     1
#ifdef MYDEBUGV
#define ddprint(x...)    dprint(x)
#else
#define ddprint(x...)   ((void)0)
#endif // MYDEBUGV


#define DEFAULT_BUFFER_SIZE 2048

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
};

struct input {
    // Buffer
    void  *data;
    struct input_attributes *attr;
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
void input_copy_packet(struct input *in, unsigned char *buff, size_t len);
/** Keep the data in the input struct as is, but reinitialize attributes */
void input_clean_attrs(struct input *in);
void input_clean_packet(struct input *in);
void input_dump(struct input *in);
int32_t input_muxid(struct input *in);
void input_set_muxid(struct input *in, int32_t mux);
void input_xchg(struct input *a, struct input *b);

struct input *input_struct_alloc(void);
void input_struct_free(struct input *in);

//void testFun(struct state * st, struct input *in);

// does the continuous packet processing by fetching packets from driver
int main_loop(struct driver *drv);

#include "gencode.h"

#define PORT_BOOL(b) ((b) ? P_true : P_false)

#endif // IMPLEMENTATION_H_
