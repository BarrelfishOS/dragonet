#ifndef _DNET_INTERFACE_H
#define _DNET_INTERFACE_H

#ifdef ENABLE_DRAGONET
// If not enabled, enable DRAGONET
#ifndef DRAGONET
#define DRAGONET        1
#endif // DRAGONET
#endif // ENABLE_DRAGONET


#ifdef DRAGONET

#include <pthread.h>  // for using pthread_mutex_locks
#include <assert.h>
#include <helpers.h>
#include <dragonet/app_lowlevel.h>
#include <udpproto.h>

//#define MYDEBUG     1
#ifdef MYDEBUG

static uint64_t get_tsc(void);
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

#define mprint(x...)    do { printf("TID:%d:Cycle:%"PRIu64":", (int)pthread_self(), get_tsc()); printf(":debug:" x); } while(0)
//#define dprint(x...)    do { printf("TID:%d:", (int)pthread_self()); printf(":debug:" x); } while(0)
//#define mprint(x...)    printf("debug:" x)
#else
#define mprint(x...)   ((void)0)
#endif // MYDEBUG




typedef void (*event_handler_fun_ptr)(const int fd, const short which, void *arg);

struct dn_thread_state {
    pthread_mutex_t dn_lock;  // lock around dragonet related state of thread
    int tindex;               // id of the thread
//    struct stack_handle *stack;
//    socket_handle_t sh;

    dnal_appq_t daq;    // application queue handle with network stack
    dnal_sockh_t dsh;   // Socket handle
    struct dnal_net_destination dnd;    // listen address for binding
    struct dnal_net_destination dest;   // destination for currently processing packet
    struct dnal_aq_event event;     // event handle to be used in polling
    event_handler_fun_ptr callback_memcached_fn;  // callback function ptr
    char app_slot[255];     // Name of application slot for this thread

    uint16_t listen_port_udp;
    int callback_memcached_fd;
    short callback_memcached_which;
    void *callback_memcached_arg;
    struct input *current_packet;
    uint32_t ip4_src;
    uint32_t ip4_dst;
    uint16_t udp_sport;
    uint16_t udp_dport;
    uint64_t pkt_count;
};

extern int use_dragonet_stack;
extern char *use_dragonet_stack_portmap;


//int dn_stack_init_specific(char *slot_name, uint16_t uport);
//int dn_stack_init(uint16_t uport);

int lowlevel_dn_stack_init(struct dn_thread_state *dn_tstate);

int register_callback_dn(void *dn_state, event_handler_fun_ptr fun, int fd,
        short which, void *arg);
int recvfrom_dn(void *dn_state, uint8_t *buff, int bufsize);
int send_dn(void *dn_state, uint8_t *buff, int bufsize);
void event_handle_loop_dn(void *dn_state);
void *parse_client_list(char *client_list_str, int thread_count);
#endif // DRAGONET


#endif // _DNET_INTERFACE_H

