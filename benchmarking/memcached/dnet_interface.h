#ifndef _DNET_INTERFACE_H
#define _DNET_INTERFACE_H

#ifdef ENABLE_DRAGONET
// If not enabled, enable DRAGONET
#ifndef DRAGONET
#define DRAGONET        1
#endif // DRAGONET
#endif // ENABLE_DRAGONET


//#define MYDEBUG     1
#ifdef MYDEBUG
#define mprint(x...)    printf("debug:" x)
#else
#define mprint(x...)   ((void)0)
#endif // MYDEBUG


#ifdef DRAGONET
#include <pthread.h>  // for using pthread_mutex_locks
#include <assert.h>
#include <helpers.h>
//#include <dragonet/app_lowlevel.h>

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
};

extern int use_dragonet_stack;


//int dn_stack_init_specific(char *slot_name, uint16_t uport);
//int dn_stack_init(uint16_t uport);

int lowlevel_dn_stack_init(struct dn_thread_state *dn_tstate);

int register_callback_dn(void *dn_state, event_handler_fun_ptr fun, int fd,
        short which, void *arg);
int recvfrom_dn(void *dn_state, uint8_t *buff, int bufsize);
int send_dn(void *dn_state, uint8_t *buff, int bufsize);
void event_handle_loop_dn(void *dn_state);

#endif // DRAGONET


#endif // _DNET_INTERFACE_H

