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
//#include <dragonet/app_lowlevel.h>

typedef void (*event_handler_fun_ptr)(const int fd, const short which, void *arg);

struct dn_thread_state {
    struct stack_handle *stack;
    socket_handle_t sh;
    event_handler_fun_ptr callback_memcached_fn;
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


int dn_stack_init(uint16_t uport);
int register_callback_dn(void *dn_state, event_handler_fun_ptr fun, int fd,
        short which, void *arg);
int recvfrom_dn(void *dn_state, uint8_t *buff, int bufsize);
int send_dn(void *dn_state, uint8_t *buff, int bufsize);
void event_handle_loop_dn(void *dn_state);

#endif // DRAGONET


#endif // _DNET_INTERFACE_H

