#ifndef HELPERS_H_
#define HELPERS_H_

#include <dragonet/app_lowlevel.h>
#include <implementation.h>
#include <app_control.h>
#include <packet_access.h>

struct stack_handle;

struct stack_handle *stack_init(const char *stackname, const char *name);
struct state *stack_get_state(struct stack_handle *sh);
void stack_process_event(struct stack_handle *sh);
struct input *stack_input_alloc(struct stack_handle *sh);
void stack_input_free(struct stack_handle *sh, struct input *in);

struct socket_handle;
typedef struct socket_handle *socket_handle_t;
struct socket_handle {
    struct stack_handle *stack;
    dnal_sockh_t lsh;
    void (*cb_receive)(socket_handle_t, struct input *, void *);
    void *data;
};


socket_handle_t socket_create(struct stack_handle *sh,
                              void (*receive)(
                                    socket_handle_t, struct input *, void *),
                              void *data);
bool socket_close(socket_handle_t handle);

bool socket_bind_udp_listen(socket_handle_t handle, uint32_t ip, uint16_t port);
bool socket_bind_udp_flow(socket_handle_t handle,
                          uint32_t s_ip, uint16_t s_port,
                          uint32_t d_ip, uint16_t d_port);

bool socket_send_udp(socket_handle_t handle, struct input *in,
                     uint32_t s_ip, uint16_t s_port,
                     uint32_t d_ip, uint16_t d_port);


#if 0
int register_callback_dn(event_handler_fun_ptr fun, int fd, short which,
        void *arg);

void event_handle_loop_dn(void);

int recvfrom_dn(uint8_t *buff, int bufsize);
int send_dn(uint8_t *buff, int bufsize);
#endif // 0 // FIXME: remove this code

#endif // ndef HELPERS_H_

