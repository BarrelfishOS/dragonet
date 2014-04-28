#ifndef HELPERS_H_
#define HELPERS_H_

#include <implementation.h>
#include <app_control.h>

void stack_init(const char *name, const char *inq, const char *outq);
struct state *stack_get_state(void);
void stack_process_event(void);

struct socket_handle;
typedef struct socket_handle *socket_handle_t;
struct socket_handle {
    uint64_t id;
    bool bound;
    bool ready;
    void (*cb_receive)(socket_handle_t, struct input *, void *);
    void *data;

    struct socket_handle *next;
};


socket_handle_t socket_create(void (*receive)(
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


#endif // ndef HELPERS_H_

