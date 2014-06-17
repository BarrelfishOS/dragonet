
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>

#include <implementation.h>
#include <helpers.h>

struct stack_handle {
    dnal_appq_t aq;
};

struct input *stack_input_alloc(struct stack_handle *sh)
{
    struct input *in;
    if (!err_is_ok(dnal_aq_buffer_alloc(sh->aq, &in))) {
        return NULL;
    }
    return in;
}

void stack_input_free(struct stack_handle *sh, struct input *in)
{
    err_expect_ok(dnal_aq_buffer_free(sh->aq, in));
}

struct stack_handle *stack_init(const char *stackname, const char *name)
{
    struct stack_handle *sh;
    dnal_appq_t aq;
    errval_t err;

    err = dnal_aq_create(stackname, name, &aq);
    if (!err_is_ok(err)) {
        return NULL;
    }

    sh = malloc(sizeof(*sh));
    sh->aq = aq;
    return sh;
}

struct state *stack_get_state(struct stack_handle *sh)
{
    return dnal_aq_state(sh->aq);
}

void stack_process_event(struct stack_handle *stack)
{
    struct socket_handle *sh;
    struct dnal_aq_event ev;
    errval_t err;

//    dprint("debug(%s:%s:%d): called\n",
//                __FILE__, __FUNCTION__, __LINE__);

    while(1) {
        err = dnal_aq_poll(stack->aq, &ev);
        if (err == DNERR_NOEVENT) {
//        dprint("ERROR (%s:%s:%d): no event\n",
//                __FILE__, __FUNCTION__, __LINE__);
        // FIXME: we need a way to return what exactly happened
        //return;
            continue;
        } else if (err == DNERR_EVENT_ABORT) {
            dprint("ERROR (%s:%s:%d): EVENT_ABORT\n",
                __FILE__, __FUNCTION__, __LINE__);
            return;
        }
        // This is normal event
        break;
    }

    assert(ev.type == DNAL_AQET_INPACKET);
    sh = dnal_socket_opaque_get(ev.data.inpacket.socket);
    sh->cb_receive(sh, ev.data.inpacket.buffer, sh->data);
}


socket_handle_t socket_create(struct stack_handle *stack,
                              void (*receive)(
                                    socket_handle_t, struct input *, void *),
                              void *data)
{
    struct socket_handle *sh;
    dnal_sockh_t lsh;

    if (!err_is_ok(dnal_socket_create(stack->aq, &lsh))) {
        return NULL;
    }

    sh = malloc(sizeof(*sh));
    sh->stack = stack;
    sh->cb_receive = receive;
    sh->data = data;

    sh->lsh = lsh;
    dnal_socket_opaque_set(lsh, sh);
    return sh;
}
// TODO
//bool socket_close(socket_handle_t handle);

bool socket_bind_udp_listen(socket_handle_t handle, uint32_t ip, uint16_t port)
{
    struct dnal_net_destination dest;

    dest.type = DNAL_NETDSTT_IP4UDP;
    dest.data.ip4udp.ip_local = ip;
    dest.data.ip4udp.ip_remote = 0;
    dest.data.ip4udp.port_local = port;
    dest.data.ip4udp.port_remote = 0;

    return err_is_ok(dnal_socket_bind(handle->lsh, &dest));
}

bool socket_bind_udp_flow(socket_handle_t handle,
                          uint32_t s_ip, uint16_t s_port,
                          uint32_t d_ip, uint16_t d_port)
{
    struct dnal_net_destination dest;

    dest.type = DNAL_NETDSTT_IP4UDP;
    dest.data.ip4udp.ip_local = d_ip;
    dest.data.ip4udp.ip_remote = s_ip;
    dest.data.ip4udp.port_local = d_port;
    dest.data.ip4udp.port_remote = s_port;

    return err_is_ok(dnal_socket_bind(handle->lsh, &dest));
}

bool socket_send_udp(socket_handle_t handle, struct input *in,
                     uint32_t s_ip, uint16_t s_port,
                     uint32_t d_ip, uint16_t d_port)
{
    struct dnal_net_destination dest;

    dest.type = DNAL_NETDSTT_IP4UDP;
    dest.data.ip4udp.ip_local = s_ip;
    dest.data.ip4udp.ip_remote = d_ip;
    dest.data.ip4udp.port_local = s_port;
    dest.data.ip4udp.port_remote = d_port;

    return err_is_ok(dnal_socket_send(handle->lsh, in, &dest));
}

