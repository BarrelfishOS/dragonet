
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

    err = dnal_aq_poll(stack->aq, &ev);
    if (err == DNERR_NOEVENT || err == DNERR_EVENT_ABORT) {
        return;
    } else {
        err_expect_ok(err);
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


#if 0
//<<<<<<< HEAD
// fixme: I should not use them
//static event_handler_fun_ptr callback_memcached_fn = NULL;
//static int callback_memcached_fd = 0;
//static short callback_memcached_which = 0;
//static void *callback_memcached_arg = NULL;



/*
 * Register a callback function which will be called when packet is received
 */
int register_callback_dn(event_handler_fun_ptr fun, int fd, short which,
        void *arg)
{
    if (callback_memcached_fn != NULL) {
        printf("dragonet: callback already registerd\n");
        exit(1);
        return -1;
    }
    callback_memcached_fn = fun;
    callback_memcached_fd = fd;
    callback_memcached_which = which;
    callback_memcached_arg = arg;
    return 0;
}

static struct input *current_packet = NULL;


void event_handle_loop_dn(void)
{
    assert(callback_memcached_fn);
    printf("looping for incoming packets\n");
    while (1){
        current_packet = stack_get_packet();
        //printf("new UDP packet came in, calling callback %p\n",(unsigned char *)&callback_memcached_fn);
        callback_memcached_fn(callback_memcached_fd, callback_memcached_which,
                callback_memcached_arg);

        input_free(current_packet);
    }
}

static uint32_t ip4_src = 0;
static uint32_t ip4_dst = 0;
static uint16_t udp_sport = 0;
static uint16_t udp_dport = 0;
int recvfrom_dn(uint8_t *buff, int bufsize)
{
    assert(buff != NULL);
//    printf("debug: %s:%s:%d\n", __FILE__, __FILE__, __LINE__);
    struct input *in = current_packet;
    int len = in->len - in->attr->offset_l5;

//    printf("debug: %s:%s:%d, l4 offset = %d\n",
//            __FILE__, __FILE__, __LINE__,
//            (int)current_packet->attr->offset_l4);
    udp_sport = pkt_read32be(in, current_packet->attr->offset_l4);
    udp_dport = pkt_read32be(in, current_packet->attr->offset_l4+2);
    udp_sport = current_packet->attr->udp_sport;
    udp_dport = current_packet->attr->udp_dport;
    ip4_src   = current_packet->attr->ip4_src;
    ip4_dst   = current_packet->attr->ip4_dst;
    assert(len <= bufsize);
//    printf("debug: %s:%s:%d, sport = %"PRIx16", dport=%"PRIx16", srcip = %"PRIx32" dstip = %"PRIx32" \n",
//            __FILE__,__FILE__, __LINE__, udp_sport, udp_dport, ip4_src, ip4_dst);

    memcpy(buff, (uint8_t *)in->data + in->attr->offset_l5, len);

//    printf("debug: %s:%s:%d: copying data of len %d, %d, %d at location %p of size %d\n",
//            __FILE__, __FILE__, __LINE__, len, in->len, in->attr->offset_l5,
//            buff, bufsize);
    return len;
}


int send_dn(uint8_t *buff, int bufsize)
{

    assert(buff != NULL);
    assert(bufsize <= 1410);
    struct input *in = input_alloc();
    assert(in != NULL);
    pkt_prepend(in, bufsize);

    memcpy(in->data, buff, bufsize);

    in->attr->udp_sport = udp_dport;
    in->attr->udp_dport = udp_sport;
    in->attr->ip4_dst   = ip4_src;
    in->attr->ip4_src   = state->local_ip;
    stack_send_udp_packet(in);
    return bufsize;
}

#endif // 0  // Code to be removed
