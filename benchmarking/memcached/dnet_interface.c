#include "dnet_interface.h"

#ifdef DRAGONET

int use_dragonet_stack = 0;

// Following are dragonet instance specific
static struct stack_handle *stack = NULL;
static socket_handle_t sh;
static struct dn_thread_state *current_thread_st = NULL;
static pthread_mutex_t dn_lock = PTHREAD_MUTEX_INITIALIZER; // dragonet lock


// Wrapper which stores the packet received with callback tempararily
// and calls event handler mechanism
static void recv_cb(socket_handle_t sh1, struct input *in, void *data)
{
    struct dn_thread_state *current_thread_st_copy;
    // we need to have one current packet for each thread.
    current_thread_st_copy = current_thread_st;
    current_thread_st_copy->current_packet = in;
    // release dragonet lock here
    pthread_mutex_unlock(&dn_lock);

    // Whichever thread called stack_process_event should get the callback
    // Most of the memcached work should happen here
    current_thread_st_copy->callback_memcached_fn(
                current_thread_st_copy->callback_memcached_fd,
                current_thread_st_copy->callback_memcached_which,
                current_thread_st_copy->callback_memcached_arg);

    // Does input_free needs to be guarded with Dragonet lock?
    pthread_mutex_lock(&dn_lock);
    input_free(current_thread_st_copy->current_packet);
    pthread_mutex_unlock(&dn_lock);
} // end function:  recv_cb



/*
 * Register a callback function which will be called when packet is received
 */
int register_callback_dn(void *dn_state, event_handler_fun_ptr fun, int fd,
        short which, void *arg)
{

    assert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;

    // every thread register one
    if (dnt_state->callback_memcached_fn != NULL) {
        printf("dragonet: callback already registerd\n");
        exit(1);
        return -1;
    }
    dnt_state->callback_memcached_fn = fun;
    dnt_state->callback_memcached_fd = fd;
    dnt_state->callback_memcached_which = which;
    dnt_state->callback_memcached_arg = arg;
    dnt_state->stack = stack;
    dnt_state->sh = sh;
    return 0;
}

void event_handle_loop_dn(void *dn_state)
{
    assert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;

    // Each thread will call this.  All other threads should wait for their turn
    // use grab the dragonet lock here
    assert(dnt_state->callback_memcached_fn);
//    printf("looping for incoming packets\n");
    while (1) {
        pthread_mutex_lock(&dn_lock);
        current_thread_st = dnt_state;
        stack_process_event(dnt_state->stack);
    }
}


int recvfrom_dn(void *dn_state, uint8_t *buff, int bufsize)
{
    assert(buff != NULL);
    assert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;

    // printf("debug: %s:%s:%d\n", __FILE__, __FILE__, __LINE__);
    struct input *in = dnt_state->current_packet;

    int len = in->len - in->attr->offset_l5;
    dnt_state->udp_sport = in->attr->udp_sport;
    dnt_state->udp_dport = in->attr->udp_dport;
    dnt_state->ip4_src = in->attr->ip4_src;
    dnt_state->ip4_dst = in->attr->ip4_dst;
    assert(len <= bufsize);
    // printf("debug: %s:%s:%d, sport = %"PRIx16", dport=%"PRIx16", srcip = %"PRIx32" dstip = %"PRIx32" \n",
    // __FILE__,__FILE__, __LINE__, udp_sport, udp_dport, ip4_src, ip4_dst);

    memcpy(buff, (uint8_t *)in->data + in->attr->offset_l5, len);

    // printf("debug: %s:%s:%d: copying data of len %d, %d, %d at location %p of size %d\n",
    // __FILE__, __FILE__, __LINE__, len, in->len, in->attr->offset_l5,
    // buff, bufsize);
    return len;
}

int send_dn(void *dn_state, uint8_t *buff, int bufsize)
{
    assert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;

    assert(buff != NULL);
    assert(bufsize <= 1410);

    pthread_mutex_lock(&dn_lock);
    struct input *in = stack_input_alloc(dnt_state->stack);
    assert(in != NULL);
    pkt_prepend(in, bufsize);

    memcpy(in->data, buff, bufsize);

    in->attr->udp_sport = dnt_state->udp_dport;
    in->attr->udp_dport = dnt_state->udp_sport;
    in->attr->ip4_dst = dnt_state->ip4_src;
    in->attr->ip4_src = dnt_state->ip4_dst;

    socket_send_udp(dnt_state->sh, in,
                in->attr->ip4_src, in->attr->udp_sport,
                in->attr->ip4_dst, in->attr->udp_dport);
    pthread_mutex_unlock(&dn_lock);
    return bufsize;
}

// initialize dragonet network stack to listen on udp-port uport
int dn_stack_init(uint16_t uport)
{

    pthread_mutex_init(&dn_lock, NULL);
    pthread_mutex_lock(&dn_lock);
    int ret = 1;
    stack = stack_init("dragonet", "AppEcho");
    sh = socket_create(stack, recv_cb, NULL);
    if (!socket_bind_udp_listen(sh, 0, uport)) {
        fprintf(stderr, "socket_bind_udp_listen failed\n");
        ret = -1;
    }
    pthread_mutex_unlock(&dn_lock);
    return ret;
} // end function: dn_stack_init

#endif // DRAGONET


