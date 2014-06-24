#include "dnet_interface.h"
#include <inttypes.h>

#ifdef DRAGONET

int use_dragonet_stack = 0;

// to record first socket which called listen
static dnal_sockh_t dsh_first = NULL;

// Lock to protect dragonet initialization
static pthread_mutex_t dn_init_lock = PTHREAD_MUTEX_INITIALIZER; // dragonet lock
static int threads_initialized_count = 0;

#if 0
// Wrapper which stores the packet received with callback tempararily
// and calls event handler mechanism
static void recv_cb(socket_handle_t sh1, struct input *in, void *data)
{

    mprint("debug: %s:%s:%d: callback arrived\n", __FILE__, __FUNCTION__, __LINE__);
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
    assert(current_thread_st_copy != NULL);
    assert(current_thread_st_copy->current_packet != NULL);
    stack_input_free(current_thread_st_copy->stack,
            current_thread_st_copy->current_packet);

    // Not unlocking as event handler will unlock this.
    //pthread_mutex_unlock(&dn_lock);
} // end function:  recv_cb
#endif // 0


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
    mprint("debug: %s:%s:%d: [TID:%d], callback registered\n",
                __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);
    return 0;
}


static void handle_single_event(struct dn_thread_state *dnt_state,
        struct dnal_aq_event *event)
{
    struct input *in;

    assert(event->type == DNAL_AQET_INPACKET);
    in = event->data.inpacket.buffer;
    assert(in != NULL);

    mprint("debug: %s:%s:%d: [TID:%d], new packet arrived\n",
            __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);

   dnt_state->current_packet = in;

    // calling callback function which will do processing in memcached
    dnt_state->callback_memcached_fn(
                dnt_state->callback_memcached_fd,
                dnt_state->callback_memcached_which,
                dnt_state->callback_memcached_arg);
    // NOTE: struct in will be freed in recvfrom_dn call
}


void event_handle_loop_dn(void *dn_state)
{
    assert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;
    assert(dnt_state->callback_memcached_fn);

    // Each thread will call this with its own dn_state structure.

    mprint("debug: %s:%s:%d: [TID:%d], looping for incoming packets\n",
            __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);
//    pthread_mutex_lock(&dnt_state->dn_lock);

    while (1) {
        //mprint("waiting for next event\n");
        errval_t err = dnal_aq_poll(dnt_state->daq, &dnt_state->event);
        if (err == SYS_ERR_OK) {
            mprint("%s:%s:%d: [TID:%d], new event arrived\n",
                __FILE__, __func__, __LINE__, dnt_state->tindex);
            handle_single_event(dnt_state, &dnt_state->event);
        }

        //stack_process_event(dnt_state->stack);
//        mprint("%s:%s:%d: [TID:%d], stack_process_event done! looping back\n",
//                __FILE__, __func__, __LINE__, dnt_state->tindex);
    } // end while: infinite
} // end function: event_handle_loop_dn


int recvfrom_dn(void *dn_state, uint8_t *buff, int bufsize)
{
    assert(buff != NULL);
    assert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;

    mprint("debug: %s:%s:%d: [TID:%d]\n",
            __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);

    struct input *in = dnt_state->current_packet;

    int len = in->len - in->attr->offset_l5;
    dnt_state->udp_sport = in->attr->udp_sport;
    dnt_state->udp_dport = in->attr->udp_dport;
    dnt_state->ip4_src = in->attr->ip4_src;
    dnt_state->ip4_dst = in->attr->ip4_dst;

    dnt_state->dest.type = DNAL_NETDSTT_IP4UDP;
    dnt_state->dest.data.ip4udp.ip_local = in->attr->ip4_dst;
    dnt_state->dest.data.ip4udp.ip_remote = in->attr->ip4_src;
    dnt_state->dest.data.ip4udp.port_local = in->attr->udp_dport;
    dnt_state->dest.data.ip4udp.port_remote = in->attr->udp_sport;

    assert(len <= bufsize);

    mprint("debug: %s:%s:%d, [TID:%d], sport = %"PRIx16", dport=%"PRIx16", "
            "srcip = %"PRIx32" dstip = %"PRIx32", len = %d \n",
            __FILE__,__FILE__, __LINE__, dnt_state->tindex,
            in->attr->udp_sport, in->attr->udp_dport, in->attr->ip4_src,
            in->attr->ip4_dst, len);

    memcpy(buff, (uint8_t *)in->data + in->attr->offset_l5, len);

    mprint("debug: %s:%s:%d: [TID:%d], copying data of len %d, %d, %d at "
            "location %p of size %d\n",
     __FILE__, __FILE__, __LINE__, dnt_state->tindex, len, in->len,
     in->attr->offset_l5,
     buff, bufsize);

    // Free the buffer used for received packet.
   errval_t err = dnal_aq_buffer_free(dnt_state->daq, in);
   if (err != SYS_ERR_OK) {
        printf("%s:%s:%d: (TID: %d) ERROR: dnal_aq_buffer_failed failed to "
                "allocate buffer\n",
                __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);
   }

    return len;
}

int send_dn(void *dn_state, uint8_t *buff, int bufsize)
{
    assert(buff != NULL);
    assert(bufsize <= 1410);
    assert(dn_state != NULL);

    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;

    // Allocate buffer to send, and copy the contents
    //
    struct input *in = NULL;
    int ret = dnal_aq_buffer_alloc(dnt_state->daq, &in);
    if (ret != SYS_ERR_OK) {
        printf("%s:%s:%d: (TID: %d) ERROR: dnal_aq_buffer_alloc failed to "
                "allocate buffer\n",
                __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);
    }
    assert(in != NULL);
    pkt_prepend(in, bufsize);
    memcpy(in->data, buff, bufsize);

    // This is implementation of send call
    dnal_socket_send(dnt_state->dsh, in, &dnt_state->dest);
    return bufsize;
}


#if 0
// initialize dragonet network stack to listen on udp-port uport
int dn_stack_init_specific(struct dn_thread_state *dn_tstate,
        char *slot_name, uint16_t uport)
{

    pthread_mutex_init(&dn_lock, NULL);
    pthread_mutex_lock(&dn_lock);
    int ret = 1;

    printf("Using %s as name to connect with port %"PRIu16"\n", appName, uport);
    stack = stack_init("dragonet", appName);
    sh = socket_create(stack, recv_cb, NULL);
    if (!socket_bind_udp_listen(sh, 0, uport)) {
        fprintf(stderr, "socket_bind_udp_listen failed\n");
        ret = -1;
    }
    pthread_mutex_unlock(&dn_lock);
    return ret;
} // end function: dn_stack_init

int dn_stack_init(uint16_t uport) {

    int sid = uport - 7777;
    char appName[50];
    snprintf(appName, sizeof(appName), "t%d", sid);
    return dn_stack_init_specific(appName, uport);
}
#endif // 0

int lowlevel_dn_stack_init(struct dn_thread_state *dn_tstate)
{
    int ret = 0;
    char appName[50];

    assert(dn_tstate != NULL);

    // Application level initialization lock
    pthread_mutex_lock(&dn_init_lock);

    // thread level dragonet-stack specific lock
    pthread_mutex_lock(&dn_tstate->dn_lock);

    dn_tstate->tindex = threads_initialized_count;

    // figure out the application slot name to connect with
    ret = snprintf(appName, sizeof(appName), "t%d", threads_initialized_count);
    strncpy(dn_tstate->app_slot, appName, ret);
    mprint("debug: %s:%s:%d: [TID:%d], connecting with slotname [%s]\n",
            __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex,
            dn_tstate->app_slot);

    // create an application queue with application slot
    ret = dnal_aq_create("dragonet", dn_tstate->app_slot, &dn_tstate->daq);
    err_expect_ok(ret);

    // create a socket
    ret = dnal_socket_create(dn_tstate->daq, &dn_tstate->dsh);
    err_expect_ok(ret);

    // If this is first thread then bind, else span
    if (dsh_first == NULL) {
        mprint("debug: %s:%s:%d: [TID:%d], This is first thread\n",
                __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex);

        dn_tstate->dnd.type = DNAL_NETDSTT_IP4UDP;
        dn_tstate->dnd.data.ip4udp.ip_remote = 0;
        dn_tstate->dnd.data.ip4udp.port_remote = 0;

        // FIXME: get the actual listen IP address
        dn_tstate->dnd.data.ip4udp.ip_local = 0;

        // FIXME: get the actual specified port number
        dn_tstate->dnd.data.ip4udp.port_local = dn_tstate->listen_port_udp;
        dn_tstate->dnd.data.ip4udp.port_local = 7777;
        mprint("debug: %s:%s:%d: [TID:%d],  Binding, using listen port %"PRIu16"\n",
                __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex,
                dn_tstate->dnd.data.ip4udp.port_local);

        err_expect_ok(dnal_socket_bind(dn_tstate->dsh, &dn_tstate->dnd));
        dsh_first = dn_tstate->dsh;
    } else {

        mprint("debug: %s:%s:%d: [TID:%d], This is not first thread, "
                "so using existing socket for spanning\n",
                __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex);
        err_expect_ok(dnal_socket_span(dsh_first, dn_tstate->daq, dn_tstate->dsh));
    }
    ++threads_initialized_count;

    pthread_mutex_unlock(&dn_tstate->dn_lock);
    pthread_mutex_unlock(&dn_init_lock);
    return ret;
}

#endif // DRAGONET

