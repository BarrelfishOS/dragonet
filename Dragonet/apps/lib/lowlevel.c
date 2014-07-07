
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <sched.h>

#include <pipelines.h>
#include <app_control.h>
#include <implementation.h>

#include <dragonet/app_lowlevel.h>


struct dnal_app_queue {
    pipeline_handle_t pipeline_handle;
    struct state *state;
    queue_handle_t *in_queue;
    queue_handle_t *out_queue;
    size_t num_inq;
    size_t num_outq;
    size_t nextpoll;
    struct dnal_socket_handle *socks;
    int control_fd;
};

struct dnal_socket_handle {
    struct dnal_app_queue *aq;
    uint64_t id;
    int32_t  mux_id;
    uint8_t  outqueue;
    bool bound;
    bool ready;
    struct dnal_net_destination dest;
    void *opaque;

    struct dnal_socket_handle *next;
};


static void control_send(struct dnal_app_queue *aq,
                         struct app_control_message *msg)
{
    if (send(aq->control_fd, msg, sizeof(*msg), 0) != sizeof(*msg)) {
        perror("control_send: Incomplete send");
        abort();
    }
}

static void control_recv(struct dnal_app_queue *aq,
                         struct app_control_message *msg)
{
    if (recv(aq->control_fd, msg, sizeof(*msg), MSG_WAITALL) != sizeof(*msg)) {
        perror("control_recv: Incomplete recv");
        abort();
    }
}

struct dnal_socket_handle *socket_by_id(struct dnal_app_queue *aq, uint64_t id)
{
    struct dnal_socket_handle *sh = aq->socks;
    while (sh != NULL) {
        if (id == sh->id) {
            return sh;
        }
        sh = sh->next;
    }
    return NULL;
}



errval_t dnal_aq_create(const char  *stackname,
                        const char  *slotname,
                        dnal_appq_t *appqueue)
{
    struct dnal_app_queue *aq;
    int s, i, num_in, num_out;
    struct sockaddr_un addr;
    struct app_control_message msg;

    if (strlen(slotname) >= MAX_APPLBL) {
        fprintf(stderr, "App label too long\n");
        abort();
    }

    // Establish Control connection
    if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("stack_init: creating socket failed");
        abort();
    }

    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    snprintf(addr.sun_path, sizeof(addr.sun_path)-1, "/tmp/%s_apps", stackname);

    if (connect(s, (struct sockaddr *) &addr, sizeof(addr)) == -1) {
        perror("stack_init: connecting socket failed");
        abort();
    }

    aq = calloc(1, sizeof(*aq));
    aq->control_fd = s;

    // Send register message
    msg.type = APPCTRL_REGISTER;
    strcpy(msg.data.register_app.label, slotname);
    control_send(aq, &msg);

    // Get welcome message
    control_recv(aq, &msg);
    assert(msg.type == APPCTRL_WELCOME);
    uint64_t appid_copy = msg.data.welcome.id;

    printf("App ID=%"PRId64"\n", msg.data.welcome.id);
    num_in = msg.data.welcome.num_inq;
    num_out = msg.data.welcome.num_outq;
    aq->in_queue = calloc(num_in, sizeof(*aq->in_queue));
    aq->out_queue = calloc(num_out, sizeof(*aq->out_queue));
    aq->num_inq = num_in;
    aq->num_outq = num_out;
    aq->nextpoll = 0;

    // Initialize pipeline interface
    aq->pipeline_handle = pl_init(stackname, slotname);
    aq->state = pl_get_state(aq->pipeline_handle);

    // Creating in queues
    for (i = 0; i < num_in; i++) {
        control_recv(aq, &msg);
        assert(msg.type == APPCTRL_INQUEUE);
        printf("  Creating in-queue: %s\n", msg.data.queue.label);
        aq->in_queue[i] =
                pl_inqueue_create(aq->pipeline_handle, msg.data.queue.label);
    }

    // Binding to outqueues
    for (i = 0; i < num_out; i++) {
        control_recv(aq, &msg);
        assert(msg.type == APPCTRL_OUTQUEUE);
        printf("  Binding out-queue: %s\n", msg.data.queue.label);
        aq->out_queue[i] =
                pl_outqueue_bind(aq->pipeline_handle, msg.data.queue.label);
    }

    // Make sure everything is ready
    pl_wait_ready(aq->pipeline_handle);
    while (aq->state->tap_handler == NULL) {
        sched_yield();
    }

    char fname[250];
    snprintf(fname, sizeof(fname), "APP%"PRIu64"%s", appid_copy, APP_READY_FNAME);

    // FIXME: create a file with appid_copy  and "appReady"
    declare_dragonet_initialized(fname, "App ready!\n");
    printf("Data path ready\n");
    *appqueue = aq;
    return SYS_ERR_OK;
}

errval_t dnal_aq_poll(dnal_appq_t           aq,
                      struct dnal_aq_event *event)
{
    struct input *in;
    struct dnal_socket_handle *sh;
    size_t i;

//    dprint("debug:%s:%s:%d:checking queus %d\n",
//            __FILE__, __FUNCTION__, __LINE__, (int)aq->num_outq);
    // First process buffers that are returned to us
    for (i = 0; i < aq->num_outq; i++) {
        bool ret = pl_process_event(aq->out_queue[i]);
        if (ret) {
            // FIXME: The return values of this function don't make sense
//            dprint("debug:%s:%s:%d:aborting event %d\n",
//            __FILE__, __FUNCTION__, __LINE__, (int)i);
//            return DNERR_EVENT_ABORT;
        }
    }

//    dprint("debug:%s:%s:%d: checking for packet start %d\n",
//            __FILE__, __FUNCTION__, __LINE__, (int)aq->nextpoll);
    // Now check for packets
    i = aq->nextpoll;
    do {
        in = pl_poll(aq->in_queue[i]);
        i = (i + 1) % aq->num_inq;
    } while (in == NULL && i != aq->nextpoll);
    aq->nextpoll = i;

//    dprint("debug:%s:%s:%d: checking for packet done %d\n",
//            __FILE__, __FUNCTION__, __LINE__, (int)i);
    if (in == NULL) {
//    dprint("debug:%s:%s:%d: null \n", __FILE__, __FUNCTION__, __LINE__);
        return DNERR_NOEVENT;
    }

    if ((sh = socket_by_id(aq, in->attr->socket_id)) == NULL) {
        dprint("debug:%s:%s:%d: no spcket\n", __FILE__, __FUNCTION__, __LINE__);
        fprintf(stderr, "Warning: dropping packet for non-existent socket\n");
        return DNERR_EVENT_ABORT;
    }

    event->type = DNAL_AQET_INPACKET;
    event->data.inpacket.socket = sh;
    event->data.inpacket.buffer = in;

    dprint("debug:%s:%s:%d: done\n", __FILE__, __FUNCTION__, __LINE__);
    return SYS_ERR_OK;
}

static errval_t aq_send_packet(dnal_appq_t   aq,
                               struct input *buf)
{
    return SYS_ERR_OK;
}

errval_t dnal_aq_buffer_alloc(dnal_appq_t    aq,
                              struct input **buffer)
{
    *buffer = input_alloc_plh(aq->pipeline_handle);
    if (buffer == NULL) {
        return DNERR_UNKNOWN;
    }
    //assert(buffer != NULL);
    return SYS_ERR_OK;
}

errval_t dnal_aq_buffer_free(dnal_appq_t   aq,
                             struct input *buffer)
{
    input_free_plh(aq->pipeline_handle, buffer);
    return SYS_ERR_OK;
}

struct state *dnal_aq_state(dnal_appq_t aq)
{
    return aq->state;
}

errval_t dnal_socket_create(dnal_appq_t   aq,
                            dnal_sockh_t *sockethandle)
{
    struct dnal_socket_handle *sh;

    sh = malloc(sizeof(*sh));
    assert(sh != NULL);

    sh->aq = aq;
    sh->id = -1ULL;
    sh->ready = sh->bound = false;
    sh->opaque = NULL;

    *sockethandle = sh;
    return SYS_ERR_OK;
}

static errval_t sockh_from_sockinfo(struct dnal_socket_handle   *sh,
                                    struct app_control_message  *msg,
                                    struct dnal_net_destination *dest)
{
    struct dnal_app_queue *aq = sh->aq;
    if (msg->type == APPCTRL_SOCKET_INFO) {
        sh->bound = true;
        sh->dest = *dest;
        sh->id = msg->data.socket_info.id;
        sh->mux_id = msg->data.socket_info.mux_id;
        sh->outqueue = msg->data.socket_info.outq;
        sh->next = aq->socks;
        aq->socks = sh;
    } else {
        assert(msg->type == APPCTRL_STATUS);
        // Something went wrong
        return DNERR_UNKNOWN;
    }
    return SYS_ERR_OK;
}

errval_t dnal_socket_bind(dnal_sockh_t                 sh,
                          struct dnal_net_destination *dest)
{
    struct app_control_message msg;
    struct dnal_app_queue *aq = sh->aq;

    if (sh->bound) {
        return DNERR_SOCKETBOUND;
    }

    if (dest->type == DNAL_NETDSTT_IP4UDP) {
        if (dest->data.ip4udp.ip_remote == 0 &&
            dest->data.ip4udp.port_remote == 0)
        {
            msg.type = APPCTRL_SOCKET_UDPLISTEN;
            msg.data.socket_udplisten.ip = dest->data.ip4udp.ip_local;
            msg.data.socket_udplisten.port = dest->data.ip4udp.port_local;
        } else {
            msg.type = APPCTRL_SOCKET_UDPFLOW;
            msg.data.socket_udpflow.s_ip = dest->data.ip4udp.ip_remote;
            msg.data.socket_udpflow.d_ip = dest->data.ip4udp.ip_local;
            msg.data.socket_udpflow.s_port = dest->data.ip4udp.port_remote;
            msg.data.socket_udpflow.d_port = dest->data.ip4udp.port_local;

            printf("This is flow binding\n");
                    printf("before bind: lIP: %"PRIu32", lPort: %"PRIu32", rIP: %"PRIu32", rPort: %"PRIu32",\n",
                           msg.data.socket_udpflow.d_ip,
                           msg.data.socket_udpflow.d_port,
                           msg.data.socket_udpflow.s_ip,
                           msg.data.socket_udpflow.s_port
                          );


        }
    } else {
        return DNERR_BADDEST;
    }

    control_send(aq, &msg);
    control_recv(aq, &msg);
    return sockh_from_sockinfo(sh, &msg, dest);
}

errval_t dnal_socket_span(dnal_sockh_t sh,
                          dnal_appq_t  naq,
                          dnal_sockh_t nsh)
{
    struct app_control_message msg;

    assert(sh->bound);
    assert(!nsh->bound);

    msg.type = APPCTRL_SOCKET_SPAN;
    msg.data.socket_span.id = sh->id;
    control_send(naq, &msg);
    control_recv(naq, &msg);
    return sockh_from_sockinfo(nsh, &msg, &sh->dest);
}

errval_t dnal_socket_send(dnal_sockh_t                 sh,
                          struct input                *buf,
                          struct dnal_net_destination *dest)
{
    uint16_t src_port, dst_port;
    uint32_t src_ip, dst_ip;
    int ret;

    if (!sh->bound) {
        return DNERR_SOCKETNOTBOUND;
    }

    // First off, get information from socket destination
    src_ip = sh->dest.data.ip4udp.ip_local;
    dst_ip = sh->dest.data.ip4udp.ip_remote;
    src_port = sh->dest.data.ip4udp.port_local;
    dst_port = sh->dest.data.ip4udp.port_remote;

    // Fill in from dest parameter
    if (dest->data.ip4udp.ip_local) {
        if (src_ip && src_ip != dest->data.ip4udp.ip_local) {
            return DNERR_BADDEST;
        }
        src_ip = dest->data.ip4udp.ip_local;
    }
    if (dest->data.ip4udp.ip_remote) {
        if (dst_ip && dst_ip != dest->data.ip4udp.ip_remote) {
            return DNERR_BADDEST;
        }
        dst_ip = dest->data.ip4udp.ip_remote;
    }
    if (dest->data.ip4udp.port_local) {
        if (src_port && src_port != dest->data.ip4udp.port_local) {
            return DNERR_BADDEST;
        }
        src_port = dest->data.ip4udp.port_local;
    }
    if (dest->data.ip4udp.port_remote) {
        if (dst_port && dst_port != dest->data.ip4udp.port_remote) {
            return DNERR_BADDEST;
        }
        dst_port = dest->data.ip4udp.port_remote;
    }


    // Check for incomplete attributes
    if (!src_port || !dst_port || !src_ip || !dst_ip) {
        return DNERR_BADDEST;
    }

    buf->attr->ip4_src   = src_ip;
    buf->attr->ip4_dst   = dst_ip;
    buf->attr->udp_sport = src_port;
    buf->attr->udp_dport = dst_port;

    // Send out packet
    input_set_muxid(buf, sh->mux_id);
    ret = pl_enqueue(sh->aq->out_queue[sh->outqueue], buf);
    if (ret < 0) {
        printf("\n\n%s:%s:%d:Warning:TX: send_packet failed as pl_enqueue failed, %d\n\n",
               __FILE__, __FUNCTION__, __LINE__, ret);
        return DNERR_UNKNOWN;
    }

    // Weird, but necessary since pl_enqueue replaces the buffer in buf with a
    // new one
    return dnal_aq_buffer_free(sh->aq, buf);
}

/**
 * Reads out the per-socket opaque value saved previously, or NULL if not
 * initialized.
 */
void *dnal_socket_opaque_get(dnal_sockh_t sockethandle)
{
    return sockethandle->opaque;
}

/**
 * Set the per-socket opaque value.
 */
void dnal_socket_opaque_set(dnal_sockh_t sockethandle,
                            void        *opaque)
{
    sockethandle->opaque = opaque;
}


