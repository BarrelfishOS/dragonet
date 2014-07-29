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
#include <errno.h>

#include <pipelines.h>
#include <app_control.h>
#include <implementation.h>

#include <dragonet/app_lowlevel.h>


struct dnal_app_queue {
    pipeline_handle_t pipeline_handle;
    struct state *state;
    size_t nextpoll;
    struct dnal_socket_handle *socks;
    int control_fd;
    struct dynr_server graphsrv;
};

struct dnal_socket_handle {
    struct dnal_app_queue *aq;
    uint64_t id;
    bool bound;
    bool ready;
    struct dnal_net_destination dest;
    struct dynamic_spawn *spawn;
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

static bool control_tryrecv(struct dnal_app_queue *aq,
                            struct app_control_message *msg)
{
    ssize_t res;
    res = recv(aq->control_fd, msg, sizeof(*msg), MSG_DONTWAIT);
    if (res == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        return false;
    }

    if (res == -1) {
        perror("control_tryrecv(1): recv failed");
        abort();
    }

    if (res >= sizeof(*msg)) {
        return true;
    }

    if (recv(aq->control_fd, ((uint8_t *) msg) + res, sizeof(*msg) - res,
                MSG_WAITALL) != sizeof(*msg) - res)
    {
        perror("control_tryrecv: Incomplete recv");
        abort();
    }
    return true;
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

static void *graph_socket_get(uint64_t socket_id, void *data)
{
    struct dnal_app_queue *aq = data;
    struct dnal_socket_handle *sh = socket_by_id(aq, socket_id);
    return sh;
}

static void graph_socket_set_spawn(void *sockdata,
                                   struct dynamic_spawn *spawn,
                                   void *data)
{
    struct dnal_socket_handle *sh = sockdata;
    sh->spawn = spawn;
}

static nodefun_t graph_fnode_resolve(const char *name, void *data)
{
    fprintf(stderr, "graph_fnode_resolve: Not implemented!\n");
    return NULL;
}


errval_t dnal_aq_create(const char  *stackname,
                        const char  *slotname,
                        dnal_appq_t *appqueue)
{
    struct dnal_app_queue *aq;
    int s;
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

    printf("App ID=%"PRId64"\n", msg.data.welcome.id);
    aq->nextpoll = 0;

    // Initialize pipeline interface
    aq->pipeline_handle = pl_init(stackname, slotname);
    aq->state = pl_get_state(aq->pipeline_handle);

    // Initialize graph
    dynrs_init(&aq->graphsrv, aq->pipeline_handle, graph_fnode_resolve, aq);
    aq->graphsrv.graph->socket_get = graph_socket_get;
    aq->graphsrv.graph->socket_set_spawn = graph_socket_set_spawn;
    aq->graphsrv.graph->socket_data = aq;

    printf("Data path ready\n");
    *appqueue = aq;
    return SYS_ERR_OK;
}

errval_t dnal_aq_poll(dnal_appq_t           aq,
                      struct dnal_aq_event *event)
{
    struct input *in;
    struct dnal_socket_handle *sh;
    struct app_control_message msg;
    struct dynamic_graph *graph;

    // Check control channel
    if (control_tryrecv(aq, &msg)) {
        // We should only get graph commands here
        assert(msg.type == APPCTRL_GRAPH_CMD);
        dynrs_action(&aq->graphsrv, &msg.data.graph_cmd.act);
        return DNERR_EVENT_ABORT;
    }

    // If the graph is currently stopped, we can't do anything but wait for the
    // next command
    if (aq->graphsrv.stopped) {
        return DNERR_EVENT_ABORT;
    }


    // Run graph for one iteration
    graph = aq->graphsrv.graph;
    graph->cur_socket = NULL;
    graph->cur_socket_buf = NULL;
    dynrs_run(&aq->graphsrv);

    in = graph->cur_socket_buf;
    sh = graph->cur_socket;
    if (in == NULL) {
        // TODO
        return DNERR_EVENT_ABORT;
    }

    assert(sh != NULL);
    event->type = DNAL_AQET_INPACKET;
    event->data.inpacket.socket = sh;
    event->data.inpacket.buffer = in;

    dprint("debug:%s:%s:%d: done\n", __FILE__, __FUNCTION__, __LINE__);
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
    // TODO: Should we do something to send this immediately
    assert(sh->spawn != NULL);
    dyn_spawn(sh->aq->graphsrv.graph, sh->spawn, buf, SPAWNPRIO_HIGH);
    return SYS_ERR_OK;
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


