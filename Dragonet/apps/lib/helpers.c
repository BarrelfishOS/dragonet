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

#include <pipelines.h>
#include <app_control.h>
#include <implementation.h>
#include <udpproto.h>
#include <proto_ipv4.h>
#include <helpers.h>

pipeline_handle_t pipeline_handle = NULL;
static struct state *state = NULL;
static queue_handle_t in_queue = NULL;
static queue_handle_t out_queue = NULL;
static struct socket_handle *socks = NULL;
static int control_fd = -1;

static void control_send(struct app_control_message *msg)
{
    if (send(control_fd, msg, sizeof(*msg), 0) != sizeof(*msg)) {
        perror("control_send: Incomplete send");
        abort();
    }
}

static void control_recv(struct app_control_message *msg)
{
    if (recv(control_fd, msg, sizeof(*msg), MSG_WAITALL) != sizeof(*msg)) {
        perror("control_recv: Incomplete recv");
        abort();
    }
}



void stack_init(const char *name, const char *inq, const char *outq)
{
    const char *stackname = "dragonet";
    pipeline_handle = pl_init(stackname, name);
    state = pl_get_state(pipeline_handle);

    in_queue = pl_inqueue_create(pipeline_handle, inq);
    out_queue = pl_outqueue_bind(pipeline_handle, outq);

    pl_wait_ready(pipeline_handle);
    printf("Data path ready\n");


    int s;
    struct sockaddr_un addr;
    struct app_control_message msg;

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
    control_fd = s;

    control_recv(&msg);
    assert(msg.type == APPCTRL_WELCOME);
    printf("App ID=%"PRId64"\n", msg.data.welcome.id);
}

struct state *stack_get_state(void)
{
    return state;
}

struct input *stack_get_packet(void)
{
    struct input *in;
    do {
        in = pl_poll(pipeline_handle);
    } while (in == NULL);
    return in;
}

void stack_send_udp_packet(struct input *in)
{
    input_set_muxid(in, 1); // Hardcoded value to get to the UDPInitiateResponse
                            // node. Not sure yet how to fix this
    pl_enqueue(out_queue, in);
    input_free(in); // Weird, but necessary since pl_enqueue replaces the buffer
                    // in in with a new one
}

void stack_process_event(void)
{
    struct input *in;
    uint64_t id;
    struct socket_handle *sh;

    in = stack_get_packet();
    id = in->attr->socket_id;

    sh = socks;
    while (sh != NULL) {
        if (id == sh->id) {
            sh->cb_receive(sh, in, sh->data);
            return;
        }
        sh = sh->next;
    }
    fprintf(stderr, "Warning: dropping packet for non-existent socket\n");
}


socket_handle_t socket_create(void (*receive)(
                                    socket_handle_t, struct input *, void *),
                              void *data)
{
    static uint64_t id = 0;
    struct socket_handle *sh = malloc(sizeof(*sh));

    sh->id = id++;
    sh->ready = sh->bound = false;
    sh->cb_receive = receive;
    sh->data = data;
    sh->next = socks;
    socks = sh;
    return sh;
}
// TODO
//bool socket_close(socket_handle_t handle);

bool socket_bind_udp_listen(socket_handle_t handle, uint32_t ip, uint16_t port)
{
    struct app_control_message msg;

    if (handle->bound) {
        return false;
    }

    msg.type = APPCTRL_SOCKET_UDPLISTEN;
    msg.data.socket_udplisten.id = handle->id;
    msg.data.socket_udplisten.ip = ip;
    msg.data.socket_udplisten.port = port;
    control_send(&msg);

    control_recv(&msg);
    assert(msg.type == APPCTRL_STATUS);
    if (msg.data.status.success) {
        handle->bound = true;
    }
    return handle->bound;
}

//bool socket_bind_udp_flow(socket_handle_t handle,
//                          uint32_t s_ip, uint16_t s_port,
//                          uint32_t d_ip, uint16_t d_port);

bool socket_send_udp(socket_handle_t handle, struct input *in,
                     uint32_t s_ip, uint16_t s_port,
                     uint32_t d_ip, uint16_t d_port)
{
    if (!handle->bound) {
        return false;
    }

    in->attr->udp_sport = s_port;
    in->attr->udp_dport = d_port;
    in->attr->ip4_src   = s_ip;
    in->attr->ip4_dst   = d_ip;

    stack_send_udp_packet(in);
    return true;
}

