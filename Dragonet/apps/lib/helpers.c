
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

//<<<<<<< HEAD
// FIXME: Remove these conflict related comments once it is clear that we
// dont need them.
//#include <packet_access.h>
//=======
// FIXME: I should be able to get rid of these header files.
#include <udpproto.h>
#include <proto_ipv4.h>
#include <helpers.h>
//>>>>>>> merge-sockets

struct stack_handle {
    pipeline_handle_t pipeline_handle;
    struct state *state;
    queue_handle_t *in_queue;
    queue_handle_t *out_queue;
    struct socket_handle *socks;
    int control_fd;
};

static void control_send(struct stack_handle *sh,
                         struct app_control_message *msg)
{
    if (send(sh->control_fd, msg, sizeof(*msg), 0) != sizeof(*msg)) {
        perror("control_send: Incomplete send");
        abort();
    }
}

static void control_recv(struct stack_handle *sh,
                         struct app_control_message *msg)
{
    if (recv(sh->control_fd, msg, sizeof(*msg), MSG_WAITALL) != sizeof(*msg)) {
        perror("control_recv: Incomplete recv");
        abort();
    }
}

struct input *stack_input_alloc(struct stack_handle *sh)
{
    return input_alloc_plh(sh->pipeline_handle);
}

void stack_input_free(struct stack_handle *sh, struct input *in)
{
    input_free_plh(sh->pipeline_handle, in);
}

struct stack_handle *stack_init(const char *stackname, const char *name)
{
    int s, i, num_in, num_out;
    struct sockaddr_un addr;
    struct app_control_message msg;
    struct stack_handle *sh;

    if (strlen(name) >= MAX_APPLBL) {
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

    sh = calloc(1, sizeof(*sh));
    sh->control_fd = s;

    // Send register message
    msg.type = APPCTRL_REGISTER;
    strcpy(msg.data.register_app.label, name);
    control_send(sh, &msg);

    // Get welcome message
    control_recv(sh, &msg);
    assert(msg.type == APPCTRL_WELCOME);
    printf("App ID=%"PRId64"\n", msg.data.welcome.id);
    num_in = msg.data.welcome.num_inq;
    num_out = msg.data.welcome.num_outq;
    sh->in_queue = calloc(num_in, sizeof(*sh->in_queue));
    sh->out_queue = calloc(num_out, sizeof(*sh->out_queue));

    // Initialize pipeline interface
    sh->pipeline_handle = pl_init(stackname, name);
    sh->state = pl_get_state(sh->pipeline_handle);

    // Creating in queues
    for (i = 0; i < num_in; i++) {
        control_recv(sh, &msg);
        assert(msg.type == APPCTRL_INQUEUE);
        printf("  Creating in-queue: %s\n", msg.data.queue.label);
        sh->in_queue[i] =
                pl_inqueue_create(sh->pipeline_handle, msg.data.queue.label);
    }

    // Binding to outqueues
    for (i = 0; i < num_out; i++) {
        control_recv(sh, &msg);
        assert(msg.type == APPCTRL_OUTQUEUE);
        printf("  Binding out-queue: %s\n", msg.data.queue.label);
        sh->out_queue[i] =
                pl_outqueue_bind(sh->pipeline_handle, msg.data.queue.label);
    }

    // Make sure everything is ready
    pl_wait_ready(sh->pipeline_handle);
    while (sh->state->tap_handler == NULL) {
        sched_yield();
    }
    printf("Data path ready\n");

    return sh;
}

struct state *stack_get_state(struct stack_handle *sh)
{
    return sh->state;
}

static struct input *stack_get_packet(struct stack_handle *sh)
{
    struct input *in;
    do {
        in = pl_poll(sh->pipeline_handle);
    } while (in == NULL);
    return in;
}

static void stack_send_packet(socket_handle_t handle, struct input *in)
{
    input_set_muxid(in, handle->mux_id);
    pl_enqueue(handle->stack->out_queue[handle->outqueue], in);

    stack_input_free(handle->stack, in); // Weird, but necessary since
                                         // pl_enqueue replaces the buffer in
                                         // in with a new one
}

void stack_process_event(struct stack_handle *stack)
{
    struct input *in;
    uint64_t id;
    struct socket_handle *sh;

    in = stack_get_packet(stack);
    id = in->attr->socket_id;

    sh = stack->socks;
    while (sh != NULL) {
        if (id == sh->id) {
            sh->cb_receive(sh, in, sh->data);
            return;
        }
        sh = sh->next;
    }
    fprintf(stderr, "Warning: dropping packet for non-existent socket\n");
}


socket_handle_t socket_create(struct stack_handle *stack,
                              void (*receive)(
                                    socket_handle_t, struct input *, void *),
                              void *data)
{
    struct socket_handle *sh = malloc(sizeof(*sh));

    sh->stack = stack;
    sh->id = -1ULL;
    sh->ready = sh->bound = false;
    sh->cb_receive = receive;
    sh->data = data;
    sh->next = stack->socks;
    stack->socks = sh;
    return sh;
}
// TODO
//bool socket_close(socket_handle_t handle);

bool socket_bind_udp_listen(socket_handle_t handle, uint32_t ip, uint16_t port)
{
    struct app_control_message msg;
    struct stack_handle *sh = handle->stack;

    if (handle->bound) {
        return false;
    }

    msg.type = APPCTRL_SOCKET_UDPLISTEN;
    msg.data.socket_udplisten.ip = ip;
    msg.data.socket_udplisten.port = port;
    control_send(sh, &msg);

    control_recv(sh, &msg);
    if (msg.type == APPCTRL_SOCKET_INFO) {
        handle->bound = true;
        handle->id = msg.data.socket_info.id;
        handle->mux_id = msg.data.socket_info.mux_id;
        handle->outqueue = msg.data.socket_info.outq;
    } else {
        assert(msg.type == APPCTRL_STATUS);
        // Something went wrong
    }
    return handle->bound;
}

bool socket_bind_udp_flow(socket_handle_t handle,
                          uint32_t s_ip, uint16_t s_port,
                          uint32_t d_ip, uint16_t d_port)
{
    struct app_control_message msg;
    struct stack_handle *sh = handle->stack;

    if (handle->bound) {
        return false;
    }

    msg.type = APPCTRL_SOCKET_UDPFLOW;
    msg.data.socket_udpflow.s_ip = s_ip;
    msg.data.socket_udpflow.d_ip = d_ip;
    msg.data.socket_udpflow.s_port = s_port;
    msg.data.socket_udpflow.d_port = d_port;
    control_send(sh, &msg);

    control_recv(sh, &msg);
    if (msg.type == APPCTRL_SOCKET_INFO) {
        handle->bound = true;
        handle->id = msg.data.socket_info.id;
        handle->mux_id = msg.data.socket_info.mux_id;
        handle->outqueue = msg.data.socket_info.outq;
    } else {
        assert(msg.type == APPCTRL_STATUS);
        // Something went wrong
    }
    return handle->bound;
}

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

    stack_send_packet(handle, in);
    return true;
}


#if 0
//<<<<<<< HEAD
// FIXME: I should not use them
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
