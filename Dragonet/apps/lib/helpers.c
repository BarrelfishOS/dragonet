#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include <pipelines.h>
#include <implementation.h>
#include <packet_access.h>

#include <helpers.h>

//#include <udpproto.h>
//#include <proto_ipv4.h>

pipeline_handle_t pipeline_handle = NULL;
static struct state *state = NULL;
static queue_handle_t in_queue = NULL;
static queue_handle_t out_queue = NULL;

static event_handler_fun_ptr callback_memcached_fn = NULL;
static int callback_memcached_fd = 0;
static short callback_memcached_which = 0;
static void *callback_memcached_arg = NULL;


void stack_init(const char *name, const char *inq, const char *outq)
{
    pipeline_handle = pl_init("dragonet", name);
    state = pl_get_state(pipeline_handle);

    in_queue = pl_inqueue_create(pipeline_handle, inq);
    out_queue = pl_outqueue_bind(pipeline_handle, outq);

    pl_wait_ready(pipeline_handle);

    printf("Application ready\n");
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


