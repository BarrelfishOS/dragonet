#include <stdio.h>
#include <pipelines.h>
#include <implementation.h>

#include <udpproto.h>
#include <proto_ipv4.h>

pipeline_handle_t pipeline_handle = NULL;
static struct state *state = NULL;
static queue_handle_t in_queue = NULL;
static queue_handle_t out_queue = NULL;

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

