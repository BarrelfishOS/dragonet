#include <stdio.h>
#include <pipelines.h>
#include <implementation.h>
#include <udpproto.h>

pipeline_handle_t pipeline_handle = NULL;
static struct state *state = NULL;
static queue_handle_t in_queue = NULL;
static queue_handle_t out_queue = NULL;

static void stack_init(const char *name, const char *inq, const char *outq)
{
    pipeline_handle = pl_init("dragonet", name);
    state = pl_get_state(pipeline_handle);

    in_queue = pl_inqueue_create(pipeline_handle, inq);
    out_queue = pl_outqueue_bind(pipeline_handle, outq);

    pl_wait_ready(pipeline_handle);

    printf("Application ready\n");
}

static struct input *stack_get_packet(void)
{
    struct input *in;
    do {
        in = pl_poll(pipeline_handle);
    } while (in == NULL);
    return in;
}

static void stack_send_packet(struct input *in)
{
    input_set_muxid(in, 1); // Hardcoded value to get to the UDPInitiateResponse
                            // node. Not sure yet how to fix this
    pl_enqueue(out_queue, in);
    input_free(in); // Weird, but necessary since pl_enqueue replaces the buffer
                    // in in with a new one
}

int main(int argc, char *argv[])
{
    printf("Hello World!\n");
    struct input *in;
    stack_init("AppEcho", "Rx_to_AppEcho", "AppEcho_to_Tx");
    while (1) {
        in = stack_get_packet();
        printf("got packet! len=%d  l5off=%d\n", in->len, in->attr->offset_l5);

        portno_t sport = udp_hdr_sport_read(in);
        portno_t dport = udp_hdr_dport_read(in);
        in->attr->udp_sport = dport;
        in->attr->udp_dport = sport;

        stack_send_packet(in);
    }
    return 0;
}
