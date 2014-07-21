#include <implementation.h>
#include <packet_access.h>
#include <inttypes.h>

static int rx_pkts = 0;
static int tx_pkts = 0;

static void tap_init(struct state *state)
{
    if (state->tap_handler != NULL) {
        printf("TAP Already intialized\n");
        return;
    }
    state->tap_handler = tap_create("dragonet0");
    tap_set_ip(state->tap_handler, "192.168.123.100");
    tap_set_mask(state->tap_handler, "255.255.255.0");
    tap_up(state->tap_handler);
}

node_out_t do_pg__TapRxQueue(struct ctx_TapRxQueue *context,
        struct state *state, struct input **in)
{
    int p_id = ++rx_pkts;
    pktoff_t maxlen;

    // Respawn this node
    spawn(context, NULL, S_TapRxQueue_poll, SPAWNPRIO_LOW);

    if (state->tap_handler == NULL) {
        tap_init(state);

        state->local_mac = 0xf86954221b00ULL;
        state->local_ip = 0xc0a87b01;
        declare_dragonet_initialized(DN_READY_FNAME, "tap driver started!\n");
        printf("Initialized\n");
        dprint("%s:%s:%d: [pktid:%d]: ############## initialized queue\n",
              __FILE__,  __func__, __LINE__, p_id);
        *in = input_alloc();
        return P_RxQueue_init;
    }


    dprint("%s:%s:%d: [pktid:%d]: ############## Trying to receive packet\n",
           __FILE__,  __func__, __LINE__, p_id);

    *in = input_alloc();
    pkt_prepend(*in, (*in)->space_before);
    ssize_t len = tap_read(state->tap_handler, (char *) (*in)->data, (*in)->len, 500);
    if (len == 0) {
        dprint("%s:%d: [pktid:%d]: pkt with zero len\n", __func__, __LINE__, p_id);
        return P_RxQueue_drop;
    }
    pkt_append(*in, -((*in)->len - len));

    dprint("%s:%d: [pktid:%d]: ############## pkt received, data: %p, len:%zu\n",
            __func__, __LINE__, p_id, (*in)->data, len);
    return P_RxQueue_out;
}

node_out_t do_pg__TapTxQueue(struct ctx_TapTxQueue *context,
        struct state *state, struct input **in)
{
    int p_id = ++tx_pkts;
    dprint("%s:%s:%d: [pktid:%d]: ############## Trying to send packet, data: %p, len:%"PRIu32"\n",
            __FILE__, __func__, __LINE__, p_id, (*in)->data, (*in)->len);
    tap_write(state->tap_handler, (*in)->data, (*in)->len);
    dprint("%s:%s:%d: [pktid:%d]: ##############  packet sent\n",
            __FILE__, __func__, __LINE__, p_id);
    return 0;
}


