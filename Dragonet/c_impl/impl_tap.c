/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdio.h>
#include <implementation.h>
#include <packet_access.h>
#include <inttypes.h>


static void tap_init(struct state *state)
{
    struct tap_handle *h;
    if (state->st_driver_handle != NULL) {
        printf("TAP Already intialized\n");
        return;
    }

    h = tap_create("dragonet0");
    tap_set_ip(h, "192.168.123.100");
    tap_set_mask(h, "255.255.255.0");
    tap_up(h);

    state->st_driver_handle = h;
}

node_out_t do_pg__TapRxQueue(struct ctx_TapRxQueue *context,
        struct state *state, struct input **in)
{
    static int rx_pkts = 0;
    int p_id = ++rx_pkts;
    pktoff_t maxlen;
    struct tap_handler *tap_handler = state->st_driver_handle;

    // Respawn this node
    spawn(context, NULL, S_TapRxQueue_poll, SPAWNPRIO_LOW);

    if (tap_handler == NULL) {
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
    ssize_t len = tap_read(tap_handler, (char *) (*in)->data, (*in)->len, 500);
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
    static int tx_pkts = 0;
    struct tap_handler *tap_handler = state->st_driver_handle;

    int p_id = ++tx_pkts;
    dprint("%s:%s:%d: [pktid:%d]: ############## Trying to send packet, data: %p, len:%"PRIu32"\n",
            __FILE__, __func__, __LINE__, p_id, (*in)->data, (*in)->len);
    tap_write(tap_handler, (*in)->data, (*in)->len);
    dprint("%s:%s:%d: [pktid:%d]: ##############  packet sent\n",
            __FILE__, __func__, __LINE__, p_id);
    return 0;
}


