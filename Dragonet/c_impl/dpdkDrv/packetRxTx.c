#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <signal.h>
#include <unistd.h>
#include <pthread.h>
#include <dpdk_backend.h>

#define MAX_QUEUES                     128

#define CONFIG_LOCAL_MAC  0x07459f671e00ULL // 00:1e:67:9f:45:07 // asiago - e10k
#define CONFIG_LOCAL_IP  0x0a71045f //   "10.113.4.95"

//#define CONFIG_LOCAL_MAC  0xc01a8f211b00ULL // 00:1B:21:8F:1A:C0 -- sbrinz1
//#define CONFIG_LOCAL_IP  0x0a71041a //   "10.113.4.26"  -- sbrinz1


static uint64_t qstat[MAX_QUEUES] = {0, 0};
static uint64_t qstat_tx[MAX_QUEUES] = {0, 0};

// number of queues that device will configure.
// TODO: This number should be provided by the stack-dpdk commandline arg
//      and should not be hardcoded here.
static int queues_to_use = 10;

static node_out_t rx_queue(struct ctx_E10kRxQueue0 *context,
    struct state *state, struct input **in, uint8_t qi)
{
    node_out_t out_decision = P_E10kRxQueue0_drop;
    assert(qi < MAX_QUEUES);
    struct dragonet_dpdk_queue *q;

    struct dragonet_dpdk *e10k = (struct dragonet_dpdk *) state->st_driver_handle;
    size_t len = 0;
    int last;

    // Respawn this node
    // FIXME: shouldn't  value S_E10kRxQueue0_poll should depend which queue-id?
    spawn(context, NULL, S_E10kRxQueue0_poll, SPAWNPRIO_LOW);

    if (e10k == NULL) {
        if (qi != 0) {
            // We'll do the intialization on queue 0
            return P_E10kRxQueue0_drop;
        }
        printf(" ############## Initializing the driver ############## \n");

        // DPDK specific init
        state->st_driver_handle = (void *)init_dpdk_wrapper_deleteme(IFNAME,
                queues_to_use);

        if (state->st_driver_handle == NULL) {
            printf("ERROR: Initializing dpdk e10k device failed\n");
            abort();
        }
        e10k = (void *) state->st_driver_handle;

        // clear up the stats array
        memset(qstat, 0, sizeof(qstat));
        memset(qstat_tx, 0, sizeof(qstat_tx));
        // set the local IP and mac
        state->local_mac = CONFIG_LOCAL_MAC;
        state->local_ip = CONFIG_LOCAL_IP;

        *in = input_alloc();  // I am not sure if this is needed,
                // but keeping there as there is similar line in e10k init code

        declare_dragonet_initialized(DN_READY_FNAME, "e10k driver started!\n");
        printf(" ############## Initialization done ############## \n");
    }
    q = e10k->queues + qi;

    // get handle on queue, and make sure that it is correct
    assert(e10k != NULL);
    q = e10k->queues + qi;
    // FIXME: currently the queue is NULL as we are supporting only one queue
    //          This should get fixed when we have multiple queues
    //assert(q->queue != NULL);

    // FIXME: make sure that queue is initialized and populated

    if (q->pkt_holder == NULL) {
        // allocate buffer to copy the packet
        q->pkt_holder = input_alloc();
        if ((q->pkt_holder) == NULL) {
            printf("%s:%s:%d: sf driver is running out of buffers\n"
                    " soon hardware will start dropping packets\n",
                    __FILE__, __FUNCTION__, __LINE__);

            // NOTE: If there is no buffer than we don't consume or drop the packet
            // we just leave there, hoping that next call might be lucky
            // and get buffer.  If not, hardware will eventually start dropping
            // packet.
            return 0;
        }
        // start working on RX space
        pkt_prepend((q->pkt_holder), (q->pkt_holder)->space_before);
    }

    //dprint("Waiting for incoming packets\n");
    size_t copylen = get_packet_nonblock((void *)e10k, q->qid, 0,
            q->qid,
            (q->pkt_holder)->data,
            (q->pkt_holder)->len);

    if (copylen == 0) {
        // There are no new packets...
        return P_SFRxQueue0_drop;
    }


    dprint("packet RX %zu\n", copylen);
    pkt_append(q->pkt_holder, -((q->pkt_holder)->len - copylen));

    // passing the buffer to in, and marking pkt_holder so that
    //      next RX will allocate buffer
    *in =  q->pkt_holder;
    q->pkt_holder = NULL;

    // We received new packet
    ++q->rx_pkts;
    dprint
    //printf
        ("%s:%s:%d: [QID:%"PRIu8"], [pktid:%d], dragonet_nic = %p, "
            "sf_if = %p, (vq0 [%p, %p], [vq-%"PRIu8": %p, %p], "
            "######## received RX packet\n",
            __FILE__,  __func__, __LINE__, qi, q->rx_pkts,
            state->st_driver_handle, e10k->e10kif,
            &e10k->queues[0], e10k->queues[0].queue,
            qi, q, q->queue);

#if SHOW_INTERVAL_STATS

    //printf("dpdk: Yay, we got a full packet!\n");
    if (qstat[qi] % INTERVAL_STAT_FREQUENCY == 0) {
        printf
            //dprint
            ("QueueID:%"PRIu8":[TID:%d]: RX:%-10"PRIu64", TX:%-10"PRIu64", RX_event\n",
             qi, (int)pthread_self(), qstat[qi], qstat_tx[qi]);

        //show_pkt_stats(&debug_pkt_stats);
        //print_stats_dpdk((void *)e10k, 0);
    }
#endif // SHOW_INTERVAL_STATS

    ++qstat[qi];
    ++debug_pkt_stats.rx_eth;

    out_decision = P_E10kRxQueue0_out;
    return out_decision;

} // end function: rx_queue


static node_out_t tx_queue(struct state *state, struct input **in, uint8_t qi)
{

    struct dragonet_dpdk *e10k;
    struct dragonet_dpdk_queue *q;
    void *op;
    struct input *qin = *in;

    // wait while you get proper handler.
    // FIXME: how do I make sure that this code is thread-safe?
    do {
        e10k = (struct dragonet_dpdk *) state->st_driver_handle;
    } while (e10k == NULL);
    q = e10k->queues + qi;
    // wait till queue is allocated
    while (q->qstate == 0);

    dprint("[QID:%"PRIu8"], [pktid:%d], dragonet_nic = %p, "
            "e10k_if = %p, (vq0 [%p, qstate %d], [vq-%"PRIu8": %p, qstate %d], "
            "######## Trying to send packet, data: %p, len:%"PRIu32"\n",
            qi, q->tx_pkts, state->st_driver_handle, e10k->e10kif,
            &e10k->queues[0], e10k->queues[0].qstate,
            qi, q, q->qstate, (*in)->data, (*in)->len);

    // Actually sending packet using DPDK
    send_packetV2((void *)e10k, qi, 0, qi, (*in)->data, (size_t)(*in)->len);

#if SHOW_INTERVAL_STATS

    //printf("dpdk: Yay, we got a full packet!\n");
    if (qstat_tx[qi] % INTERVAL_STAT_FREQUENCY == 0) {
        printf
//        dprint
            ("QueueID:%"PRIu8":[TID:%d]: RX:%-10"PRIu64", TX:%-10"PRIu64", TX_event\n",
               qi, (int)pthread_self(), qstat[qi], qstat_tx[qi]);

    //show_pkt_stats(&debug_pkt_stats);
    //print_stats_dpdk((void *)e10k, 0);
   }
#endif // SHOW_INTERVAL_STATS

    ++q->tx_pkts;
    ++qstat_tx[qi];
    ++debug_pkt_stats.tx_eth;

    dprint("[QID:%"PRIu8"], [pktid:%d]:"
            "##############  packet sent, data: %p, len:%"PRIu32"\n",
            qi, q->tx_pkts, (*in)->data, (*in)->len);

    return P_RxQueue_out;
}

node_out_t do_pg__E10kRxQueue0(struct ctx_E10kRxQueue0 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 0);
}

node_out_t do_pg__E10kRxQueue1(struct ctx_E10kRxQueue1 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 1);
}

node_out_t do_pg__E10kRxQueue2(struct ctx_E10kRxQueue2 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 2);
}

node_out_t do_pg__E10kRxQueue3(struct ctx_E10kRxQueue3 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 3);
}


node_out_t do_pg__E10kRxQueue4(struct ctx_E10kRxQueue4 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 4);
}

node_out_t do_pg__E10kRxQueue5(struct ctx_E10kRxQueue5 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 5);
}

node_out_t do_pg__E10kRxQueue6(struct ctx_E10kRxQueue6 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 6);
}

node_out_t do_pg__E10kRxQueue7(struct ctx_E10kRxQueue7 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 7);
}


node_out_t do_pg__E10kRxQueue8(struct ctx_E10kRxQueue8 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 8);
}


node_out_t do_pg__E10kRxQueue9(struct ctx_E10kRxQueue9 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 9);
}


node_out_t do_pg__E10kRxQueue10(struct ctx_E10kRxQueue10 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_E10kRxQueue0 *) context, state, in, 10);
}






node_out_t do_pg__E10kTxQueue0(struct ctx_E10kTxQueue0 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 0);
}


node_out_t do_pg__E10kTxQueue1(struct ctx_E10kTxQueue1 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 1);
}

node_out_t do_pg__E10kTxQueue2(struct ctx_E10kTxQueue2 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 2);
}

node_out_t do_pg__E10kTxQueue3(struct ctx_E10kTxQueue3 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 3);
}

node_out_t do_pg__E10kTxQueue4(struct ctx_E10kTxQueue4 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 4);
}

node_out_t do_pg__E10kTxQueue5(struct ctx_E10kTxQueue5 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 5);
}

node_out_t do_pg__E10kTxQueue6(struct ctx_E10kTxQueue6 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 6);
}

node_out_t do_pg__E10kTxQueue7(struct ctx_E10kTxQueue7 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 7);
}

node_out_t do_pg__E10kTxQueue8(struct ctx_E10kTxQueue8 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 8);
}

node_out_t do_pg__E10kTxQueue9(struct ctx_E10kTxQueue9 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 9);
}

node_out_t do_pg__E10kTxQueue10(struct ctx_E10kTxQueue10 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 10);
}



