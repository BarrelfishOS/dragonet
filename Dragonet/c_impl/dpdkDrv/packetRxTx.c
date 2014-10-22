#if 0
#include <implementation.h>

// I need PCI address and not the interface name
//#define IFNAME              "eth2"
#define IFNAME              "eth2"

#define CONFIG_LOCAL_MAC_sf    0x64188f211b00ULL  //    "00:1b:21:8f:18:64"
#define CONFIG_LOCAL_IP_sf   0x0a160425          //   "10.22.4.37"
        // NOTE: Can be generated in python with ``print "%02x%02x%02x%02x" % (10,113,4,71)``


#define IFNAME              "eth3"
#define CONFIG_LOCAL_MAC_sf  0x06459F671E00ULL   //  "00:1E:67:9F:45:06 "
#define CONFIG_LOCAL_IP_sf   0x0a16040b //   "10.22.4.11"

struct dpdk_info;
static
uint64_t dpdk_mac_read(device_t ttd) {
    return (CONFIG_LOCAL_MAC_sf);
}

static
uint32_t dpdk_ip_read(device_t ttd) {
    return (CONFIG_LOCAL_IP_sf);
}

static
void *init_dpdk_wrapper(char *arg)
{
    return ((void *) init_dpdk_setup_and_get_default_queue(IFNAME));
}

static
pktoff_t dpdk_rx_wrapper(device_t dev, uint8_t *data, pktoff_t len)
{
    struct dpdk_info *dpdk = (struct dpdk_info *)dev;
    return ((pktoff_t)get_packet_wrapper(dpdk, (char *)data, len));
}

static
int dpdk_tx_wrapper(device_t dev, uint8_t *data, pktoff_t len)
{
    struct dpdk_info *dpdk = (struct dpdk_info *)dev;
    return (send_packet_wrapper(dpdk, (char *)data, len));
}

static struct driver dpdk_driver = {
    .drv_handle = NULL,

    .drv_init = init_dpdk_wrapper,
    .drv_rx = dpdk_rx_wrapper,
    .drv_tx = dpdk_tx_wrapper,

    .drv_mac_read = dpdk_mac_read,
    .drv_ip_read = dpdk_ip_read,
};

static
struct driver *get_dpdk_driver(void)
{
    return &dpdk_driver;
}

int main(int argc, char *argv[])
{
    struct driver *drv = NULL;
    drv = get_dpdk_driver();
    return main_loop(drv);
}
#endif // 0

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <signal.h>
#include <unistd.h>
#include <pthread.h>
#include <packet_access.h>
#include <pipelines.h>
#include <dpdk_backend.h>

#define MAX_QUEUES                     128

#define CONFIG_LOCAL_MAC  0ULL //
#define CONFIG_LOCAL_IP  0x0a71045f //   "10.113.4.95"

static uint64_t qstat[MAX_QUEUES] = {0, 0};
static void *drv = NULL;

static node_out_t rx_queue(struct ctx_E10kRxQueue0 *context,
    struct state *state, struct input **in, uint8_t qi)
{
    node_out_t out_decision = P_E10kRxQueue0_drop;
    assert(qi < MAX_QUEUES);
    struct dragonet_dpdk_queue *q;

    struct dragonet_dpdk *e10k = (struct dragonet_dpdk *) state->tap_handler;
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

        // DPDK specific init
        state->tap_handler = (void *)init_dpdk_wrapper_deleteme(IFNAME);

        if (state->tap_handler == NULL) {
            printf("ERROR: Initializing dpdk e10k device failed\n");
            abort();
        }
        e10k = (void *) state->tap_handler;

        // clear up the stats array
        memset(qstat, 0, sizeof(qstat));
        // set the local IP and mac
        state->local_mac = CONFIG_LOCAL_MAC;
        state->local_ip = CONFIG_LOCAL_IP;

        *in = input_alloc();  // I am not sure if this is needed,
                // but keeping there as there is similar line in e10k init code

        declare_dragonet_initialized(DN_READY_FNAME, "e10k driver started!\n");
        printf("Initialized\n");
    }
    q = e10k->queues + qi;

    // get handle on queue, and make sure that it is correct
    assert(e10k != NULL);
    q = e10k->queues + qi;
    // FIXME: currently the queue is NULL as we are supporting only one queue
    //          This should get fixed when we have multiple queues
    //assert(q->queue != NULL);

    // FIXME: make sure that queue is initialized and populated

    // FIXME: code to get a packet
    //ssize_t len = dpdk_rx_wrapper(q, in);

    if (len == 0) {
        // There are no new packets...
        return P_SFRxQueue0_drop;
    }
    // FIXME: follwoing steps should be done where packet is received
    //          so, it should be in dpdk_rx_wrapper
/*    qin->qid = qi;
    // Set packet boundaries
    pkt_append(qin, -(qin->len - len));
    *in = qin;
*/

    // We received new packet
    ++q->rx_pkts;
    dprint
    //printf
        ("%s:%s:%d: [QID:%"PRIu8"], [pktid:%d], dragonet_nic = %p, "
            "sf_if = %p, (vq0 [%p, %p], [vq-%"PRIu8": %p, %p], "
            "######## received RX packet\n",
            __FILE__,  __func__, __LINE__, qi, q->rx_pkts,
            state->tap_handler, e10k->e10kif,
            &e10k->queues[0], e10k->queues[0].queue,
            qi, q, q->queue);

#if SHOW_INTERVAL_STATS

    //printf("dpdk: Yay, we got a full packet!\n");
    if (qstat[qi] % INTERVAL_STAT_FREQUENCY == 0) {
        printf
//        dprint
            ("QueueID:%"PRIu8":[TID:%d]: has handled %"PRIu64" packets, size:%zu\n",
               qi, (int)pthread_self(), qstat[qi], len);
   }
#endif // SHOW_INTERVAL_STATS
    ++qstat[qi];


    out_decision = P_E10kRxQueue0_out;

    // FIXME: Add new buffer

    return out_decision;

} // end function: rx_queue


static node_out_t tx_queue(struct state *state, struct input **in, uint8_t qi)
{

    assert(!"NYI");
#if 0
    struct dragonet_e10k *e10k;
    struct dragonet_e10k_queue *q;
    void *op;
    struct input *qin = *in;

    // wait while you get proper handler.
    // FIXME: how do I make sure that this code is thread-safe?
    do {
        e10k = (struct dragonet_e10k *) state->tap_handler;
    } while (e10k == NULL);
    q = e10k->queues + qi;
    while (!q->populated);

    // FIXME: Check if there are processed buffers on the TX queue
    while (e10k_queue_get_txbuf(q->queue, &op) == 0) {
        qin = op;
        input_free(qin);
    }

    *in = NULL;
#endif // 0
    return 0;
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



