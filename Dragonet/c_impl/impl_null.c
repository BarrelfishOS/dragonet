#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <signal.h>
#include <unistd.h>
#include <pthread.h>

#include <implementation.h>
#include <packet_access.h>
#include <pipelines.h>
#include <dragonet_null.h>

extern pipeline_handle_t pipeline_handle;

#if 0 // unreachable code
/** Checks that this is a supported PCI device and maps the device regions */
static bool null_pci_init(struct null_card *c, struct usp_pci_desc *pci)
{
    struct usp_uio_desc *uio;
    if (pci->vendor != NULLD_PCI_VENDOR || pci->device != NULLD_PCI_DEVID) {
        fprintf(stderr, "null_pci_init: unsupported PCI vendor or device id\n");
        return false;
    }

    if (pci->num_uio == 0) {
        fprintf(stderr, "null_pci_init: no UIO device found\n");
        return false;
    }

    if (strcmp(pci->driver_name, NULLD_PCI_UIODRV)) {
        fprintf(stderr, "null_pci_init: unsupported driver name (%s)\n",
                pci->driver_name);
        return false;
    }

    uio = pci->uio;
    if (uio->num_mappings == 0) {
        fprintf(stderr, "null_pci_init: no UIO mapping found\n");
        return false;
    }

    if (!usp_uio_map(uio)) {
        fprintf(stderr, "null_pci_init: mapping UIO regions failed\n");
        return false;
    }

    null_initialize(&c->d, uio->mappings[0].mapped);
    return true;
}



void null_qcb_write_queue_tails(struct null_card *c,
                                struct null_queue_state *q)
{
    struct dragonet_null_queue *i = q->opaque;
    null_queue_bump_txtail(i->queue);
    null_queue_bump_rxtail(i->queue);
}

/******************************************************************************/
/*  */


static bool mem_region_create(struct mem_region_alloc *alloc)
{
    return huge_alloc_phys(&alloc->virt, &alloc->phys, &alloc->left);
}

static bool mem_region_alloc(struct mem_region_alloc *alloc, size_t size,
                             void **virt, uint64_t *phys)
{
    if (alloc->left < size) {
        return false;
    }

    *virt = alloc->virt;
    *phys = alloc->phys;

    alloc->virt = (void *) ((uintptr_t) alloc->virt + size);
    alloc->phys += size;
    alloc->left -= size;
    return true;
}

static void queue_update_txtail(void *opaque, size_t index)
{
    struct dragonet_null_queue *i = opaque;
    null_tdt_wr(&i->null->card.d, QUEUE_INDEX(i), index);
}

static void queue_update_rxtail(void *opaque, size_t index)
{
    struct dragonet_null_queue *q = opaque;
    size_t queue = QUEUE_INDEX(q);
    if (queue < 64) {
        null_rdt_1_wr(&q->null->card.d, queue, index);
    } else {
        null_rdt_2_wr(&q->null->card.d, queue, index);
    }
}


static null_queue_t *queue_prepare(struct mem_region_alloc *alloc,
                                   uint64_t *rx_phys, uint64_t *tx_phys,
                                   uint64_t *txhwb_phys, void *opaque)
{
    void *rx_ring, *tx_ring, *tx_hwb;
    static struct null_queue_ops ops = {
        .update_txtail = queue_update_txtail,
        .update_rxtail = queue_update_rxtail,
    };



    if (!mem_region_alloc(alloc, NUM_TXDESCS * null_q_tdesc_adv_wb_size,
                &tx_ring, tx_phys))
    {
        fprintf(stderr, "queue_prepare: Failed to allocate TX ring\n");
        return NULL;
    }

    if (!mem_region_alloc(alloc, NUM_RXDESCS * null_q_rdesc_adv_wb_size,
                &rx_ring, rx_phys))
    {
        fprintf(stderr, "queue_prepare: Failed to allocate RX ring\n");
        return NULL;
    }

    // Way too much, but we want resonable alignment afterwards
    if (!mem_region_alloc(alloc, 4096, &tx_hwb, txhwb_phys))
    {
        fprintf(stderr, "queue_prepare: Failed to allocate TXHWB\n");
        return NULL;
    }

    return null_queue_init(tx_ring, NUM_TXDESCS, tx_hwb,
                           rx_ring, NUM_RXDESCS, &ops, opaque);
}

static void cleanup_handler(pipeline_handle_t plh, void *op)
{
    struct dragonet_null *i = op;

    printf("cleanup_handler\n");
    if (i->card.running) {
        null_card_release(&i->card);
    }
}

static void queue_add_rxbuf(null_queue_t *q, struct input *in)
{
    pkt_append(in, in->space_after);
    pkt_prepend(in, in->space_before);
    null_queue_add_rxbuf(q, in->phys, in);
}

static bool null_if_init(struct state *state, const char *pciaddr)
{
    struct dragonet_null *i = calloc(1, sizeof(*i));
    struct mem_region_alloc alloc;
    int res;
    size_t k;

    if (!usp_pci_parse(&i->dev, pciaddr)) {
        fprintf(stderr, "Parsing PCI information from /sys failed\n");
        goto out_err;
    }
    printf("PCI device found:\n");
    usp_pci_desc_dump(&i->dev);

    if (!mem_region_create(&alloc)) {
        fprintf(stderr, "Failed allocating mem region\n");
        goto out_err;
    }

    struct dragonet_null_queue *iq = i->queues;
    uint64_t rx_phys[QUEUES], tx_phys[QUEUES], txhwb_phys[QUEUES];
    for (k = 0; k < QUEUES; k++) {
        iq[k].null = i;
        // Prepare queue and allocate buffers, populate with receive buffers
        // We can do all of that here, to reduce the risk of failing after the card
        // is already running. The only thing we need to do below, is bumping the
        // RX tail pointer
        iq[k].queue = queue_prepare(&alloc, rx_phys + k, tx_phys + k,
                txhwb_phys + k, iq + k);
        if (iq[k].queue == NULL) {
            fprintf(stderr, "Preparing queue failed\n");
            goto out_err;
        }
    }


    // Start initializing card
    if (!null_pci_init(&i->card, &i->dev)) {
        fprintf(stderr, "null_pci_init failed\n");
        goto out_err;
    }

    pl_cleanup_handler(pipeline_handle, true, cleanup_handler, i);
    pl_cleanup_handler(pipeline_handle, false, cleanup_handler, i);

    if (!null_card_init(&i->card)) {
        fprintf(stderr, "null_card_init failed\n");
        goto out_err;
    }
    printf("MACADDR %"PRIx64"\n", i->card.macaddr);

    for (k = 0; k < QUEUES; k++) {
        struct null_queue_params params = {
            .tx_ring_phys = tx_phys[k],
            .tx_ring_size = iq[k].queue->tx_size * null_q_tdesc_adv_wb_size,
            .tx_hwb_phys = txhwb_phys[k],
            .rx_ring_phys = rx_phys[k],
            .rx_ring_size = iq[k].queue->rx_size * null_q_rdesc_adv_wb_size,
            .rxbufsz = NULLD_BUFSZ,
            .use_irq = false,
            .use_rsc = false,
            .opaque = iq,
        };
        if (!null_card_queue_init(&i->card, k, &params)) {
            fprintf(stderr, "Initializing queue 0 failed\n");
            return false;
        }
    }

    state->tap_handler = (void *) i;
    return true;
out_err:
    free(i);
    return false;
}
#endif // 0 // unreachable code

#define MAX_QUEUES                     128
static uint64_t qstat[MAX_QUEUES] = {0, 0};

static node_out_t rx_queue(struct ctx_NullRxQueue0 *context,
    struct state *state, struct input **in, uint8_t qi)
{
    assert(qi < MAX_QUEUES);
    assert(state != NULL);

    struct dragonet_null *null = (struct dragonet_null *) state->tap_handler;
    struct dragonet_null_queue *q;
    void *op;
    size_t len;
    int last;
    uint64_t flags;
    struct input *qin;
    node_out_t port;

    // Respawn this node
    // FIXME: shouldn't  value S_NullRxQueue0_poll should depend which queue-id?
    spawn(context, NULL, S_NullRxQueue0_poll, SPAWNPRIO_LOW);

    if (null == NULL) {
        if (qi != 0) {
            // We'll do the intialization on queue 0
            return P_NullRxQueue0_drop;
        }

#if 0 // no hardware initialization as this is NULL device
        if (!null_if_init(state, CONFIG_PCI_ADDR)) {
            pl_panic(pipeline_handle, "Initializing null device failed\n");
        }
#endif // 0

        // clear up the stats array
        memset(qstat, 0, sizeof(qstat));
        // set the local IP and mac
        //state->local_mac = null->card.macaddr;
        //state->local_ip = CONFIG_LOCAL_IP;
        declare_dragonet_initialized(DN_READY_FNAME, "null driver started!\n");
        printf("Initialized\n");
    }

    // infinite loop instead of receiving
    printf("%s:%s:%d:%d: infinite loop instead of receiving packets for queue %"PRIu8"\n",
            __FILE__, __FUNCTION__, __LINE__, (int)pthread_self(), qi);

    while(1) {
        sleep(1);
    }



    printf("%s:%s:%d: debug location\n", __FILE__, __FUNCTION__, __LINE__);
    q = null->queues + qi;
    printf("%s:%s:%d: debug location\n", __FILE__, __FUNCTION__, __LINE__);

    if (!q->populated) {
#if 0 // no hardware initialization as this is NULL device
        int j;
        for (j = 0; j < NUM_RXBUFS; j++) {
            struct input *i = input_alloc();
            if (i == NULL) {
                fprintf(stderr, "Allocating receive buffer %d failed\n", j);
                break;
            }

            queue_add_rxbuf(q->queue, i);
        }
        null_queue_bump_rxtail(q->queue);
#endif // 0

    printf("%s:%s:%d: debug location\n", __FILE__, __FUNCTION__, __LINE__);
        q->populated = true;
        if (qi == 0) {
            *in = input_alloc();
            return P_NullRxQueue0_init;
        }
    }
    printf("%s:%s:%d: debug location\n", __FILE__, __FUNCTION__, __LINE__);

//    if (null_queue_get_rxbuf(q->queue, &op, &len, &last, &flags) != 0) {
//        return P_NullRxQueue0_drop;
//    }
    // infinite loop instead of receiving
    printf("%s:%s:%d:%d: infinite loop instead of receiving packets for queue %"PRIu8"\n",
            __FILE__, __FUNCTION__, __LINE__, (int)pthread_self(), qi);

    while(1) {
        sleep(1);
    }

#if 0 // unreachable code
    // TODO: following code will never be executed.  I should get rid of it

    qin = op;

    if (!last || q->chained) {
        if (!q->chained) {
            printf("null: Received chained buffer, we cannot currently deal "
                    "with this, dropping packet\n");
            q->chained = true;
        } else if (last) {
            q->chained = false;
        }
        port = P_NullRxQueue0_drop;
        goto add_buf;
    }


#if SHOW_INTERVAL_STATS
    //printf("null: Yay, we got a full packet!\n");
    if (qstat[qi] % INTERVAL_STAT_FREQUENCY == 0) {
        printf
//        dprint
            ("QueueID:%"PRIu8":[TID:%d]: has handled %"PRIu64" packets, size:%zu\n",
               qi, (int)pthread_self(), qstat[qi], len);
   }
#endif // SHOW_INTERVAL_STATS
    ++qstat[qi];

    qin->qid = qi;
    // Set packet boundaries
    pkt_append(qin, -(qin->len - len));

    *in = qin;
    port = P_NullRxQueue0_out;

add_buf:
    qin = input_alloc();
    if (qin != NULL) {
        queue_add_rxbuf(q->queue, qin);
    }
    null_queue_bump_rxtail(q->queue);

#endif // 0 // unreachable code
    return port;
}


static node_out_t tx_queue(struct state *state, struct input **in, uint8_t qi)
{
    struct dragonet_null *null;
    struct dragonet_null_queue *q;
    void *op;
    struct input *qin = *in;

    do {
        null = (struct dragonet_null *) state->tap_handler;
    } while (null == NULL);
    q = null->queues + qi;
    while (!q->populated);

    printf("%s:%s:%d:%d: infinite loop instead of sending packets for queue %"PRIu8"\n",
            __FILE__, __FUNCTION__, __LINE__, (int)pthread_self(), qi);

    while(1) {
        sleep(1);
    }

    // TODO: following code will never be executed.  I should get rid of it
#if 0 // unreachable code
    null_queue_add_txbuf(q->queue, qin->phys, qin->len, qin, 1, 1, qin->len);
    null_queue_bump_txtail(q->queue);

    // Check if there are processed buffers on the TX queue
    while (null_queue_get_txbuf(q->queue, &op) == 0) {
        qin = op;
        input_free(qin);
    }

    *in = NULL;
#endif // 0  // unreachable code
    return 0;
}

node_out_t do_pg__NullRxQueue0(struct ctx_NullRxQueue0 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 0);
}

node_out_t do_pg__NullRxQueue1(struct ctx_NullRxQueue1 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 1);
}

node_out_t do_pg__NullRxQueue2(struct ctx_NullRxQueue2 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 2);
}

node_out_t do_pg__NullRxQueue3(struct ctx_NullRxQueue3 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 3);
}

node_out_t do_pg__NullRxQueue4(struct ctx_NullRxQueue4 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 4);
}

node_out_t do_pg__NullRxQueue5(struct ctx_NullRxQueue5 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 5);
}

node_out_t do_pg__NullRxQueue6(struct ctx_NullRxQueue6 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 6);
}

node_out_t do_pg__NullRxQueue7(struct ctx_NullRxQueue7 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 7);
}


node_out_t do_pg__NullRxQueue8(struct ctx_NullRxQueue8 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 8);
}


node_out_t do_pg__NullRxQueue9(struct ctx_NullRxQueue9 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 9);
}


node_out_t do_pg__NullRxQueue10(struct ctx_NullRxQueue10 *context,
        struct state *state, struct input **in)
{
    return rx_queue((struct ctx_NullRxQueue0 *) context, state, in, 10);
}




node_out_t do_pg__NullTxQueue0(struct ctx_NullTxQueue0 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 0);
}


node_out_t do_pg__NullTxQueue1(struct ctx_NullTxQueue1 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 1);
}

node_out_t do_pg__NullTxQueue2(struct ctx_NullTxQueue2 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 2);
}

node_out_t do_pg__NullTxQueue3(struct ctx_NullTxQueue3 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 3);
}

node_out_t do_pg__NullTxQueue4(struct ctx_NullTxQueue4 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 4);
}

node_out_t do_pg__NullTxQueue5(struct ctx_NullTxQueue5 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 5);
}

node_out_t do_pg__NullTxQueue6(struct ctx_NullTxQueue6 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 6);
}

node_out_t do_pg__NullTxQueue7(struct ctx_NullTxQueue7 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 7);
}

node_out_t do_pg__NullTxQueue8(struct ctx_NullTxQueue8 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 8);
}

node_out_t do_pg__NullTxQueue9(struct ctx_NullTxQueue9 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 9);
}

node_out_t do_pg__NullTxQueue10(struct ctx_NullTxQueue10 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 10);
}


