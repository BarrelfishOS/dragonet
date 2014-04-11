#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <signal.h>
#include <unistd.h>

#include <linux_pci_uio.h>
#include <linux_hugepage.h>

#include <e10k.h>
#include <e10k_queue.h>

#include <implementation.h>
#include <packet_access.h>
#include <pipelines.h>

#define NUM_RXDESCS 512
#define NUM_TXDESCS 512
#define E10K_BUFSZ 2048
#define NUM_RXBUFS 256

//#define CONFIG_PCI_ADDR "0000:81:00.0"
//#define CONFIG_LOCAL_IP 0x0a160427 // "10.22.4.39"
//  For asigao, directly connected NIC
#define CONFIG_PCI_ADDR "0000:04:00.0"
#define CONFIG_LOCAL_IP  0x0a16040b //   "10.22.4.11"


struct mem_region_alloc {
    void    *virt;
    uint64_t phys;
    size_t   left;
};

struct dragonet_e10k {
    struct usp_pci_desc dev;
    struct e10k_card card;
    bool chained;
    e10k_queue_t *queue;
};

extern pipeline_handle_t pipeline_handle;

/** Checks that this is a supported PCI device and maps the device regions */
static bool e10k_pci_init(struct e10k_card *c, struct usp_pci_desc *pci)
{
    struct usp_uio_desc *uio;
    if (pci->vendor != E10K_PCI_VENDOR || pci->device != E10K_PCI_DEVID) {
        fprintf(stderr, "e10k_pci_init: unsupported PCI vendor or device id\n");
        return false;
    }

    if (pci->num_uio == 0) {
        fprintf(stderr, "e10k_pci_init: no UIO device found\n");
        return false;
    }

    if (strcmp(pci->driver_name, E10K_PCI_UIODRV)) {
        fprintf(stderr, "e10k_pci_init: unsupported driver name (%s)\n",
                pci->driver_name);
        return false;
    }

    uio = pci->uio;
    if (uio->num_mappings == 0) {
        fprintf(stderr, "e10k_pci_init: no UIO mapping found\n");
        return false;
    }

    if (!usp_uio_map(uio)) {
        fprintf(stderr, "e10k_pci_init: mapping UIO regions failed\n");
        return false;
    }

    e10k_initialize(&c->d, uio->mappings[0].mapped);
    return true;
}



void e10k_qcb_write_queue_tails(struct e10k_card *c,
                                struct e10k_queue_state *q)
{
    struct dragonet_e10k *i = q->opaque;
    e10k_queue_bump_txtail(i->queue);
    e10k_queue_bump_rxtail(i->queue);
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
    struct dragonet_e10k *i = opaque;
    e10k_tdt_wr(&i->card.d, 0, index);
}

static void queue_update_rxtail(void *opaque, size_t index)
{
    struct dragonet_e10k *i = opaque;
    size_t queue = 0;
    if (queue < 64) {
        e10k_rdt_1_wr(&i->card.d, queue, index);
    } else {
        e10k_rdt_2_wr(&i->card.d, queue, index);
    }
}


static e10k_queue_t *queue_prepare(struct mem_region_alloc *alloc,
                                   uint64_t *rx_phys, uint64_t *tx_phys,
                                   uint64_t *txhwb_phys, void *opaque)
{
    void *rx_ring, *tx_ring, *tx_hwb;
    static struct e10k_queue_ops ops = {
        .update_txtail = queue_update_txtail,
        .update_rxtail = queue_update_rxtail,
    };



    if (!mem_region_alloc(alloc, NUM_TXDESCS * e10k_q_tdesc_adv_wb_size,
                &tx_ring, tx_phys))
    {
        fprintf(stderr, "queue_prepare: Failed to allocate TX ring\n");
        return NULL;
    }

    if (!mem_region_alloc(alloc, NUM_RXDESCS * e10k_q_rdesc_adv_wb_size,
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

    return e10k_queue_init(tx_ring, NUM_TXDESCS, tx_hwb,
                           rx_ring, NUM_RXDESCS, &ops, opaque);
}

static void cleanup_handler(pipeline_handle_t plh, void *op)
{
    struct dragonet_e10k *i = op;

    printf("cleanup_handler\n");
    if (i->card.running) {
        e10k_card_release(&i->card);
    }
}

static void queue_add_rxbuf(e10k_queue_t *q, struct input *in)
{
    pkt_append(in, in->space_after);
    pkt_prepend(in, in->space_before);
    e10k_queue_add_rxbuf(q, in->phys, in);
}

static bool e10k_if_init(struct state *state, const char *pciaddr)
{
    struct dragonet_e10k *i = calloc(1, sizeof(*i));
    struct mem_region_alloc alloc;
    int res;

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

    // Prepare queue and allocate buffers, populate with receive buffers
    // We can do all of that here, to reduce the risk of failing after the card
    // is already running. The only thing we need to do below, is bumping the
    // RX tail pointer
    uint64_t rx_phys, tx_phys, txhwb_phys;
    i->queue = queue_prepare(&alloc, &rx_phys, &tx_phys, &txhwb_phys, i);
    if (i->queue == NULL) {
        fprintf(stderr, "Preparing queue failed\n");
        goto out_err;
    }

    int j;
    for (j = 0; j < NUM_RXBUFS; j++) {
        struct input *in = input_alloc();
        if (in == NULL) {
            fprintf(stderr, "Allocating receive buffer failed\n");
            goto out_err;
        }

        queue_add_rxbuf(i->queue, in);
    }

    // Start initializing card
    if (!e10k_pci_init(&i->card, &i->dev)) {
        fprintf(stderr, "e10k_pci_init failed\n");
        goto out_err;
    }

    pl_cleanup_handler(pipeline_handle, true, cleanup_handler, i);
    pl_cleanup_handler(pipeline_handle, false, cleanup_handler, i);

    if (!e10k_card_init(&i->card)) {
        fprintf(stderr, "e10k_card_init failed\n");
        goto out_err;
    }
    printf("MACADDR %"PRIx64"\n", i->card.macaddr);

    struct e10k_queue_params params = {
        .tx_ring_phys = tx_phys,
        .tx_ring_size = i->queue->tx_size * e10k_q_tdesc_adv_wb_size,
        .tx_hwb_phys = txhwb_phys,
        .rx_ring_phys = rx_phys,
        .rx_ring_size = i->queue->rx_size * e10k_q_rdesc_adv_wb_size,
        .rxbufsz = E10K_BUFSZ,
        .use_irq = false,
        .use_rsc = false,
        .opaque = i,
    };
    if (!e10k_card_queue_init(&i->card, 0, &params)) {
        fprintf(stderr, "Initializing queue 0 failed\n");
        return false;
    }

    e10k_queue_bump_rxtail(i->queue);

    state->tap_handler = (void *) i;
    return true;
out_err:
    free(i);
    return false;
}


node_out_t do_pg__TapRxQueue(struct state *state, struct input *in)
{
    struct dragonet_e10k *e10k = (struct dragonet_e10k *) state->tap_handler;
    void *op;
    size_t len;
    int last;
    uint64_t flags;
    struct input *qin;
    node_out_t port;

    if (e10k == NULL) {
        if (!e10k_if_init(state, CONFIG_PCI_ADDR)) {
            pl_panic(pipeline_handle, "Initializing e10k device failed\n");
        }
        e10k = (struct dragonet_e10k *) state->tap_handler;

        state->local_mac = e10k->card.macaddr;
        state->local_ip = CONFIG_LOCAL_IP;
        printf("Initialized\n");
    }

    if (e10k_queue_get_rxbuf(e10k->queue, &op, &len, &last, &flags) != 0) {
	//usleep(1000*1000);
        return P_Queue_drop;
    }
    qin = op;

    if (!last || e10k->chained) {
        if (!e10k->chained) {
            printf("e10k: Received chained buffer, we cannot currently deal "
                    "with this, dropping packet\n");
            e10k->chained = true;
        } else if (last) {
            e10k->chained = false;
        }
        port = P_Queue_drop;
        goto add_buf;
    }
    //printf("e10k: Yay, we got a full packet!\n");

    // Set packet boundaries
    pkt_append(qin, -(qin->len - len));

    // Add current in as replacement buffer
    input_xchg(qin, in);
    port = P_Queue_out;

add_buf:
    queue_add_rxbuf(e10k->queue, qin);
    e10k_queue_bump_rxtail(e10k->queue);
    return port;
}

node_out_t do_pg__TapTxQueue(struct state *state, struct input *in)
{
    struct dragonet_e10k *e10k = (struct dragonet_e10k *) state->tap_handler;
    void *op;
    struct input *qin;

    // Replacement input
    qin = input_alloc();
    input_xchg(qin, in);

    e10k_queue_add_txbuf(e10k->queue, qin->phys, qin->len, qin, 1, 1, qin->len);
    e10k_queue_bump_txtail(e10k->queue);

    /*printf("Sent packet! :-D\n");
    puts("---------------------------------------------------------\n\n\n");*/

    // Check if there are processed buffers on the TX queue
    while (e10k_queue_get_txbuf(e10k->queue, &op) == 0) {
        qin = op;
        input_free(qin);
    }
    return 0;
}




