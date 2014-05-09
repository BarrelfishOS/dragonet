/*
 * Copyright (c) 2007-2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E10K_H_
#define E10K_H_

#include <stdbool.h>
#include <stddef.h>

#include <dev/e10k_dev.h>

#define E10K_PCI_VENDOR 0x8086
#define E10K_PCI_DEVID  0x10FB
#define E10K_PCI_UIODRV "igb_uio"

#define E10K_NQUEUES    128
#define E10K_N5TFILTERS 128


/******************************************************************************/
/* 5-Tuple filters */

enum e10k_5tfilter_l4type {
    L4_OTHER = 0,
    L4_UDP   = 1,
    L4_TCP   = 2,
    L4_SCTP  = 3
};

enum e10k_5tfilter_mask {
    MASK_L4PROTO    = (1 << 0),
    MASK_SRCIP      = (1 << 1),
    MASK_DSTIP      = (1 << 2),
    MASK_SRCPORT    = (1 << 3),
    MASK_DSTPORT    = (1 << 4),
};

/** 5-tuple Filter */
struct e10k_5tfilter {
    bool enabled;
    uint8_t priority;
    uint8_t queue;

    uint32_t src_ip;
    uint32_t dst_ip;
    uint16_t src_port;
    uint16_t dst_port;

    uint16_t mask;
    uint16_t l4_type;
};

/******************************************************************************/
/* HW Queues */

struct e10k_queue_state {
    bool enabled;

    uint64_t tx_ring_phys;
    uint64_t tx_ring_size;
    uint64_t tx_hwb_phys;

    uint64_t rx_ring_phys;
    uint64_t rx_ring_size;
    uint32_t rxbufsz;

    /*size_t msix_index;
    int16_t msix_intvec;
    uint8_t msix_intdest;*/
    bool use_irq;
    bool use_rsc;

    uint64_t rx_head;
    uint64_t tx_head;

    void *opaque;
};

/** Parameters used for a queue */
struct e10k_queue_params {
    uint64_t tx_ring_phys;
    uint64_t tx_ring_size;
    uint64_t tx_hwb_phys;

    uint64_t rx_ring_phys;
    uint64_t rx_ring_size;
    uint32_t rxbufsz;

    /*int16_t msix_intvec;
    uint8_t msix_intdest;*/

    bool use_irq;
    bool use_rsc;
    void *opaque;
};


/******************************************************************************/

/** Struct keeping all state for a particular card */
struct e10k_card {
    /** Mackerel device */
    e10k_t d;

    /** MAC address of this card */
    uint64_t macaddr;

    /** Is the card initialized? */
    bool initialized;
    /** RX/TX operation enabled? */
    bool rxtx_enabled;

    /** Hardware queue state */
    struct e10k_queue_state queues[E10K_NQUEUES];

    /** State of 5-tuple filters */
    struct e10k_5tfilter ftfilters[E10K_N5TFILTERS];

    /** Is the card actively running? */
    bool running;

    void *opaque;
};



bool e10k_card_init(struct e10k_card *c);
bool e10k_card_release(struct e10k_card *c);
bool e10k_card_reset(struct e10k_card *c);
bool e10k_card_queue_init(struct e10k_card *c, uint8_t n,
                          struct e10k_queue_params *params);
bool e10k_card_queue_stop(struct e10k_card *c, uint8_t n);
bool e10k_5tfilter_setup(struct e10k_card *c, uint8_t index,
                         struct e10k_5tfilter *filter);


// Callbacks that need to be provided by the driver interface
void e10k_qcb_write_queue_tails(struct e10k_card *c,
                                struct e10k_queue_state *q);

// Internal
void e10k_phy_init(e10k_t* d);

#endif // ndef E10K_H_

