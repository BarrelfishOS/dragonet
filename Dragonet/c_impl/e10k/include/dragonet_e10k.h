#ifndef DRAGONET_E10K_H_
#define DRAGONET_E10K_H_

#include <assert.h>

#include <e10k.h>
#include <e10k_queue.h>
#include <linux_pci_uio.h>

#define QUEUES 10
#define QUEUE_INDEX(q) ((q) - (q)->e10k->queues)

struct dragonet_e10k;
struct dragonet_e10k_queue {
    struct dragonet_e10k *e10k;
    bool populated;
    bool chained;
    e10k_queue_t *queue;
};

struct dragonet_e10k {
    struct usp_pci_desc dev;
    struct e10k_card card;
    struct dragonet_e10k_queue queues[QUEUES];
};


#endif // ndef DRAGONET_E10K_H_

