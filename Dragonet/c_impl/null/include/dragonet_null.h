/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef DRAGONET_NULL_H_
#define DRAGONET_NULL_H_

#include <assert.h>

//#include <null.h>
//#include <null_queue.h>
//#include <linux_pci_uio.h>

#define QUEUES 4
#define QUEUE_INDEX(q) ((q) - (q)->null->queues)

struct dragonet_null;
struct dragonet_null_queue {
    struct dragonet_null *null;
    bool populated;
    bool chained;
//    null_queue_t *queue;
};

struct dragonet_null {
//    struct usp_pci_desc dev;
//    struct null_card card;
    struct dragonet_null_queue queues[QUEUES];
};


#endif // ndef DRAGONET_NULL_H_

