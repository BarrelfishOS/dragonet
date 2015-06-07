/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef BULK_LINUXSHM_H
#define BULK_LINUXSHM_H

#include <bulk_transfer/bulk_transfer.h>

struct bulk_linuxshm_endpoint_descriptor {
    struct bulk_endpoint_descriptor generic;

    const char *name;
    size_t num_slots;
};

errval_t bulk_linuxshm_ep_init(struct bulk_linuxshm_endpoint_descriptor *ep,
                               const char *name,
                               size_t      num_slots);

#endif

