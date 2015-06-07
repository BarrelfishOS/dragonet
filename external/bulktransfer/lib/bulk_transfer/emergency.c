/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <sys/mman.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_helpers.h>
#include <sys/ipc.h>
#include <sys/shm.h>


extern struct bulk_ll_channel *bulk_channels;
extern struct bulk_pool *bulk_pools;

extern void bulk_linuxshm_emergency_cleanup(struct bulk_ll_channel *chan);
extern void bulk_pool_emergency_cleanup(struct bulk_pool *pool);

/** Cleans up all shared memory resources that were created by this
 *  applicaiton */
void bulk_emergency_cleanup(void)
{
    struct bulk_pool *pool = bulk_pools;
    struct bulk_ll_channel *chan = bulk_channels;
    while (chan != NULL) {
        bulk_linuxshm_emergency_cleanup(chan);
        chan = chan->internal.next;
    }

    while (pool != NULL) {
        bulk_pool_emergency_cleanup(pool);
        pool = pool->internal.next;
    }
}


