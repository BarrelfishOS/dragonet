/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef BULK_HELPERS_H
#define BULK_HELPERS_H

#include <bulk_transfer/bulk_transfer.h>

struct pool_meta;

errval_t bulk_int_pool_map(struct bulk_pool      *p,
                           enum bulk_buffer_state state,
                           struct pool_meta *meta,
                           int datahandle);

struct bulk_pool *bulk_int_pool_byid(struct bulk_pool_id id);

#endif // ndef BULK_HELPERS_H
