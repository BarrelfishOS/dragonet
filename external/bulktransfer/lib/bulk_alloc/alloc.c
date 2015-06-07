/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdlib.h>
#include <bulk_transfer/bulk_allocator.h>

struct bulk_buffer_mng {
    struct bulk_buffer *buffer;
    struct bulk_buffer_mng *next;
};


errval_t bulk_alloc_init_empty(struct bulk_allocator *alloc,
                               size_t                 capacity)
{
    alloc->mngs = calloc(capacity, sizeof(*alloc->mngs));
    if (alloc->mngs == NULL) {
        return BULK_TRANSFER_MEM;
    }

    alloc->capacity = capacity;
    alloc->num_free = 0;
    alloc->free_buffers = NULL;

    return SYS_ERR_OK;
}


errval_t bulk_alloc_init_pool(struct bulk_allocator *alloc,
                              struct bulk_pool      *pool)
{
    errval_t err;
    size_t i;

    err = bulk_alloc_init_empty(alloc, pool->num_buffers);
    if (err_is_fail(err)) {
        return err;
    }

    for (i = 0; i < pool->num_buffers; i++) {
        err = bulk_alloc_return_buffer(alloc, pool->buffers[i]);
        err_expect_ok(err);
    }

    return SYS_ERR_OK;
}

errval_t bulk_alloc_free(struct bulk_allocator *alloc)
{
    free(alloc->mngs);
    return SYS_ERR_OK;
}

struct bulk_buffer *bulk_alloc_new_buffer(struct bulk_allocator *alloc)
{
    struct bulk_buffer_mng *mng;

    if (alloc->num_free == 0) {
        return NULL;
    }

    mng = alloc->free_buffers;
    alloc->free_buffers = mng->next;
    alloc->num_free--;

    return mng->buffer;
}

size_t bulk_alloc_free_buffer_count(struct bulk_allocator *alloc)
{
    return alloc->num_free;
}

errval_t bulk_alloc_return_buffer(struct bulk_allocator *alloc,
                                  struct bulk_buffer    *buffer)
{
    struct bulk_buffer_mng *mng;

    if (alloc->num_free == alloc->capacity) {
        return BULK_TRANSFER_ALLOC_FULL;
    }

    mng = alloc->mngs + alloc->num_free;
    mng->buffer = buffer;
    mng->next = alloc->free_buffers;
    alloc->free_buffers = mng;
    alloc->num_free++;

    return SYS_ERR_OK;
}

