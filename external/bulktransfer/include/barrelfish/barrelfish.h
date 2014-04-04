#ifndef BARRELFISH_BARRELFISH_H
#define BARRELFISH_BARRELFISH_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define BULK_LINUX 1
#define STATIC_ASSERT(COND,MSG) typedef char static_assertion_##MSG[(COND)?1:-1]

typedef uintptr_t lvaddr_t;
typedef uintptr_t lpaddr_t;

/* errvals */
enum errvals {
    SYS_ERR_OK,
    SYS_ERR_NYI,

    WS_NO_EVENT,

    SHM_CHAN_NOSPACE,
    SHM_CHAN_NOMSG,

    BULK_TRANSFER_MEM,
    BULK_TRANSFER_NO_CALLBACK,
    BULK_TRANSFER_CHAN_CREATE,
    BULK_TRANSFER_CHAN_BIND,
    BULK_TRANSFER_CHAN_ASSIGN_POOL,
    BULK_TRANSFER_CHAN_STATE,
    BULK_TRANSFER_CHAN_TRUST,
    BULK_TRANSFER_CHAN_INVALID_EP,
    BULK_TRANSFER_CHAN_DIRECTION,
    BULK_TRANSFER_CHAN_ROLE,
    BULK_TRANSFER_POOL_INVALD,
    BULK_TRANSFER_POOL_NOT_ASSIGNED,
    BULK_TRANSFER_POOL_ASSIGN_VETO,
    BULK_TRANSFER_POOL_MAP,
    BULK_TRANSFER_POOL_UNMAP,
    BULK_TRANSFER_POOL_ALREADY_ASSIGNED,
    BULK_TRANSFER_POOL_ALREADY_REMAPPED,
    BULK_TRANSFER_BUFFER_NOT_OWNED,
    BULK_TRANSFER_BUFFER_INVALID,
    BULK_TRANSFER_BUFFER_ALREADY_MAPPED,
    BULK_TRANSFER_BUFFER_STATE,
    BULK_TRANSFER_BUFFER_REFCOUNT,
    BULK_TRANSFER_BUFFER_NOT_A_COPY,
    BULK_TRANSFER_BUFFER_MAP,
    BULK_TRANSFER_BUFFER_UNMAP,
    BULK_TRANSFER_ALLOC_FULL,
    BULK_TRANSFER_ALLOC_BUFFER_SIZE,
    BULK_TRANSFER_ALLOC_BUFFER_COUNT,
    BULK_TRANSFER_INVALID_ARGUMENT,
    BULK_TRANSFER_SM_NO_PENDING_MSG,
    BULK_TRANSFER_SM_EXCLUSIVE_WS,
    BULK_TRANSFER_NET_MAX_QUEUES,
    BULK_TRANSFER_NET_POOL_USED,
};

typedef enum errvals errval_t;

static inline bool err_is_ok(errval_t err)
{
    return err == SYS_ERR_OK;
}

static inline bool err_is_fail(errval_t err)
{
    return err != SYS_ERR_OK;
}

#endif
