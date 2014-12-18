#ifndef BARRELFISH_BARRELFISH_H
#define BARRELFISH_BARRELFISH_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define BULK_LINUX 1
#define STATIC_ASSERT(COND,MSG) typedef char static_assertion_##MSG[(COND)?1:-1]

#define assert_fix assert

typedef uintptr_t lvaddr_t;
typedef uintptr_t lpaddr_t;

/* errvals */
enum errvals {
    SYS_ERR_OK,
    SYS_ERR_NYI,

    WS_NO_EVENT,

    SHM_CHAN_NOSPACE,
    SHM_CHAN_NOMSG,
    SHM_CHAN_NOTCREATED,
    SHM_CHAN_NOTREADY,

    BULK_TRANSFER_MEM,
    BULK_TRANSFER_NO_CALLBACK,
    BULK_TRANSFER_CHAN_CREATE,
    BULK_TRANSFER_CHAN_BIND,
    BULK_TRANSFER_CHAN_NOTCREATED,
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
    BULK_TRANSFER_ASYNC,
    BULK_TRANSFER_NOEVENT,
    BULK_TRANSFER_EVENTABORT,

    DNERR_EVENT_ABORT,
    DNERR_NOEVENT,
    DNERR_SOCKETBOUND,
    DNERR_SOCKETNOTBOUND,
    DNERR_BADDEST,
    DNERR_UNKNOWN,
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

static const char *err_str(errval_t err)
{
    switch (err) {
        case SYS_ERR_OK:
            return "SYS_ERR_OK";
        case SYS_ERR_NYI:
            return "SYS_ERR_NYI";

        case WS_NO_EVENT:
            return "WS_NO_EVENT";

        case SHM_CHAN_NOSPACE:
            return "SHM_CHAN_NOSPACE";
        case SHM_CHAN_NOMSG:
            return "SHM_CHAN_NOMSG";

        case BULK_TRANSFER_MEM:
            return "BULK_TRANSFER_MEM";
        case BULK_TRANSFER_NO_CALLBACK:
            return "BULK_TRANSFER_NO_CALLBACK";
        case BULK_TRANSFER_CHAN_CREATE:
            return "BULK_TRANSFER_CHAN_CREATE";
        case BULK_TRANSFER_CHAN_BIND:
            return "BULK_TRANSFER_NO_CALLBACK";
        case BULK_TRANSFER_CHAN_ASSIGN_POOL:
            return "BULK_TRANSFER_CHAN_ASSIGN_POOL";
        case BULK_TRANSFER_CHAN_STATE:
            return "BULK_TRANSFER_CHAN_STATE";
        case BULK_TRANSFER_CHAN_TRUST:
            return "BULK_TRANSFER_CHAN_TRUST";
        case BULK_TRANSFER_CHAN_INVALID_EP:
            return "BULK_TRANSFER_CHAN_INVALID_EP";
        case BULK_TRANSFER_CHAN_DIRECTION:
            return "BULK_TRANSFER_CHAN_DIRECTION";
        case BULK_TRANSFER_CHAN_ROLE:
            return "BULK_TRANSFER_CHAN_ROLE";
        case BULK_TRANSFER_POOL_INVALD:
            return "BULK_TRANSFER_POOL_INVALD";
        case BULK_TRANSFER_POOL_NOT_ASSIGNED:
            return "BULK_TRANSFER_POOL_NOT_ASSIGNED";
        case BULK_TRANSFER_POOL_ASSIGN_VETO:
            return "BULK_TRANSFER_POOL_ASSIGN_VETO";
        case BULK_TRANSFER_POOL_MAP:
            return "BULK_TRANSFER_POOL_MAP";
        case BULK_TRANSFER_POOL_UNMAP:
            return "BULK_TRANSFER_POOL_UNMAP";
        case BULK_TRANSFER_POOL_ALREADY_ASSIGNED:
            return "BULK_TRANSFER_POOL_ALREADY_ASSIGNED";
        case BULK_TRANSFER_POOL_ALREADY_REMAPPED:
            return "BULK_TRANSFER_POOL_ALREADY_REMAPPED";
        case BULK_TRANSFER_BUFFER_NOT_OWNED:
            return "BULK_TRANSFER_BUFFER_NOT_OWNED";
        case BULK_TRANSFER_BUFFER_INVALID:
            return "BULK_TRANSFER_BUFFER_INVALID";
        case BULK_TRANSFER_BUFFER_ALREADY_MAPPED:
            return "BULK_TRANSFER_BUFFER_ALREADY_MAPPED";
        case BULK_TRANSFER_BUFFER_STATE:
            return "BULK_TRANSFER_BUFFER_STATE";
        case BULK_TRANSFER_BUFFER_REFCOUNT:
            return "BULK_TRANSFER_BUFFER_REFCOUNT";
        case BULK_TRANSFER_BUFFER_NOT_A_COPY:
            return "BULK_TRANSFER_BUFFER_NOT_A_COPY";
        case BULK_TRANSFER_BUFFER_MAP:
            return "BULK_TRANSFER_BUFFER_MAP";
        case BULK_TRANSFER_BUFFER_UNMAP:
            return "BULK_TRANSFER_BUFFER_UNMAP";
        case BULK_TRANSFER_ALLOC_FULL:
            return "BULK_TRANSFER_ALLOC_FULL";
        case BULK_TRANSFER_ALLOC_BUFFER_SIZE:
            return "BULK_TRANSFER_ALLOC_BUFFER_SIZE";
        case BULK_TRANSFER_ALLOC_BUFFER_COUNT:
            return "BULK_TRANSFER_ALLOC_BUFFER_COUNT";
        case BULK_TRANSFER_INVALID_ARGUMENT:
            return "BULK_TRANSFER_INVALID_ARGUMENT";
        case BULK_TRANSFER_SM_NO_PENDING_MSG:
            return "BULK_TRANSFER_SM_NO_PENDING_MSG";
        case BULK_TRANSFER_SM_EXCLUSIVE_WS:
            return "BULK_TRANSFER_SM_EXCLUSIVE_WS";
        case BULK_TRANSFER_NET_MAX_QUEUES:
            return "BULK_TRANSFER_NET_MAX_QUEUES";
        case BULK_TRANSFER_NET_POOL_USED:
            return "BULK_TRANSFER_NET_POOL_USED";
        case BULK_TRANSFER_ASYNC:
            return "BULK_TRANSFER_ASYNC";
        case BULK_TRANSFER_NOEVENT:
            return "BULK_TRANSFER_NOEVENT";
        case BULK_TRANSFER_EVENTABORT:
            return "BULK_TRANSFER_EVENTABORT";
        case DNERR_EVENT_ABORT:
            return "DNERR_EVENT_ABORT";
        case DNERR_NOEVENT:
            return "DNERR_NOEVENT";
        case DNERR_SOCKETBOUND:
            return "DNERR_SOCKETBOUND";
        case DNERR_SOCKETNOTBOUND:
            return "DNERR_SOCKETNOTBOUND";
        case DNERR_BADDEST:
            return "DNERR_BADDEST";
        case DNERR_UNKNOWN:
            return "DNERR_UNKNOWN";
        default:
            return "(unknown error value)";
    }
}

#define err_expect_ok(err) err_expect_ok_(err, __LINE__, __FILE__, __func__)
static inline void err_expect_ok_(errval_t err, int line, const char *file,
        const char *func)
{
    if (!err_is_ok(err)) {
        fprintf(stderr, "err_expect_ok: Unexpected failure '%s' at %s:%d "
                "(function %s)\n", err_str(err), file, line, func);
        abort();
    }
}

#endif
