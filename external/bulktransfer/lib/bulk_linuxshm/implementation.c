#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_linuxshm.h>
#include <bulk_helpers.h>

#include "shm_channel.h"

//#define debug_printf printf
#define debug_printf(...) do {} while (0)

struct lsm_opstate {
    struct lsm_internal *internal;
    errval_t (*action) (struct lsm_opstate   *ops,
                        struct bulk_ll_event *event,
                        errval_t              err);
    union {
        struct {
            struct bulk_pool *pool;
        } assign;
    } state;
    bulk_correlation_t corr;
    uint8_t op;


    struct lsm_opstate *next;
};

struct lsm_internal {
    const char          *name;
    size_t               num_slots;
    struct bulk_ll_channel *chan;
    bool                 creator;
    struct lsm_opstate  *ops;
    struct lsm_opstate  *ops_free;

    struct shm_channel   rx;
    void                *rx_meta;
    struct shm_channel   tx;
    void                *tx_meta;

    bulk_correlation_t   bind_corr;
};


static struct lsm_opstate *ops_alloc(struct lsm_internal *internal)
{
    struct lsm_opstate *ops = internal->ops_free;
    if (ops != NULL) {
        internal->ops_free = ops->next;
    }
    return ops;
}

static void ops_free(struct lsm_opstate *ops)
{
    ops->next = ops->internal->ops_free;
    ops->internal->ops_free = ops;
}

static inline uint32_t ops_id(struct lsm_opstate *ops)
{
    return ops - ops->internal->ops;
}


/******************************************************************************/
/* Helpers for channel initialization */

/** Allocate and initialize internal channel state */
static errval_t internal_init(struct bulk_ll_channel *channel)
{
    struct lsm_internal *internal;
    struct bulk_linuxshm_endpoint_descriptor *ep =
        (struct bulk_linuxshm_endpoint_descriptor *) channel->ep;
    struct lsm_opstate *ops;
    size_t i;

    internal = calloc(1, sizeof(*internal));
    if (internal == NULL) {
        return BULK_TRANSFER_MEM;
    }
    internal->ops = calloc(ep->num_slots, sizeof(*internal->ops));
    if (internal->ops == NULL) {
        free(internal);
        return BULK_TRANSFER_MEM;
    }

    internal->chan = channel;
    internal->name = ep->name;
    internal->num_slots = ep->num_slots;
    channel->impl_data = internal;
    for (i = 0; i < ep->num_slots; i++) {
        ops = internal->ops + i;
        ops->internal = internal;

        ops->next = internal->ops_free;
        internal->ops_free = ops;
    }

    return SYS_ERR_OK;
}

/** Free memory for internal channel state */
static void internal_release(struct lsm_internal *internal)
{
    free(internal->ops);
    free(internal);
}


/** Get name for meta-data SHM region */
static void get_metas_name(struct lsm_internal *internal, bool tx, char *dest)
{
    strcpy(dest, internal->name);
    strcat(dest, (tx ^ internal->creator ? "_txm" : "_rxm"));
}

/** Map (and create) SHM meta data region */
static errval_t map_metas(struct lsm_internal *internal, bool create, bool tx)
{
    int fd, res;
    void *mapped;
    char name[strlen(internal->name) + 5];
    size_t size = internal->num_slots * internal->chan->meta_size;

    if (size == 0) {

    }
    get_metas_name(internal, tx, name);

    debug_printf("shm_open(%s)\n", name);
    if (create) {
        fd = shm_open(name, O_CREAT | O_RDWR | O_EXCL, 0600);
        assert_fix(fd != -1);
        res = ftruncate(fd, size);
        assert_fix(res == 0);
    } else {
        fd = shm_open(name, O_RDWR, 0600);
        if (fd == -1 && errno == ENOENT) {
            return BULK_TRANSFER_CHAN_NOTCREATED;
        } else {
            assert_fix(fd != -1);
        }
    }

    mapped = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    debug_printf("error: %s\n", strerror(errno));
    assert_fix(mapped != MAP_FAILED);

    close(fd);

    if (tx) {
        internal->tx_meta = mapped;
    } else {
        internal->rx_meta = mapped;
    }

    return SYS_ERR_OK;
}

/** Unmap SHM meta data region */
static void unmap_metas(struct lsm_internal *internal, bool tx)
{
    void *addr = (tx ? internal->tx_meta : internal->rx_meta);
    int res;

    if (addr != NULL) {
        res = munmap(addr, internal->num_slots * internal->chan->meta_size);
        assert_fix(res == 0);
    }
}

/** Unmap and destroy SHM meta data region */
static errval_t destroy_metas(struct lsm_internal *internal, bool tx)
{
    char name[strlen(internal->name) + 5];
    int res;

    unmap_metas(internal, tx);

    if (internal->chan->meta_size != 0) {
        get_metas_name(internal, tx, name);

        res = shm_unlink(name);
        assert_fix(res == 0);
    }
    return SYS_ERR_OK;
}


/******************************************************************************/
/* Implementation of struct bulk_implementation */

static errval_t op_channel_create(struct bulk_ll_channel *channel,
                                  bulk_correlation_t      corr)
{
    errval_t err;
    struct lsm_internal *internal;
    struct bulk_linuxshm_endpoint_descriptor *ep =
        (struct bulk_linuxshm_endpoint_descriptor *) channel->ep;
    char name[strlen(ep->name) + 5];

    err = internal_init(channel);
    if (err_is_fail(err)) {
        return err;
    }
    internal = channel->impl_data;
    internal->creator = true;

    // Prepare meta buffers
    err = map_metas(internal, true, false);
    if (err_is_fail(err)) {
        goto fail_mrx;
    }
    err = map_metas(internal, true, true);
    if (err_is_fail(err)) {
        goto fail_mtx;
    }

    // Prepare SHM channels
    strcpy(name, internal->name);
    strcat(name, "_rxc");
    err = shm_chan_create(&internal->rx, name, ep->num_slots, false);
    if (err_is_fail(err)) {
        goto fail_rx;
    }

    strcpy(name, internal->name);
    strcat(name, "_txc");
    err = shm_chan_create(&internal->tx, name, ep->num_slots, true);
    if (err_is_fail(err)) {
        goto fail_tx;
    }

    channel->state = BULK_STATE_INITIALIZED;
    return SYS_ERR_OK;

fail_tx:
    shm_chan_release(&internal->rx);
fail_rx:
    destroy_metas(internal, true);
fail_mtx:
    destroy_metas(internal, false);
fail_mrx:
    internal_release(internal);
    return err;
}

static errval_t op_channel_bind(struct bulk_ll_channel *channel,
                                bulk_correlation_t      corr)
{
    errval_t err;
    struct lsm_internal *internal;
    struct bulk_linuxshm_endpoint_descriptor *ep =
        (struct bulk_linuxshm_endpoint_descriptor *) channel->ep;
    char name[strlen(ep->name) + 5];
    struct shm_message *msg;

    err = internal_init(channel);
    if (err_is_fail(err)) {
        return err;
    }
    internal = channel->impl_data;
    internal->creator = false;

    // Establish SHM channels
    strcpy(name, internal->name);
    strcat(name, "_txc");
    err = shm_chan_bind(&internal->rx, name, false);
    if (err == SHM_CHAN_NOTCREATED) {
        err = BULK_TRANSFER_CHAN_NOTCREATED;
        goto fail_rx;
    } else if (err_is_fail(err)) {
        goto fail_rx;
    }

    strcpy(name, internal->name);
    strcat(name, "_rxc");
    err = shm_chan_bind(&internal->tx, name, true);
    if (err_is_fail(err)) {
        goto fail_tx;
    }

    internal->num_slots = internal->tx.size;
    internal->bind_corr = corr;

    // this part really shouldn't fail unless something is seriously wrong
    err = shm_chan_alloc(&internal->tx, &msg);
    err_expect_ok(err);

    channel->state = BULK_STATE_BINDING;
    msg->type = SHM_MSG_BIND;
    msg->content.bind_request.role = channel->role;
    shm_chan_send(&internal->tx, msg);

    return BULK_TRANSFER_ASYNC;

fail_tx:
    shm_chan_release(&internal->rx);
fail_rx:
    internal_release(internal);
    return err;
}

// XXX: this might be buggy --AKK
static errval_t
op_channel_destroy(struct bulk_ll_channel *channel,
                   bulk_correlation_t corr)
{
    struct lsm_internal *internal;
    // just guessing here...
    channel->state = BULK_STATE_TEARDOWN;
    internal = channel->impl_data;
    if (internal->creator) { // from op_channel_create()
        shm_chan_release(&internal->tx);
        shm_chan_release(&internal->rx);
        destroy_metas(internal, true);
        destroy_metas(internal, false);
        internal_release(internal);
    } else { // from op_channel_bind()
        shm_chan_release(&internal->tx);
        shm_chan_release(&internal->rx);
        internal_release(internal);
    }
    channel->state = BULK_STATE_CLOSED;

    return SYS_ERR_OK;
}



static errval_t do_pool_assign(struct lsm_internal *internal,
                               struct bulk_pool    *pool)
{
    struct bulk_pool_list *pl;

    pl = malloc(sizeof(*pl));
    if (pl == NULL) {
        return BULK_TRANSFER_MEM;
    }

    pl->pool = pool;
    pl->next = internal->chan->pools;
    internal->chan->pools = pl;

    return SYS_ERR_OK;
}

static errval_t ac_basic_cont(struct lsm_opstate   *ops,
                                  struct bulk_ll_event *event,
                                  errval_t              err)
{
    event->type = BULK_LLEV_ASYNC_DONE;
    event->data.async_done.err = err;
    event->data.async_done.corr = ops->corr;
    event->data.async_done.op = ops->op;
    ops_free(ops);
    return SYS_ERR_OK;

}

static errval_t ac_pool_assigned(struct lsm_opstate   *ops,
                                 struct bulk_ll_event *event,
                                 errval_t              err)
{
    struct lsm_internal *internal = ops->internal;

    if (err_is_ok(err)) {
        err = do_pool_assign(internal, ops->state.assign.pool);
    }

    return ac_basic_cont(ops, event, err);
}

static errval_t op_assign_pool(struct bulk_ll_channel *channel,
                               struct bulk_pool       *pool,
                               bulk_correlation_t      corr)
{
    errval_t err;
    struct lsm_internal *internal = channel->impl_data;
    struct lsm_opstate *ops;
    struct shm_message *msg;

    ops = ops_alloc(internal);
    if (ops == NULL) {
        return BULK_TRANSFER_MEM;
    }

    ops->action = ac_pool_assigned;
    ops->corr = corr;
    ops->state.assign.pool = pool;
    ops->op = BULK_ASYNC_POOL_ASSIGN;

    err = shm_chan_alloc(&internal->tx, &msg);
    if (err_is_fail(err)) {
        ops_free(ops);
        return err;
    }

    msg->type = SHM_MSG_ASSIGN;
    msg->content.assign.op = ops_id(ops);
    msg->content.assign.pool = pool->id;

    shm_chan_send(&internal->tx, msg);
    return BULK_TRANSFER_ASYNC;
}

static errval_t buffer_op_helper(struct bulk_ll_channel  *channel,
                                 struct bulk_buffer      *buffer,
                                 void                    *meta,
                                 bulk_correlation_t       corr,
                                 enum shm_message_type    type,
                                 enum bulk_ll_async_op    op)
{
    errval_t err;
    struct lsm_internal *internal = channel->impl_data;
    struct lsm_opstate *ops;
    struct shm_message *msg;
    uint32_t oid;
    void *destmeta;


    ops = ops_alloc(internal);
    if (ops == NULL) {
        return BULK_TRANSFER_MEM;
    }
    oid = ops_id(ops);

    ops->action = ac_basic_cont;
    ops->corr = corr;
    ops->op = op;

    err = shm_chan_alloc(&internal->tx, &msg);
    if (err_is_fail(err)) {
        ops_free(ops);
        return err;
    }

    msg->type = type;
    msg->content.buffer.op = oid;
    msg->content.buffer.meta = (meta == NULL ? -1U : oid);
    msg->content.buffer.buffer = buffer->bufferid;
    msg->content.buffer.pool = buffer->pool->id;

    if (meta != NULL) {
        destmeta = (void *) ((uintptr_t) internal->tx_meta +
                oid * channel->meta_size);
        memcpy(destmeta, meta, channel->meta_size);
    }

    shm_chan_send(&internal->tx, msg);
    return BULK_TRANSFER_ASYNC;
}


static errval_t op_move(struct bulk_ll_channel *channel,
                        struct bulk_buffer     *buffer,
                        void                   *meta,
                        bulk_correlation_t      corr)
{
    debug_printf("op_move(ch=%p,bu=%p,me=%p,corr=%lx)\n", channel, buffer, meta,
            corr);
    return buffer_op_helper(channel, buffer, meta, corr, SHM_MSG_MOVE,
            BULK_ASYNC_BUFFER_MOVE);
}

static errval_t op_pass(struct bulk_ll_channel *channel,
                        struct bulk_buffer     *buffer,
                        void                   *meta,
                        bulk_correlation_t      corr)
{
    debug_printf("op_pass(ch=%p,bu=%p,me=%p,corr=%lx)\n", channel, buffer, meta,
            corr);
    return buffer_op_helper(channel, buffer, meta, corr, SHM_MSG_PASS,
            BULK_ASYNC_BUFFER_PASS);
}

static errval_t op_copy(struct bulk_ll_channel *channel,
                        struct bulk_buffer     *buffer,
                        void                   *meta,
                        bulk_correlation_t      corr)
{
    debug_printf("op_copy(ch=%p,bu=%p,me=%p,corr=%lx)\n", channel, buffer, meta,
            corr);
    return buffer_op_helper(channel, buffer, meta, corr, SHM_MSG_COPY,
            BULK_ASYNC_BUFFER_COPY);
}

static errval_t op_release(struct bulk_ll_channel *channel,
                           struct bulk_buffer     *buffer,
                           bulk_correlation_t      corr)
{
    debug_printf("op_release(ch=%p,bu=%p,corr=%lx)\n", channel, buffer, corr);
    return buffer_op_helper(channel, buffer, NULL, corr, SHM_MSG_RELEASE,
            BULK_ASYNC_BUFFER_RELEASE);
}



/*****************************************************************************/
/* Incoming messages */

static void done_status(struct lsm_internal  *internal,
                        struct bulk_ll_event *event,
                        errval_t              err)
{
    errval_t e;
    struct shm_message *out;


    e = shm_chan_alloc(&internal->tx, &out);
    err_expect_ok(e); // FIXME

    out->type = SHM_MSG_STATUS;
    out->content.status.op = (uintptr_t) event->impl_data;
    out->content.status.err = err;

    shm_chan_send(&internal->tx, out);
}


static void done_bind(struct lsm_internal  *internal,
                      struct bulk_ll_event *event,
                      errval_t              err)
{
    errval_t e;
    struct shm_message *out;

     // TODO: error handling in case the binding is not successful

    e = shm_chan_alloc(&internal->tx, &out);
    err_expect_ok(e); // FIXME

    out->type = SHM_MSG_BIND_DONE;
    out->content.bind_done.meta_size = internal->chan->meta_size;
    out->content.bind_done.direction =
        (internal->chan->direction == BULK_DIRECTION_TX ?
            BULK_DIRECTION_RX : BULK_DIRECTION_TX);
    out->content.bind_done.err = err;

    shm_chan_send(&internal->tx, out);
}

static errval_t msg_bind(struct lsm_internal *internal,
                         struct shm_message *msg,
                         struct bulk_ll_event *event)
{
    errval_t err = SYS_ERR_OK;

    debug_printf("msg_bind\n");

    if (msg->content.bind_request.role  == internal->chan->role) {
        err = BULK_TRANSFER_CHAN_ROLE;
        goto out_err;
    }

    internal->chan->state = BULK_STATE_CONNECTED;

    event->type = BULK_LLEV_CHAN_BIND;
    return SYS_ERR_OK;

out_err:
    done_bind(internal, NULL, err);
    return BULK_TRANSFER_EVENTABORT;
}

static errval_t msg_bind_done(struct lsm_internal  *internal,
                              struct shm_message   *msg,
                              struct bulk_ll_event *event)
{
    debug_printf("msg_bind_done\n");
    errval_t err = msg->content.bind_done.err;

    if (err_is_ok(err)) {
        internal->chan->meta_size = msg->content.bind_done.meta_size;
        internal->chan->direction = msg->content.bind_done.direction;

        // Map meta buffers
        err = map_metas(internal, false, false);
        err_expect_ok(err); // FIXME
        err = map_metas(internal, false, true);
        err_expect_ok(err); // FIXME

        internal->chan->state = BULK_STATE_CONNECTED;
    }

    event->type = BULK_LLEV_ASYNC_DONE;
    event->data.async_done.op = BULK_ASYNC_CHAN_BIND;
    event->data.async_done.err = err;
    event->data.async_done.corr = internal->bind_corr;
    return SYS_ERR_OK;
}

struct assign_event {
    struct bulk_pool *pool;
    uint32_t op;
};

static void done_assign(struct lsm_internal  *internal,
                        struct bulk_ll_event *event,
                        errval_t              err)
{
    struct assign_event *ae = event->impl_data;
    if (!err_is_ok(err)) {
        bulk_pool_free(ae->pool);
    }

    event->impl_data = (void *) (uintptr_t) ae->op;
    free(ae);
    return done_status(internal, event, err);
}

static errval_t msg_assign(struct lsm_internal  *internal,
                           struct shm_message   *msg,
                           struct bulk_ll_event *event)
{
    errval_t err = SYS_ERR_OK;
    errval_t e;
    struct bulk_pool *p;
    struct shm_message *out;
    struct assign_event *ae;

    // Check if we already know the pool
    p = bulk_int_pool_byid(msg->content.assign.pool);
    if (p == NULL) {
        if ((p = calloc(1, sizeof(*p))) == NULL) {
            err = BULK_TRANSFER_MEM;
            goto out;
        }

        p->id = msg->content.assign.pool;
        err = bulk_int_pool_map(p, BULK_BUFFER_INVALID, NULL, -1);
        if (!err_is_ok(err)) {
            goto fail_map;
        }
    }

    event->type = BULK_LLEV_POOL_ASSIGN;
    event->data.pool.pool = p;
    ae = malloc(sizeof(*ae));
    ae->pool = p;
    ae->op = msg->content.assign.op;
    event->impl_data = ae;

    return SYS_ERR_OK;

fail_map:
    free(p);
out:
    e = shm_chan_alloc(&internal->tx, &out);
    err_expect_ok(e); // FIXME

    out->type = SHM_MSG_STATUS;
    out->content.status.op = msg->content.assign.op;
    out->content.status.err = err;

    shm_chan_send(&internal->tx, out);
    return BULK_TRANSFER_EVENTABORT;
}

static errval_t msg_buffer(struct lsm_internal  *internal,
                           struct shm_message   *msg,
                           struct bulk_ll_event *event)
{
    errval_t err = SYS_ERR_OK, e;
    struct bulk_pool *p;
    struct bulk_buffer *b;
    struct shm_message *out;
    void *meta;

    p = bulk_int_pool_byid(msg->content.buffer.pool);
    if (p == NULL) {
        err = BULK_TRANSFER_POOL_NOT_ASSIGNED;
        goto out_err;
    }

    b = p->buffers[msg->content.buffer.buffer];
    if (msg->content.buffer.meta == -1U) {
        meta = NULL;
    } else {
        meta = (void *) ((uintptr_t) internal->rx_meta +
                msg->content.buffer.meta * internal->chan->meta_size);
    }

    debug_printf("msg_buffer: p=[%d,%d,%d] b=%d m=%d\n",
            msg->content.buffer.pool.machine, msg->content.buffer.pool.dom,
            msg->content.buffer.pool.local, msg->content.buffer.buffer,
            (int) msg->content.buffer.meta);

    switch (msg->type) {
        case SHM_MSG_MOVE:
            event->type = BULK_LLEV_BUF_MOVE;
            break;

        case SHM_MSG_PASS:
            event->type = BULK_LLEV_BUF_PASS;
            break;

        case SHM_MSG_COPY:
            event->type = BULK_LLEV_BUF_COPY;
            break;

        case SHM_MSG_RELEASE:
            event->type = BULK_LLEV_BUF_RELEASE;
            break;

        default:
            assert(!"Invalid message type");
    }
    event->impl_data = (void *) (uintptr_t) msg->content.buffer.op;
    event->data.buffer.buffer = b;
    event->data.buffer.meta = meta;

    return SYS_ERR_OK;
out_err:
    e = shm_chan_alloc(&internal->tx, &out);
    err_expect_ok(e); // FIXME

    out->type = SHM_MSG_STATUS;
    out->content.status.op = msg->content.buffer.op;
    out->content.status.err = err;

    shm_chan_send(&internal->tx, out);
    return BULK_TRANSFER_EVENTABORT;
}

static errval_t msg_status(struct lsm_internal  *internal,
                           struct shm_message   *msg,
                           struct bulk_ll_event *event)
{
    struct lsm_opstate *ops;
    debug_printf("msg_status: op=%d err=%d\n", msg->content.status.op,
            msg->content.status.err);

    ops = internal->ops + msg->content.status.op;
    return ops->action(ops, event, msg->content.status.err);
}

static errval_t op_event_poll(struct bulk_ll_channel *channel,
                              struct bulk_ll_event   *event)
{
    struct lsm_internal *internal = channel->impl_data;
    struct shm_message  *msg = NULL;
    errval_t err;

    err = shm_chan_poll(&internal->rx, &msg);
    if (err == SHM_CHAN_NOMSG) {
        return BULK_TRANSFER_NOEVENT;
    } else if (!err_is_ok(err)) {
        return err;
    }

    event->impl_data = NULL;
    switch (msg->type) {
        case SHM_MSG_BIND:
            err = msg_bind(internal, msg, event);
            break;

        case SHM_MSG_BIND_DONE:
            err = msg_bind_done(internal, msg, event);
            break;

        case SHM_MSG_ASSIGN:
            err = msg_assign(internal, msg, event);
            break;

        case SHM_MSG_MOVE:
        case SHM_MSG_PASS:
        case SHM_MSG_COPY:
        case SHM_MSG_RELEASE:
            err = msg_buffer(internal, msg, event);
            break;

        case SHM_MSG_STATUS:
            err = msg_status(internal, msg, event);
            break;

        default:
            debug_printf("Unhandled message type: %d\n", msg->type);
            assert(!"NYI");
    }
    shm_chan_free(&internal->rx, msg);
    return err;
}

static errval_t op_event_done(struct bulk_ll_channel *channel,
                              struct bulk_ll_event   *event,
                              errval_t                err)
{
    struct lsm_internal *internal = channel->impl_data;
    switch (event->type) {
        case BULK_LLEV_CHAN_BIND:
            done_bind(internal, event, err);
            break;

        case BULK_LLEV_POOL_ASSIGN:
            done_assign(internal, event, err);
            break;

        case BULK_LLEV_BUF_MOVE:
        case BULK_LLEV_BUF_PASS:
        case BULK_LLEV_BUF_COPY:
        case BULK_LLEV_BUF_RELEASE:
            done_status(internal, event, err);
            break;

    }
    return SYS_ERR_OK;
}

static struct bulk_implementation implementation = {
    .channel_create = op_channel_create,
    .channel_bind = op_channel_bind,
    .channel_destroy = op_channel_destroy,
    .pool_assign = op_assign_pool,
    .buffer_move = op_move,
    .buffer_pass = op_pass,
    .buffer_copy = op_copy,
    .buffer_release = op_release,
    .event_poll = op_event_poll,
    .event_done = op_event_done,
};


errval_t bulk_linuxshm_ep_init(struct bulk_linuxshm_endpoint_descriptor *ep,
                               const char *name,
                               size_t      num_slots)
{
    ep->generic.f = &implementation;
    ep->name = name;
    ep->num_slots = num_slots;
    return SYS_ERR_OK;
}

void bulk_linuxshm_emergency_cleanup(struct bulk_ll_channel *chan)
{
    struct lsm_internal *internal = chan->impl_data;
    char name[strlen(internal->name) + 5];

    get_metas_name(internal, true, name);
    shm_unlink(name);
    get_metas_name(internal, false, name);
    shm_unlink(name);

    strcpy(name, internal->name);
    strcat(name, "_rxc");
    shm_unlink(name);

    strcpy(name, internal->name);
    strcat(name, "_txc");
    shm_unlink(name);
}

