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
    void (*action) (struct lsm_opstate *ops);
    union {
        struct {
            struct bulk_pool *pool;
        } assign;
    } state;
    struct bulk_continuation cont;
    errval_t err;

    struct lsm_opstate *next;
};

struct lsm_internal {
    const char          *name;
    size_t               num_slots;
    struct bulk_channel *chan;
    bool                 creator;
    struct lsm_opstate  *ops;
    struct lsm_opstate  *ops_free;

    struct shm_channel   rx;
    struct shm_channel_wsstate rx_wss;
    void                *rx_meta;
    struct shm_channel   tx;
    struct shm_channel_wsstate tx_wss;
    void                *tx_meta;

    struct bulk_continuation bind_cont;
};

static void msg_handler(struct shm_message *msg, void *opaque);


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
static errval_t internal_init(struct bulk_channel *channel)
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

    if (create) {
        fd = shm_open(name, O_CREAT | O_RDWR | O_EXCL, 0600);
        assert_fix(fd != -1);
        res = ftruncate(fd, size);
        assert_fix(res == 0);
        close(fd);
    }

    debug_printf("shm_open(%s)\n", name);
    fd = shm_open(name, O_RDWR, 0600);
    assert_fix(fd != -1);

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

static errval_t op_channel_create(struct bulk_channel *channel)
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
    shm_waitset_register(&internal->rx, &internal->rx_wss, msg_handler,
            internal, channel->waitset);

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


static errval_t op_channel_bind(struct bulk_channel     *channel,
                                struct bulk_continuation cont)
{
    errval_t err;
    struct lsm_internal *internal;
    struct bulk_linuxshm_endpoint_descriptor *ep =
        (struct bulk_linuxshm_endpoint_descriptor *) channel->ep;
    char name[strlen(ep->name) + 5];
    struct shm_message *msg;
    //struct lsm_opstate *ops;

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
    if (err_is_fail(err)) {
        goto fail_rx;
    }
    shm_waitset_register(&internal->rx, &internal->rx_wss, msg_handler,
            internal, channel->waitset);

    strcpy(name, internal->name);
    strcat(name, "_rxc");
    err = shm_chan_bind(&internal->tx, name, true);
    if (err_is_fail(err)) {
        goto fail_tx;
    }

    internal->num_slots = internal->tx.size;



#if 0
    ops = ops_alloc(internal);
    // should not fail, since we just filled the allocator
    assert(ops != NULL);
    ops->action = ac_bind_completed;
    ops->cont = cont;
#endif
    internal->bind_cont = cont;

    // this part really shouldn't fail unless something is seriously wrong
    err = shm_chan_alloc(&internal->tx, &msg);
    err_expect_ok(err);

    channel->state = BULK_STATE_BINDING;
    msg->type = SHM_MSG_BIND;
    //msg->content.bind_request.op = ops_id(ops);
    msg->content.bind_request.role = channel->role;
    msg->content.bind_request.direction = channel->direction;
    shm_chan_send(&internal->tx);

    return SYS_ERR_OK;

fail_tx:
    shm_chan_release(&internal->rx);
fail_rx:
    internal_release(internal);
    return err;
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

static void ac_pool_assigned(struct lsm_opstate *ops)
{
    struct lsm_internal *internal = ops->internal;
    errval_t err = ops->err;

    if (err_is_ok(err)) {
        err = do_pool_assign(internal, ops->state.assign.pool);
    }

    bulk_continuation_call(&ops->cont, err, internal->chan);
}

static errval_t op_assign_pool(struct bulk_channel *channel,
                               struct bulk_pool    *pool,
                               struct bulk_continuation cont)
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
    ops->cont = cont;
    ops->state.assign.pool = pool;

    err = shm_chan_alloc(&internal->tx, &msg);
    if (err_is_fail(err)) {
        ops_free(ops);
        return err;
    }

    msg->type = SHM_MSG_ASSIGN;
    msg->content.assign.op = ops_id(ops);
    msg->content.assign.pool = pool->id;

    shm_chan_send(&internal->tx);
    return SYS_ERR_OK;
}

static void ac_basic_cont(struct lsm_opstate *ops)
{
    bulk_continuation_call(&ops->cont, ops->err, ops->internal->chan);
}

static errval_t buffer_op_helper(struct bulk_channel  *channel,
                                 struct bulk_buffer   *buffer,
                                 void                 *meta,
                                 struct bulk_continuation cont,
                                 enum shm_message_type type)
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
    ops->cont = cont;

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

    shm_chan_send(&internal->tx);
    return SYS_ERR_OK;
}


static errval_t op_move(struct bulk_channel  *channel,
                        struct bulk_buffer   *buffer,
                        void                 *meta,
                        struct bulk_continuation cont)
{
    return buffer_op_helper(channel, buffer, meta, cont, SHM_MSG_MOVE);
}

static errval_t op_pass(struct bulk_channel  *channel,
                        struct bulk_buffer   *buffer,
                        void                 *meta,
                        struct bulk_continuation cont)
{
    return buffer_op_helper(channel, buffer, meta, cont, SHM_MSG_PASS);
}

static errval_t op_copy(struct bulk_channel  *channel,
                        struct bulk_buffer   *buffer,
                        void                 *meta,
                        struct bulk_continuation cont)
{
    return buffer_op_helper(channel, buffer, meta, cont, SHM_MSG_COPY);
}

static errval_t op_release(struct bulk_channel  *channel,
                           struct bulk_buffer   *buffer,
                           struct bulk_continuation cont)
{
    return buffer_op_helper(channel, buffer, NULL, cont, SHM_MSG_RELEASE);
}


static struct bulk_implementation implementation = {
    .channel_create = op_channel_create,
    .channel_bind = op_channel_bind,
    .assign_pool = op_assign_pool,
    .move = op_move,
    .pass = op_pass,
    .copy = op_copy,
    .release = op_release,
};


/*****************************************************************************/
/* Incoming messages */

static void msg_bind(struct lsm_internal *internal, struct shm_message *msg)
{
    struct shm_message *out;
    errval_t err = SYS_ERR_OK;
    errval_t e;

    debug_printf("msg_bind\n");

    if (msg->content.bind_request.role  == internal->chan->role) {
        err = BULK_TRANSFER_CHAN_ROLE;
        goto out;
    }
    if (msg->content.bind_request.direction ==  internal->chan->direction) {
        err = BULK_TRANSFER_CHAN_DIRECTION;
        goto out;
    }

    internal->chan->state = BULK_STATE_CONNECTED;

    err = internal->chan->callbacks->bind_received(internal->chan);

out:
    // TODO: error handling in case the binding is not successful

    e = shm_chan_alloc(&internal->tx, &out);
    err_expect_ok(e); // FIXME

    out->type = SHM_MSG_BIND_DONE;
    out->content.bind_done.meta_size = internal->chan->meta_size;
    out->content.bind_done.err = err;

    shm_chan_send(&internal->tx);
}

static void msg_bind_done(struct lsm_internal *internal, struct shm_message *msg)
{
    debug_printf("msg_bind_done\n");
    errval_t err = msg->content.bind_done.err;

    if (err_is_ok(err)) {
        internal->chan->meta_size = msg->content.bind_done.meta_size;

        // Map meta buffers
        err = map_metas(internal, false, false);
        err_expect_ok(err); // FIXME
        err = map_metas(internal, false, true);
        err_expect_ok(err); // FIXME

        internal->chan->state = BULK_STATE_CONNECTED;
    }

    bulk_continuation_call(&internal->bind_cont, err, internal->chan);
}

static void msg_assign(struct lsm_internal *internal, struct shm_message *msg)
{
    errval_t err = SYS_ERR_OK;
    errval_t e;
    struct bulk_pool *p;
    struct shm_message *out;
    debug_printf("msg_assign\n");

    if ((p = calloc(1, sizeof(*p))) == NULL) {
        err = BULK_TRANSFER_MEM;
        goto out;
    }

    p->id = msg->content.assign.pool;
    err = bulk_int_pool_map(p, BULK_BUFFER_INVALID, NULL, -1);
    if (!err_is_ok(err)) {
        goto fail_map;
    }

    err = internal->chan->callbacks->pool_assigned(internal->chan, p);
    if (!err_is_ok(err)) {
        goto fail_cb;
    }

    goto out;

fail_cb:
    bulk_pool_free(p);
fail_map:
    free(p);
out:
    e = shm_chan_alloc(&internal->tx, &out);
    err_expect_ok(e); // FIXME

    out->type = SHM_MSG_STATUS;
    out->content.status.op = msg->content.assign.op;
    out->content.status.err = err;

    shm_chan_send(&internal->tx);
}

static void msg_buffer(struct lsm_internal *internal, struct shm_message *msg)
{
    errval_t err = SYS_ERR_OK, e;
    struct bulk_pool *p;
    struct bulk_buffer *b;
    struct shm_message *out;
    void *meta;

    p = bulk_int_pool_byid(msg->content.buffer.pool);
    if (p == NULL) {
        err = BULK_TRANSFER_POOL_NOT_ASSIGNED;
        goto out;
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
            internal->chan->callbacks->move_received(internal->chan, b, meta);
            break;

        case SHM_MSG_PASS:
            internal->chan->callbacks->buffer_received(internal->chan, b, meta);
            break;

        case SHM_MSG_COPY:
            internal->chan->callbacks->copy_received(internal->chan, b, meta);
            break;

        case SHM_MSG_RELEASE:
            internal->chan->callbacks->copy_released(internal->chan, b);
            break;

        default:
            assert(!"Invalid message type");
    }

out:
    e = shm_chan_alloc(&internal->tx, &out);
    err_expect_ok(e); // FIXME

    out->type = SHM_MSG_STATUS;
    out->content.status.op = msg->content.buffer.op;
    out->content.status.err = err;

    shm_chan_send(&internal->tx);
}

static void msg_status(struct lsm_internal *internal, struct shm_message *msg)
{
    struct lsm_opstate *ops;
    debug_printf("msg_status: op=%d err=%d\n", msg->content.status.op,
            msg->content.status.err);

    ops = internal->ops + msg->content.status.op;
    ops->action(ops);

    ops_free(ops);
}

static void msg_handler(struct shm_message *msg, void *opaque)
{
    struct lsm_internal *internal = opaque;
    debug_printf("msg_handler: %d\n", msg->type);
    switch (msg->type) {
        case SHM_MSG_BIND:
            msg_bind(internal, msg);
            break;

        case SHM_MSG_BIND_DONE:
            msg_bind_done(internal, msg);
            break;

        case SHM_MSG_ASSIGN:
            msg_assign(internal, msg);
            break;

        case SHM_MSG_MOVE:
        case SHM_MSG_PASS:
        case SHM_MSG_COPY:
        case SHM_MSG_RELEASE:
            msg_buffer(internal, msg);
            break;

        case SHM_MSG_STATUS:
            msg_status(internal, msg);
            break;

        default:
            debug_printf("Unhandled message type: %d\n", msg->type);
            assert(!"NYI");
    }

    shm_chan_free(&internal->rx, msg);
}


errval_t bulk_linuxshm_ep_create(struct bulk_linuxshm_endpoint_descriptor *ep,
                                 const char *name,
                                 size_t      num_slots)
{
    ep->generic.f = &implementation;
    ep->name = name;
    ep->num_slots = num_slots;
    return SYS_ERR_OK;
}

