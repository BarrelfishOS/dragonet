/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/waitset.h>
#include <bulk_transfer/bulk_transfer.h>

struct bulk_cont_alloc {
    struct bulk_continuation cont;
    struct bulk_cont_alloc  *next;
};

static inline struct bulk_cont_alloc *bca_alloc(struct bulk_channel *channel,
                                                struct bulk_continuation *cont)
{
    struct bulk_cont_alloc *bca = malloc(sizeof(*bca));
    bca->cont = *cont;
    return bca;
}


static inline void bca_free(struct bulk_channel *channel,
                     struct bulk_cont_alloc     *bca)
{
    // TODO
    free(bca);
}

static void channel_init_common(struct bulk_channel           *chan,
                                struct bulk_channel_callbacks *cbs,
                                struct waitset                *ws)
{
    chan->callbacks = cbs;
    chan->waitset = ws;
    chan->cont_cache = NULL;
    chan->ws_state.channel = chan;
}

static bool channel_poll(struct waitset_chanstate *wscs)
{
    errval_t err;
    struct bulk_waitset_state *wss = (struct bulk_waitset_state *) wscs;
    struct bulk_channel *chan = wss->channel;
    struct bulk_ll_event event;
    struct bulk_cont_alloc *bca;

    err = bulk_ll_channel_event_poll(&chan->ll, &event);
    if (err == BULK_TRANSFER_NOEVENT) {
        return false;
    }
    if (err == BULK_TRANSFER_EVENTABORT) {
        // Implementation did some processing but we did not get back an event.
        // So we there might still be more to do for this channel
        return true;
    }
    err_expect_ok(err);

    switch (event.type) {
        case BULK_LLEV_ASYNC_DONE:
            bca = (struct bulk_cont_alloc *) event.data.async_done.corr;
            bulk_continuation_call(
                    &bca->cont, event.data.async_done.err, chan);
            break;

        case BULK_LLEV_CHAN_BIND:
            err = chan->callbacks->bind_received(chan);
            break;

        case BULK_LLEV_CHAN_DESTROY:
            chan->callbacks->teardown_received(chan);
            break;

        case BULK_LLEV_POOL_ASSIGN:
            err = chan->callbacks->pool_assigned(chan, event.data.pool.pool);
            break;

        case BULK_LLEV_POOL_REMOVE:
            err = chan->callbacks->pool_removed(chan, event.data.pool.pool);
            break;

        case BULK_LLEV_BUF_MOVE:
            chan->callbacks->move_received(chan, event.data.buffer.buffer,
                    event.data.buffer.meta);
            break;

        case BULK_LLEV_BUF_PASS:
            chan->callbacks->buffer_received(chan, event.data.buffer.buffer,
                    event.data.buffer.meta);
            break;

        case BULK_LLEV_BUF_COPY:
            chan->callbacks->copy_received(chan, event.data.buffer.buffer,
                    event.data.buffer.meta);
            break;

        case BULK_LLEV_BUF_RELEASE:
            chan->callbacks->copy_released(chan, event.data.buffer.buffer);
            break;

        default:
            fprintf(stderr, "Unknown event type %d\n", event.type);
            abort();
    }

    errval_t e = bulk_ll_channel_event_done(&chan->ll, &event, err);
    if (e == SYS_ERR_OK) {
        return true;
    } else {
        return false;
    }
}

static void channel_start_waitset(struct bulk_channel *chan)
{
    chan->ws_state.wscs.poll = channel_poll;
    ws_addchan(chan->waitset, &chan->ws_state.wscs);
}

/**
 * If err == OK, call cont and then free bca, return OK
 * If err == ASYNC, return OK
 * Otherwise return err and free bca
 */
static errval_t cont_call_helper(struct bulk_channel    *channel,
                                 struct bulk_cont_alloc *bca,
                                 errval_t                err)
{
    if (err == SYS_ERR_OK) {
        bulk_continuation_call(&bca->cont, err, channel);
        return err;
    } else if (err == BULK_TRANSFER_ASYNC) {
        return SYS_ERR_OK;
    } else {
        bca_free(channel, bca);
        return err;
    }
}


errval_t bulk_channel_create(struct bulk_channel              *channel,
                             struct bulk_endpoint_descriptor  *ep_desc,
                             struct bulk_channel_callbacks    *callbacks,
                             struct bulk_channel_setup        *setup,
                             struct waitset                   *ws,
                             struct bulk_continuation          cont)
{
    errval_t err;
    struct bulk_cont_alloc *bca;

    channel_init_common(channel, callbacks, ws);
    bca = bca_alloc(channel, &cont);
    err = bulk_ll_channel_create(&channel->ll, ep_desc, setup,
            (bulk_correlation_t) bca);

    if (err == SYS_ERR_OK || err == BULK_TRANSFER_ASYNC) {
        channel_start_waitset(channel);
    }

    return cont_call_helper(channel, bca, err);
}

errval_t bulk_channel_bind(struct bulk_channel              *channel,
                           struct bulk_endpoint_descriptor  *ep_desc,
                           struct bulk_channel_callbacks    *callbacks,
                           struct bulk_channel_bind_params  *params,
                           struct waitset                   *ws,
                           struct bulk_continuation          cont)
{
    errval_t err;
    struct bulk_cont_alloc *bca;

    channel_init_common(channel, callbacks, ws);
    bca = bca_alloc(channel, &cont);
    err = bulk_ll_channel_bind(&channel->ll, ep_desc, params,
            (bulk_correlation_t) bca);

    if (err == SYS_ERR_OK || err == BULK_TRANSFER_ASYNC) {
        channel_start_waitset(channel);
    }

    return cont_call_helper(channel, bca, err);
}

errval_t bulk_channel_assign_pool(struct bulk_channel *channel,
                                  struct bulk_pool    *pool,
                                  struct bulk_continuation cont)
{
    struct bulk_cont_alloc *bca = bca_alloc(channel, &cont);
    return cont_call_helper(channel, bca,
            bulk_ll_channel_assign_pool(
                &channel->ll, pool, (bulk_correlation_t) bca));
}

errval_t bulk_channel_move(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont)
{
    struct bulk_cont_alloc *bca = bca_alloc(channel, &cont);
    return cont_call_helper(channel, bca,
            bulk_ll_channel_move(
                &channel->ll, buffer, meta, (bulk_correlation_t) bca));
}

errval_t bulk_channel_pass(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont)
{
    struct bulk_cont_alloc *bca = bca_alloc(channel, &cont);
    return cont_call_helper(channel, bca,
            bulk_ll_channel_pass(
                &channel->ll, buffer, meta, (bulk_correlation_t) bca));
}

errval_t bulk_channel_copy(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont)
{
    struct bulk_cont_alloc *bca = bca_alloc(channel, &cont);
    return cont_call_helper(channel, bca,
            bulk_ll_channel_copy(
                &channel->ll, buffer, meta, (bulk_correlation_t) bca));
}

errval_t bulk_channel_release(struct bulk_channel       *channel,
                              struct bulk_buffer        *buffer,
                              struct bulk_continuation   cont)
{
    struct bulk_cont_alloc *bca = bca_alloc(channel, &cont);
    return cont_call_helper(channel, bca,
            bulk_ll_channel_release(
                &channel->ll, buffer, (bulk_correlation_t) bca));
}

