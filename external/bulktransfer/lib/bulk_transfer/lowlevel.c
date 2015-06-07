/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <bulk_transfer/bulk_lowlevel.h>

#define ll_exit()  printf("%s: exit\n", __func__)

struct bulk_ll_channel *bulk_channels = NULL;

errval_t bulk_ll_channel_create(struct bulk_ll_channel           *channel,
                                struct bulk_endpoint_descriptor  *ep_desc,
                                struct bulk_channel_setup        *setup,
                                bulk_correlation_t                corr)
{
    errval_t err;
    channel->ep = ep_desc;
    channel->pools = NULL;
    channel->direction = setup->direction;
    channel->role = setup->role;
    channel->trust = setup->trust;
    channel->constraints = setup->constraints;
    channel->meta_size = setup->meta_size;
    channel->internal.creator = true;


    err = ep_desc->f->channel_create(channel, corr);

    if (err_is_ok(err) || err == BULK_TRANSFER_ASYNC) {
        channel->internal.next = bulk_channels;
        bulk_channels = channel;
    }
    return err;
}

errval_t bulk_ll_channel_bind(struct bulk_ll_channel          *channel,
                              struct bulk_endpoint_descriptor *remote_ep_desc,
                              struct bulk_channel_bind_params *params,
                              bulk_correlation_t               corr)
{
    errval_t err;

    channel->ep = remote_ep_desc;
    channel->pools = NULL;
    channel->role = params->role;
    channel->trust = params->trust;
    channel->constraints = params->constraints;
    channel->internal.creator = false;


    // To be set when binding: channel->{direction,meta_size}
    err = remote_ep_desc->f->channel_bind(channel, corr);

    if (err_is_ok(err) || err == BULK_TRANSFER_ASYNC) {
        channel->internal.next = bulk_channels;
        bulk_channels = channel;
    }
    return err;
}

errval_t bulk_ll_channel_destroy(struct bulk_ll_channel *channel,
                                 bulk_correlation_t      corr)
{

    if (channel->ep->f->channel_destroy == NULL) {
        fprintf(stderr, "%s: channel_destroy not implemented\n", __FUNCTION__);
        abort();
    }

    return channel->ep->f->channel_destroy(channel, corr);
}



errval_t bulk_ll_channel_assign_pool(struct bulk_ll_channel *channel,
                                     struct bulk_pool       *pool,
                                     bulk_correlation_t      corr)
{
    return channel->ep->f->pool_assign(channel, pool, corr);
}

errval_t bulk_ll_channel_remove_pool(struct bulk_ll_channel *channel,
                                     struct bulk_pool       *pool,
                                     bulk_correlation_t      corr)
{
    return channel->ep->f->pool_remove(channel, pool, corr);
}



errval_t bulk_ll_channel_move(struct bulk_ll_channel *channel,
                              struct bulk_buffer     *buffer,
                              void                   *meta,
                              bulk_correlation_t      corr)
{
    return channel->ep->f->buffer_move(channel, buffer, meta, corr);
}

errval_t bulk_ll_channel_pass(struct bulk_ll_channel *channel,
                              struct bulk_buffer     *buffer,
                              void                   *meta,
                              bulk_correlation_t      corr)
{
    return channel->ep->f->buffer_pass(channel, buffer, meta, corr);
}

errval_t bulk_ll_channel_copy(struct bulk_ll_channel *channel,
                              struct bulk_buffer     *buffer,
                              void                   *meta,
                              bulk_correlation_t      corr)
{
    return channel->ep->f->buffer_copy(channel, buffer, meta, corr);
}

errval_t bulk_ll_channel_release(struct bulk_ll_channel *channel,
                                 struct bulk_buffer     *buffer,
                                 bulk_correlation_t      corr)
{
    return channel->ep->f->buffer_release(channel, buffer, corr);
}

errval_t bulk_ll_channel_event_poll(struct bulk_ll_channel *channel,
                                    struct bulk_ll_event   *event)
{
    return channel->ep->f->event_poll(channel, event);
}

errval_t bulk_ll_channel_event_done(struct bulk_ll_channel *channel,
                                    struct bulk_ll_event   *event,
                                    errval_t                err)
{
    return channel->ep->f->event_done(channel, event, err);
}

