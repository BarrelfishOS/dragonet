#include <bulk_transfer/bulk_transfer.h>

errval_t bulk_channel_create(struct bulk_channel              *channel,
                             struct bulk_endpoint_descriptor  *ep_desc,
                             struct bulk_channel_callbacks    *callbacks,
                             struct bulk_channel_setup        *setup)
{
    channel->callbacks = callbacks;
    channel->ep = ep_desc;
    channel->pools = NULL;
    channel->direction = setup->direction;
    channel->role = setup->role;
    channel->trust = setup->trust;
    channel->constraints = setup->constraints;
    channel->meta_size = setup->meta_size;
    channel->waitset = setup->waitset;

    return ep_desc->f->channel_create(channel);
}

errval_t bulk_channel_bind(struct bulk_channel              *channel,
                           struct bulk_endpoint_descriptor  *ep_desc,
                           struct bulk_channel_callbacks    *callbacks,
                           struct bulk_channel_bind_params  *params,
                           struct bulk_continuation cont)
{
    channel->callbacks = callbacks;
    channel->ep = ep_desc;
    channel->pools = NULL;
    channel->role = params->role;
    channel->trust = params->trust;
    channel->constraints = params->constraints;
    channel->waitset = params->waitset;

    // To be set when binding: channel->{direction,meta_size}
    return ep_desc->f->channel_bind(channel, cont);
}

errval_t bulk_channel_assign_pool(struct bulk_channel *channel,
                                  struct bulk_pool    *pool,
                                  struct bulk_continuation cont)
{
    return channel->ep->f->assign_pool(channel, pool, cont);
}

errval_t bulk_channel_move(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont)
{
    return channel->ep->f->move(channel, buffer, meta, cont);
}

errval_t bulk_channel_pass(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont)
{
    return channel->ep->f->pass(channel, buffer, meta, cont);
}

errval_t bulk_channel_copy(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont)
{
    return channel->ep->f->copy(channel, buffer, meta, cont);
}

errval_t bulk_channel_release(struct bulk_channel       *channel,
                              struct bulk_buffer        *buffer,
                              struct bulk_continuation   cont)
{
    return channel->ep->f->release(channel, buffer, cont);
}

