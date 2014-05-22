/**
 * \file
 * \brief Generic bulk data transfer mechanism
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_TRANSFER_H
#define BULK_TRANSFER_H

#include <bulk_transfer/bulk_internal.h>
#include <bulk_transfer/bulk_lowlevel.h>

struct bulk_channel;
struct bulk_cont_alloc;

/**
 * continuation to make the interface asynchronous
 */
struct bulk_continuation {
    void (*handler)(void *arg, errval_t err, struct bulk_channel *channel);
    void *arg;
};


#define MK_BULK_CONT(h,a) ((struct bulk_continuation) {.handler=(h), .arg=(a)})
#define BULK_CONT_NOP     MK_BULK_CONT(NULL, NULL)

/**
 * Helper function to call a bulk continuation with given arguments.
 */
static inline void bulk_continuation_call(struct bulk_continuation *cont,
                                          errval_t                  err,
                                          struct bulk_channel      *channel)
{
    if (cont->handler) {
        cont->handler(cont->arg, err, channel);
    }
}

struct bulk_waitset_state {
    struct waitset_chanstate wscs;
    struct bulk_channel      *channel;
};



/** Callbacks for events */
struct bulk_channel_callbacks {
    /**
     * For exporting side: other endpoint successfully bound
     */
    errval_t (*bind_received)(struct bulk_channel *channel);

    /**
     * the other side wants to teardown the channel
     * For initiating side: teardown completed
     * For other side: teardown initiated
     */
    void (*teardown_received)(struct bulk_channel *channel);

    /**
     * The other endpoint requests to assign a new pool to this channel.
     * @return If an error value is returned, the pool is not assigned and the
     *         error code is sent to the other side (veto).
     */
    errval_t (*pool_assigned)(struct bulk_channel *channel,
                              struct bulk_pool    *pool);

    /**
     * The other endpoint wants to remove a pool from this channel
     */
    errval_t (*pool_removed)(struct bulk_channel *channel,
                             struct bulk_pool    *pool);

    /** Incoming moved buffer (sink) */
    void (*move_received)(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta);

    /** Incoming passed buffer (source) */
    void (*buffer_received)(struct bulk_channel *channel,
                            struct bulk_buffer  *buffer,
                            void                *meta);

    /** Incoming copied buffer (sink) */
    void (*copy_received)(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          void                *meta);

    /** Released copied buffer (source) */
    void (*copy_released)(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer);
};


/** Handle/Representation for one end of a bulk transfer channel */
struct bulk_channel {
    struct bulk_ll_channel         ll;
    /** callbacks for the channel events */
    struct bulk_channel_callbacks *callbacks;
    /** the waitset for this channel */
    struct waitset                *waitset;
    /** state for waitset */
    struct bulk_waitset_state      ws_state;
    /** bulk continuation allocation cache */
    struct bulk_cont_alloc        *cont_cache;
    /** pointer to user specific state for this channel */
    void                          *user_state;
};



/*
 * ---------------------------------------------------------------------------
 * Channel Management >>>
 */

/**
 * Create a new channel.
 *
 * @param channel   Pointer to unused channel handle
 * @param ep_desc   Description of endpoint to bind to
 * @param callbacks Callbacks for events on this channel
 * @param setup     struct containing the setup information
 */
errval_t bulk_channel_create(struct bulk_channel              *channel,
                             struct bulk_endpoint_descriptor  *ep_desc,
                             struct bulk_channel_callbacks    *callbacks,
                             struct bulk_channel_setup        *setup,
                             struct waitset                   *ws,
                             struct bulk_continuation          cont);

/**
 * Bind to an existing unbound channel.
 *
 * @param channel   Pointer to unused channel handle
 * @param ep_desc   Description of endpoint to bind to
 * @param callbacks Callbacks for events on this channel
 * @param params    parameters for the binding process
 *
 * There is the bind done callback that serves as a continuation for this.
 */
errval_t bulk_channel_bind(struct bulk_channel              *channel,
                           struct bulk_endpoint_descriptor  *remote_ep_desc,
                           struct bulk_channel_callbacks    *callbacks,
                           struct bulk_channel_bind_params  *params,
                           struct waitset                   *ws,
                           struct bulk_continuation          cont);


/**
 * Assign a pool to a channel.
 *
 * @param channel Channel
 * @param pool    Pool to assign (must not be assigned to this channel yet)
 *
 * * There is the pool assigned callback that serves as a continuation for this.
 */
errval_t bulk_channel_assign_pool(struct bulk_channel *channel,
                                  struct bulk_pool    *pool,
                                  struct bulk_continuation cont);

/**
 * Remove a pool from a channel
 *
 * @param channel Channel
 * @param pool    Pool to remove (must be previously assigned to the channel)
 *
 */
errval_t bulk_channel_remove_pool(struct bulk_channel       *channel,
                                  struct bulk_pool          *pool,
                                  struct bulk_continuation   cont);

/**
 * Free a channel
 *
 * @param channel        Channel to be freed
 */
errval_t bulk_channel_destroy(struct bulk_channel      *channel,
                              struct bulk_continuation cont);

/*
 * ---------------------------------------------------------------------------
 * <<< Channel Management
 */



/**
 * Move buffer on the channel. Data and ownership are passed to the other
 * endpoint. After the other endpoint is done with the respective buffer, it can
 * pass it back.
 *
 * @param channel Channel, this endpoint must be source
 * @param buffer  Buffer, must hold ownership and belong to a pool on this
 *                channel
 * @param meta    Pointer to metadata to be passed along with the data
 *                (channel-wide meta_size is used).
 * @param cont    event continuation
 */
errval_t bulk_channel_move(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont);

/**
 * Pass buffer ownership to the other endpoint, the buffer contents are not
 * guaranteed to be transported.
 *
 * @param channel Channel
 * @param buffer  Buffer, must hold ownership and belong to a pool on this
 *                channel
 * @param meta    Pointer to metadata to be passed along with the buffer
 *                (channel-wide meta_size is used).
 * @param cont    event continuation
 */
errval_t bulk_channel_pass(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont);

/**
 * Copy buffer to other endpoint.
 *
 * @param channel Channel, this endpoint must be source
 * @param buffer  Buffer, must belong to a pool on this channel. Must hold
 *                ownersihp, or hold a copy of this buffer.
 * @param meta    Pointer to metadata to be passed along with the buffer
 *                (channel-wide meta_size is used).
 * @param cont    event continuation
 */
errval_t bulk_channel_copy(struct bulk_channel      *channel,
                           struct bulk_buffer       *buffer,
                           void                     *meta,
                           struct bulk_continuation  cont);
/**
 * Release copy received over channel. Must only be called after all outgoing
 * copies from this domain of the same buffer have been released.
 *
 * @param channel Channel, this endpoint must be sink
 * @param buffer  Buffer, must have received it as a copy over this channel, all
 *                outgoing copies must have been released.
 * @param cont    event continuation
 */
errval_t bulk_channel_release(struct bulk_channel       *channel,
                              struct bulk_buffer        *buffer,
                              struct bulk_continuation   cont);


/*
 * ---------------------------------------------------------------------------
 * Pool Management >>>
 */

struct bulk_pool_constraints {
    uintptr_t range_min;
    uintptr_t range_max;
    uintptr_t alignment;
    //TRUST_NONE or TRUST_HALF implies seperate capabilities per buffer
    enum bulk_trust_level   trust;
};

errval_t bulk_pool_alloc(struct bulk_pool             *p,
                         size_t                        buffer_size,
                         size_t                        buffer_count,
                         struct bulk_pool_constraints *constraints);

errval_t bulk_pool_free(struct bulk_pool *pool);


void bulk_emergency_cleanup(void);

#endif /* BULK_TRANSFER_H */

