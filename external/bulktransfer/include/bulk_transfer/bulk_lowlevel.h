/*
 * Copyright (c) 2013-2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_LOWLEVEL_H
#define BULK_LOWLEVEL_H

#include <bulk_transfer/bulk_internal.h>


/**
 * Specifies the direction of data flow over a channel.
 */
enum bulk_channel_direction {
    BULK_DIRECTION_TX,  ///< This side of the channel is the data source
    BULK_DIRECTION_RX   ///< This side of the channel is the data sink
};

/**
 * The role of the domain with respect to the channel.
 *
 * 1) Creation: upon channel creation the role can either be given or generic
 * 2) Binding: The roles are given either Master-Slave or Slave-Master
 */
enum bulk_channel_role {
    BULK_ROLE_GENERIC,  ///< the role of this endpoint depends on the binding side
    BULK_ROLE_MASTER,   ///< this endpoint is the channel master
    BULK_ROLE_SLAVE     ///< this endpoint is the channel slave
};

/**
 * the trust levels of the channel
 */
enum bulk_trust_level {
    BULK_TRUST_UNINITIALIZED, ///< trust level is not initialized
    BULK_TRUST_NONE,          ///< untrusted case, policies are enforced
    BULK_TRUST_HALF,          ///< same as untrusted, but no revocation of caps
    BULK_TRUST_FULL           ///< complete trust, no unmapping
};

/**
 *
 */
enum bulk_channel_state {
    BULK_STATE_UNINITIALIZED,  ///< channel not initialized, no endpoint assigned
    BULK_STATE_INITIALIZED,    ///< local endpoint assigned, ready for binding
    BULK_STATE_BINDING,        ///< binding is in progress
    BULK_STATE_BIND_NEGOTIATE, ///< channel properties are negotiated (role, trust)
    BULK_STATE_CONNECTED,      ///< binding is completed and ready for use
    BULK_STATE_TEARDOWN,       ///< teardown is initiated
    BULK_STATE_CLOSED          ///< the channel has been closed
};

/**
 * represents the state of a buffer
 */
enum bulk_buffer_state {
    BULK_BUFFER_INVALID,    ///< the buffer is not present XXX: name?
    BULK_BUFFER_READ_ONLY,  ///< the buffer is mapped read only
    BULK_BUFFER_RO_OWNED,   ///< the buffer is copied first
    BULK_BUFFER_READ_WRITE  ///< the buffer is mapped read write
};





/* forward declarations */
struct bulk_ll_channel;
struct bulk_ll_event;
struct bulk_pool;
struct bulk_pool_list;
struct bulk_buffer;
struct bulk_implementation;

/**
 * specifies constraints on the channel. This involves limiting the supported
 * memory range or alignment requirements.
 */
struct bulk_channel_constraints {
    uintptr_t mem_range_min;    ///< minimum physical address supported
    uintptr_t mem_range_max;    ///< maximum physical address supported
    uintptr_t men_align;        ///< minimum memory alignment constraint
};


/**
 * generic bulk endpoint
 *
 * This serves as an abstract representation of an endpoint. This data structure
 * must be part of the implementation specific endpoint struct.
 */
struct bulk_endpoint_descriptor {
    /** Pointer to backend-function pointers for this endpoint */
    struct bulk_implementation *f;
};

/**
    this struct represents the pool id which consists of the domain id of the
    allocator and the domain local allocation counter
    TODO: ensure system wide uniquenes also between different machines
 */
struct bulk_pool_id {
    uint32_t    machine;
    uint32_t    dom;//warning: disp_get_domain_id() is core-local
    uint32_t    local;
};


/**
 * The bulk pool is a continuous region in (virtual) memory that consists of
 * equally sized buffers.
 */
struct bulk_pool {
    struct bulk_pool_id     id;
    /** the base address of the pool */
    void                    *base_address;
    /** the size of a single buffer in bytes */
    size_t                   buffer_size;
    /**  pool trust level depending on first assignment */
    enum bulk_trust_level    trust;
    /** the maximum number of buffers in this pool */
    size_t                   num_buffers;
    /** array of the buffers for this pool (pre allocated) */
    struct bulk_buffer     **buffers;
    /** platform specific data, must be treated as opaque */
    struct bulk_int_pool     internal;
};

/**
 * a list of bulk pools assigned to a channel, keep the list ordered by the id
 */
struct bulk_pool_list {
    struct bulk_pool_list *next;    ///< pointer to the next element
    struct bulk_pool      *pool;    ///< the pool
};

/**
 * a bulk buffer is the base unit for bulk data transfer in the system
 */
struct bulk_buffer {
    /** the virtual address of the buffer */
    void                     *address;
    /** the physical address */
    uintptr_t                 phys;
    /** XXX: maybe we have to use the pool_id here */
    struct bulk_pool         *pool;
    /** index of this buffer within the pool's array of buffers */
    uint32_t                  bufferid;
    /** state of the buffer */
    enum bulk_buffer_state    state;
    /** local refrence counting */
    uint32_t                  local_ref_count;
    /** platform specific data, must be treated as opaque */
    struct bulk_int_buffer    internal;
    void                     *opaque;
};

/**
 * setup parameters for creating a new bulk channel
 */
struct bulk_channel_setup {
    /** Channel direction (RX/TX) */
    enum bulk_channel_direction       direction;
    /** Endpoint role (master/slave) */
    enum bulk_channel_role            role;
    /** trust level for this channel */
    enum bulk_trust_level             trust;
    /** */
    struct bulk_channel_constraints   constraints;
    /** Size of metadata to be passed along with transfers and passed buffers. */
    size_t                            meta_size;
};

/**
 * parameters used on binding ot a channel
 */
struct bulk_channel_bind_params {
    /** Endpoint role (master/slave) */
    enum bulk_channel_role            role;
    /** trust level for this channel */
    enum bulk_trust_level             trust;
    /** the channel constraints */
    struct bulk_channel_constraints   constraints;
};





/**
 * Function pointers provided by an implementation of the bulk transfer
 * mechanism over a specific backend. Functions correspond closely to the
 * public interface.
 *
 * XXX: do we want to give a pointer to the closure or the closure itself?
 *      the event_closure just has two fields, so it may be reasonable to do so.
 *      - RA
 */
struct bulk_implementation {
    errval_t (*channel_create)(struct bulk_ll_channel *channel,
                               bulk_correlation_t      corr);

    errval_t (*channel_bind)(struct bulk_ll_channel *channel,
                             bulk_correlation_t      corr);

    errval_t (*channel_destroy)(struct bulk_ll_channel *channel,
                                bulk_correlation_t      corr);

    errval_t (*pool_assign)(struct bulk_ll_channel *channel,
                            struct bulk_pool       *pool,
                            bulk_correlation_t      corr);

    errval_t (*pool_remove)(struct bulk_ll_channel *channel,
                            struct bulk_pool       *pool,
                            bulk_correlation_t      corr);

    errval_t (*buffer_move)(struct bulk_ll_channel *channel,
                            struct bulk_buffer     *buffer,
                            void                   *meta,
                            bulk_correlation_t      corr);

    errval_t (*buffer_copy)(struct bulk_ll_channel  *channel,
                            struct bulk_buffer      *buffer,
                            void                    *meta,
                            bulk_correlation_t       corr);

    errval_t (*buffer_release)(struct bulk_ll_channel  *channel,
                               struct bulk_buffer      *buffer,
                               bulk_correlation_t       corr);

    errval_t (*buffer_pass)(struct bulk_ll_channel *channel,
                            struct bulk_buffer     *buffer,
                            void                   *meta,
                            bulk_correlation_t      corr);

    errval_t (*event_poll)(struct bulk_ll_channel   *channel,
                             struct bulk_ll_event   *event);

    errval_t (*event_done)(struct bulk_ll_channel  *channel,
                           struct bulk_ll_event    *event,
                           errval_t                 err);
};

/** Handle/Representation for one end of a bulk transfer channel */
struct bulk_ll_channel {
    /** the local endpoint for this channel */
    struct bulk_endpoint_descriptor *ep;
    /** the current channel state */
    enum bulk_channel_state          state;
    /** orderd list of assigned pools to this channel */
    struct bulk_pool_list           *pools;
    /** the direction of data flow */
    enum bulk_channel_direction      direction;
    /** role of this side of the channel */
    enum bulk_channel_role           role;
    /** the trust level of this channel */
    enum bulk_trust_level            trust;
    /** constraints of this channel */
    struct bulk_channel_constraints  constraints;
    /** the size of the transmitted meta information per bulk transfer */
    size_t                           meta_size;
    /** platform specific data, must be treated as opaque */
    struct bulk_int_channel         internal;
    /** pointer to user specific state for this channel */
    void                            *user_state;
    /** implementation specific data */
    void                            *impl_data;
};

enum bulk_ll_event_type {
    BULK_LLEV_ASYNC_DONE,
    BULK_LLEV_CHAN_BIND,    // ERR
    BULK_LLEV_CHAN_DESTROY,
    BULK_LLEV_POOL_ASSIGN,  // ERR
    BULK_LLEV_POOL_REMOVE,  // ERR
    BULK_LLEV_BUF_MOVE,
    BULK_LLEV_BUF_PASS,
    BULK_LLEV_BUF_COPY,
    BULK_LLEV_BUF_RELEASE,
};

struct bulk_ll_event {
    uint8_t type;
    union {
        struct {
            bulk_correlation_t corr;
            errval_t           err;
        } async_done;

        struct { } chan_bind;

        struct { } chan_destroy;

        struct {
            struct bulk_pool *pool;
        } pool;

        struct {
            struct bulk_buffer *buffer;
            void               *meta;
        } buffer;
    } data;
    void *impl_data;
};

/**
 * Create a new channel.
 *
 * @param channel   Pointer to unused channel handle
 * @param ep_desc   Description of endpoint to bind to
 * @param setup     struct containing the setup information
 */
errval_t bulk_ll_channel_create(struct bulk_ll_channel           *channel,
                                struct bulk_endpoint_descriptor  *ep_desc,
                                struct bulk_channel_setup        *setup,
                                bulk_correlation_t                corr);

/**
 * Bind to an existing unbound channel.
 *
 * @param channel   Pointer to unused channel handle
 * @param ep_desc   Description of endpoint to bind to
 * @param params    parameters for the binding process
 */
errval_t bulk_ll_channel_bind(struct bulk_ll_channel          *channel,
                              struct bulk_endpoint_descriptor *remote_ep_desc,
                              struct bulk_channel_bind_params *params,
                              bulk_correlation_t               corr);


/**
 * Assign a pool to a channel.
 *
 * @param channel Channel
 * @param pool    Pool to assign (must not be assigned to this channel yet)
 */
errval_t bulk_ll_channel_assign_pool(struct bulk_ll_channel *channel,
                                     struct bulk_pool       *pool,
                                     bulk_correlation_t      corr);

/**
 * Remove a pool from a channel
 *
 * @param channel Channel
 * @param pool    Pool to remove (must be previously assigned to the channel)
 *
 */
errval_t bulk_ll_channel_remove_pool(struct bulk_ll_channel *channel,
                                     struct bulk_pool       *pool,
                                     bulk_correlation_t      corr);

/**
 * Free a channel
 *
 * @param channel        Channel to be freed
 */
errval_t bulk_ll_channel_destroy(struct bulk_ll_channel *channel,
                                 bulk_correlation_t      corr);


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
 */
errval_t bulk_ll_channel_move(struct bulk_ll_channel *channel,
                              struct bulk_buffer     *buffer,
                              void                   *meta,
                              bulk_correlation_t      corr);

/**
 * Pass buffer ownership to the other endpoint, the buffer contents are not
 * guaranteed to be transported.
 *
 * @param channel Channel
 * @param buffer  Buffer, must hold ownership and belong to a pool on this
 *                channel
 * @param meta    Pointer to metadata to be passed along with the buffer
 *                (channel-wide meta_size is used).
 */
errval_t bulk_ll_channel_pass(struct bulk_ll_channel *channel,
                              struct bulk_buffer     *buffer,
                              void                   *meta,
                              bulk_correlation_t      corr);

/**
 * Copy buffer to other endpoint.
 *
 * @param channel Channel, this endpoint must be source
 * @param buffer  Buffer, must belong to a pool on this channel. Must hold
 *                ownersihp, or hold a copy of this buffer.
 * @param meta    Pointer to metadata to be passed along with the buffer
 *                (channel-wide meta_size is used).
 */
errval_t bulk_ll_channel_copy(struct bulk_ll_channel *channel,
                              struct bulk_buffer     *buffer,
                              void                   *meta,
                              bulk_correlation_t      corr);

/**
 * Release copy received over channel. Must only be called after all outgoing
 * copies from this domain of the same buffer have been released.
 *
 * @param channel Channel, this endpoint must be sink
 * @param buffer  Buffer, must have received it as a copy over this channel, all
 *                outgoing copies must have been released.
 */
errval_t bulk_ll_channel_release(struct bulk_ll_channel *channel,
                                 struct bulk_buffer     *buffer,
                                 bulk_correlation_t      corr);

errval_t bulk_ll_channel_event_poll(struct bulk_ll_channel *channel,
                                    struct bulk_ll_event   *event);

errval_t bulk_ll_channel_event_done(struct bulk_ll_channel *channel,
                                    struct bulk_ll_event   *event,
                                    errval_t                err);




#endif // ndef BULK_LOWLEVEL_H_

