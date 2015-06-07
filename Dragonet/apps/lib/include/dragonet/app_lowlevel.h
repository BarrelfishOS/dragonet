/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef DRAGONET_APP_LOWLEVEL_H_
#define DRAGONET_APP_LOWLEVEL_H_

#include <stdint.h>

#include <barrelfish/barrelfish.h>

#include <implementation.h>

#include <app_control.h> // for flags
#define DNAL_FLAGS_MORE APPCTRL_MSG_FLAGS_MORE
typedef app_flags_t dnal_flags_t; // uint32_t

// The following structures are opaque to applications

/**
 * Connection of the application to the network stack.
 * Concepitionally this is a queue pair packets can be received on or sent out
 * through. An application can have multiple application connections (e.g. for
 * different threads).
 */
struct dnal_app_queue;

/**
 * A socket handle represents a particular socket on a particular application
 * queue.
 * It will be used to send out packets from the respective socket through the
 * respective application queue, as well as to specify the destination socket
 * for packets received on the respective application queue.
 */
struct dnal_socket_handle;


enum dnal_aq_event_type {
    DNAL_AQET_INPACKET,
};

/** Event received on app queue */
struct dnal_aq_event {
    enum dnal_aq_event_type type;
    union {
        struct {
            /** Socket handle this packet is destined for */
            struct dnal_socket_handle *socket;
            /** Buffer for received packet */
            struct input *buffer;
        } inpacket;
    } data;
};


enum dnal_net_destination_type {
    DNAL_NETDSTT_IP4UDP,
};

/** Specifies a network destination. */
struct dnal_net_destination {
    enum dnal_net_destination_type type;
    union {
        struct {
            // 0 can be used as a wildcard
            uint32_t ip_local;
            uint32_t ip_remote;
            uint16_t port_local;
            uint16_t port_remote;
        } ip4udp;
    } data;
};


/******************************************************************************/
/* Application queues */


/**
 * Create application queue
 *
 * @param stackname Label for the stack to connect to
 *                    (currently always `Dragonet')
 * @param slotname  Label for slot this queue connects to on Dragonet side
 *                    (can only connect one application queue to each slot)
 * @param appqueue  Location to store handle
 *
 * @param flags Flags
 */
errval_t dnal_aq_create(const char  *stackname,
                        const char  *slotname,
                        struct dnal_app_queue **appqueue,
                        dnal_flags_t flags);

/**
 * Destroy application queue (not implemented).
 * All sockets created on or spanned to this app queue need to be destroyed
 * first.
 *
 * @param appqueue Handle for app queue to destroy
 */
errval_t dnal_aq_destroy(struct dnal_app_queue *appqueue);

/**
 * Poll application queue for an event
 *
 * @param appqueue Application queue to poll
 * @param event    Location to store the received event
 *
 * @return Four cases to handle:
 *   - If an event is found, SYS_ERR_OK is returned
 *   - If internall processing is done, but no event is generated
 *       DNERR_EVENT_ABORT will be returned. Polling again immediately could
 *       return an event.
 *   - If no event was found, DNERR_NOEVENT
 *   - Other error codes might be returned in case of failure
 */
errval_t dnal_aq_poll(struct dnal_app_queue *appqueue,
                      struct dnal_aq_event  *event);

/**
 * Allocate new buffer for use on this queue.
 *
 * NOTE: Eventually we should decopule buffer allocation from application
 * queues.
 *
 * @param appqueue Application queue to allocate buffer from
 * @param buffer   Location to store pointer to the buffer
 */
errval_t dnal_aq_buffer_alloc(struct dnal_app_queue *appqueue,
                              struct input          **buffer);

/**
 * Free buffer
 *
 * @param appqueue Application queue to free buffer to
 * @param buffer   Buffer to free
 */
errval_t dnal_aq_buffer_free(struct dnal_app_queue *appqueue,
                             struct input          *buffer);

/**
 * Get pointer to shared global dragonet state.
 */
struct state *dnal_aq_state(struct dnal_app_queue *appqueue);


/******************************************************************************/
/* Sockets */



/**
 * Create new socket.
 * Note this socket needs to be bound to a network endpoint before it can be
 * used.
 *
 * @param appqueue     App queue to create socket on
 * @param sockethandle Location to store socket handle
 */
errval_t dnal_socket_create(struct dnal_app_queue      *appqueue,
                            struct dnal_socket_handle **sockethandle);

/**
 * Bind socket to network endpoint.
 *
 * @param sockethandle Socket handle
 * @param destination  Network endpoint to bind to
 */
errval_t dnal_socket_bind(struct dnal_socket_handle   *sockethandle,
                          struct dnal_net_destination *destination,
                          dnal_flags_t flags);

/**
 *  Register flows in a socket
 *
 * @param sockethandle Socket handle
 * @param flow  Flow to register
 */
errval_t dnal_socket_register_flow(struct dnal_socket_handle *sockhandle,
                                   struct dnal_net_destination *flow,
                                   dnal_flags_t flags);

/**
 * Span socket to other queue.
 * Creates a new socket handle that can be used to receive packets that belong
 * to the specified socket on the specified application queue. The new socket
 * handle can also be used to send out packets from this socket using the new
 * application queue.
 * Note: This provides no kind of guarantees about which queue which packets
 *       will be received on.
 *
 * @param orig         Handle for socket to be spanned (must be bound)
 * @param newqueue     App queue to create socket on
 * @param sockethandle Undbound socket handle (from socket_create) on newqueue
 */
errval_t dnal_socket_span(struct dnal_socket_handle *orig,
                          struct dnal_app_queue *newqueue,
                          struct dnal_socket_handle *sockethandle,
                          dnal_flags_t flags);

/**
 * Close particular socket handle.
 * Other handles to the same socket will remain untouched.
 *
 * @param sockethandle Handle to be closed
 */
errval_t dnal_socket_close(struct dnal_socket_handle *sockethandle);

/**
 * Send out data on a socket handle.
 *
 * @param sockethandle Handle to send on
 * @param buffer       Buffer to send out
 * @param dest         Network destination to send to. Can be NULL for
 *                       flow-based sockets (UDP flows, or TCP connections in
 *                       the future).
 */
errval_t dnal_socket_send(struct dnal_socket_handle   *sockethandle,
                          struct input                *buffer,
                          struct dnal_net_destination *dest);

/**
 * Reads out the per-socket opaque value saved previously, or NULL if not
 * initialized.
 */
void *dnal_socket_opaque_get(struct dnal_socket_handle *sockethandle);

/**
 * Set the per-socket opaque value.
 */
void dnal_socket_opaque_set(struct dnal_socket_handle *sockethandle,
                            void        *opaque);

/**
 * No-op
 */
errval_t dnal_noop(struct dnal_app_queue *appqueue,
                   dnal_flags_t flags);

#endif // ndef DRAGONET_APP_LOWLEVEL_H_

