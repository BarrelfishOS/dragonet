#ifndef DRAGONET_APP_LOWLEVEL_H_
#define DRAGONET_APP_LOWLEVEL_H_

#include <stdint.h>

#include <barrelfish/barrelfish.h>

#include <implementation.h>

// These structures are opaque to applications
struct dnal_app_queue;
struct dnal_socket_handle;


/**
 * Connection of the application to the network stack.
 * Concepitionally this is a queue pair packets can be received on or sent out
 * through. An application can have multiple application connections (e.g. for
 * different threads).
 */
typedef struct dnal_app_queue *dnal_appq_t;

/**
 * A socket handle represents a particular socket on a particular application
 * queue.
 * It will be used to send out packets from the respective socket through the
 * respective application queue, as well as to specify the destination socket
 * for packets received on the respective application queue.
 */
typedef struct dnal_socket_handle *dnal_sockh_t;


enum dnal_aq_event_type {
    DNAL_AQET_INPACKET,
};

/** Event received on app queue */
struct dnal_aq_event {
    enum dnal_aq_event_type type;
    union {
        struct {
            /** Socket handle this packet is destined for */
            dnal_sockh_t  socket;
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
 */
errval_t dnal_aq_create(const char  *stackname,
                        const char  *slotname,
                        dnal_appq_t *appqueue);

/**
 * Destroy application queue (not implemented).
 * All sockets created on or spanned to this app queue need to be destroyed
 * first.
 *
 * @param appqueue Handle for app queue to destroy
 */
errval_t dnal_aq_destroy(dnal_appq_t appqueue);

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
errval_t dnal_aq_poll(dnal_appq_t           appqueue,
                      struct dnal_aq_event *event);

/**
 * Allocate new buffer for use on this queue.
 *
 * NOTE: Eventually we should decopule buffer allocation from application
 * queues.
 *
 * @param appqueue Application queue to allocate buffer from
 * @param buffer   Location to store pointer to the buffer
 */
errval_t dnal_aq_buffer_alloc(dnal_appq_t    appqueue,
                              struct input **buffer);

/**
 * Free buffer
 *
 * @param appqueue Application queue to free buffer to
 * @param buffer   Buffer to free
 */
errval_t dnal_aq_buffer_free(dnal_appq_t   appqueue,
                             struct input *buffer);

/**
 * Get pointer to shared global dragonet state.
 */
struct state *dnal_aq_state(dnal_appq_t appqueue);


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
errval_t dnal_socket_create(dnal_appq_t   appqueue,
                            dnal_sockh_t *sockethandle);

/**
 * Bind socket to network endpoint.
 *
 * @param sockethandle Socket handle
 * @param destination  Network endpoint to bind to
 */
errval_t dnal_socket_bind(dnal_sockh_t                 sockethandle,
                          struct dnal_net_destination *destination);

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
errval_t dnal_socket_span(dnal_sockh_t orig,
                          dnal_appq_t  newqueue,
                          dnal_sockh_t sockethandle);

/**
 * Close particular socket handle.
 * Other handles to the same socket will remain untouched.
 *
 * @param sockethandle Handle to be closed
 */
errval_t dnal_socket_close(dnal_sockh_t sockethandle);

/**
 * Send out data on a socket handle.
 *
 * @param sockethandle Handle to send on
 * @param buffer       Buffer to send out
 * @param dest         Network destination to send to. Can be NULL for
 *                       flow-based sockets (UDP flows, or TCP connections in
 *                       the future).
 */
errval_t dnal_socket_send(dnal_sockh_t                 sockethandle,
                          struct input                *buffer,
                          struct dnal_net_destination *dest);

/**
 * Reads out the per-socket opaque value saved previously, or NULL if not
 * initialized.
 */
void *dnal_socket_opaque_get(dnal_sockh_t sockethandle);

/**
 * Set the per-socket opaque value.
 */
void dnal_socket_opaque_set(dnal_sockh_t sockethandle,
                            void        *opaque);

#endif // ndef DRAGONET_APP_LOWLEVEL_H_

