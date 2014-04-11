#ifndef SHM_CHANNEL_H
#define SHM_CHANNEL_H

#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <bulk_transfer/bulk_transfer.h>

#define CACHE_LINE 64
#define SHM_MSGLEN \
    (((sizeof(struct shm_message) + sizeof(uintptr_t) + CACHE_LINE - 1) \
      / CACHE_LINE) * CACHE_LINE)

#define SHM_FLAGS_TXDONE 0x1

enum shm_message_type {
    SHM_MSG_BIND,
    SHM_MSG_BIND_DONE,
    SHM_MSG_ASSIGN,
    SHM_MSG_MOVE,
    SHM_MSG_COPY,
    SHM_MSG_PASS,
    SHM_MSG_RELEASE,
    SHM_MSG_STATUS,
};

struct shm_message {
    enum shm_message_type type;
    union {
        struct {
            uint8_t role;
            uint32_t num_slots;
        } bind_request;
        struct {
            errval_t err;
            uint32_t meta_size;
            uint8_t direction;
        } bind_done;
        struct {
            uint32_t op;
            struct bulk_pool_id pool;
        } assign;
        /* used for move,copy,pass,release */
        struct {
            uint32_t op;
            struct bulk_pool_id pool;
            uint32_t buffer;
            uint32_t meta;
        } buffer;
        struct {
            uint32_t op;
            errval_t err;
        } status;
    } content;
};

struct shm_channel {
    void *data;
    size_t size;
    size_t current;
    bool sender;
    bool creator;
    const char *name;
};

struct shm_channel_wsstate {
    struct waitset_chanstate wscs;
    struct shm_channel *chan;
    void (*msg_handler)(struct shm_message *msg, void *opaque);
    void *opaque;
};

errval_t shm_chan_create(struct shm_channel *chan, const char *name,
                         size_t num_slots, bool sender);
errval_t shm_chan_bind(struct shm_channel *chan, const char *name,
                       bool sender);
errval_t shm_chan_release(struct shm_channel *chan);
void shm_waitset_register(
                struct shm_channel *chan,
                struct shm_channel_wsstate *wscs,
                void (*msg_handler)(struct shm_message *msg, void *opaque),
                void *opaque, struct waitset *ws);

static inline struct shm_message *shm_chan_slot(struct shm_channel *chan,
                                                size_t index)
{
    return (void *) ((uintptr_t) chan->data + SHM_MSGLEN * index);
}

static inline volatile uintptr_t *shm_chan_slotflags(struct shm_message *msg)
{
    return (volatile uintptr_t *)
        ((uintptr_t) msg + SHM_MSGLEN - sizeof(uintptr_t));
}

/** TX: Get next slot for sending */
static inline errval_t shm_chan_alloc(struct shm_channel  *chan,
                                      struct shm_message **message)
{
    struct shm_message *msg;
    volatile uintptr_t *flags;
    assert(chan->sender);

    msg = shm_chan_slot(chan, chan->current);
    flags = shm_chan_slotflags(msg);

    if ((*flags & SHM_FLAGS_TXDONE) != 0) {
        return SHM_CHAN_NOSPACE;
    }

    *message = msg;
    return SYS_ERR_OK;
}

/** TX: Send next slot */
static inline void shm_chan_send(struct shm_channel *chan)
{
    volatile uintptr_t *flags;
    assert(chan->sender);

    flags = shm_chan_slotflags(shm_chan_slot(chan, chan->current));

    *flags |= SHM_FLAGS_TXDONE;
    chan->current = (chan->current + 1) % chan->size;
}

/** RX: Poll for a new message */
static inline errval_t shm_chan_poll(struct shm_channel  *chan,
                                     struct shm_message **message)
{
    volatile uintptr_t *flags;
    struct shm_message *msg;
    assert(!chan->sender);

    msg = shm_chan_slot(chan, chan->current);
    flags = shm_chan_slotflags(msg);

    if ((*flags & SHM_FLAGS_TXDONE) == 0) {
        return SHM_CHAN_NOMSG;
    }

    *message = msg;
    chan->current = (chan->current + 1) % chan->size;
    return SYS_ERR_OK;
}

/** RX: Free processed message */
static inline void shm_chan_free(struct shm_channel *chan,
                                 struct shm_message *message)
{
    volatile uintptr_t *flags;
    assert(!chan->sender);

    flags = shm_chan_slotflags(message);
    assert((*flags & SHM_FLAGS_TXDONE) != 0);

    *flags &= ~SHM_FLAGS_TXDONE;
}

#endif

