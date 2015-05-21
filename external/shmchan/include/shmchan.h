#ifndef SHMCHAN_H
#define SHMCHAN_H

#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

#define CACHE_LINE 64
#define SHM_MSGLEN(t) \
    (((sizeof(t) + sizeof(uintptr_t) + CACHE_LINE - 1) \
      / CACHE_LINE) * CACHE_LINE)

#define SHM_FLAGS_TXDONE 0x1

struct shm_channel {
    void *data;
    size_t size;
    size_t current;
    bool sender;
    bool creator;
    char *name;
};

struct shm_channel_wsstate {
    struct waitset_chanstate wscs;
    struct shm_channel *chan;
    size_t slotsz;
    void (*msg_handler)(void *msg, void *opaque);
    void *opaque;
};

errval_t shmchan_create_(struct shm_channel *chan, const char *name,
                        size_t slotsz, size_t num_slots, bool sender);
errval_t shmchan_bind_(struct shm_channel *chan, const char *name,
                       size_t slotsz, bool sender);
errval_t shmchan_release_(struct shm_channel *chan, size_t slotsz);
void shmchan_show_(struct shm_channel *chan);
void shmchan_waitset_register_(
                struct shm_channel *chan,
                size_t slotsz,
                struct shm_channel_wsstate *wscs,
                void (*msg_handler)(void *msg, void *opaque),
                void *opaque, struct waitset *ws);


static inline void *shmchan_slot_(struct shm_channel *chan,
                                  size_t slotsz, size_t index)
{
    return (void *) ((uintptr_t) chan->data + slotsz * index);
}

static inline volatile uintptr_t *shmchan_slotflags_(void *msg,
                                                     size_t slotsz)
{
    return (volatile uintptr_t *)
        ((uintptr_t) msg + slotsz - sizeof(uintptr_t));
}

/** RX: Poll for a new message */
static inline errval_t shmchan_poll_(struct shm_channel  *chan,
                                     size_t               slotsz,
                                     void               **message)
{
    volatile uintptr_t *flags;
    void *msg;
    assert(!chan->sender);

    msg = shmchan_slot_(chan, slotsz, chan->current);
    flags = shmchan_slotflags_(msg, slotsz);

    if ((*flags & SHM_FLAGS_TXDONE) == 0) {
        return SHM_CHAN_NOMSG;
    }

    *message = msg;
    chan->current = (chan->current + 1) % chan->size;
    return SYS_ERR_OK;
}

#define SHMCHAN(prefix,type)                                                   \
    static inline errval_t prefix##create(struct shm_channel *chan,            \
                                          const char *name,                    \
                                          size_t num_slots,                    \
                                          bool sender)                         \
    {                                                                          \
        return shmchan_create_(chan, name, SHM_MSGLEN(type), num_slots,        \
                               sender);                                        \
    }                                                                          \
                                                                               \
    static inline errval_t prefix##bind(struct shm_channel *chan,              \
                                        const char *name,                      \
                                        bool sender)                           \
    {                                                                          \
        return shmchan_bind_(chan, name, SHM_MSGLEN(type), sender);            \
    }                                                                          \
                                                                               \
    static inline errval_t prefix##release(struct shm_channel *chan)           \
    {                                                                          \
        return shmchan_release_(chan, SHM_MSGLEN(type));                       \
    }                                                                          \
                                                                               \
    static inline void prefix##waitset_register(                               \
                    struct shm_channel *chan,                                  \
                    struct shm_channel_wsstate *wscs,                          \
                    void (*msg_handler)(type *msg, void *opaque),              \
                    void *opaque, struct waitset *ws)                          \
    {                                                                          \
        shmchan_waitset_register_(chan,                                        \
                SHM_MSGLEN(type), wscs,                                        \
                (void (*)(void*,void*)) msg_handler,                           \
                opaque, ws);                                                   \
    }                                                                          \
                                                                               \
    static inline type *prefix##slot(struct shm_channel *chan,                 \
                                     size_t index)                             \
    {                                                                          \
        return (type *) shmchan_slot_(chan, SHM_MSGLEN(type), index);          \
    }                                                                          \
                                                                               \
    static inline volatile uintptr_t *prefix##slotflags(type *msg)             \
    {                                                                          \
        return shmchan_slotflags_(msg, SHM_MSGLEN(type));                      \
    }                                                                          \
                                                                               \
    /** TX: Get next slot for sending */                                       \
    static inline errval_t prefix##alloc(struct shm_channel *chan,             \
                                         type              **message)          \
    {                                                                          \
        type *msg;                                                             \
        volatile uintptr_t *flags;                                             \
        assert(chan->sender);                                                  \
                                                                               \
        msg = prefix##slot(chan, chan->current);                               \
        flags = prefix##slotflags(msg);                                        \
                                                                               \
        if ((*flags & SHM_FLAGS_TXDONE) != 0) {                                \
            /* printf("alloc_failed\n"); */                                    \
            /* shmchan_show_(chan); */                                         \
            return SHM_CHAN_NOSPACE;                                           \
        }                                                                      \
                                                                               \
        chan->current = (chan->current + 1) % chan->size;                      \
        *message = msg;                                                        \
        return SYS_ERR_OK;                                                     \
    }                                                                          \
                                                                               \
    /** TX: Send next slot */                                                  \
    static inline void prefix##send(struct shm_channel *chan,                  \
                                     type               *msg)                  \
    {                                                                          \
        volatile uintptr_t *flags;                                             \
        assert(chan->sender);                                                  \
                                                                               \
        flags = prefix##slotflags(msg);                                        \
                                                                               \
        *flags |= SHM_FLAGS_TXDONE;                                            \
    }                                                                          \
                                                                               \
    /** RX: Poll for a new message */                                          \
    static inline errval_t prefix##poll(struct shm_channel  *chan,             \
                                        type               **message)          \
    {                                                                          \
        return shmchan_poll_(chan, SHM_MSGLEN(type), (void **) message);       \
    }                                                                          \
                                                                               \
    /** RX: Free processed message */                                          \
    static inline void prefix##free(struct shm_channel *chan,                  \
                                    type               *message)               \
    {                                                                          \
        volatile uintptr_t *flags;                                             \
        assert(!chan->sender);                                                 \
                                                                               \
        flags = prefix##slotflags(message);                                    \
        assert((*flags & SHM_FLAGS_TXDONE) != 0);                              \
                                                                               \
        *flags &= ~SHM_FLAGS_TXDONE;                                           \
    }                                                                          \
    /** show debug information about channel */                                \
    static inline void prefix##show(struct shm_channel  *chan)                 \
    {                                                                          \
        return shmchan_show_(chan);                                            \
    }                                                                          \

#endif

