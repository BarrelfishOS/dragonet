#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>

#include <bulk_transfer/bulk_internal.h>
#include "shm_channel.h"

#define SHM_CHAN_EXTRA 4096
struct shm_chan_meta {
    size_t num_slots;
};

errval_t shm_chan_create(struct shm_channel *chan, const char *name,
                         size_t num_slots, bool sender)
{
    errval_t err;
    int fd, res;
    struct shm_chan_meta *meta;

    // Create and initialize shm area
    fd = shm_open(name, O_CREAT | O_RDWR | O_EXCL, 0600);
    assert_fix(fd != -1);
    res = ftruncate(fd, num_slots * SHM_MSGLEN + SHM_CHAN_EXTRA);
    assert_fix(res == 0);
    meta = mmap(NULL, SHM_CHAN_EXTRA, PROT_READ | PROT_WRITE, MAP_SHARED, fd,
                0);
    assert_fix(meta != MAP_FAILED);

    meta->num_slots = num_slots;

    res = munmap(meta, SHM_CHAN_EXTRA);
    assert_fix(res == 0);
    close(fd);

    err = shm_chan_bind(chan, name, sender);
    chan->creator = true;
    return err;
}

errval_t shm_chan_bind(struct shm_channel *chan, const char *name,
                       bool sender)
{
    int fd, res;
    struct shm_chan_meta *meta;

    fd = shm_open(name, O_RDWR, 0600);
    assert_fix(fd != -1);

    // Map meta-data and data area
    meta = mmap(NULL, SHM_CHAN_EXTRA, PROT_READ | PROT_WRITE, MAP_SHARED, fd,
                0);
    assert_fix(meta != MAP_FAILED);
    chan->data = mmap(NULL, meta->num_slots * SHM_MSGLEN,
                      PROT_READ | PROT_WRITE, MAP_SHARED, fd, SHM_CHAN_EXTRA);
    assert_fix(chan->data != MAP_FAILED);

    chan->size = meta->num_slots;
    chan->sender = sender;
    chan->creator = false;
    chan->current = 0;
    chan->name = name;

    // Unmap meta area
    res = munmap(meta, SHM_CHAN_EXTRA);
    assert_fix(res == 0);

    return SYS_ERR_OK;
}

errval_t shm_chan_release(struct shm_channel *chan)
{
    int res;

    res = munmap(chan->data, chan->size * SHM_MSGLEN);
    assert_fix(res == 0);

    if (chan->creator) {
        res = shm_unlink(chan->name);
        assert_fix(res == 0);
    }

    return SYS_ERR_OK;
}

static bool ws_poll(struct waitset_chanstate *wscs)
{
    struct shm_channel_wsstate *wss = (struct shm_channel_wsstate *) wscs;
    struct shm_message *msg;

    if (!err_is_ok(shm_chan_poll(wss->chan, &msg))) {
        return false;
    }

    wss->msg_handler(msg, wss->opaque);
    return true;
}

void shm_waitset_register(
                struct shm_channel *chan,
                struct shm_channel_wsstate *wscs,
                void (*msg_handler)(struct shm_message *msg, void *opaque),
                void *opaque, struct waitset *ws)
{
    wscs->chan = chan;
    wscs->msg_handler = msg_handler;
    wscs->opaque = opaque;
    wscs->wscs.poll = ws_poll;
    ws_addchan(ws, &wscs->wscs);
}

