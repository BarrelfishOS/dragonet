#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include <shmchan.h>

#define SHM_CHAN_EXTRA 4096
struct shm_chan_meta {
    size_t num_slots;
};

errval_t shmchan_create_(struct shm_channel *chan, const char *name,
                         size_t slotsz, size_t num_slots, bool sender)
{
    errval_t err;
    int fd, res;
    struct shm_chan_meta *meta;

    // Create and initialize shm area
    fd = shm_open(name, O_CREAT | O_RDWR | O_EXCL, 0600);
    if (fd == -1) {
        fprintf(stderr, "%s: shm_open failed: ", __FUNCTION__);
        perror(name);
        abort();
    }
    res = ftruncate(fd, num_slots * slotsz + SHM_CHAN_EXTRA);
    assert_fix(res == 0);
    meta = mmap(NULL, SHM_CHAN_EXTRA, PROT_READ | PROT_WRITE, MAP_SHARED, fd,
                0);
    assert_fix(meta != MAP_FAILED);

    meta->num_slots = num_slots;

    res = munmap(meta, SHM_CHAN_EXTRA);
    assert_fix(res == 0);
    close(fd);

    printf("%s(): channel %s (%p) created\n", __FUNCTION__, name, chan);
    err = shmchan_bind_(chan, name, slotsz, sender);
    chan->creator = true;
    return err;
}

errval_t shmchan_bind_(struct shm_channel *chan, const char *name,
                       size_t slotsz, bool sender)
{
    int fd, res;
    struct shm_chan_meta *meta;

    fd = shm_open(name, O_RDWR, 0600);
    if (fd == -1 && errno == ENOENT) {
        return SHM_CHAN_NOTCREATED;
    }
    assert_fix(fd != -1);

    // Map meta-data and data area
    meta = mmap(NULL, SHM_CHAN_EXTRA, PROT_READ | PROT_WRITE, MAP_SHARED, fd,
                0);
    assert_fix(meta != MAP_FAILED);
    chan->data = mmap(NULL, meta->num_slots * slotsz,
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

    printf("%s(): channel %s (%p) binded\n", __FUNCTION__, name, chan);
    return SYS_ERR_OK;
}

errval_t shmchan_release_(struct shm_channel *chan, size_t slotsz)
{
    int res;

    res = munmap(chan->data, chan->size * slotsz);
    if (res < 0) {
        fprintf(stderr, "%s: channel:%p munmap failed:", __FUNCTION__, chan);
        perror("munmap");
    }

    if (chan->creator) {
        res = shm_unlink(chan->name);
        if (res < 0) {
            fprintf(stderr, "%s: channel:%p shm_unlink failed:", __FUNCTION__, chan);
            perror(chan->name);
        }
    }

    return SYS_ERR_OK;
}

static bool ws_poll(struct waitset_chanstate *wscs)
{
    struct shm_channel_wsstate *wss = (struct shm_channel_wsstate *) wscs;
    void *msg;

    if (!err_is_ok(shmchan_poll_(wss->chan, wss->slotsz, &msg))) {
        return false;
    }

    wss->msg_handler(msg, wss->opaque);
    return true;
}

void shmchan_waitset_register_(
                struct shm_channel *chan,
                size_t slotsz,
                struct shm_channel_wsstate *wscs,
                void (*msg_handler)(void *msg, void *opaque),
                void *opaque, struct waitset *ws)
{
    wscs->chan = chan;
    wscs->slotsz = slotsz;
    wscs->msg_handler = msg_handler;
    wscs->opaque = opaque;
    wscs->wscs.poll = ws_poll;
    ws_addchan(ws, &wscs->wscs);
}

