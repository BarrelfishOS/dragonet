/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <string.h> // strdup()

#include <shmchan.h>
#define CANARY_VAL      (0x34242c)
#define SHM_CHAN_EXTRA 4096
struct shm_chan_meta {
    size_t num_slots;
    uint64_t canary;
    uint8_t ready;
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
    if (meta == MAP_FAILED) {
        perror("mmap meta create: ");
    }
    assert_fix(meta != MAP_FAILED);

    meta->num_slots = num_slots;
    meta->canary = CANARY_VAL;
    meta->ready = 1;
    __sync_synchronize();
    //
    // Not sure about the above memory barrier, but the two calls below have to
    // do with synchronizing to disk, which for a shm_open() file descriptor
    // does not make a lot of sense -AKK
    //syncfs(fd);
    //sync();

    res = munmap(meta, SHM_CHAN_EXTRA);
    assert_fix(res == 0);
    close(fd);
    // ditto
    //sync();

    //printf("%s(): channel %s (%p) created\n", __FUNCTION__, name, chan);

    err = shmchan_bind_(chan, name, slotsz, sender);
    if (err == SHM_CHAN_NOTREADY) {
        printf("Number of slots requested is %zu here\n", num_slots);
        assert(!"failed in shmchan_create");
    }
    chan->creator = true;
    return err;
}

// Shows the details about channel for debugging purposes
void shmchan_show_(struct shm_channel *chan)
{
    assert(chan != NULL);
    printf("CHANNEL_DETAILS: chan:%p: shm_name %s, no_of_slots = %zu, "
            " current = %zu, dataLoc = %p, isSender=%d, isCreator=%d\n",
                chan, chan->name, chan->size,
                chan->current, chan->data, chan->sender, chan->creator);

} // end function:shmchan_show_

errval_t shmchan_bind_(struct shm_channel *chan, const char *name,
                       size_t slotsz, bool sender)
{
    int fd, res, i;
    volatile struct shm_chan_meta *meta;

    fd = shm_open(name, O_RDWR, 0600);
    if (fd == -1 && errno == ENOENT) {
        return SHM_CHAN_NOTCREATED;
    }
    assert_fix(fd != -1);

    // Map meta-data and data area
    meta = mmap(NULL, SHM_CHAN_EXTRA, PROT_READ | PROT_WRITE, MAP_SHARED, fd,
                0);
    if (meta == MAP_FAILED) {
        perror("mmap meta bind: ");
    }
    assert_fix(meta != MAP_FAILED);
    for (i = 0; i < 5; ++i) {
        if ((meta->canary == CANARY_VAL) && (meta->ready == 1)) {
            break;
        }
        printf("#### channel not ready!  shm_name %s, fd=%d, size = %zu (num_slots (%zu) * slotsz (%zu)), isSender=%d\n",
                name, fd,  (size_t)(meta->num_slots * slotsz),
                meta->num_slots, slotsz, sender);
        printf("##### sleeping and trying again: %d < %d\n", i, 5);
        sleep(1);
    }
    assert(meta->canary == CANARY_VAL);
    assert(meta->ready == 1);
    if (i > 0) {
        printf("#### channel ready after %d attempts! (possible race condition) ... \n"
                " ... shm_name %s, fd=%d, size = %zu (num_slots (%zu) * slotsz (%zu)), isSender=%d\n",
                i, name, fd,  (size_t)(meta->num_slots * slotsz),
                meta->num_slots, slotsz, sender);
    }

    chan->data = mmap(NULL, meta->num_slots * slotsz,
                      PROT_READ | PROT_WRITE, MAP_SHARED, fd, SHM_CHAN_EXTRA);
    if (chan->data == MAP_FAILED) {
        perror("mmap data bind: ");
        printf("args are: shm_name %s, fd=%d, size = %zu (num_slots (%zu) * slotsz (%zu)), isSender=%d\n",
                name, fd,  (size_t)(meta->num_slots * slotsz),
                meta->num_slots, slotsz, sender);
        return SHM_CHAN_NOTREADY;
    }
    assert_fix(chan->data != MAP_FAILED);

    chan->size = meta->num_slots;
    chan->sender = sender;
    chan->creator = false;
    chan->current = 0;

    chan->name = strdup(name);
    if (chan->name == NULL) {
        fprintf(stderr, "%s: strdup failed:", __FUNCTION__);
        perror("strdup");
        abort();
    }

    // Unmap meta area
    res = munmap(meta, SHM_CHAN_EXTRA);
    assert_fix(res == 0);

    //printf("%s(): channel %s (%p) binded\n", __FUNCTION__, name, chan);
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

    free(chan->name);

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

