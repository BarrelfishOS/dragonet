#include "pipelines.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>

#include <barrelfish/waitset.h>
#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_linuxshm.h>

#include <implementation.h>

#define SHARED_STATE_SIZE 8192
#define QUEUE_SLOTS 512

struct dragonet_shared_state {
    size_t count;
    volatile size_t ch_created;
    volatile size_t ch_bound;

    struct state state;
};

struct dragonet_pipeline {
    struct dragonet_shared_state *shared;
    const char *name;
    struct waitset ws;

    void (*incoming)(struct input *, void *);
    void *incoming_param;
};


void init_shared_state(const char *name, size_t chancount)
{
    int fd, res;
    struct dragonet_shared_state *dss;

    assert(sizeof(*dss) <= SHARED_STATE_SIZE);

    // allocate and initialize shared memory area for shared state
    fd = shm_open(name, O_CREAT | O_RDWR | O_EXCL, 0600);
    assert(fd != -1);
    res = ftruncate(fd, SHARED_STATE_SIZE);
    assert(res == 0);

    dss = mmap(NULL, SHARED_STATE_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, fd,
            0);
    assert(dss != MAP_FAILED);

    dss->count = chancount;
    /*dss->gstate_len = gs_len;
    memcpy(dss->gstate, gs, gs_len);*/

    munmap(dss, SHARED_STATE_SIZE);
    close(fd);
}

pipeline_handle_t pl_init(const char *stackname, const char *plname, void *st,
                          void (*incoming)(struct input *, void *))
{
    int fd;
    struct dragonet_pipeline *pl;

    pl = calloc(1, sizeof(*pl));
    pl->name = plname;
    pl->incoming = incoming;
    pl->incoming_param = st;
    ws_init(&pl->ws);

    // Map shared state
    fd = shm_open(stackname, O_RDWR, 0600);
    assert(fd != -1);
    pl->shared = mmap(NULL, SHARED_STATE_SIZE, PROT_READ | PROT_WRITE,
                      MAP_SHARED, fd, 0);
    assert(pl->shared != MAP_FAILED);
    close(fd);

    return pl;
}

static errval_t cb_bind_received(struct bulk_channel *chan)
{
    struct dragonet_pipeline *pl = chan->user_state;
    printf("cb_bind_received: %s\n", pl->name);
    return SYS_ERR_OK;
}

static errval_t cb_pool_assigned(struct bulk_channel *chan,
                                 struct bulk_pool    *pool)
{
    struct dragonet_pipeline *pl = chan->user_state;
    printf("cb_pool_assigned: %s\n", pl->name);
    return SYS_ERR_OK;
}

static void cb_move_received(struct bulk_channel *chan,
                             struct bulk_buffer  *buffer,
                             void                *meta)
{

    struct dragonet_pipeline *pl = chan->user_state;
    printf("cb_move_received: %s\n", pl->name);
}

static void cb_buffer_received(struct bulk_channel *chan,
                               struct bulk_buffer  *buffer,
                               void                *meta)
{

    struct dragonet_pipeline *pl = chan->user_state;
    printf("cb_buffer_received: %s\n", pl->name);
}


static struct bulk_channel_callbacks cbs = {
    .bind_received = cb_bind_received,
    .pool_assigned = cb_pool_assigned,
    .move_received = cb_move_received,
    .buffer_received = cb_buffer_received,
};

queue_handle_t pl_inqueue_create(pipeline_handle_t plh, const char *name)
{
    struct dragonet_pipeline *pl = plh;
    struct bulk_linuxshm_endpoint_descriptor *epd = malloc(sizeof(*epd));
    struct bulk_channel *chan = malloc(sizeof(*chan));
    errval_t err;
    struct bulk_channel_setup setup = { .direction = BULK_DIRECTION_RX,
        .role = BULK_ROLE_SLAVE, .trust = BULK_TRUST_FULL,
        .meta_size = 4, .waitset = &pl->ws };

    printf("pl_inqueue_create: pl=%s q=%s\n", pl->name, name);

    err = bulk_linuxshm_ep_create(epd, name, QUEUE_SLOTS);
    assert(err_is_ok(err));

    chan->user_state = pl;
    err = bulk_channel_create(chan, &epd->generic, &cbs, &setup);
    assert(err_is_ok(err));

    __sync_fetch_and_add(&pl->shared->ch_created, 1);
    printf("pl_inqueue_create: pl=%s created=%d count=%d\n", pl->name, (int)
            pl->shared->ch_created, (int) pl->shared->count);

    return chan;
}

static void cb_bind_done(void *arg, errval_t err, struct bulk_channel *channel)
{
    struct dragonet_pipeline *pl = channel->user_state;
    __sync_fetch_and_add(&pl->shared->ch_bound, 1);
    printf("cb_bind_done: pl=%s bound=%d count=%d\n", pl->name, (int)
            pl->shared->ch_bound, (int) pl->shared->count);
}

queue_handle_t pl_outqueue_bind(pipeline_handle_t plh, const char *name)
{
    struct dragonet_pipeline *pl = plh;
    struct bulk_linuxshm_endpoint_descriptor *epd = malloc(sizeof(*epd));
    struct bulk_channel *chan = malloc(sizeof(*chan));
    errval_t err;
    struct bulk_channel_bind_params params = { .role = BULK_ROLE_SLAVE,
        .trust = BULK_TRUST_FULL, .waitset = &pl->ws };


    // Wait until all channels are created
    // TODO: can we avoid the busy wait here?
    while (pl->shared->ch_created != pl->shared->count) {
        ws_event_dispatch_nonblock(&pl->ws);
    }

    printf("pl_inqueue_bind: pl=%s q=%s\n", pl->name, name);

    err = bulk_linuxshm_ep_create(epd, name, QUEUE_SLOTS);
    assert(err_is_ok(err));

    chan->user_state = pl;
    err = bulk_channel_bind(chan, &epd->generic, &cbs, &params,
            MK_BULK_CONT(cb_bind_done, NULL));
    assert(err_is_ok(err));

    return chan;

}

void pl_wait_ready(pipeline_handle_t plh)
{
    struct dragonet_pipeline *pl = plh;
    // TODO: busy wait?
    while (pl->shared->ch_bound != pl->shared->count) {
        ws_event_dispatch_nonblock(&pl->ws);
    }
}

void pl_eventloop(pipeline_handle_t plh)
{
    struct dragonet_pipeline *pl = plh;
    while (true) {
        ws_event_dispatch(&pl->ws);
    }
}

