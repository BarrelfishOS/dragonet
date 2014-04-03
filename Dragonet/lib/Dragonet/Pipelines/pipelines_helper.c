#define _GNU_SOURCE
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
#include <inttypes.h>

#include <barrelfish/waitset.h>
#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_linuxshm.h>
#include <bulk_transfer/bulk_allocator.h>

#include <implementation.h>

#define SHARED_STATE_SIZE 8192
#define QUEUE_SLOTS 512
#define BULK_BUFFERSZ 2048
#define BULK_BUFFERNUM 512

//#define dprintf printf
#define dprintf(...) do {} while (0)


struct dragonet_shared_state {
    size_t count;
    volatile size_t ch_created;
    volatile size_t ch_bound;
    volatile uint32_t pl_id_alloc;

    struct state state;
};

struct dragonet_pipeline {
    struct dragonet_shared_state *shared;
    const char *name;
    const char *stackname;
    uint32_t id;
    struct waitset ws;

    struct bulk_pool pool;
    struct bulk_allocator alloc;

    struct input *queue;
};

struct dragonet_queue {
    struct dragonet_pipeline *pl;
    char *name;

    struct input *pending;
};

void pg_state_init(struct state *st);

void init_shared_state(const char *name, size_t chancount)
{
    int fd, res;
    struct dragonet_shared_state *dss;
    dprintf("init_shared_state(%s,%"PRIx64")\n", name, chancount);

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
    pg_state_init(&dss->state);
    /*dss->gstate_len = gs_len;
    memcpy(dss->gstate, gs, gs_len);*/

    munmap(dss, SHARED_STATE_SIZE);
    close(fd);

}

pipeline_handle_t pl_init(const char *stackname, const char *plname)
{
    int fd;
    struct dragonet_pipeline *pl;
    errval_t err;
    dprintf("pl_init(%s,%s)\n", stackname, plname);

    pl = calloc(1, sizeof(*pl));
    pl->name = plname;
    pl->stackname = stackname;
    ws_init(&pl->ws);

    // Map shared state
    fd = shm_open(stackname, O_RDWR, 0600);
    assert(fd != -1);
    pl->shared = mmap(NULL, SHARED_STATE_SIZE, PROT_READ | PROT_WRITE,
                      MAP_SHARED, fd, 0);
    assert(pl->shared != MAP_FAILED);
    close(fd);

    pl->id = __sync_fetch_and_add(&pl->shared->pl_id_alloc, 1);

    // Make sure we get a unique machine id for each pipeline, so we get unique
    // pool ids
    extern uint32_t bulk_machine_id;
    bulk_machine_id = pl->id;

    // Allocate pool for this pipeline
    err = bulk_pool_alloc(&pl->pool, BULK_BUFFERSZ, BULK_BUFFERNUM, NULL);
    err_expect_ok(err);

    err = bulk_alloc_init_pool(&pl->alloc, &pl->pool);
    err_expect_ok(err);

    return pl;
}

struct state *pl_get_state(pipeline_handle_t plh)
{
    struct dragonet_pipeline *pl = plh;
    return &pl->shared->state;
}

static void cb_assign_done(void *arg, errval_t err, struct bulk_channel *chan)
{
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;
    err_expect_ok(err);
    __sync_fetch_and_add(&pl->shared->ch_bound, 1);
    dprintf("cb_assign_done: pl=%s bound=%d count=%d\n", pl->name, (int)
            pl->shared->ch_bound, (int) pl->shared->count);
}


static errval_t cb_bind_received(struct bulk_channel *chan)
{
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;

    dprintf("cb_bind_received: %s\n", pl->name);
    return SYS_ERR_OK;
}

static errval_t cb_pool_assigned(struct bulk_channel *chan,
                                 struct bulk_pool    *pool)
{
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;
    dprintf("cb_pool_assigned: %s\n", pl->name);
    return SYS_ERR_OK;
}

static void cb_pass_done(void *arg, errval_t err, struct bulk_channel *chan)
{
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;
    //printf("cb_pass_done: pl=%s\n", pl->name);
    err_expect_ok(err);
}


static void cb_move_received(struct bulk_channel *chan,
                             struct bulk_buffer  *buffer,
                             void                *meta)
{
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;
    struct input *in, *prev;
    errval_t err;
    uint32_t *len = meta;

    dprintf("cb_move_received: %s %"PRIx32"\n", pl->name, buffer->bufferid);
    if (q->pending == NULL) {
        q->pending = input_alloc();

        in = buffer->address;
        // Copy attributes
        memcpy(&q->pending->offset_l2, &in->offset_l2,
                sizeof(*in) - offsetof(struct input, offset_l2));
        q->pending->next = NULL;
    } else {
        in = q->pending;
        q->pending = NULL;

        input_copy_packet(in, buffer->address, *len);

        // Add packet to pl->queue
        in->next = NULL;
        if (pl->queue == NULL) {
            pl->queue = in;
        } else {
            prev = pl->queue;
            while (prev->next != NULL) {
                prev = prev->next;
            }
            prev->next = in;
        }
    }

    err = bulk_channel_pass(chan, buffer, NULL,
            MK_BULK_CONT(cb_pass_done, NULL));
    err_expect_ok(err);
}

static void cb_buffer_received(struct bulk_channel *chan,
                               struct bulk_buffer  *buffer,
                               void                *meta)
{
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;
    dprintf("cb_buffer_received: %s\n", pl->name);
    bulk_alloc_return_buffer(&pl->alloc, buffer);
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
    struct dragonet_queue *dq = calloc(1, sizeof(*dq));

    asprintf(&dq->name, "%s_%s", pl->stackname, name);
    dprintf("pl_inqueue_create: pl=%s q=%s\n", pl->name, dq->name);

    err = bulk_linuxshm_ep_create(epd, dq->name, QUEUE_SLOTS);
    err_expect_ok(err);

    dq->pl = pl;
    chan->user_state = dq;
    err = bulk_channel_create(chan, &epd->generic, &cbs, &setup);
    err_expect_ok(err);

    __sync_fetch_and_add(&pl->shared->ch_created, 1);
    dprintf("pl_inqueue_create: pl=%s created=%d count=%d\n", pl->name, (int)
            pl->shared->ch_created, (int) pl->shared->count);

    return chan;
}

static void cb_bind_done(void *arg, errval_t err, struct bulk_channel *chan)
{
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;
    err_expect_ok(err);
    dprintf("cb_bind_done: pl=%s bound=%d count=%d\n", pl->name, (int)
            pl->shared->ch_bound, (int) pl->shared->count);

    err = bulk_channel_assign_pool(chan, &pl->pool,
            MK_BULK_CONT(cb_assign_done,NULL));
    err_expect_ok(err);
}

queue_handle_t pl_outqueue_bind(pipeline_handle_t plh, const char *name)
{
    struct dragonet_pipeline *pl = plh;
    struct bulk_linuxshm_endpoint_descriptor *epd = malloc(sizeof(*epd));
    struct bulk_channel *chan = malloc(sizeof(*chan));
    errval_t err;
    struct bulk_channel_bind_params params = { .role = BULK_ROLE_MASTER,
        .trust = BULK_TRUST_FULL, .waitset = &pl->ws };
    struct dragonet_queue *dq = calloc(1, sizeof(*dq));


    // Wait until all channels are created
    // TODO: can we avoid the busy wait here?
    while (pl->shared->ch_created != pl->shared->count) {
        ws_event_dispatch_nonblock(&pl->ws);
    }

    asprintf(&dq->name, "%s_%s", pl->stackname, name);
    dprintf("pl_inqueue_bind: pl=%s q=%s\n", pl->name, dq->name);

    err = bulk_linuxshm_ep_create(epd, dq->name, QUEUE_SLOTS);
    err_expect_ok(err);

    dq->pl = pl;
    chan->user_state = dq;
    err = bulk_channel_bind(chan, &epd->generic, &cbs, &params,
            MK_BULK_CONT(cb_bind_done, NULL));
    err_expect_ok(err);

    return chan;

}

void pl_wait_ready(pipeline_handle_t plh)
{
    struct dragonet_pipeline *pl = plh;
    dprintf("pl_wait_ready: pl=%s bound=%"PRId64" count=%"PRId64"\n",
            pl->name, pl->shared->ch_bound, pl->shared->count);
    // TODO: busy wait?
    while (pl->shared->ch_bound != pl->shared->count) {
        ws_event_dispatch_nonblock(&pl->ws);
    }
    dprintf("pl_wait_ready: end pl=%s\n", pl->name);
}

static void cb_move_done(void *arg, errval_t err, struct bulk_channel *chan)
{
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;
    //printf("cb_move_done: pl=%s\n", pl->name);
    err_expect_ok(err);
}

void pl_enqueue(queue_handle_t queue, struct input *in)
{
    struct bulk_channel *chan = queue;
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;
    struct bulk_buffer *in_buf, *data_buf;
    uint32_t len = in->len;
    errval_t err;

    //printf("pl_enqueue: pl=%s q=%s\n", q->name, pl->name);

    in_buf = bulk_alloc_new_buffer(&pl->alloc);
    assert(in_buf != NULL);
    data_buf = bulk_alloc_new_buffer(&pl->alloc);
    assert(data_buf != NULL);

    memcpy(in_buf->address, in, sizeof(*in));
    memcpy(data_buf->address, in->data, in->len);

    err = bulk_channel_move(chan, in_buf, &len,
            MK_BULK_CONT(cb_move_done, NULL));
    err_expect_ok(err);

    err = bulk_channel_move(chan, data_buf, &len,
            MK_BULK_CONT(cb_move_done, NULL));
    err_expect_ok(err);
}

void pl_process_events(pipeline_handle_t plh)
{
    struct dragonet_pipeline *pl = plh;
    errval_t err;
    // Note: It might be necessary to bound this loop somehow as soon as we
    // start looking into performance
    do {
        err = ws_event_dispatch_nonblock(&pl->ws);
    } while (err_is_ok(err));
}

struct input *pl_poll(pipeline_handle_t plh)
{
    struct dragonet_pipeline *pl = plh;
    struct input *in = NULL;
    errval_t err;

    // When queue is empty, process pending events until there is a packet in
    // the queue or there are no more pending events.
    if (pl->queue == NULL) {
        do {
            err = ws_event_dispatch_nonblock(&pl->ws);
        } while (pl->queue == NULL && err_is_ok(err));
    }
    if (pl->queue != NULL) {
        in = pl->queue;
        pl->queue = in->next;
        /*puts("\n\n\n-------------------------------------------------------");
        printf("Success polling! :-D\n");*/
    }
    return in;
}

