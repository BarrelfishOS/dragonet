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
#include <signal.h>
#include <stdarg.h>

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

struct dragonet_bulk_meta {
    pktoff_t off;
    pktoff_t len;
};

struct dragonet_termination_handler {
    void (*handler)(pipeline_handle_t,void *);
    pipeline_handle_t *plh;
    void *data;
    struct dragonet_termination_handler *next;
};

struct dragonet_shared_state {
    char name[64];
    volatile bool running;
    size_t count;
    volatile size_t ch_created;
    volatile size_t ch_bound;
    volatile size_t num_running;
    volatile uint32_t pl_id_alloc;

    // HACK: Only works as long as the pipelines are executing in the same
    // address space
    struct dragonet_termination_handler *term;
    pid_t control_pid;

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
    struct dragonet_termination_handler *term;
};

struct dragonet_queue {
    struct dragonet_pipeline *pl;
    char *name;

    struct input *pending;
};

// HACK: Don't know where else to put this for the signal handlers
static struct dragonet_shared_state *sig_dss = NULL;

void pg_state_init(struct state *st);

static void signal_abort(int sig)
{
    struct dragonet_termination_handler *th;

    printf("Oops received signal: %s\n", strsignal(sig));
    puts("Terminating ASAP");

    if (sig_dss == NULL) {
        puts("Oh no, sig_dss=NULL, nothing we can do. :'(");
        _exit(-1);
    }

    th = sig_dss->term;
    while (th != NULL) {
        th->handler(th->plh, th->data);
        th = th->next;
    }
    puts("Done with cleanup");
    _exit(-1);
}

// Eventually this should be the nicer variant, that induces regular termination
static void signal_terminate(int sig)
{
    signal_abort(sig);
}

static void dss_cleanup_handler(struct dragonet_shared_state *dss,
        void (*handler)(pipeline_handle_t,void *), void * data)
{
    struct dragonet_termination_handler *h = malloc(sizeof(*h));
    h->handler = handler;
    h->plh = NULL;
    h->data = data;
    h->next = dss->term;
    dss->term = h;
}

static void shared_state_cleanup(pipeline_handle_t plh, void *data)
{
    struct dragonet_shared_state *dss = data;
    shm_unlink(dss->name);
    bulk_emergency_cleanup();
}

void* init_shared_state(const char *name, size_t chancount)
{
    int fd, res;
    struct dragonet_shared_state *dss;
    sighandler_t sigh;
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

    strncpy(dss->name, name, sizeof(dss->name));
    dss->name[sizeof(dss->name) - 1] = 0;

    dss->running = true;
    dss->count = chancount;
    dss->control_pid = getpid();
    pg_state_init(&dss->state);
    /*dss->gstate_len = gs_len;
    memcpy(dss->gstate, gs, gs_len);*/

    // Add some signal handlers to deal with problems during execution, so we
    // can at least do some minimal cleanup (i.e. stopping the NIC from writing
    // to memory.
    sig_dss = dss;
    sigh = signal(SIGTERM, signal_terminate);
    sigh = signal(SIGINT,  signal_terminate);
    sigh = signal(SIGHUP,  signal_terminate);
    sigh = signal(SIGUSR1, signal_terminate);
    sigh = signal(SIGUSR2, signal_terminate);
    sigh = signal(SIGTERM, signal_abort);
    sigh = signal(SIGABRT, signal_abort);
    sigh = signal(SIGFPE,  signal_abort);
    sigh = signal(SIGILL,  signal_abort);
    sigh = signal(SIGPIPE, signal_abort);
    sigh = signal(SIGPIPE, signal_abort);
    sigh = signal(SIGQUIT, signal_abort);
    sigh = signal(SIGSEGV, signal_abort);

    dss_cleanup_handler(dss, shared_state_cleanup, dss);

    //munmap(dss, SHARED_STATE_SIZE);
    close(fd);
    return dss;
}

void stop_stack(void *handle)
{
    struct dragonet_shared_state *dss = handle;
    printf("stop_stack()\n");
    dss->running = false;
    while (dss->num_running > 0) {
    }
    printf("All pipelines terminated\n");

    shm_unlink(dss->name);
    bulk_emergency_cleanup();
}

static void pl_cleanup_irregular(pipeline_handle_t plh, void *data)
{
    struct dragonet_pipeline *pl = plh;
    bulk_emergency_cleanup();
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

    sig_dss = pl->shared;
    __sync_fetch_and_add(&pl->shared->num_running, 1);
    pl->id = __sync_fetch_and_add(&pl->shared->pl_id_alloc, 1);

    pl_cleanup_handler(pl, true, pl_cleanup_irregular, NULL);
    pl_cleanup_handler(pl, false, pl_cleanup_irregular, NULL);

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

bool pl_get_running(pipeline_handle_t plh)
{
    struct dragonet_pipeline *pl = plh;
    return pl->shared->running;
}

void pl_terminated(pipeline_handle_t plh)
{
    struct dragonet_pipeline *pl = plh;
    struct dragonet_termination_handler *h = pl->term;
    while (h != NULL) {
        h->handler(plh, h->data);
        h = h->next;
    }
    __sync_fetch_and_sub(&pl->shared->num_running, 1);
}

void pl_cleanup_handler(pipeline_handle_t plh, bool irregular,
                        void (*handler)(pipeline_handle_t,void *), void * data)
{
    struct dragonet_pipeline *pl = plh;
    struct dragonet_termination_handler *h = malloc(sizeof(*h));
    h->handler = handler;
    h->plh = plh;
    h->data = data;
    if (irregular && pl->shared->control_pid == getpid()) {
        h->next = pl->shared->term;
        pl->shared->term = h;
    } else {
        h->next = pl->term;
        pl->term = h;
    }
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
    struct dragonet_bulk_meta *dbmeta = meta;

    buffer->opaque = chan;
    dprintf("cb_move_received: %s %"PRIx32" len=%d off=%d\n", pl->name,
            buffer->bufferid, dbmeta->len, dbmeta->off);
    if (q->pending == NULL) {
        in = input_struct_alloc();
        in->attr = buffer->address;
        in->attr_buffer = buffer;
        q->pending = in;
    } else {
        in = q->pending;
        q->pending = NULL;

        in->data = (void *) ((uintptr_t) buffer->address + dbmeta->off);
        in->phys = buffer->phys + dbmeta->off;
        in->len = dbmeta->len;
        in->space_before = dbmeta->off;
        in->space_after =
            buffer->pool->buffer_size - (dbmeta->len + dbmeta->off);
        in->data_buffer = buffer;

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

}

static void free_bulk_buffer(struct dragonet_pipeline *pl,
                             struct bulk_buffer *buf)
{
    struct bulk_channel *chan = buf->opaque;
    errval_t err;

    if (chan == NULL) {
        // Our own buffer
        bulk_alloc_return_buffer(&pl->alloc, buf);
    } else {
        // We received it over this channel
        //
        err = bulk_channel_pass(chan, buf, NULL,
            MK_BULK_CONT(cb_pass_done, NULL));
        err_expect_ok(err);
    }
}

static void cb_buffer_received(struct bulk_channel *chan,
                               struct bulk_buffer  *buffer,
                               void                *meta)
{
    struct dragonet_queue *q = chan->user_state;
    struct dragonet_pipeline *pl = q->pl;
    dprintf("cb_buffer_received: %s\n", pl->name);
    free_bulk_buffer(pl, buffer);
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
        .meta_size = sizeof(struct dragonet_bulk_meta), .waitset = &pl->ws };
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
    struct bulk_buffer *data_buf = in->data_buffer;
    struct bulk_buffer *attr_buf = in->attr_buffer;
    struct dragonet_bulk_meta meta = {
        .len = in->len, .off = in->space_before, };
    uint32_t len;
    errval_t err;

    //printf("pl_enqueue: pl=%s q=%s\n", q->name, pl->name);
    err = bulk_channel_move(chan, attr_buf, &meta,
            MK_BULK_CONT(cb_move_done, NULL));
    err_expect_ok(err);

    err = bulk_channel_move(chan, data_buf, &meta,
            MK_BULK_CONT(cb_move_done, NULL));
    err_expect_ok(err);

    // We need to replace these buffers in the current input, and make it empty
    attr_buf = bulk_alloc_new_buffer(&pl->alloc);
    assert(attr_buf != NULL);
    attr_buf->opaque = NULL;

    data_buf = bulk_alloc_new_buffer(&pl->alloc);
    assert(data_buf != NULL);
    data_buf->opaque = NULL;

    in->len = 0;
    in->space_after = 0;
    len = data_buf->pool->buffer_size;
    in->space_before = len;
    in->data = (void *) ((uintptr_t) data_buf->address + len);
    in->phys = data_buf->phys + len;
    in->attr = attr_buf->address;
    in->data_buffer = data_buf;
    in->attr_buffer = attr_buf;
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

void pl_panic(pipeline_handle_t plh, const char *fmt, ...)
{
    struct dragonet_pipeline *pl = plh;
    va_list val;
    fprintf(stderr, "panic in pipeline %s:", pl->name);
    va_start(val, fmt);
    vfprintf(stderr, fmt, val);
    va_end(val);
    abort();
}

buffer_handle_t pl_buffer_alloc(pipeline_handle_t plh, void **buf,
                                uint64_t *phys, size_t *len)
{
    struct dragonet_pipeline *pl = plh;
    struct bulk_buffer *buffer;

    buffer = bulk_alloc_new_buffer(&pl->alloc);
    assert(buffer != NULL);
    buffer->opaque = NULL;

    if (buf != NULL) {
        *buf = buffer->address;
    }
    if (phys != NULL) {
        *phys = buffer->phys;
    }
    if (len != NULL) {
        *len = buffer->pool->buffer_size;
    }
    return buffer;
}

void pl_buffer_free(pipeline_handle_t plh, buffer_handle_t bufh)
{
    free_bulk_buffer((struct dragonet_pipeline *) plh,
                     (struct bulk_buffer *) bufh);
}

