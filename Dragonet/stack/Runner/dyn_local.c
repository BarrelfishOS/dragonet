#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "dyn_local.h"

extern pipeline_handle_t pipeline_handle;

struct qaction {
    struct dynr_action act;
    struct qaction *next;
};

struct dyn_local {
    struct dynr_client client;

    pipeline_handle_t plh;
    struct dynr_server server;
    volatile bool running;

    pthread_mutex_t qlock;
    struct qaction * volatile qhead;
    struct qaction * volatile qtail;
};

static void enqueue_action(struct dynr_action *act, void *data)
{
    struct dyn_local *l = data;
    struct qaction *qa = malloc(sizeof(*qa));
    struct qaction *q;
    //printf("enqueue(%p)\n", qa);

    memcpy(&qa->act, act, sizeof(*act));
    qa->next = NULL;

    pthread_mutex_lock(&l->qlock);
    q = l->qtail;
    if (q == NULL) {
        l->qhead = qa;
    } else {
        q->next = qa;
    }
    l->qtail = qa;
    pthread_mutex_unlock(&l->qlock);
}

static struct qaction *try_dequeue_action(struct dyn_local *l)
{
    struct qaction *qa;
    qa = l->qhead;
    if (qa == NULL) {
        return NULL;
    }

    pthread_mutex_lock(&l->qlock);
    l->qhead = qa->next;
    if (qa->next == NULL) {
        l->qtail = NULL;
    }
    pthread_mutex_unlock(&l->qlock);
    //printf("dequeue(%p)\n", qa);
    return qa;
}

struct dyn_local *dyn_local_init(const char *stackname, const char *plname,
        fn_resolver_t resolve_fnode)
{
    struct dyn_local *l = malloc(sizeof(*l));
    int res;

    l->running = true;
    l->plh = pl_init(stackname, plname);
    pipeline_handle = l->plh;
    l->qhead = NULL;
    l->qtail = NULL;
    res = pthread_mutex_init(&l->qlock, NULL);

    dynrs_init(&l->server, l->plh, resolve_fnode, l);
    dynrc_init(&l->client, enqueue_action, l);

    return l;
}

struct dynr_client *dyn_local_client(struct dyn_local *local)
{
    return &local->client;
}

void dyn_local_run(struct dyn_local *local)
{
    struct qaction *qa;

    printf("dyn_local_run called...\n");
    while (local->running) {
        // apply all the queued actions
        while ((qa = try_dequeue_action(local)) != NULL ||
               local->server.stopped)
        {
            if (qa == NULL) continue;
            dynrs_action(&local->server, &qa->act);
            free(qa);
        }
        dynrs_run(&local->server);
    }
}

