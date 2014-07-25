#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <assert.h>

#include "dynamic.h"

#define INVALID_VERSION -1

#define dprintf(x...) do { } while (0)
//#define dprintf printf

struct node_stack {
    struct dynamic_node **stack;
    size_t top;
    size_t capacity;
};

static void node_stack_init(struct node_stack *ns)
{
    size_t cap = 32;
    ns->capacity = cap;
    ns->top = 0;
    ns->stack = malloc(sizeof(*ns->stack) * cap);
}

static void node_stack_free(struct node_stack *ns)
{
    free(ns->stack);
}

static void node_stack_clear(struct node_stack *ns)
{
    ns->top = 0;
}

static void node_stack_push(struct node_stack *ns, struct dynamic_node *n)
{
    if (ns->top == ns->capacity) {
        ns->capacity *= 2;
        ns->stack = realloc(ns->stack, ns->capacity * sizeof(*ns->stack));
    }

    ns->stack[ns->top++] = n;
}

static struct dynamic_node *node_stack_pop(struct node_stack *ns)
{
    if (ns->top == 0) {
        return NULL;
    }
    return ns->stack[--ns->top];
}


struct dynamic_graph *dyn_mkgraph(pipeline_handle_t plh)
{
    dprintf("dyn_mkgraph\n");
    struct dynamic_graph *g = malloc(sizeof(*g));
    g->num_outqs = 0;
    g->num_nodes = 0;
    g->outqueues = NULL;
    g->nodes = NULL;
    g->spawns = NULL;
    g->plh = plh;
    g->stop = false;
    task_queue_init(&g->tqueue);
    pthread_mutex_init(&g->lock, NULL);
    return g;
}

void dyn_add_init(struct dynamic_graph *graph,
                  struct dynamic_spawn *spawn)
{
    dprintf("dyn_add_init\n");
    if (!task_queue_put(&graph->tqueue, spawn, NULL, SPAWNPRIO_HIGH)) {
        fprintf(stderr, "dyn_add_source: task_queue_put failed\n");
        abort();
    }
}

static node_out_t run_onode(struct dynamic_node *n,
                            int                  version)
{
    node_out_t shortc;
    node_out_t out;
    bool negate = false;
    bool all = true;
    struct dynamic_node *m;
    struct dynamic_edge *e;

    // We've already triggered (can happen when short-circuiting)
    if (n->out_version == version) {
        //printf("Already triggered!\n");
        return -1;
    }

    switch (n->tdata.onode.op) {
        case DYN_OP_NAND:
            negate = true; /* FALLTHRU */
        case DYN_OP_AND:
            shortc = P_false;
            out = P_true;
            break;

        case DYN_OP_NOR:
            negate = true; /* FALLTHRU */
        case DYN_OP_OR:
            shortc = P_true;
            out = P_false;
            break;

        default:
            fprintf(stderr, "Unsupported operation\n");
            abort();
    }

    e = n->preds;
    if (e == NULL) {
        fprintf(stderr, "An o-node as a source node, something is wrong :-/\n");
        abort();
    }
    while (e != NULL) {
        m = e->source;
        //printf("m=%s ver=%d val=%d\n", m->name, m->out_version, m->out_value);
        if (m->out_version != version ||
                (m->out_value != P_true && m->out_value != P_false))
        {
            //printf("Pity, not all enabled (%s)\n", m->name);
            all = false;
            e = e->si_next;
            continue;
        }
        if (m->out_value == shortc) {
            out = shortc;
            goto out;
        }
        e = e->si_next;
    }

    // No short-circuiting, we need all predecessors to be enabled!
    if (!all) {
        e = e->si_next;
        return -1;
    }

out:
    if (negate) {
        out = !out;
    }
    return out;
}

static node_out_t run_node(struct dynamic_node *n,
                           pipeline_handle_t    plh,
                           struct state        *st,
                           struct input        **in,
                           int                  version)
{
    node_out_t out;
    struct input *din;
    int ret;
    switch (n->type) {
        case DYN_FNODE:
            return n->tdata.fnode.nodefun(n->tdata.fnode.ctx, st, in);

        case DYN_ONODE:
            return run_onode(n, version);;

        case DYN_FROMQUEUE:
            assert(n->spawns != NULL);
            task_queue_put(&n->graph->tqueue, n->spawns[0], NULL,
                            SPAWNPRIO_LOW);
            din = pl_poll(n->tdata.queue.queue);
            if (din != NULL) {
                *in = din;
                return 1;
            } else {
                return 0;
            }

        case DYN_DEMUX:
            out = input_muxid(*in);
            input_set_muxid(*in, 0);
            assert(out >= 0 && out < n->num_ports);
            return out;

        case DYN_MUX:
            input_set_muxid(*in, n->tdata.mux.muxid);
            return 0;

        case DYN_TOQUEUE:
            ret = pl_enqueue(n->tdata.queue.queue, *in);
            if (ret < 0) {
                printf("%s:%s:%d:Warning: packet/event is lost as, pl_enqueue failed, %d\n",
                        __FILE__, __FUNCTION__, __LINE__, ret);
            }
            *in = NULL;
            return -2; // FIXME: why there is return value of -2?

        default:
            fprintf(stderr, "Invalid node type: %d\n", n->type);
            abort();
    }
}

static void reset_versions(struct dynamic_node *n)
{
    size_t i;
    struct dynamic_edge *e;

    if (n->out_version == INVALID_VERSION) {
        return;
    }

    n->out_version = INVALID_VERSION;
    for (i = 0; i < n->num_ports; i++) {
        e = n->ports[i];
        while (e != NULL) {
            reset_versions(e->sink);
            e = e->so_next;
        }
    }
}

void dyn_rungraph(struct dynamic_graph *graph)
{
    dprintf("dyn_rungraph\n");
    struct node_stack ns;
    struct dynamic_node *n, *source;
    struct dynamic_spawn *spawn;
    struct dynamic_edge *e;
    struct state *st;
    struct input *in;
    node_out_t out;
    int version = 0;
    size_t count, i;
    enum spawn_priority prio;
    pipeline_handle_t plh = graph->plh;

    st = pl_get_state(plh);

    node_stack_init(&ns);

    while (!graph->stop) {
        if (!task_queue_get(&graph->tqueue, (void **) &spawn, &in, &prio)) {
            fprintf(stderr, "PANIC: task queue empty\n");
            abort();
        }
        if (spawn->node == NULL) {
            fprintf(stderr, "Warning: Task starting at no longer existing "
                            "node, dropping task...\n");
            input_free_plh(plh, in);
            continue;
        }
        source = spawn->node;

        // Run Graph
        node_stack_push(&ns, source);
        count = 0;
        while ((n = node_stack_pop(&ns)) != NULL) {
            count++;
            dprintf("Execute: n=%p n->n=%p\n", n, n->name);
            dprintf("Execute: %s\n", n->name);
            out = run_node(n, plh, st, &in, version);
            dprintf("   port=%d\n", out);
            if (out >= 0 && n->num_ports == 0) {
                // Terminal node
                out = -2;
            }

            // If the node failed, we can stop here
            if (out == -2) {
                // Abort execution for this packet
                node_stack_clear(&ns);
                break;
            } else if (out < 0) {
                continue;
            }

            n->out_value = out;
            n->out_version = version;

            // Enable successors
            assert(out < n->num_ports);
            e = n->ports[out];
            while (e != NULL) {
                node_stack_push(&ns, e->sink);
                e = e->so_next;
            }
        }
        dprintf("Execution done\n");
        if (in != NULL) {
            input_free_plh(plh, in);
            in = NULL;
        }

        version++;
        if (version == -1) {
            reset_versions(source);
            version = 0;
        }

        // Process events on outqueues (freed buffers)
        for (i = 0; i < graph->num_outqs; i++) {
            while (pl_process_event(graph->outqueues[i]));
        }
    }
    graph->stop = false;

    node_stack_free(&ns);
    dprintf("dyn_rungraph done\n");
}

void dyn_stopgraph(struct dynamic_graph *graph)
{
    dprintf("dyn_stopgraph\n");
    graph->stop = true;
}


static void free_node(struct dynamic_node *node)
{
    struct dynamic_edge *e, *f;
    size_t i;

    if (node->type == DYN_FNODE) {
        free(node->tdata.fnode.ctx);
    }

    for (i = 0; i < node->num_ports; i++) {
        e = node->ports[i];
        while (e != NULL) {
            f = e;
            e = e->so_next;
            free(f);
        }
    }
    free((char *) node->name);
    free(node->ports);
    free(node->spawns);
    free(node);
}

void dyn_cleargraph(struct dynamic_graph *graph)
{
    dprintf("dyn_cleargraph\n");
    struct dynamic_node *n = graph->nodes;
    struct dynamic_node *m;
    struct dynamic_spawn *s = graph->spawns;

    while (n != NULL) {
        m = n;
        n = n->prev;
        free_node(m);
    }
    while (s != NULL) {
        s->node = NULL;
        s = s->next;
    }

    free(graph->outqueues);
    graph->outqueues = NULL;
    graph->num_outqs = 0;
    graph->num_nodes = 0;
    graph->nodes = NULL;
}


static struct dynamic_node *mknode(struct dynamic_graph *graph,
                                   const char *name,
                                   enum dynamic_node_type t)
{
    struct dynamic_node *n = malloc(sizeof(*n));
    dprintf("mknode(%p=%s) = %p\n", name, name, n);
    graph->num_nodes++;
    n->graph = graph;
    n->name = strdup(name);
    n->type = t;
    n->out_version = INVALID_VERSION;
    n->preds = NULL;
    n->num_ports = 0;
    n->ports = NULL;
    n->num_spawns = 0;
    n->spawns = NULL;

    n->next = NULL;
    n->prev = graph->nodes;
    if (n->prev != NULL) {
        n->prev->next = n;
    }
    graph->nodes = n;
    return n;
}

struct dynamic_node *dyn_mkfnode(struct dynamic_graph *graph,
                                 const char *name,
                                 nodefun_t   nodefun)
{
    dprintf("dyn_mkfnode\n");
    struct dynamic_node *n = mknode(graph, name, DYN_FNODE);
    n->tdata.fnode.nodefun = nodefun;
    n->tdata.fnode.ctx = malloc(sizeof(n->tdata.fnode.ctx));
    n->tdata.fnode.ctx->implementation = n;
    return n;
}

static struct dynamic_node *mkonode(struct dynamic_graph *graph,
                                    const char *name,
                                    enum dynamic_node_op op)
{
    dprintf("dyn_mkonode\n");
    struct dynamic_node *n = mknode(graph, name, DYN_ONODE);
    n->tdata.onode.op = op;
    return n;
}

struct dynamic_node *dyn_mkonode_and(struct dynamic_graph *graph,
                                     const char *name)
{
    return mkonode(graph, name, DYN_OP_AND);
}

struct dynamic_node *dyn_mkonode_or(struct dynamic_graph *graph,
                                    const char *name)
{
    return mkonode(graph, name, DYN_OP_OR);
}

struct dynamic_node *dyn_mkonode_nand(struct dynamic_graph *graph,
                                      const char *name)
{
    return mkonode(graph, name, DYN_OP_NAND);
}

struct dynamic_node *dyn_mkonode_nor(struct dynamic_graph *graph,
                                     const char *name)
{
    return mkonode(graph, name, DYN_OP_NOR);
}

struct dynamic_node *dyn_mknode_demux(struct dynamic_graph *graph,
                                      const char *name)
{
    return mknode(graph, name, DYN_DEMUX);
}

struct dynamic_node *dyn_mknode_mux(struct dynamic_graph *graph,
                                    const char           *name,
                                    node_out_t            muxid)
{
    dprintf("dyn_mknode_mux\n");
    struct dynamic_node *n = mknode(graph, name, DYN_MUX);
    n->tdata.mux.muxid = muxid;
    return n;
}

struct dynamic_node *dyn_mknode_toqueue(struct dynamic_graph *graph,
                                        const char           *name,
                                        queue_handle_t        queue)
{
    dprintf("dyn_mknode_toqueue\n");
    struct dynamic_node *n = mknode(graph, name, DYN_TOQUEUE);
    n->tdata.queue.queue = queue;

    graph->outqueues = realloc(graph->outqueues,
            sizeof(*graph->outqueues) * (graph->num_outqs + 1));
    graph->outqueues[graph->num_outqs] = queue;
    graph->num_outqs++;
    return n;
}

struct dynamic_node *dyn_mknode_fromqueue(struct dynamic_graph *graph,
                                          const char           *name,
                                          queue_handle_t        queue)
{
    dprintf("dyn_mknode_fromqueue\n");
    struct dynamic_node *n = mknode(graph, name, DYN_FROMQUEUE);
    n->tdata.queue.queue = queue;
    return n;
}

struct dynamic_spawn *dyn_mkspawn(struct dynamic_graph *graph,
                                  struct dynamic_node *node)
{
    dprintf("dyn_mkspawn\n");
    struct dynamic_spawn *s = malloc(sizeof(*s));
    s->node = node;
    s->refcount = 1;
    s->next = graph->spawns;
    graph->spawns = s;
    return s;
}

void dyn_updatespawn(struct dynamic_spawn *spawn,
                     struct dynamic_node *node)
{
    dprintf("dyn_updatespawn\n");
    spawn->node = node;
}

void dyn_rmspawn(struct dynamic_graph *graph,
                 struct dynamic_spawn *spawn)
{
    dprintf("dyn_rmspawn\n");
    struct dynamic_spawn *s, *p;

    // Remove from linked list
    p = NULL;
    s = graph->spawns;
    while (s != NULL) {
        if (s == spawn) {
            if (p == NULL) {
                graph->spawns = spawn->next;
            } else {
                p->next = spawn->next;
            }
            break;
        }
        p = s;
        s = s->next;
    }

    if (--spawn->refcount == 0) {
        free(spawn);
    }
}



size_t dyn_addports(struct dynamic_node *node, size_t num)
{
    dprintf("dyn_addports\n");
    size_t n = num + node->num_ports;
    node->ports = realloc(node->ports, n * sizeof(*node->ports));
    memset(node->ports + node->num_ports, 0, sizeof(*node->ports) * num);
    node->num_ports = n;
    return n - num;
}

struct dynamic_edge *dyn_addedge(struct dynamic_node *source,
                                 size_t port,
                                 struct dynamic_node *sink)
{
    dprintf("dyn_addedge\n");
    struct dynamic_edge *e = malloc(sizeof(*e));
    e->source = source;
    e->sink = sink;
    e->port = port;

    e->si_next = sink->preds;
    sink->preds = e;

    e->so_next = source->ports[port];
    source->ports[port] = e;

    return e;
}

void dyn_addspawn(struct dynamic_node *node, struct dynamic_spawn *spawn)
{
    dprintf("dyn_addspawn\n");
    node->num_spawns++;
    node->spawns = realloc(node->spawns,
            node->num_spawns * sizeof(*node->spawns));
    node->spawns[node->num_spawns - 1] = spawn;
}

