#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <assert.h>

#include "dynamic.h"

#define INVALID_VERSION -1

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


struct dynamic_graph *dyn_mkgraph(void)
{
    struct dynamic_graph *g = malloc(sizeof(*g));
    g->sources = NULL;
    g->num_sources = 0;
    g->num_nodes = 0;
    pthread_mutex_init(&g->lock, NULL);
    return g;
}

void dyn_add_source(struct dynamic_graph *graph,
                    struct dynamic_node  *node)
{
    graph->sources = realloc(graph->sources,
            (graph->num_sources + 1) * sizeof(*graph->sources));
    graph->sources[graph->num_sources] = node;
    graph->num_sources++;
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
                           struct input        *in,
                           int                  version)
{
    node_out_t out;
    struct input *din;
    switch (n->type) {
        case DYN_FNODE:
            return n->tdata.fnode.nodefun(st, in);

        case DYN_ONODE:
            return run_onode(n, version);;

        case DYN_DEMUX:
            din = pl_poll(plh);
            if (din != NULL) {
                //printf("Polling succeeded\n");
                out = input_muxid(din);
                input_set_muxid(din, 0);
                input_xchg(in, din);
                input_free_plh(plh, din);
            } else {
                out = 0;
            }
            assert(out >= 0 && out < n->num_ports);
            return out;

        case DYN_MUX:
            input_set_muxid(in, n->tdata.mux.muxid);
            return 0;

        case DYN_TOQUEUE:
            pl_enqueue(n->tdata.toqueue.queue, in);
            return -2;

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

void dyn_rungraph(struct dynamic_graph *graph,
                  pipeline_handle_t     plh)
{
    struct node_stack ns;
    struct dynamic_node *n, *source;
    struct dynamic_edge *e;
    struct state *st;
    struct input *in;
    node_out_t out;
    int version = 0;
    size_t count, i;

    st = pl_get_state(plh);

    in = input_alloc_plh(plh);
    node_stack_init(&ns);

    while (pl_get_running(plh)) {
        // Run Graph
        pthread_mutex_lock(&graph->lock);
        for (i = 0; i < graph->num_sources; i++) {
            source = graph->sources[i];
            node_stack_push(&ns, source);
            count = 0;
            while ((n = node_stack_pop(&ns)) != NULL) {
                count++;
                //if (count > 1) printf("Execute: %s\n", n->name);
                out = run_node(n, plh, st, in, version);
                //if (count > 1) printf("   port=%d\n", out);
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
            //if (count > 1) printf("\n");

            version++;
            if (version == -1) {
                reset_versions(source);
                version = 0;
            }
            input_clean_attrs(in);
            input_clean_packet(in);
        }
        pthread_mutex_unlock(&graph->lock);

        pl_process_events(plh);
    }

    node_stack_free(&ns);
    input_free_plh(plh, in);
}



static struct dynamic_node *mknode(struct dynamic_graph *graph,
                                   const char *name,
                                   enum dynamic_node_type t)
{
    struct dynamic_node *n = malloc(sizeof(*n));
    graph->num_nodes++;
    n->graph = graph;
    n->name = strdup(name);
    n->type = t;
    n->out_version = INVALID_VERSION;
    n->preds = NULL;
    n->num_ports = 0;
    n->ports = NULL;
    return n;
}

struct dynamic_node *dyn_mkfnode(struct dynamic_graph *graph,
                                 const char *name,
                                 nodefun_t   nodefun)
{
    struct dynamic_node *n = mknode(graph, name, DYN_FNODE);
    n->tdata.fnode.nodefun = nodefun;
    return n;
}

static struct dynamic_node *mkonode(struct dynamic_graph *graph,
                                    const char *name,
                                    enum dynamic_node_op op)
{
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
    struct dynamic_node *n = mknode(graph, name, DYN_MUX);
    n->tdata.mux.muxid = muxid;
    return n;
}

struct dynamic_node *dyn_mknode_toqueue(struct dynamic_graph *graph,
                                        const char           *name,
                                        queue_handle_t        queue)
{
    struct dynamic_node *n = mknode(graph, name, DYN_TOQUEUE);
    n->tdata.toqueue.queue = queue;
    return n;

}


size_t dyn_addports(struct dynamic_node *node, size_t num)
{
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



