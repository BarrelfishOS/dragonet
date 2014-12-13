#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <assert.h>
#include <unistd.h>
#include <proto_ipv4.h>
#include <udpproto.h>

#include "dynamic.h"

#define INVALID_VERSION -1

//#define dprintf(x...)    do { printf("TID:%d:%s:%s:%d: ", (int)pthread_self(), __FILE__, __FUNCTION__, __LINE__); printf(":" x); } while(0)
#define dprintf(x...) do { } while (0)
//#define dprintf printf


static void node_stack_init(struct dyn_node_stack *ns)
{
    size_t cap = 32;
    ns->capacity = cap;
    ns->top = 0;
    ns->stack = malloc(sizeof(*ns->stack) * cap);
}

static void node_stack_free(struct dyn_node_stack *ns)
{
    free(ns->stack);
}

static void node_stack_clear(struct dyn_node_stack *ns)
{
    ns->top = 0;
}

static void node_stack_push(struct dyn_node_stack *ns, struct dynamic_node *n)
{
    if (ns->top == ns->capacity) {
        ns->capacity *= 2;
        ns->stack = realloc(ns->stack, ns->capacity * sizeof(*ns->stack));
    }

    ns->stack[ns->top++] = n;
}

static struct dynamic_node *node_stack_pop(struct dyn_node_stack *ns)
{
    if (ns->top == 0) {
        return NULL;
    }
    return ns->stack[--ns->top];
}


struct dynamic_graph *dyn_mkgraph(pipeline_handle_t plh,
                                  fn_resolver_t     resolver,
                                  void             *resolver_data)
{
    dprintf("dyn_mkgraph\n");
    struct dynamic_graph *g = calloc(1, sizeof(*g));
    g->plh = plh;
    g->resolver = resolver;
    g->resolver_data = resolver_data;
    g->version = 1;
    node_stack_init(&g->nodestack);
    task_queue_init(&g->tqueue);
    pthread_mutex_init(&g->lock, NULL);
    return g;
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
        //printf("  m=%s ver=%d val=%d\n", m->name, m->out_version, m->out_value);
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

static node_out_t run_node(struct dynamic_graph *graph,
                           struct dynamic_node *n,
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
            graph->was_productive =
                graph->was_productive || n->tdata.fnode.productive;
            return n->tdata.fnode.nodefun(n->tdata.fnode.ctx, st, in);

        case DYN_ONODE:
            return run_onode(n, version);

        case DYN_FROMQUEUE:
            assert(n->spawns != NULL);
            dyn_spawn(n->graph, n->spawns[0], NULL, SPAWNPRIO_LOW);
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
            graph->was_productive = true;
            ret = pl_enqueue(n->tdata.queue.queue, *in);
            if (ret < 0) {
                dprintf("%s:%s:%d:Warning: packet/event is lost as, pl_enqueue failed, %d\n",
                        __FILE__, __FUNCTION__, __LINE__, ret);
            }
            *in = NULL;
            return -2; // FIXME: why there is return value of -2?

        case DYN_TOSOCKET:
            graph->was_productive = true;
            n->graph->cur_socket_buf = *in;
            n->graph->cur_socket = n->tdata.socket.sdata;
            *in = NULL;
            return -2;

        case DYN_FROMSOCKET:
            return 0;

        case DYN_UDPDEMUX:
            out = PORT_BOOL(
                (!n->tdata.udpdemux.s_ip ||
                    n->tdata.udpdemux.s_ip == ipv4_srcIP_rd(*in)) &&
                (!n->tdata.udpdemux.d_ip ||
                    n->tdata.udpdemux.d_ip == ipv4_dstIP_rd(*in)) &&
                (!n->tdata.udpdemux.s_port ||
                    n->tdata.udpdemux.s_port == udp_hdr_sport_read(*in)) &&
                (!n->tdata.udpdemux.d_port ||
                    n->tdata.udpdemux.d_port == udp_hdr_dport_read(*in)));
            return out;

        case DYN_BALANCE:
            out = n->tdata.balance.next_port % n->num_ports;
            n->tdata.balance.next_port = (out + 1) % n->num_ports;
            return out;

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
    struct dyn_node_stack *ns = &graph->nodestack;
    struct dynamic_node *n, *source;
    struct dynamic_spawn *spawn;
    struct dynamic_edge *e;
    struct state *st;
    struct input *in;
    node_out_t out;
    int version;
    size_t count, i;
    enum spawn_priority prio;
    pipeline_handle_t plh = graph->plh;

    st = pl_get_state(plh);
    version = graph->version;

    node_stack_clear(ns);
    if (!task_queue_get(&graph->tqueue, (void **) &spawn, &in, &prio)) {
        fprintf(stderr, "PANIC: task queue empty\n");
        abort();
    }
    dprintf("  -> spawn=%p\n", spawn);
    if (spawn->node == NULL) {
        fprintf(stderr, "Warning: Task starting at no longer existing "
                        "node, dropping task...\n");
        if (in != NULL) {
            printf("%s:%d: calling input_free\n", __FUNCTION__, __LINE__);
            input_free_plh(plh, in);
        }
        goto out;
    }
    source = spawn->node;

    // Run Graph
    graph->was_productive = false;
    node_stack_push(ns, source);
    count = 0;
    while ((n = node_stack_pop(ns)) != NULL) {
        count++;
        dprintf("Execute: n=%p n->n=%p\n", n, n->name);
        dprintf("Execute: %s, count: %zu \n", n->name, count);
        out = run_node(graph, n, plh, st, &in, version);
        dprintf("   port=%d\n", out);
        if (out >= 0 && n->num_ports == 0) {
            // Terminal node
            out = -2;
        }

        // If the node failed, we can stop here
        if (out == -2) {
            // Abort execution for this packet
            dprintf("Execute: abort. %s\n", n->name);
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
            node_stack_push(ns, e->sink);
            e = e->so_next;
        }
    } // end while:
    if (in != NULL) {
        dprintf("freeing the buffer\n");
        input_free_plh(plh, in);
        in = NULL;
    }
    dprintf("Execution done\n");

    version++;
    if (version == -1) {
        reset_versions(source);
        version = 0;
    }
    graph->version = version;

    // Process events on outqueues (freed buffers)
    for (i = 0; i < graph->num_outqs; i++) {
        while (pl_process_event(graph->outqueues[i]));
    }

out:
    if (--spawn->refcount == 0) {
        dprintf("calling free spawn\n");
        free(spawn);
    }
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

    while (s != NULL) {
        fprintf(stderr, "******* %s(): cancelling spawn task on node:%s\n",
                        __FUNCTION__,  s->node->name);
        s->node = NULL;
        s = s->next;
    }

    while (n != NULL) {
        m = n;
        n = n->prev;
        free_node(m);
    }


    free(graph->outqueues);
    graph->outqueues = NULL;
    graph->num_outqs = 0;
    graph->num_nodes = 0;
    graph->nodes = NULL;
    graph->version = 1;
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
                                 const char *impl,
                                 bool productive)
{
    dprintf("dyn_mkfnode\n");
    struct dynamic_node *n = mknode(graph, name, DYN_FNODE);
    n->tdata.fnode.nodefun = graph->resolver(impl, graph->resolver_data);
    n->tdata.fnode.ctx = malloc(sizeof(n->tdata.fnode.ctx));
    n->tdata.fnode.ctx->implementation = n;
    n->tdata.fnode.productive = productive;
    return n;
}

struct dynamic_node *dyn_mkonode(struct dynamic_graph *graph,
                                 const char *name,
                                 enum dynamic_node_op op)
{
    dprintf("dyn_mkonode: %s %d\n", name, op);
    struct dynamic_node *n = mknode(graph, name, DYN_ONODE);
    n->tdata.onode.op = op;
    return n;
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

static void *get_sockdata(struct dynamic_graph *graph,
                          uint64_t sid)
{
    if (graph->socket_get == NULL) {
        return NULL;
    }
    return graph->socket_get(sid, graph->socket_data);

}

struct dynamic_node *dyn_mknode_tosocket(struct dynamic_graph *graph,
                                         const char           *name,
                                         uint64_t              sid)
{
    dprintf("dyn_mknode_tosocket\n");
    struct dynamic_node *n = mknode(graph, name, DYN_TOSOCKET);
    n->tdata.socket.sid = sid;
    n->tdata.socket.sdata = get_sockdata(graph, sid);
    return n;
}

struct dynamic_node *dyn_mknode_fromsocket(struct dynamic_graph *graph,
                                           const char           *name,
                                           uint64_t              sid)
{
    dprintf("dyn_mknode_fromsocket\n");
    struct dynamic_node *n = mknode(graph, name, DYN_FROMSOCKET);
    n->tdata.socket.sid = sid;
    n->tdata.socket.sdata = get_sockdata(graph, sid);
    return n;
}

struct dynamic_node *dyn_mknode_udpdemux(struct dynamic_graph *graph,
                                         const char           *name,
                                         uint32_t              s_ip,
                                         uint16_t              s_port,
                                         uint32_t              d_ip,
                                         uint16_t              d_port)
{
    dprintf("dyn_mknode_udpdemux\n");
    struct dynamic_node *n = mknode(graph, name, DYN_UDPDEMUX);
    n->tdata.udpdemux.s_ip = s_ip;
    n->tdata.udpdemux.s_port = s_port;
    n->tdata.udpdemux.d_ip = d_ip;
    n->tdata.udpdemux.d_port = d_port;
    return n;
}

struct dynamic_node *dyn_mknode_balance(struct dynamic_graph *graph,
                                        const char *name)
{
    struct dynamic_node *n = mknode(graph, name, DYN_BALANCE);
    n->tdata.balance.next_port = 0;
    return n;
}

static void update_sockspawn(struct dynamic_graph *graph,
                             void                 *sock_data,
                             struct dynamic_spawn *spawn)
{
    if (graph->socket_set_spawn != NULL) {
        graph->socket_set_spawn(sock_data, spawn, graph->socket_data);
    } else {
        fprintf(stderr, "update_sockspawn: socket_set_spawn not set\n");
    }
}

struct dynamic_spawn *dyn_mkspawn(struct dynamic_graph *graph,
                                  struct dynamic_node *node)
{
    struct dynamic_spawn *s = malloc(sizeof(*s));
    dprintf("dyn_mkspawn n=%p -> %p\n", node, s);
    s->node = node;
    s->refcount = 1;
    s->sock_data = NULL;
    s->next = graph->spawns;
    graph->spawns = s;

    if (node->type == DYN_FROMSOCKET) {
        s->sock_data = node->tdata.socket.sdata;
        update_sockspawn(graph, s->sock_data, s);
    }
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

    if (spawn->sock_data != NULL) {
        update_sockspawn(graph, spawn->sock_data, NULL);
    }
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
    dprintf("dyn_addedge: %s:%"PRId64" -> %s\n", source->name, port, sink->name);
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

void dyn_add_init(struct dynamic_graph *graph,
                  struct dynamic_spawn *spawn)
{
    dprintf("dyn_add_init %s\n",
            (spawn->node != NULL ? spawn->node->name : NULL));
    if (!dyn_spawn(graph, spawn, NULL, SPAWNPRIO_HIGH)) {
        fprintf(stderr, "dyn_add_init: task_queue_put failed\n");
        abort();
    }
}


bool dyn_spawn(struct dynamic_graph *graph,
               struct dynamic_spawn *spawn,
               struct input *in,
               enum spawn_priority p)
{
    dprintf("dyn_addspawn %s\n",
            (spawn->node != NULL ? spawn->node->name : NULL));
    spawn->refcount++;
    return task_queue_put(&graph->tqueue, spawn, in, p);
}

