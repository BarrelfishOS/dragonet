#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <uthash.h>
#include "dyn_remote.h"

//#define dprintf printf
#define dprintf(x...) do {} while (0)

struct dynrs_queue {
    dynr_queue_t   id;
    queue_handle_t qh;
    UT_hash_handle hh;
};

struct dynrs_node {
    dynr_node_t          id;
    struct dynamic_node *node;
    UT_hash_handle       hh;
};

struct dynrs_edge {
    dynr_edge_t          id;
    struct dynamic_edge *edge;
    UT_hash_handle       hh;
};

struct dynrs_spawn {
    dynr_spawn_t          id;
    struct dynamic_spawn *spawn;
    UT_hash_handle        hh;
};


static struct dynrs_queue *queue_get(struct dynr_server *srv, dynr_queue_t id)
{
    struct dynrs_queue *q;
    HASH_FIND(hh, srv->queues, &id, sizeof(id), q);
    return q;
}

static void queue_add(struct dynr_server *srv, struct dynrs_queue *q)
{
    HASH_ADD(hh, srv->queues, id, sizeof(dynr_queue_t), q);
}

static struct dynrs_queue *
queue_remove(struct dynr_server *srv, dynr_queue_t id)
{
    struct dynrs_queue *q;
    HASH_FIND(hh, srv->queues, &id, sizeof(id), q);
    if (q)
        HASH_DELETE(hh, srv->queues, q);
    return q;
}

static struct dynrs_queue *queue_create(dynr_queue_t id, queue_handle_t qh)
{
    struct dynrs_queue *q = malloc(sizeof(*q));
    q->id = id;
    q->qh = qh;
    return q;
}

static void
queue_destroy(struct dynrs_queue *q)
{
    free(q);
}


static struct dynrs_node *node_get(struct dynr_server *srv, dynr_node_t id)
{
    struct dynrs_node *n;
    HASH_FIND(hh, srv->nodes, &id, sizeof(id), n);
    return n;
}

static void node_add(struct dynr_server *srv, struct dynrs_node *n)
{
    HASH_ADD(hh, srv->nodes, id, sizeof(dynr_node_t), n);
}

static struct dynrs_node *node_create(dynr_node_t id, struct dynamic_node *dn)
{
    struct dynrs_node *n = malloc(sizeof(*n));
    n->id = id;
    n->node = dn;
    return n;
}


static struct dynrs_edge *edge_get(struct dynr_server *srv, dynr_edge_t id)
{
    struct dynrs_edge *e;
    HASH_FIND(hh, srv->edges, &id, sizeof(id), e);
    return e;
}

static void edge_add(struct dynr_server *srv, struct dynrs_edge *e)
{
    HASH_ADD(hh, srv->edges, id, sizeof(dynr_edge_t), e);
}

static struct dynrs_edge *edge_create(dynr_edge_t id, struct dynamic_edge *de)
{
    struct dynrs_edge *e = malloc(sizeof(*e));
    e->id = id;
    e->edge = de;
    return e;
}


static struct dynrs_spawn *spawn_get(struct dynr_server *srv, dynr_spawn_t id)
{
    struct dynrs_spawn *s;
    HASH_FIND(hh, srv->spawns, &id, sizeof(id), s);
    return s;
}

static void spawn_add(struct dynr_server *srv, struct dynrs_spawn *s)
{
    HASH_ADD(hh, srv->spawns, id, sizeof(dynr_spawn_t), s);
}

static struct dynrs_spawn *spawn_create(dynr_spawn_t id,
                                        struct dynamic_spawn *ds)
{
    struct dynrs_spawn *s = malloc(sizeof(*s));
    s->id = id;
    s->spawn = ds;
    return s;
}


static void clear_graph(struct dynr_server *srv)
{
    struct dynrs_node *n, *ntmp;
    struct dynrs_edge *e, *etmp;

    HASH_ITER(hh, srv->nodes, n, ntmp) {
        HASH_DEL(srv->nodes, n);
        free(n);
    }

    HASH_ITER(hh, srv->edges, e, etmp) {
        HASH_DEL(srv->edges, e);
        free(e);
    }
}


void dynrs_init(struct dynr_server *server,
                pipeline_handle_t    plh,
                fn_resolver_t        resolver,
                void                *resolver_data)
{
    server->graph = dyn_mkgraph(plh, resolver, resolver_data);
    server->stopped = true;
    server->queues = NULL;
    server->nodes = NULL;
    server->spawns = NULL;
    server->edges = NULL;
}

void dynrs_action(struct dynr_server *server,
                  struct dynr_action *a)
{
    struct dynamic_graph *g = server->graph;
    struct dynamic_node  *n;
    struct dynamic_edge  *e;
    struct dynamic_spawn *s;
    struct dynrs_queue   *rq;
    struct dynrs_node    *rn, *rm;
    struct dynrs_spawn   *rs;

    switch (a->type) {
        case DYNR_ACT_STARTGRAPH:
            dprintf("dynrs_action: DYNR_ACT_STARTGRAPH\n");
            pl_wait_ready(g->plh);
            server->stopped = false;
            break;
        case DYNR_ACT_STOPGRAPH:
            dprintf("dynrs_action: DYNR_ACT_STOPGRAPH\n");
            server->stopped = true;
            break;
        case DYNR_ACT_CLEARGRAPH:
            dprintf("dynrs_action: DYNR_ACT_CLEARGRAPH\n");
            dyn_cleargraph(server->graph);
            clear_graph(server);
            break;

        case DYNR_ACT_MKFNODE:
            dprintf("dynrs_action: DYNR_ACT_MKFNODE\n");
            assert(node_get(server, a->data.mkfnode.node) == NULL);
            n = dyn_mkfnode(g, a->data.mkfnode.name, a->data.mkfnode.impl,
                    a->data.mkfnode.productive);
            node_add(server, node_create(a->data.mkfnode.node, n));
            break;
        case DYNR_ACT_MKONODE:
            dprintf("dynrs_action: DYNR_ACT_MKONODE\n");
            assert(node_get(server, a->data.mkonode.node) == NULL);
            n = dyn_mkonode(g, a->data.mkonode.name, a->data.mkonode.op);
            node_add(server, node_create(a->data.mkonode.node, n));
            break;
        case DYNR_ACT_MKNODE_DEMUX:
            dprintf("dynrs_action: DYNR_ACT_MKNODE_DEMUX\n");
            assert(node_get(server, a->data.mknode_demux.node) == NULL);
            n = dyn_mknode_demux(g, a->data.mknode_demux.name);
            node_add(server, node_create(a->data.mknode_demux.node, n));
            break;
        case DYNR_ACT_MKNODE_MUX:
            dprintf("dynrs_action: DYNR_ACT_MKNODE_MUX\n");
            assert(node_get(server, a->data.mknode_mux.node) == NULL);
            n = dyn_mknode_mux(g, a->data.mknode_mux.name,
                                  a->data.mknode_mux.muxid);
            node_add(server, node_create(a->data.mknode_mux.node, n));
            break;
        case DYNR_ACT_MKNODE_TOQUEUE:
            dprintf("dynrs_action: DYNR_ACT_MKNODE_TOQUEUE\n");
            assert(node_get(server, a->data.mknode_queue.node) == NULL);
            rq = queue_get(server, a->data.mknode_queue.queue);
            assert(rq != NULL);
            n = dyn_mknode_toqueue(g, a->data.mknode_queue.name, rq->qh);
            node_add(server, node_create(a->data.mknode_queue.node, n));
            break;
        case DYNR_ACT_MKNODE_FROMQUEUE:
            dprintf("dynrs_action: DYNR_ACT_MKNODE_FROMQUEUE\n");
            assert(node_get(server, a->data.mknode_queue.node) == NULL);
            rq = queue_get(server, a->data.mknode_queue.queue);
            assert(rq != NULL);
            n = dyn_mknode_fromqueue(g, a->data.mknode_queue.name, rq->qh);
            node_add(server, node_create(a->data.mknode_queue.node, n));
            break;
        case DYNR_ACT_MKNODE_TOSOCKET:
            dprintf("dynrs_action: DYNR_ACT_MKNODE_TOSOCKET\n");
            assert(node_get(server, a->data.mknode_socket.node) == NULL);
            n = dyn_mknode_tosocket(g, a->data.mknode_socket.name,
                    a->data.mknode_socket.socket);
            node_add(server, node_create(a->data.mknode_socket.node, n));
            break;
        case DYNR_ACT_MKNODE_FROMSOCKET:
            dprintf("dynrs_action: DYNR_ACT_MKNODE_FROMSOCKET\n");
            assert(node_get(server, a->data.mknode_socket.node) == NULL);
            n = dyn_mknode_fromsocket(g, a->data.mknode_socket.name,
                    a->data.mknode_socket.socket);
            node_add(server, node_create(a->data.mknode_socket.node, n));
            break;
        case DYNR_ACT_MKNODE_UDPDEMUX:
            dprintf("dynrs_action: DYNR_ACT_MKNODE_UDPDEMUX\n");
            assert(node_get(server, a->data.mknode_udpdemux.node) == NULL);
            n = dyn_mknode_udpdemux(g, a->data.mknode_udpdemux.name,
                    a->data.mknode_udpdemux.s_ip,
                    a->data.mknode_udpdemux.s_port,
                    a->data.mknode_udpdemux.d_ip,
                    a->data.mknode_udpdemux.d_port);
            node_add(server, node_create(a->data.mknode_udpdemux.node, n));
            break;
        case DYNR_ACT_MKNODE_BALANCE:
            dprintf("dynrs_action: DYNR_ACT_MKNODE_BALANCE\n");
            assert(node_get(server, a->data.mknode_balance.node) == NULL);
            n = dyn_mknode_balance(g, a->data.mknode_balance.name);
            node_add(server, node_create(a->data.mknode_balance.node, n));
            break;

        case DYNR_ACT_MKSPAWN:
            dprintf("dynrs_action: DYNR_ACT_MKSPAWN\n");
            assert(spawn_get(server, a->data.mkupspawn.spawn) == NULL);
            rn = node_get(server, a->data.mkupspawn.node);
            assert(rn != NULL);
            s = dyn_mkspawn(g, rn->node);
            spawn_add(server, spawn_create(a->data.mkupspawn.spawn, s));
            break;
        case DYNR_ACT_UPDATESPAWN:
            dprintf("dynrs_action: DYNR_ACT_UPDATESPAWN\n");
            rs = spawn_get(server, a->data.mkupspawn.spawn);
            assert(rs != NULL);
            rn = node_get(server, a->data.mkupspawn.node);
            assert(rn != NULL);
            dyn_updatespawn(rs->spawn, rn->node);
            break;
        case DYNR_ACT_RMSPAWN:
            dprintf("dynrs_action: DYNR_ACT_RMSPAWN\n");
            rs = spawn_get(server, a->data.rmspawn.spawn);
            assert(rs != NULL);
            dyn_rmspawn(g, rs->spawn);
            // TODO: remove from HT and free
            break;

        case DYNR_ACT_ADDEDGE:
            dprintf("dynrs_action: DYNR_ACT_ADDEDGE %"PRId64"\n",
                    a->data.addedge.edge);
            dprintf("  =%p\n", edge_get(server, a->data.addedge.edge));
            assert(edge_get(server, a->data.addedge.edge) == NULL);
            rn = node_get(server, a->data.addedge.source);
            assert(rn != NULL);
            rm = node_get(server, a->data.addedge.sink);
            assert(rm != NULL);
            e = dyn_addedge(rn->node, a->data.addedge.port, rm->node);
            edge_add(server, edge_create(a->data.addedge.edge, e));
            break;
        case DYNR_ACT_ADDPORTS:
            dprintf("dynrs_action: DYNR_ACT_ADDPORTS\n");
            rn = node_get(server, a->data.addports.node);
            assert(rn != NULL);
            dyn_addports(rn->node, a->data.addports.num);
            break;
        case DYNR_ACT_ADDSPAWN:
            dprintf("dynrs_action: DYNR_ACT_ADDSPAWN\n");
            rn = node_get(server, a->data.addspawn.node);
            assert(rn != NULL);
            rs = spawn_get(server, a->data.addspawn.spawn);
            assert(rs != NULL);
            dyn_addspawn(rn->node, rs->spawn);
            break;
        case DYNR_ACT_ADDINIT:
            dprintf("dynrs_action: DYNR_ACT_ADDINIT\n");
            rs = spawn_get(server, a->data.addinit.spawn);
            assert(rs != NULL);
            dyn_add_init(g, rs->spawn);
            break;

        case DYNR_ACT_ADDINQ: {
            dynr_queue_t qid = a->data.addqueue.queue;
            const char *qname = a->data.addqueue.endpoint;
            dprintf("dynrs_action: DYNR_ACT_ADDINQ id:%lu name=%s\n", qid, qname);
            struct dynrs_queue *q;
            queue_handle_t qh;

            if (queue_get(server, qid) != NULL) {
                fprintf(stderr, "DYNR_ACT_ADDINQ: queue already exists");
                abort();
            }

            qh = pl_inqueue_create(g->plh, qname);
            q = queue_create(qid, qh);
            queue_add(server, q);
        } break;

        case DYNR_ACT_RMINQ: {
            dprintf("dynrs_action: DYNR_ACT_RMINQ\n");
            dynr_queue_t qid = a->data.rmqueue.queue;
            struct dynrs_queue *q;

            q = queue_remove(server, qid);
            if (q == NULL) {
                fprintf(stderr, "DYNR_ACT_RMINQ: error: queue does not exist");
                abort();
            }
            pl_inqueue_destroy(g->plh, q->qh);
            queue_destroy(q);
        } break;

        case DYNR_ACT_ADDOUTQ: {
            dprintf("dynrs_action: DYNR_ACT_ADDOUTQ\n");
            queue_handle_t qh;
            assert(queue_get(server, a->data.addqueue.queue) == NULL);
            qh = pl_outqueue_bind(g->plh, a->data.addqueue.endpoint);
            queue_add(server, queue_create(a->data.addqueue.queue, qh));
        } break;

        case DYNR_ACT_RMOUTQ:
            fprintf(stderr, "TODO: DYNR_ACT_RMOUTQ\n");
            break;

        default:
            fprintf(stderr, "dynrs_action: Unknown action type!\n");
    }
}

void dynrs_run(struct dynr_server *server)
{
    dyn_rungraph(server->graph);
}

