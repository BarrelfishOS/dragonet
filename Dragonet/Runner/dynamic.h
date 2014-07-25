#ifndef DYNAMIC_H_
#define DYNAMIC_H_

#include <stddef.h>
#include <implementation.h>
#include <pipelines.h>

#include "rt_queue.h"

enum dynamic_node_type {
    DYN_FNODE,
    DYN_ONODE,
    DYN_DEMUX,
    DYN_MUX,
    DYN_TOQUEUE,
    DYN_FROMQUEUE,
};

enum dynamic_node_op {
    DYN_OP_AND,
    DYN_OP_OR,
    DYN_OP_NAND,
    DYN_OP_NOR,
};

typedef node_out_t (*nodefun_t)(struct ctx_generic *ctx, // I don't think we can
                                                         // do better here.
                                struct state *state,
                                struct input **in);

struct dynamic_edge;
struct dynamic_node {
    struct dynamic_graph  *graph;
    const char            *name;
    enum dynamic_node_type type;

    node_out_t             out_value;
    int                    out_version;

    struct dynamic_edge   *preds;
    size_t                 num_ports;
    struct dynamic_edge  **ports;
    size_t                 num_spawns;
    struct dynamic_spawn **spawns;
    union {
        struct {
            nodefun_t           nodefun;
            struct ctx_generic *ctx;
        } fnode;
        struct {
            enum dynamic_node_op op;
        } onode;
        struct {
            int32_t muxid;
        } mux;
        struct {
            queue_handle_t queue;
        } queue;
    } tdata;

    /** Linked list of nodes in graph */
    struct dynamic_node   *next;
    struct dynamic_node   *prev;
};

struct dynamic_edge {
    struct dynamic_node *source;
    struct dynamic_node *sink;

    struct dynamic_edge *so_next;
    struct dynamic_edge *si_next;

    node_out_t           port;
};

struct dynamic_graph {
    pipeline_handle_t     plh;
    queue_handle_t       *outqueues;
    size_t                num_outqs;
    size_t                num_nodes;
    pthread_mutex_t       lock;
    struct task_queue     tqueue;
    struct dynamic_node  *nodes;
    struct dynamic_spawn *spawns;
    volatile bool         stop;
};

struct dynamic_spawn {
    struct dynamic_node *node;
    uint32_t refcount;
    struct dynamic_spawn *next;
};



struct dynamic_graph *dyn_mkgraph(pipeline_handle_t plh);
void dyn_add_init(struct dynamic_graph *graph,
                  struct dynamic_spawn *spawn);
void dyn_rungraph(struct dynamic_graph *graph);
void dyn_stopgraph(struct dynamic_graph *graph);
void dyn_cleargraph(struct dynamic_graph *graph);

struct dynamic_node *dyn_mkfnode(struct dynamic_graph *graph,
                                 const char *name,
                                 nodefun_t   nodefun);
struct dynamic_node *dyn_mkonode_and(struct dynamic_graph *graph,
                                     const char *name);
struct dynamic_node *dyn_mkonode_or(struct dynamic_graph *graph,
                                    const char *name);
struct dynamic_node *dyn_mkonode_nand(struct dynamic_graph *graph,
                                      const char *name);
struct dynamic_node *dyn_mkonode_nor(struct dynamic_graph *graph,
                                     const char *name);
struct dynamic_node *dyn_mknode_demux(struct dynamic_graph *graph,
                                      const char           *name);
struct dynamic_node *dyn_mknode_mux(struct dynamic_graph *graph,
                                    const char           *name,
                                    int32_t               muxid);
struct dynamic_node *dyn_mknode_toqueue(struct dynamic_graph *graph,
                                        const char           *name,
                                        queue_handle_t        queue);
struct dynamic_node *dyn_mknode_fromqueue(struct dynamic_graph *graph,
                                          const char           *name,
                                          queue_handle_t        queue);

struct dynamic_spawn *dyn_mkspawn(struct dynamic_graph *graph,
                                  struct dynamic_node *node);
void dyn_updatespawn(struct dynamic_spawn *spawn,
                     struct dynamic_node *node);
void dyn_rmspawn(struct dynamic_graph *graph,
                 struct dynamic_spawn *spawn);

size_t dyn_addports(struct dynamic_node *node, size_t num);
struct dynamic_edge *dyn_addedge(struct dynamic_node *source,
                                 size_t port,
                                 struct dynamic_node *sink);

void dyn_addspawn(struct dynamic_node *node, struct dynamic_spawn *dst);

#endif // ndef DYNAMIC_H_

