#ifndef DYNAMIC_H_
#define DYNAMIC_H_

#include <stddef.h>
#include <implementation.h>
#include <pipelines.h>

enum dynamic_node_type {
    DYN_FNODE,
    DYN_ONODE,
    DYN_DEMUX,
    DYN_MUX,
    DYN_TOQUEUE,
};

enum dynamic_node_op {
    DYN_OP_AND,
    DYN_OP_OR,
    DYN_OP_NAND,
    DYN_OP_NOR,
};

typedef node_out_t (*nodefun_t)(struct state *state,
                                struct input *in);

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
    union {
        struct {
            nodefun_t nodefun;
        } fnode;
        struct {
            enum dynamic_node_op op;
        } onode;
        struct {
            int32_t muxid;
        } mux;
        struct {
            queue_handle_t queue;
        } toqueue;
    } tdata;
};

struct dynamic_edge {
    struct dynamic_node *source;
    struct dynamic_node *sink;

    struct dynamic_edge *so_next;
    struct dynamic_edge *si_next;

    node_out_t           port;
};

struct dynamic_graph {
    struct dynamic_node *source;
    size_t               num_nodes;
    pthread_mutex_t      lock;
};



struct dynamic_graph *dyn_mkgraph(void);
void dyn_set_source(struct dynamic_graph *graph,
                    struct dynamic_node  *node);

void dyn_rungraph(struct dynamic_graph *graph,
                  pipeline_handle_t     plh);

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

size_t dyn_addports(struct dynamic_node *node, size_t num);
struct dynamic_edge *dyn_addedge(struct dynamic_node *source,
                                 size_t port,
                                 struct dynamic_node *sink);

#endif // ndef DYNAMIC_H_

