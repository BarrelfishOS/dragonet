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
    DYN_TOSOCKET,
    DYN_FROMSOCKET,
    DYN_UDPDEMUX,
    DYN_BALANCE,
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

typedef nodefun_t (*fn_resolver_t)(const char *name,
                                   void *data);

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
            bool                productive;
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
        struct {
            void    *sdata;
            uint64_t sid;
        } socket;
        struct {
            uint32_t s_ip;
            uint32_t d_ip;
            uint16_t s_port;
            uint16_t d_port;
        } udpdemux;
        struct {
            node_out_t next_port;
        } balance;
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

// Only here for allocation
struct dyn_node_stack {
    struct dynamic_node **stack;
    size_t top;
    size_t capacity;
};

struct dynamic_graph {
    pipeline_handle_t     plh;
    struct dyn_node_stack nodestack;
    queue_handle_t       *outqueues;
    size_t                num_outqs;
    size_t                num_nodes;
    pthread_mutex_t       lock;
    struct task_queue     tqueue;
    struct dynamic_node  *nodes;
    struct dynamic_spawn *spawns;
    fn_resolver_t         resolver;
    void                 *resolver_data;
    int                   version;
    bool                  was_productive;
    volatile bool         stop;

    // Hacks for socket node
    void               *(*socket_get)(uint64_t, void *);
    void                (*socket_set_spawn)(void *, struct dynamic_spawn *,
                                            void *);
    void                 *socket_data;
    // Current socket and buffer
    struct input         *cur_socket_buf;
    void                 *cur_socket;

};

struct dynamic_spawn {
    struct dynamic_node *node;
    uint32_t refcount;
    void *sock_data;

    struct dynamic_spawn *next;
};



struct dynamic_graph *dyn_mkgraph(pipeline_handle_t plh,
                                  fn_resolver_t resolver,
                                  void *resolver_data);
void dyn_rungraph(struct dynamic_graph *graph);
void dyn_stopgraph(struct dynamic_graph *graph);
void dyn_cleargraph(struct dynamic_graph *graph);

struct dynamic_node *dyn_mkfnode(struct dynamic_graph *graph,
                                 const char *name,
                                 const char *nodefun,
                                 bool productive);
struct dynamic_node *dyn_mkonode(struct dynamic_graph *graph,
                                 const char *name,
                                 enum dynamic_node_op op);
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
struct dynamic_node *dyn_mknode_tosocket(struct dynamic_graph *graph,
                                         const char           *name,
                                         uint64_t              sid);
struct dynamic_node *dyn_mknode_fromsocket(struct dynamic_graph *graph,
                                           const char           *name,
                                           uint64_t              sid);
struct dynamic_node *dyn_mknode_udpdemux(struct dynamic_graph *graph,
                                         const char           *name,
                                         uint32_t              s_ip,
                                         uint16_t              s_port,
                                         uint32_t              d_ip,
                                         uint16_t              d_port);
struct dynamic_node *dyn_mknode_balance(struct dynamic_graph *graph,
                                        const char           *name);

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

void dyn_add_init(struct dynamic_graph *graph,
                  struct dynamic_spawn *spawn);
bool dyn_spawn(struct dynamic_graph *graph,
               struct dynamic_spawn *spawn,
               struct input *in,
               enum spawn_priority p);

#endif // ndef DYNAMIC_H_

