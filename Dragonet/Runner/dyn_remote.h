#ifndef DYN_REMOTE_H_
#define DYN_REMOTE_H_

#include "dynamic.h"

#define DYNR_MAXNAME 40

typedef uint64_t dynr_queue_t;
typedef uint64_t dynr_node_t;
typedef uint64_t dynr_spawn_t;
typedef uint64_t dynr_edge_t;

enum dynr_action_type {
    DYNR_ACT_STARTGRAPH,
    DYNR_ACT_STOPGRAPH,
    DYNR_ACT_CLEARGRAPH,

    DYNR_ACT_MKFNODE,
    DYNR_ACT_MKONODE,
    DYNR_ACT_MKNODE_DEMUX,
    DYNR_ACT_MKNODE_MUX,
    DYNR_ACT_MKNODE_TOQUEUE,
    DYNR_ACT_MKNODE_FROMQUEUE,

    DYNR_ACT_MKSPAWN,
    DYNR_ACT_UPDATESPAWN,
    DYNR_ACT_RMSPAWN,

    DYNR_ACT_ADDEDGE,
    DYNR_ACT_ADDPORTS,
    DYNR_ACT_ADDSPAWN,
    DYNR_ACT_ADDINIT,

    DYNR_ACT_ADDINQ,
    DYNR_ACT_RMINQ,
    DYNR_ACT_ADDOUTQ,
    DYNR_ACT_RMOUTQ,
};

struct dynr_action {
    enum dynr_action_type type;
    union {
        struct {
            dynr_node_t node;
            char name[DYNR_MAXNAME + 1];
            char impl[DYNR_MAXNAME + 1];
        } mkfnode;
        struct {
            dynr_node_t node;
            char name[DYNR_MAXNAME + 1];
            enum dynamic_node_op op;
        } mkonode;
        struct {
            dynr_node_t node;
            char name[DYNR_MAXNAME + 1];
        } mknode_demux;
        struct {
            dynr_node_t node;
            char name[DYNR_MAXNAME + 1];
            int32_t muxid;
        } mknode_mux;
        struct {
            dynr_node_t node;
            char name[DYNR_MAXNAME + 1];
            dynr_queue_t queue;
        } mknode_queue;
        struct {
            dynr_spawn_t spawn;
            dynr_node_t  node;
        } mkupspawn;
        struct {
            dynr_spawn_t spawn;
        } rmspawn;
        struct {
            dynr_edge_t edge;
            dynr_node_t source;
            dynr_node_t sink;
            int32_t port;
        } addedge;
        struct {
            dynr_node_t node;
            uint32_t num;
        } addports;
        struct {
            dynr_node_t node;
            dynr_spawn_t spawn;
        } addspawn;
        struct {
            dynr_spawn_t spawn;
        } addinit;
        struct {
            dynr_queue_t queue;
            char endpoint[DYNR_MAXNAME + 1];
        } addqueue;
        struct {
            dynr_queue_t queue;
        } rmqueue;
    } data;
};


/******************************************************************************/
/* Server side */

struct dynrs_queue;
struct dynrs_node;
struct dynrs_spawn;
struct dynrs_edge;
struct dynr_server {
    struct dynamic_graph *graph;
    bool stopped;

    // Used for resolution of ids to pointers
    struct dynrs_queue   *queues;
    struct dynrs_node    *nodes;
    struct dynrs_spawn   *spawns;
    struct dynrs_edge    *edges;
};


void dynrs_init(struct dynr_server *server,
                pipeline_handle_t    plh,
                fn_resolver_t        resolver,
                void                *resolver_data);

void dynrs_action(struct dynr_server *server,
                  struct dynr_action *action);

void dynrs_run(struct dynr_server *server);


/******************************************************************************/
/* Client (dragonet) side */

typedef void (*dyn_client_send_t)(struct dynr_action *action,
                                  void               *data);
struct dynr_client {
    dyn_client_send_t send;
    void             *send_data;
};


void dynrc_init(struct dynr_client *client,
                dyn_client_send_t   send,
                void               *send_data);


void dynrc_startgraph(struct dynr_client *client);
void dynrc_stopgraph(struct dynr_client *client);
void dynrc_cleargraph(struct dynr_client *client);

void dynrc_mkfnode(struct dynr_client *client,
                   dynr_node_t node,
                   const char *name,
                   const char *nodefun);
void dynrc_mkonode_and(struct dynr_client *client,
                       dynr_node_t         node,
                       const char         *name);
void dynrc_mkonode_or(struct dynr_client *client,
                      dynr_node_t         node,
                      const char         *name);
void dynrc_mkonode_nand(struct dynr_client *client,
                        dynr_node_t         node,
                        const char         *name);
void dynrc_mkonode_nor(struct dynr_client *client,
                       dynr_node_t         node,
                       const char         *name);
void dynrc_mknode_demux(struct dynr_client *client,
                        dynr_node_t         node,
                        const char         *name);
void dynrc_mknode_mux(struct dynr_client *client,
                      dynr_node_t         node,
                      const char         *name,
                      int32_t             muxid);
void dynrc_mknode_toqueue(struct dynr_client *client,
                          dynr_node_t         node,
                          const char         *name,
                          dynr_queue_t        queue);
void dynrc_mknode_fromqueue(struct dynr_client *client,
                            dynr_node_t         node,
                            const char         *name,
                            dynr_queue_t        queue);

void dynrc_mkspawn(struct dynr_client *client,
                   dynr_spawn_t        spawn,
                   dynr_node_t         node);
void dynrc_updatespawn(struct dynr_client *client,
                       dynr_spawn_t        spawn,
                       dynr_node_t         node);
void dynrc_rmspawn(struct dynr_client *client,
                   dynr_spawn_t        spawn);

void dynrc_addedge(struct dynr_client *client,
                   dynr_edge_t         edge,
                   dynr_node_t         source,
                   int32_t             port,
                   dynr_node_t         sink);
void dynrc_addports(struct dynr_client *client,
                    dynr_node_t         node,
                    size_t              num);
void dynrc_addspawn(struct dynr_client *client,
                    dynr_node_t         source,
                    dynr_spawn_t        spawn);
void dynrc_add_init(struct dynr_client *client,
                    dynr_spawn_t        spawn);

void dynrc_addinqueue(struct dynr_client *client,
                       dynr_queue_t        queue,
                       const char         *endpoint);
void dynrc_rminqueue(struct dynr_client *client,
                     dynr_queue_t        queue);
void dynrc_addoutqueue(struct dynr_client *client,
                        dynr_queue_t        queue,
                        const char         *endpoint);
void dynrc_rmoutqueue(struct dynr_client *client,
                      dynr_queue_t        queue);


#endif // ndef DYN_REMOTE_H_

