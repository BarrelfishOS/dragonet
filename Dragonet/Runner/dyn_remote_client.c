#include <string.h>
#include <assert.h>

#include "dyn_remote.h"

//#define dprintf printf
#define dprintf(x...) do {} while (0)

void dynrc_init(struct dynr_client *client,
                dyn_client_send_t   send,
                void               *send_data)
{
    client->send = send;
    client->send_data = send_data;
}


void dynrc_startgraph(struct dynr_client *client)
{
    dprintf("dynrc_startgraph\n");
    struct dynr_action act = {
        .type = DYNR_ACT_STARTGRAPH,
    };
    client->send(&act, client->send_data);
}

void dynrc_stopgraph(struct dynr_client *client)
{
    dprintf("dynrc_stopgraph\n");
    struct dynr_action act = {
        .type = DYNR_ACT_STOPGRAPH,
    };
    client->send(&act, client->send_data);
}

void dynrc_cleargraph(struct dynr_client *client)
{
    dprintf("dynrc_cleargraph\n");
    struct dynr_action act = {
        .type = DYNR_ACT_CLEARGRAPH,
    };
    client->send(&act, client->send_data);
}


void dynrc_mkfnode(struct dynr_client *client,
                   dynr_node_t node,
                   const char *name,
                   const char *nodefun)
{
    dprintf("dynrc_mkfnode\n");
    struct dynr_action act = {
        .type = DYNR_ACT_MKFNODE,
        .data.mkfnode = {
            .node = node,
        },
    };
    assert(strlen(name) <= DYNR_MAXNAME);
    assert(strlen(nodefun) <= DYNR_MAXNAME);
    strcpy(act.data.mkfnode.name, name);
    strcpy(act.data.mkfnode.impl, nodefun);
    client->send(&act, client->send_data);
}

static void dynrc_mkonode(struct dynr_client  *client,
                          dynr_node_t          node,
                          const char          *name,
                          enum dynamic_node_op op)
{
    struct dynr_action act = {
        .type = DYNR_ACT_MKONODE,
        .data.mkonode = {
            .node = node,
            .op = op,
        },
    };
    assert(strlen(name) <= DYNR_MAXNAME);
    strcpy(act.data.mkonode.name, name);
    client->send(&act, client->send_data);
}

void dynrc_mkonode_and(struct dynr_client *client,
                       dynr_node_t         node,
                       const char         *name)
{
    dprintf("dynrc_mkonode_and\n");
    dynrc_mkonode(client, node, name, DYN_OP_AND);
}

void dynrc_mkonode_or(struct dynr_client *client,
                      dynr_node_t         node,
                      const char         *name)
{
    dprintf("dynrc_mkonode_or\n");
    dynrc_mkonode(client, node, name, DYN_OP_OR);
}

void dynrc_mkonode_nand(struct dynr_client *client,
                        dynr_node_t         node,
                        const char         *name)
{
    dprintf("dynrc_mkonode_nand\n");
    dynrc_mkonode(client, node, name, DYN_OP_NAND);
}

void dynrc_mkonode_nor(struct dynr_client *client,
                       dynr_node_t         node,
                       const char         *name)
{
    dprintf("dynrc_mkonode_nor\n");
    dynrc_mkonode(client, node, name, DYN_OP_NOR);
}

void dynrc_mknode_demux(struct dynr_client *client,
                        dynr_node_t         node,
                        const char         *name)
{
    dprintf("dynrc_mknode_demux\n");
    struct dynr_action act = {
        .type = DYNR_ACT_MKNODE_DEMUX,
        .data.mknode_demux = {
            .node = node,
        },
    };
    assert(strlen(name) <= DYNR_MAXNAME);
    strcpy(act.data.mknode_demux.name, name);
    client->send(&act, client->send_data);
}

void dynrc_mknode_mux(struct dynr_client *client,
                      dynr_node_t         node,
                      const char         *name,
                      int32_t             muxid)
{
    dprintf("dynrc_mknode_mux\n");
    struct dynr_action act = {
        .type = DYNR_ACT_MKNODE_MUX,
        .data.mknode_mux = {
            .node = node,
            .muxid = muxid,
        },
    };
    assert(strlen(name) <= DYNR_MAXNAME);
    strcpy(act.data.mknode_mux.name, name);
    client->send(&act, client->send_data);
}

void dynrc_mknode_toqueue(struct dynr_client *client,
                          dynr_node_t         node,
                          const char         *name,
                          dynr_queue_t        queue)
{
    dprintf("dynrc_mknode_toqueue\n");
    struct dynr_action act = {
        .type = DYNR_ACT_MKNODE_TOQUEUE,
        .data.mknode_queue = {
            .node = node,
            .queue = queue,
        },
    };
    assert(strlen(name) <= DYNR_MAXNAME);
    strcpy(act.data.mknode_queue.name, name);
    client->send(&act, client->send_data);
}

void dynrc_mknode_fromqueue(struct dynr_client *client,
                            dynr_node_t         node,
                            const char         *name,
                            dynr_queue_t        queue)
{
    dprintf("dynrc_mknode_fromqueue\n");
    struct dynr_action act = {
        .type = DYNR_ACT_MKNODE_FROMQUEUE,
        .data.mknode_queue = {
            .node = node,
            .queue = queue,
        },
    };
    assert(strlen(name) <= DYNR_MAXNAME);
    strcpy(act.data.mknode_queue.name, name);
    client->send(&act, client->send_data);
}


void dynrc_mkspawn(struct dynr_client *client,
                   dynr_spawn_t        spawn,
                   dynr_node_t         node)
{
    dprintf("dynrc_mknode_mkspawn\n");
    struct dynr_action act = {
        .type = DYNR_ACT_MKSPAWN,
        .data.mkupspawn = {
            .spawn = spawn,
            .node = node,
        },
    };
    client->send(&act, client->send_data);
}

void dynrc_updatespawn(struct dynr_client *client,
                       dynr_spawn_t        spawn,
                       dynr_node_t         node)
{
    dprintf("dynrc_updatespawn\n");
    struct dynr_action act = {
        .type = DYNR_ACT_UPDATESPAWN,
        .data.mkupspawn = {
            .spawn = spawn,
            .node = node,
        },
    };
    client->send(&act, client->send_data);
}

void dynrc_rmspawn(struct dynr_client *client,
                   dynr_spawn_t        spawn)
{
    dprintf("dynrc_rmspawn\n");
    struct dynr_action act = {
        .type = DYNR_ACT_RMSPAWN,
        .data.rmspawn = {
            .spawn = spawn,
        },
    };
    client->send(&act, client->send_data);
}

void dynrc_addedge(struct dynr_client *client,
                   dynr_edge_t         edge,
                   dynr_node_t         source,
                   int32_t             port,
                   dynr_node_t         sink)
{
    dprintf("dynrc_addedge\n");
    struct dynr_action act = {
        .type = DYNR_ACT_ADDEDGE,
        .data.addedge = {
            .edge = edge,
            .source = source,
            .port = port,
            .sink = sink,
        },
    };
    client->send(&act, client->send_data);
}

void dynrc_addports(struct dynr_client *client,
                    dynr_node_t         node,
                    size_t              num)
{
    dprintf("dynrc_addports\n");
    struct dynr_action act = {
        .type = DYNR_ACT_ADDPORTS,
        .data.addports = {
            .node = node,
            .num = num,
        },
    };
    client->send(&act, client->send_data);
}

void dynrc_addspawn(struct dynr_client *client,
                    dynr_node_t         source,
                    dynr_spawn_t        spawn)
{
    dprintf("dynrc_addspawn\n");
    struct dynr_action act = {
        .type = DYNR_ACT_ADDSPAWN,
        .data.addspawn = {
            .node = source,
            .spawn = spawn,
        },
    };
    client->send(&act, client->send_data);
}

void dynrc_add_init(struct dynr_client *client,
                    dynr_spawn_t        spawn)
{
    dprintf("dynrc_add_init\n");
    struct dynr_action act = {
        .type = DYNR_ACT_ADDINIT,
        .data.addinit = {
            .spawn = spawn,
        },
    };
    client->send(&act, client->send_data);
}


void dynrc_addinqueue(struct dynr_client *client,
                       dynr_queue_t        queue,
                       const char         *endpoint)
{
    dprintf("dynrc_addinqueue\n");
    struct dynr_action act = {
        .type = DYNR_ACT_ADDINQ,
        .data.addqueue = {
            .queue = queue,
        },
    };
    assert(strlen(endpoint) <= DYNR_MAXNAME);
    strcpy(act.data.addqueue.endpoint, endpoint);
    client->send(&act, client->send_data);
}

void dynrc_rminqueue(struct dynr_client *client,
                     dynr_queue_t        queue)
{
    dprintf("dynrc_rminqueue\n");
    struct dynr_action act = {
        .type = DYNR_ACT_RMINQ,
        .data.rmqueue = {
            .queue = queue,
        },
    };
    client->send(&act, client->send_data);
}

void dynrc_addoutqueue(struct dynr_client *client,
                        dynr_queue_t        queue,
                        const char         *endpoint)
{
    dprintf("dynrc_addoutqueue\n");
    struct dynr_action act = {
        .type = DYNR_ACT_ADDOUTQ,
        .data.addqueue = {
            .queue = queue,
        },
    };
    assert(strlen(endpoint) <= DYNR_MAXNAME);
    strcpy(act.data.addqueue.endpoint, endpoint);
    client->send(&act, client->send_data);
}

void dynrc_rmoutqueue(struct dynr_client *client,
                      dynr_queue_t        queue)
{
    dprintf("dynrc_rmoutqueue\n");
    struct dynr_action act = {
        .type = DYNR_ACT_RMOUTQ,
        .data.rmqueue = {
            .queue = queue,
        },
    };
    client->send(&act, client->send_data);
}


