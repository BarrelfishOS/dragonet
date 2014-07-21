#include <stdio.h>
#include <stdlib.h>

#include <implementation.h>

#include "dynamic.h"
#include "rt_queue.h"

bool spawn_impl(struct ctx_generic *ctx, struct input *in, enum out_spawns s,
        enum spawn_priority p)
{
    struct dynamic_node *node = ctx->implementation;
    size_t ns = (size_t) s;

    if (ns >= node->num_spawns) {
        fprintf(stderr, "spawn_impl: invalid spawn id (%lu)\n",
                (unsigned long) ns);
        abort();
    }
    node = node->spawns[ns];
    //printf("Spawn n=%s sp=%d pr=%d\n", node->name, s, p);
    return task_queue_put(&node->graph->tqueue, node, in, p);
}

