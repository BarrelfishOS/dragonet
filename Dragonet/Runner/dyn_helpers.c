#include <stdio.h>
#include <stdlib.h>

#include <implementation.h>

#include "dynamic.h"
#include "rt_queue.h"

bool spawn_impl(struct ctx_generic *ctx, struct input *in, enum out_spawns s,
        enum spawn_priority p)
{
    struct dynamic_node *node = ctx->implementation;
    struct dynamic_spawn *spawn;
    size_t ns = (size_t) s;

    if (ns >= node->num_spawns) {
        fprintf(stderr, "spawn_impl: invalid spawn id (%lu)\n",
                (unsigned long) ns);
        abort();
    }
    return dyn_spawn(node->graph, node->spawns[ns], in, p);
}

