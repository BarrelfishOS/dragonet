#include <string.h>

#include <implementation.h>
#include "config.h"

#define POOL_MAX 8
pipeline_handle_t pipeline_handle;
static size_t pool_count;
static struct input *in_pool = NULL;

void set_pipeline_handle(pipeline_handle_t h)
{
    pipeline_handle = h;
}

/** Allocate/initialize a new input structure including a buffer */
struct input *input_alloc(void)
{
    struct input *in = in_pool;
    if (in != NULL) {
        in_pool = in->next;
        pool_count--;
        return in;
    }

    return input_alloc_plh(pipeline_handle);
}

void input_free(struct input *in)
{
    // if the pool is at the max, just free the thing
    if (pool_count >= POOL_MAX) {
        input_free_plh(pipeline_handle, in);
        return;
    }

    input_clean_attrs(in);
    input_clean_packet(in);
    pool_count++;
    in->next = in_pool;
    in_pool = in;
}


bool ip_from_string(const char *ip, uint32_t *dst)
{
    if (inet_pton(AF_INET, ip, dst) != 1) {
        return false;
    }
    *dst = __builtin_bswap32(*dst);
    return true;
}

