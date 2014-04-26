#include "implementation.h"

#define STRUCT_POOL_MAX 16

static struct input *s_pool = NULL;
static size_t s_count;

void pg_state_init(struct state *st);
void pg_state_init(struct state *st)
{
    st->local_mac      = 0;
    st->local_ip       = 0;
    st->arp_pending    = NULL;
    st->arp_cache      = NULL;
    st->pkt_counter    = 0;
    st->driver_handler = 0;
}

struct input *input_struct_alloc(void)
{
    struct input *in = s_pool;
    if (in != NULL) {
        s_count--;
        s_pool = in->next;
        return in;
    }

    return malloc(sizeof(*in));
}

void input_struct_free(struct input *in)
{
    if (s_count < STRUCT_POOL_MAX) {
        in->next = s_pool;
        s_pool = in;
        s_count++;
    } else {
        free(in);
    }
}


