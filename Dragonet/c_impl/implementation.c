#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

#include <implementation.h>
#include "config.h"

static struct input *in_pool = NULL;

/** Allocate/initialize a new input structure including a buffer */
struct input *input_alloc(void)
{
    struct input *in = in_pool;
    if (in != NULL) {
        in_pool = in->next;
        return in;
    }

    in = calloc(1, sizeof(*in) + sizeof(*in->attr));
    in->data = malloc(DEFAULT_BUFFER_SIZE);
    in->attr = (struct input_attributes *) (in + 1);
    in->len = 0;
    // Currently we start filling the buffer from the rear
    in->data = (void *) ((uintptr_t) in->data + DEFAULT_BUFFER_SIZE);
    in->space_before = DEFAULT_BUFFER_SIZE;

    return in;
}

void input_copy_packet(struct input *in, unsigned char *buff, size_t len)
{
    pkt_prepend(in, len);
    memcpy(in->data, buff, len);
}


void input_free(struct input *in)
{
    input_clean_attrs(in);
    input_clean_packet(in);
    in->next = in_pool;
    in_pool = in;
    /*free((void *) ((uintptr_t) in->data - in->space_before));
    free(in);*/
}

void input_clean_attrs(struct input *in)
{
    memset(in->attr, 0, sizeof(*in->attr));
}

void input_zero(struct input *in)
{
    memset(in, 0, sizeof(*in));
}

void input_clean_packet(struct input *in)
{
    in->space_before = in->space_before + in->space_after + in->len;
    in->space_after = 0;
    in->data = (void *) ((uintptr_t) in->data + in->space_after + in->len);
    in->len = 0;
}

void input_dump(struct input *in)
{
    pktoff_t len = in->len;
    printf("input[%"PRIx32"]: ", in->len);
    pktoff_t i = 0;
    while (i < len) {
        printf("%02"PRIx8" ", *((uint8_t *) in->data + i));
        i++;
    }
    printf("\n");

}

void pg_state_init(struct state *st)
{
    st->local_mac      = 0;
    st->local_ip       = 0;
    st->arp_pending    = NULL;
    st->arp_cache      = NULL;
    st->pkt_counter    = 0;
    st->driver_handler = 0;
}

int32_t input_muxid(struct input *in)
{
    return in->attr->mux_id;
}

void input_set_muxid(struct input *in, int32_t mux)
{
    in->attr->mux_id = mux;
}

void input_xchg(struct input *a, struct input *b)
{
    struct input tmp;
    memcpy(&tmp, a, sizeof(*a));
    memcpy(a, b, sizeof(*b));
    memcpy(b, &tmp, sizeof(*b));
}

