#include <string.h>
#include <stdlib.h>
#include <inttypes.h>
#include <assert.h>

#include <implementation.h>
#include "config.h"

#define POOL_MAX 8
extern pipeline_handle_t pipeline_handle;
static size_t pool_count;
static struct input *in_pool = NULL;

/** Allocate/initialize a new input structure including a buffer */
struct input *input_alloc(void)
{
    struct input *in = in_pool;
    void *data;
    size_t len;
    buffer_handle_t buf_data, buf_attr;
    if (in != NULL) {
        in_pool = in->next;
        pool_count--;
        return in;
    }

    in = input_struct_alloc();

    buf_data = pl_buffer_alloc(pipeline_handle, &data, &len);
    assert(buf_data != NULL);
    in->data = (void *) ((uintptr_t) data + len);
    in->space_before = len;
    in->space_after = 0;
    in->len = 0;
    in->data_buffer = buf_data;

    buf_attr = pl_buffer_alloc(pipeline_handle, &data, &len);
    assert(buf_attr != NULL);
    in->attr = data;
    in->attr_buffer = buf_attr;
    memset(in->attr, 0, sizeof(*in->attr));

    return in;
}

void input_copy_packet(struct input *in, unsigned char *buff, size_t len)
{
    pkt_prepend(in, len);
    memcpy(in->data, buff, len);
}


void input_free(struct input *in)
{
    // if the pool is at the max, just free the thing
    if (pool_count >= POOL_MAX) {
        pl_buffer_free(pipeline_handle, in->data_buffer);
        pl_buffer_free(pipeline_handle, in->attr_buffer);
        input_struct_free(in);
        return;
    }

    input_clean_attrs(in);
    input_clean_packet(in);
    pool_count++;
    in->next = in_pool;
    in_pool = in;
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

