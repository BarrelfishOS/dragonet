#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

#include <implementation.h>
#include "config.h"

/** Allocate/initialize a new input structure including a buffer */
struct input *input_alloc(void)
{
    struct input *in = calloc(1, sizeof(*in));
    in->data = malloc(DEFAULT_BUFFER_SIZE);
    in->len = 0;
    // Currently we start filling the buffer from the rear
    in->data = (void *) ((uintptr_t) in->data + DEFAULT_BUFFER_SIZE);
    in->space_before = DEFAULT_BUFFER_SIZE;

    return in;
}

void input_free(struct input *in)
{
    free((void *) ((uintptr_t) in->data - in->space_before));
    free(in);
}

void input_clean_attrs(struct input *in)
{
    void *data = in->data;
    size_t len = in->len;
    size_t space_before = in->space_before;
    size_t space_after = in->space_after;

    memset(in, 0, sizeof(*in));
    in->data = data;
    in->len  = len;
    in->space_before = space_before;
    in->space_after = space_after;
}

void input_clean_packet(struct input *in)
{

    in->data = (void *) ((uintptr_t) in->data - in->space_before);
    // NOTE: Not clearing buffer explicitly as it will anyway get overwritten
    //memset(in->data, 0, DEFAULT_BUFFER_SIZE);
    in->data = (void *) ((uintptr_t) in->data + DEFAULT_BUFFER_SIZE);
    in->len = 0;
    in->space_before = DEFAULT_BUFFER_SIZE;
}

void input_dump(struct input *in)
{
    pktoff_t len = in->len;
    printf("input[%"PRIx64"]: ", in->len);
    pktoff_t i = 0;
    while (i < len) {
        printf("%02"PRIx8" ", *((uint8_t *) in->data + i));
        i++;
    }
    printf("\n");

}

