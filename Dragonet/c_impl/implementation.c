#include <string.h>
#include <stdlib.h>

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

