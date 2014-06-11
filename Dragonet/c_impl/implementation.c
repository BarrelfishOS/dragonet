#include <string.h>

#include <implementation.h>
#include "config.h"

pipeline_handle_t pipeline_handle;

void set_pipeline_handle(pipeline_handle_t h)
{
    pipeline_handle = h;
}

/** Allocate/initialize a new input structure including a buffer */
struct input *input_alloc(void)
{
    return input_alloc_plh(pipeline_handle);
}

void input_free(struct input *in)
{
    input_free_plh(pipeline_handle, in);
}

