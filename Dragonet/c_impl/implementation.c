#include <string.h>
#include <stdio.h>
#include <unistd.h>

#include <implementation.h>
#include <assert.h>
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
    assert(pipeline_handle != NULL);
    assert(in != NULL);
    input_free_plh(pipeline_handle, in);
}

void declare_dragonet_initialized(char *fname, char *msg)
{
    printf("##################### creating file [%s] ############\n", fname);
   int fid = creat(fname, 0644);
   assert(fid >= 0);
   int ret = write(fid, msg, strlen(msg));
   assert(ret >= 0);
   ret = fsync(fid);
   assert(ret >= 0);
   close(fid);
}


