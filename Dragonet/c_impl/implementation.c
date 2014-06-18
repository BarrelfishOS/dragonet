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


__inline__ uint64_t
get_tsc(void) {
    uint32_t lo, hi;
    __asm__ __volatile__ ( /* serialize */
            "xorl %%eax,%%eax \n cpuid"
            ::: "%rax", "%rbx", "%rcx", "%rdx");
    /* We cannot use "=A", since this would use %rax on x86_64 and
     * return only the lower 32bits of the TSC
     */
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return (uint64_t)hi << 32 | lo;
}

