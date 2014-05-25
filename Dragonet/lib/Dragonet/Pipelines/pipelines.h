#ifndef PIPELINES_H_
#define PIPELINES_H_

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

typedef void *pipeline_handle_t;
typedef void *queue_handle_t;
typedef void *buffer_handle_t;

struct input;

void *init_shared_state(const char *name, size_t chancount);
void stop_stack(void *handle);

pipeline_handle_t pl_init(const char *stackname, const char *plname);
struct state *pl_get_state(pipeline_handle_t plh);
queue_handle_t pl_inqueue_create(pipeline_handle_t plh, const char *name);
queue_handle_t pl_outqueue_bind(pipeline_handle_t plh, const char *name);
void pl_wait_ready(pipeline_handle_t plh);
void pl_enqueue(queue_handle_t queue, struct input *in);
bool pl_process_event(queue_handle_t queue);
struct input *pl_poll(queue_handle_t queue);
bool pl_get_running(pipeline_handle_t plh);
void pl_terminated(pipeline_handle_t plh);
void pl_cleanup_handler(pipeline_handle_t plh, bool irregular,
                        void (*handler)(pipeline_handle_t,void *), void * data);
void pl_panic(pipeline_handle_t plh, const char *fmt, ...)
    __attribute__((noreturn));

buffer_handle_t pl_buffer_alloc(pipeline_handle_t plh, void **buf,
                                uint64_t *phys, size_t *len);
void pl_buffer_free(pipeline_handle_t plh, buffer_handle_t buf);

#endif

