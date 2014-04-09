#ifndef PIPELINES_H_
#define PIPELINES_H_

#include <stddef.h>
#include <stdbool.h>
#include <implementation.h>

typedef void *pipeline_handle_t;
typedef void *queue_handle_t;

void *init_shared_state(const char *name, size_t chancount);
void stop_stack(void *handle);

pipeline_handle_t pl_init(const char *stackname, const char *plname);
struct state *pl_get_state(pipeline_handle_t plh);
queue_handle_t pl_inqueue_create(pipeline_handle_t plh, const char *name);
queue_handle_t pl_outqueue_bind(pipeline_handle_t plh, const char *name);
void pl_wait_ready(pipeline_handle_t plh);
void pl_enqueue(queue_handle_t queue, struct input *in);
void pl_process_events(pipeline_handle_t plh);
struct input *pl_poll(pipeline_handle_t plh);
bool pl_get_running(pipeline_handle_t plh);
void pl_terminated(pipeline_handle_t plh);
void pl_cleanup_handler(pipeline_handle_t plh, bool irregular,
                        void (*handler)(pipeline_handle_t,void *), void * data);

#endif

