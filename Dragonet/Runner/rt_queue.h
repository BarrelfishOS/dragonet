#ifndef RT_QUEUE_H_
#define RT_QUEUE_H_

#include <implementation.h>

// Basic deque (only exposed for allocation purposes)
struct rt_deque_element;
struct rt_deque {
    struct rt_deque_element *front;
    struct rt_deque_element *back;
};


struct task_queue {
    struct rt_deque deque;
};

void task_queue_init(struct task_queue  *tq);

/** Add element to task queue. */
bool task_queue_put(struct task_queue   *tq,
                    void                *node_handle,
                    struct input        *in,
                    enum spawn_priority  p);

/** Get top element from task queue. */
bool task_queue_get(struct task_queue   *tq,
                    void               **node_handle,
                    struct input       **in,
                    enum spawn_priority *p);


#endif // ndef RT_QUEUE_H_
