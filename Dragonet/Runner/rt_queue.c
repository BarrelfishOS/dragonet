#include <stddef.h>

#include "rt_queue.h"

/******************************************************************************/
/* Deque implementation */

struct rt_deque_element {
    struct rt_deque_element *next;
    struct rt_deque_element *prev;
};

struct rt_deque_element;
static void rtdq_init(struct rt_deque *rtdq)
{
    rtdq->front = NULL;
    rtdq->back = NULL;
}

static void rtdq_push_front(struct rt_deque *rtdq, struct rt_deque_element *el)
{
    el->prev = NULL;
    el->next = rtdq->front;
    if (rtdq->front == NULL) {
        rtdq->back = el;
    } else {
        rtdq->front->prev = el;
    }
    rtdq->front = el;
}

static void rtdq_push_back(struct rt_deque *rtdq, struct rt_deque_element *el)
{
    el->next = NULL;
    el->prev = rtdq->back;
    if (rtdq->back == NULL) {
        rtdq->front = el;
    } else {
        rtdq->back->next = el;
    }
    rtdq->back = el;
}

static struct rt_deque_element *rtdq_pop_front(struct rt_deque *rtdq)
{
    struct rt_deque_element *el = rtdq->front;
    struct rt_deque_element *nx;
    if (el == NULL) {
        return NULL;
    }

    nx = el->next;
    if (nx == NULL) {
        rtdq->front = rtdq->back = NULL;
    } else {
        nx->prev = NULL;
        rtdq->front = nx;
    }
    return el;
}

static struct rt_deque_element *rtdq_pop_back(struct rt_deque *rtdq)
{
    struct rt_deque_element *el = rtdq->back;
    struct rt_deque_element *pr;
    if (el == NULL) {
        return NULL;
    }

    pr = el->prev;
    if (pr == NULL) {
        rtdq->front = rtdq->back = NULL;
    } else {
        pr->next = NULL;
        rtdq->back = pr;
    }
    return el;
}


/******************************************************************************/
/* Task queue implementation */

struct tq_element {
    struct rt_deque_element dqe;
    void               *node;
    struct input       *in;
    enum spawn_priority prio;
};

void task_queue_init(struct task_queue *tq)
{
    rtdq_init(&tq->deque);
}

/** Add element to task queue. */
bool task_queue_put(struct task_queue   *tq,
                    void                *node_handle,
                    struct input        *in,
                    enum spawn_priority  p)
{
    struct tq_element *el = malloc(sizeof(*el));
    if (el == NULL) {
        return false;
    }

    el->node = node_handle;
    el->in = in;
    el->prio = p;

    if (p == SPAWNPRIO_HIGH) {
        rtdq_push_front(&tq->deque, &el->dqe);
    } else {
        rtdq_push_back(&tq->deque, &el->dqe);
    }
    return true;
}

/** Get top element from task queue. */
bool task_queue_get(struct task_queue   *tq,
                    void               **node_handle,
                    struct input       **in,
                    enum spawn_priority *p)
{
    struct tq_element *el;
    el = (struct tq_element *) rtdq_pop_front(&tq->deque);
    if (el == NULL) {
        return false;
    }

    *node_handle = el->node;
    *in = el->in;
    *p = el->prio;
    free(el);
    return true;
}


