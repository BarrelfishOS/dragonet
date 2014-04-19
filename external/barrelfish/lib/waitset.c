#include <string.h>
#include <barrelfish/waitset.h>

void ws_init(struct waitset *ws)
{
    memset(ws, 0, sizeof(*ws));
}

void ws_addchan(struct waitset *ws, struct waitset_chanstate *cs)
{
    cs->next = NULL;
    cs->prev = ws->chans_tail;
    if (cs->prev == NULL) {
        ws->chans_head = cs;
    } else {
        cs->prev->next = cs;
    }
    ws->chans_tail = cs;
}

static struct waitset_chanstate *get_next(struct waitset *ws)
{
    struct waitset_chanstate *cs = ws->chans_head;
    if (cs != NULL) {
        if (cs->next != NULL) {
            cs->next->prev = NULL;
        } else {
            ws->chans_tail = NULL;
        }
        ws->chans_head = cs->next;
    }
    return cs;
}

errval_t ws_event_dispatch(struct waitset *ws)
{
    struct waitset_chanstate *cs;
    bool event_fired = false;

    while (!event_fired) {
        cs = get_next(ws);
        if (cs != NULL) {
            event_fired = cs->poll(cs);
            ws_addchan(ws, cs);
        }
    }

    return SYS_ERR_OK;
}

errval_t ws_event_dispatch_nonblock(struct waitset *ws)
{
    struct waitset_chanstate *cs, *csfirst = NULL;
    bool event_fired = false;

    while (!event_fired && ws->chans_head != csfirst) {
        cs = get_next(ws);
        if (cs == NULL) {
            // No channels on the waitset
            break;
        } else if (csfirst == NULL) {
            csfirst = cs;
        }

        event_fired = cs->poll(cs);
        ws_addchan(ws, cs);
    }

    return (event_fired ? SYS_ERR_OK : WS_NO_EVENT);
}

