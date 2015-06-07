/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef BARRELFISH_WAITSET_H
#define BARRELFISH_WAITSET_H

#include <barrelfish/barrelfish.h>

struct waitset {
    struct waitset_chanstate *chans_head;
    struct waitset_chanstate *chans_tail;
};

struct waitset_chanstate {
    struct waitset_chanstate *next;
    struct waitset_chanstate *prev;
    bool (*poll)(struct waitset_chanstate *cs);
};

void ws_init(struct waitset *ws);
void ws_addchan(struct waitset *ws, struct waitset_chanstate *cs);
errval_t ws_event_dispatch(struct waitset *ws);
errval_t ws_event_dispatch_nonblock(struct waitset *ws);

#endif
