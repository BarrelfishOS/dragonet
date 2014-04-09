/** \file
 *  \brief Simple sleep call
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __SLEEP_H__
#define __SLEEP_H__

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <errno.h>

static inline void milli_sleep(uint64_t ms)
{
    struct timespec ts =
        { .tv_sec = ms / 1000, .tv_nsec = (ms % 1000) * 1000000 };
    int res;
    do {
        res = nanosleep(&ts, &ts);
        if (res != 0 && errno != EINTR) {
            fprintf(stderr, "milli_sleep: nansleep failed (%s)\n",
                    strerror(errno));
            abort();
        }
    } while (res != 0);
}

#endif
