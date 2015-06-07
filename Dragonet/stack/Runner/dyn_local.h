/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef DYN_LOCAL_H_
#define DYN_LOCAL_H_

#include "dyn_remote.h"

struct dyn_local;

struct dyn_local *dyn_local_init(const char *stackname, const char *plname,
        fn_resolver_t resolve_fnode);
struct dynr_client *dyn_local_client(struct dyn_local *local);
void dyn_local_run(struct dyn_local *local);


#endif // ndef DYN_LOCAL_H_
