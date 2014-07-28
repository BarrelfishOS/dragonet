#ifndef DYN_LOCAL_H_
#define DYN_LOCAL_H_

#include "dyn_remote.h"

struct dyn_local;

struct dyn_local *dyn_local_init(const char *stackname, const char *plname,
        fn_resolver_t resolve_fnode);
struct dynr_client *dyn_local_client(struct dyn_local *local);
void dyn_local_run(struct dyn_local *local);


#endif // ndef DYN_LOCAL_H_
