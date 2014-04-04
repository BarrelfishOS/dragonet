#ifndef BULK_HELPERS_H
#define BULK_HELPERS_H

#include <bulk_transfer/bulk_transfer.h>

errval_t bulk_int_pool_map(struct bulk_pool      *p,
                           enum bulk_buffer_state state);

struct bulk_pool *bulk_int_pool_byid(struct bulk_pool_id id);

#endif // ndef BULK_HELPERS_H
