#ifndef BULK_INTERNAL_H
#define BULK_INTERNAL_H


#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

#include <assert.h>
#define assert_fix assert

struct bulk_int_buffer {
};

struct bulk_pool;
struct bulk_int_pool {
    void *meta;
    struct bulk_pool *next;
};


#endif // ndef BULK_INTERNAL_H

