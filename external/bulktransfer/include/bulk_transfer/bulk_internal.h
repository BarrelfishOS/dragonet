#ifndef BULK_INTERNAL_H
#define BULK_INTERNAL_H


#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

struct bulk_int_buffer {
};

struct bulk_channel;
struct bulk_int_channel {
    bool creator;
    struct bulk_channel *next;
};

struct bulk_pool;
struct bulk_int_pool {
    void *meta;
    int shmid;
    struct bulk_pool *next;
};


#endif // ndef BULK_INTERNAL_H

