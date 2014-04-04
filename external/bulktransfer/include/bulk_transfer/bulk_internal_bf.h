#ifndef BULK_INTERNAL_H
#define BULK_INTERNAL_H

struct bulk_int_buffer {
    /** capability for this buffer */
    struct capref cap;
    size_t        cap_offset;
};

struct bulk_int_pool {
    /** capability for the entire pool */
    struct capref pool_cap;
}

#endif // ndef BULK_INTERNAL_H

