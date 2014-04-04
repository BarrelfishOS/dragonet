#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <inttypes.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_helpers.h>

#define MAX_POOLNAME 32
#define POOL_EXTRA 4096

struct pool_meta {
    int refs;
    size_t buffer_size;
    size_t num_buffers;
    size_t pool_size;
};

static struct bulk_pool *pools = NULL;


static void alloc_poolid(struct bulk_pool_id *id)
{
    static uint32_t local = 0;
    id->machine = 0; // TODO
    id->dom = (uint32_t) getpid();
    id->local = ++local;
}

static void get_poolname(struct bulk_pool_id *id, char *name)
{
    snprintf(name, MAX_POOLNAME, "bulk_pool_%"PRIx32"_%"PRIx32"_%"PRIx32,
            id->machine, id->dom, id->local);
}

errval_t bulk_int_pool_map(struct bulk_pool      *p,
                           enum bulk_buffer_state state)
{
    struct bulk_buffer *buffers;
    void *buffers_vbase;
    size_t i;
    struct pool_meta *meta;
    char name[MAX_POOLNAME];
    size_t bufsz = p->buffer_size;
    size_t bufcnt = p->num_buffers;
    int fd;


    p->buffers = calloc(bufcnt, sizeof(*p->buffers));
    assert_fix(p->buffers != NULL);

    buffers = calloc(bufcnt, sizeof(*buffers));
    assert_fix(buffers != NULL);


    // Open pool SHM
    get_poolname(&p->id, name);
    fd = shm_open(name, O_RDWR, 0600);
    assert_fix(fd != -1);

    // Map meta
    meta = mmap(NULL, POOL_EXTRA, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    assert_fix(meta != MAP_FAILED);
    p->internal.meta = meta;
    __sync_add_and_fetch(&meta->refs, 1);

    bufsz = p->buffer_size = meta->buffer_size;
    bufcnt = p->num_buffers = meta->num_buffers;

    // Map buffers
    buffers_vbase = mmap(NULL, bufsz * bufcnt,
            PROT_READ | PROT_WRITE, MAP_SHARED, fd, POOL_EXTRA);
    assert_fix(buffers_vbase != MAP_FAILED);


    close(fd);

    p->base_address = buffers_vbase;
    for (i = 0; i < bufcnt; i++) {
        p->buffers[i] = buffers + i;

        buffers[i].address = buffers_vbase;
        // TODO: buffers[i].phys
        buffers[i].pool = p;
        buffers[i].bufferid = i;
        buffers[i].state = state;
        buffers[i].local_ref_count = 0;

        buffers_vbase = (void *) ((uintptr_t) buffers_vbase + bufsz);
    }

    p->internal.next = pools;
    pools = p;

    return SYS_ERR_OK;

}

struct bulk_pool *bulk_int_pool_byid(struct bulk_pool_id id)
{
    struct bulk_pool *p;
    for (p = pools; p != NULL; p = p->internal.next) {
        if (!memcmp(&id, &p->id, sizeof(id))) {
            return p;
        }
    }

    return NULL;
}

errval_t bulk_pool_alloc(struct bulk_pool             *p,
                         size_t                        buffer_size,
                         size_t                        buffer_count,
                         struct bulk_pool_constraints *constraints)
{
    char name[MAX_POOLNAME];
    int fd;
    int res;
    struct pool_meta *meta;

    // Allocate ID for pool
    alloc_poolid(&p->id);
    p->buffer_size = buffer_size;
    p->num_buffers = buffer_count;
    p->trust = BULK_TRUST_UNINITIALIZED;

    // Open SHM region and set its size
    get_poolname(&p->id, name);
    fd = shm_open(name, O_CREAT | O_RDWR | O_EXCL, 0600);
    assert_fix(fd != -1);
    res = ftruncate(fd, buffer_size * buffer_count + POOL_EXTRA);
    assert_fix(res == 0);

    // Initialize meta data
    meta = mmap(NULL, POOL_EXTRA, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    assert_fix(meta != MAP_FAILED);

    meta->buffer_size = buffer_size;
    meta->num_buffers = buffer_count;

    close(fd);

    return bulk_int_pool_map(p, BULK_BUFFER_READ_WRITE);
}

errval_t bulk_pool_free(struct bulk_pool *pool)
{
    int res;
    char name[MAX_POOLNAME];
    struct pool_meta *meta;


    // Unmap region
    res = munmap(pool->base_address, pool->num_buffers * pool->buffer_size);
    assert_fix(res == 0);

    // Free other memory associated
    free(pool->buffers[0]);
    free(pool->buffers);

    // Unlink shm region, if noone else has it mapped
    meta = pool->internal.meta;
    if (__sync_sub_and_fetch(&meta->refs, 1) == 0) {
        get_poolname(&pool->id, name);
        res = shm_unlink(name);
        assert_fix(res == 0);
    }

    // Unmap meta region
    res = munmap(meta, POOL_EXTRA);
    assert_fix(res == 0);

    return SYS_ERR_OK;

}

