#define _LARGEFILE64_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <unistd.h>
#include <inttypes.h>
#include <errno.h>


#include <bulk_transfer/bulk_transfer.h>
#include <bulk_helpers.h>

#define MAX_POOLNAME 32
#define POOL_EXTRA 4096
#define HUGEPAGESZ (2*1024*1024)

struct pool_meta {
    int refs;
    size_t buffer_size;
    size_t num_buffers;
    size_t pool_size;
    key_t  key;
};

struct bulk_pool *bulk_pools = NULL;
uint32_t bulk_machine_id = 0;

static void alloc_poolid(struct bulk_pool_id *id)
{
    static uint32_t local = 0;
    id->machine = bulk_machine_id; // TODO
    id->dom = (uint32_t) getpid();
    id->local = __sync_add_and_fetch(&local, 1);
}

static void get_poolname(struct bulk_pool_id *id, char *name)
{
    snprintf(name, MAX_POOLNAME, "bulk_pool_%"PRIx32"_%"PRIx32"_%"PRIx32,
            id->machine, id->dom, id->local);
}

/** Resolve virtual address into physical address */
bool linux_virt_to_phys(void *addr, uint64_t *phys)
{
    int fd;
    bool success = true;
    uint64_t val;
    size_t page_size;
    off64_t off;

    if ((fd = open("/proc/self/pagemap", O_RDONLY)) < 0) {
        fprintf(stderr, "page_virt_to_phys: opening pagemap failed\n");
        return false;
    }

    page_size = getpagesize();
    off = (uintptr_t) addr / page_size * 8;
    if (lseek64(fd, off, SEEK_SET) != off) {
        fprintf(stderr, "page_virt_to_phys: lseek failed\n");
        success = false;
    }

    if (success && read(fd, &val, sizeof(val)) != sizeof(val)) {
        fprintf(stderr, "page_virt_to_phys: read failed\n");
        success = false;
    }
    close(fd);

    if (success) {
        /* See: https://www.kernel.org/doc/Documentation/vm/pagemap.txt
         *
         * Bits 0-54  page frame number (PFN) if present
         * Bits 0-4   swap type if swapped
         * Bits 5-54  swap offset if swapped
         * Bit  55    pte is soft-dirty (see Documentation/vm/soft-dirty.txt)
         * Bits 56-60 zero
         * Bit  61    page is file-page or shared-anon
         * Bit  62    page swapped
         * Bit  63    page present
         */
        if ((val & (1ULL << 63)) == 0 || (val & (1ULL << 62)) == 1) {
            fprintf(stderr, "page_virt_to_phys: read failed\n");
            success = false;
        } else {
            *phys = (val & ~(-1ULL << 55)) * page_size +
                    (uintptr_t) addr % page_size;
        }
    }

    return success;
}

/** Resolve virtual address of huge page into physical address */
bool linux_huge_virt_to_phys(void *addr, uint64_t *phys)
{
    uintptr_t off = (uintptr_t) addr & (HUGEPAGESZ - 1);
    addr = (void *) ((uintptr_t) addr - off);
    if (!linux_virt_to_phys(addr, phys)) {
        return false;
    }

    (*phys) += off;
    return true;
}

errval_t bulk_int_pool_map(struct bulk_pool      *p,
                           enum bulk_buffer_state state,
                           struct pool_meta *meta,
                           int datahandle)
{
    struct bulk_buffer *buffers;
    void *buffers_vbase;
    size_t i;
    char name[MAX_POOLNAME];
    size_t bufsz, bufcnt;
    int fd;




    // Map meta
    if (meta == NULL) {
        // Open pool SHM
        get_poolname(&p->id, name);
        fd = shm_open(name, O_RDWR, 0600);
        assert_fix(fd != -1);

        meta = mmap(NULL, POOL_EXTRA, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
        assert_fix(meta != MAP_FAILED);
        close(fd);
    }

    p->internal.meta = meta;
    __sync_add_and_fetch(&meta->refs, 1);

    bufsz = p->buffer_size = meta->buffer_size;
    bufcnt = p->num_buffers = meta->num_buffers;

    // Map buffers
    if (datahandle == -1) {
        datahandle = shmget(meta->key, meta->pool_size, SHM_HUGETLB | 0666);
        assert(datahandle != -1);

    }
    p->internal.shmid = datahandle;
    buffers_vbase = shmat(datahandle, NULL, 0);
    assert_fix(buffers_vbase != MAP_FAILED);
    memset(buffers_vbase, 0, meta->pool_size);

    p->base_address = buffers_vbase;

    p->buffers = calloc(bufcnt, sizeof(*p->buffers));
    assert_fix(p->buffers != NULL);

    buffers = calloc(bufcnt, sizeof(*buffers));
    assert_fix(buffers != NULL);

    printf("buffer_vbase: %p\n", buffers_vbase);
    for (i = 0; i < bufcnt; i++) {
        p->buffers[i] = buffers + i;

        buffers[i].address = buffers_vbase;
        assert_fix(linux_huge_virt_to_phys(buffers_vbase, &buffers[i].phys));
        buffers[i].pool = p;
        buffers[i].bufferid = i;
        buffers[i].state = state;
        buffers[i].local_ref_count = 0;

        buffers_vbase = (void *) ((uintptr_t) buffers_vbase + bufsz);
    }

    p->internal.next = bulk_pools;
    bulk_pools = p;

    return SYS_ERR_OK;

}

struct bulk_pool *bulk_int_pool_byid(struct bulk_pool_id id)
{
    struct bulk_pool *p;
    for (p = bulk_pools; p != NULL; p = p->internal.next) {
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
    char path[MAX_POOLNAME + 16];
    int fd;
    int res;
    struct pool_meta *meta;
    size_t sz;

    // Allocate ID for pool
    alloc_poolid(&p->id);
    p->buffer_size = buffer_size;
    p->num_buffers = buffer_count;
    p->trust = BULK_TRUST_UNINITIALIZED;

    // Open SHM region and set its size
    get_poolname(&p->id, name);
    fd = shm_open(name, O_CREAT | O_RDWR | O_EXCL, 0600);
    assert_fix(fd != -1);
    res = ftruncate(fd, POOL_EXTRA);
    assert_fix(res == 0);

    // Initialize meta data
    meta = mmap(NULL, POOL_EXTRA, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    assert_fix(meta != MAP_FAILED);

    meta->buffer_size = buffer_size;
    meta->num_buffers = buffer_count;
    sz = buffer_size * buffer_count;
    meta->pool_size = (sz % HUGEPAGESZ != 0 ?
            ((sz + HUGEPAGESZ - 1) & ~(HUGEPAGESZ - 1)) :
            sz);
    snprintf(path, sizeof(path), "/dev/shm/%s", name);
    meta->key = ftok(path, 1);
    assert(meta->key != -1);
    close(fd);

    fd = shmget(meta->key, meta->pool_size,
            IPC_CREAT | IPC_EXCL | SHM_HUGETLB | 0666);
    assert(fd != -1);

    return bulk_int_pool_map(p, BULK_BUFFER_READ_WRITE, meta, fd);
}

errval_t bulk_pool_free(struct bulk_pool *pool)
{
    int res;
    char name[MAX_POOLNAME];
    struct pool_meta *meta;


    // Unmap region
    res = shmdt(pool->base_address);
    assert_fix(res == 0);

    // Free other memory associated
    free(pool->buffers[0]);
    free(pool->buffers);

    // Unlink shm region, if noone else has it mapped
    meta = pool->internal.meta;
    get_poolname(&pool->id, name);
    res = shm_unlink(name);
    assert_fix(res == 0);

    // Unmap meta region
    res = munmap(meta, POOL_EXTRA);
    assert_fix(res == 0);

    return SYS_ERR_OK;

}

void bulk_pool_emergency_cleanup(struct bulk_pool *p)
{
    char name[MAX_POOLNAME];
    shmctl(p->internal.shmid, IPC_RMID, NULL);
    get_poolname(&p->id, name);
    shm_unlink(name);
}

