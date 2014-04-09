#ifndef LINUX_HUGEPAGE_H_
#define LINUX_HUGEPAGE_H_

#include <stdbool.h>
#include <stdint.h>

/** Allocate a huge page (pinned), and return its virt/phys address and size */
bool huge_alloc_phys(void **virt, uint64_t *phys, size_t *size);
/** Resolve virtual address to physical address (also works for normal pages)*/
bool huge_virt_to_phys(void *addr, uint64_t *phys);

#endif // ndef LINUX_HUGEPAGE_H_
