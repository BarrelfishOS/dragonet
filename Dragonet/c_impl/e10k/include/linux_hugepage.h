/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef LINUX_HUGEPAGE_H_
#define LINUX_HUGEPAGE_H_

#include <stdbool.h>
#include <stdint.h>

/** Allocate a huge page (pinned), and return its virt/phys address and size */
bool huge_alloc_phys(void **virt, uint64_t *phys, size_t *size);
/** Resolve virtual address to physical address (also works for normal pages)*/
bool huge_virt_to_phys(void *addr, uint64_t *phys);

#endif // ndef LINUX_HUGEPAGE_H_
