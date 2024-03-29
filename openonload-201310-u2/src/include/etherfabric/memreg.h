/*
** Copyright 2005-2014  Solarflare Communications Inc.
**                      7505 Irvine Center Drive, Irvine, CA 92618, USA
** Copyright 2002-2005  Level 5 Networks Inc.
**
** This library is free software; you can redistribute it and/or
** modify it under the terms of version 2.1 of the GNU Lesser General Public
** License as published by the Free Software Foundation.
**
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Lesser General Public License for more details.
*/

/****************************************************************************
 * Copyright 2012-2012: Solarflare Communications Inc,
 *                      9501 Jeronimo Road, Suite 250,
 *                      Irvine, CA 92618, USA
 *
 * Maintained by Solarflare Communications
 *  <linux-xen-drivers@solarflare.com>
 *  <onload-dev@solarflare.com>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation, incorporated herein by reference.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 ****************************************************************************
 */

#ifndef __EFAB_MEMREG_H__
#define __EFAB_MEMREG_H__

#include <etherfabric/base.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ef_memreg {
	unsigned mr_resource_id;
	ef_addr* mr_dma_addrs;
} ef_memreg;


struct ef_pd;


  /*! Register memory for use with ef_vi.
  **
  **  The physical addresses of this memory must not change, as they will
  **  be used directly by the NIC.  In the case where an application forks
  **  this can result in new pages being allocated.  If this memory is to be
  **  used in an application that will fork, then the caller can use madvise,
  **  setting the MADV_DONTFORK advice to prevent pages in this range being
  **  available to the child after a fork, allowing the parent to maintain the
  **  original pages.
  **
  **   \param vi The VI that these buffers will be used with
  **   \param size The size in bytes of each I/O buffer
  **   \param num The number of buffers required
  **   \param align The alignment requirement of the start of the buffer.
  **          This must be 1, 2, 4 or 8.
  **   \param offset The offset of the start of the I/O buffer
  */
extern int ef_memreg_alloc(ef_memreg*, ef_driver_handle,
			   struct ef_pd*, ef_driver_handle pd_dh,
			   void* p_mem, int len_bytes);

  /*! Unregister a memory region. */
extern int ef_memreg_free(ef_memreg*, ef_driver_handle);


  /*! Returns the [ef_addr] corresponding to the given offset within
   * registered region.
   */
ef_vi_inline ef_addr ef_memreg_dma_addr(ef_memreg* mr, int offset)
{
	return mr->mr_dma_addrs[offset >> EF_VI_NIC_PAGE_SHIFT] |
		(offset & (EF_VI_NIC_PAGE_SIZE - 1));
}

#ifdef __cplusplus
}
#endif

#endif  /* __EFAB_MEMREG_H__ */
