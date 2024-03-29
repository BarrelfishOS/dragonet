/*
** Copyright 2005-2014  Solarflare Communications Inc.
**                      7505 Irvine Center Drive, Irvine, CA 92618, USA
** Copyright 2002-2005  Level 5 Networks Inc.
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of version 2 of the GNU General Public License as
** published by the Free Software Foundation.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
*/

/****************************************************************************
 * Copyright 2002-2005: Level 5 Networks Inc.
 * Copyright 2005-2008: Solarflare Communications Inc,
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

/*! \cidoxg_include_ci_compat  */

#ifndef __CI_COMPAT_X86_H__
#define __CI_COMPAT_X86_H__


#define CI_MY_BYTE_ORDER   CI_LITTLE_ENDIAN

#define CI_WORD_SIZE       4
#define CI_PTR_SIZE        4

#define CI_PAGE_SIZE       4096
#define CI_PAGE_SHIFT      12
#define CI_PAGE_MASK       (~(CI_PAGE_SIZE - 1))

#define CI_CPU_HAS_SSE	   1	/* SSE extensions supported */
#define CI_CPU_HAS_SSE2	   0	/* SSE2 extensions supported */
#define CI_CPU_OOS	   0	/* CPU does out of order stores */

#define CI_CPU_HAS_IOSPACE 1 /* CPU has a separate IO space */

#endif  /* __CI_COMPAT_X86_H__ */

/*! \cidoxg_end */
