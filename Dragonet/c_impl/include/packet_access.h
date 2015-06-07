/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef PACKET_ACCESS_H_
#define PACKET_ACCESS_H_

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <implementation.h>

#define ENABLE_CHECK_BOUNDS 1
#if ENABLE_CHECK_BOUNDS
    #define CHECK_BOUNDS(in,o,l,f) do { if (o + l > in->len) \
            panic("Out of bounds in "#f"\n") } while (0)
#else
    #define CHECK_BOUNDS(in,o,l,f) do { } while (0)
#endif

/** Read `len' bytes from offset `off' to buffer `d' */
static inline void pkt_read(struct input *in, pktoff_t off, pktoff_t len,
                            void *d)
{
    CHECK_BOUNDS(in, off, len, "pkt_read");
    memcpy(d, (uint8_t *) in->data + off, len);
}

/** Write `len' bytes from buffer `s' to offset `off' in packet */
static inline void pkt_write(struct input *in, pktoff_t off, pktoff_t len,
                             void *s)
{
    CHECK_BOUNDS(in, off, len, "pkt_read");
    memcpy((uint8_t *) in->data + off, s, len);
}

/** Add or remove bytes to/from the end of the packet (uninitialized) */
static inline void pkt_append(struct input *in, ssize_t change)
{
    if (change > 0) {
        if (change > in->space_after) {
            panic("Cannot append space to packet\n");
        }
    } else {
        if (-change > in->len) {
            panic("Trying to remove more bytes than packet len (append)\n");
        }
    }
    in->space_after -= change;
    in->len += change;
}

/** Add or remove bytes to/from the beginning of the packet (uninitialized) */
static inline void pkt_prepend(struct input *in, ssize_t change)
{
    if (change > 0) {
        if (change > in->space_before) {
            panic("Cannot prepend space to packet\n");
        }
    } else {
        if (-change > in->len) {
            panic("Trying to remove more bytes than packet len (prepend)\n");
        }
    }
    in->data = (void *) ((uintptr_t) in->data - change);
    in->phys -= change;
    in->space_before -= change;
    in->len += change;
}


/** clear `len' bytes from offset `off' in the packet buffer */
static inline void pkt_clear(struct input *in, size_t off, size_t len)
{
    CHECK_BOUNDS(in, off, len, "pkt_clear");
    memset((uint8_t *) in->data + off, 0, len);
}

/******************************************************************************/
/* Read without conversion to machine order */

static inline uint8_t pkt_read8(struct input *in, pktoff_t off)
{
    CHECK_BOUNDS(in, off, 1, "pkt_read8");
    return *((uint8_t *) in->data + off);
}

static inline uint16_t pkt_read16(struct input *in, pktoff_t off)
{
    CHECK_BOUNDS(in, off, 2, "pkt_read16");
    return *((uint16_t *) ((uint8_t *) in->data + off));
}

static inline uint32_t pkt_read32(struct input *in, pktoff_t off)
{
    CHECK_BOUNDS(in, off, 4, "pkt_read32");
    return *((uint32_t *) ((uint8_t *) in->data + off));
}

static inline uint64_t pkt_read48(struct input *in, pktoff_t off)
{
    CHECK_BOUNDS(in, off, 6, "pkt_read48");
    // TODO: We can probably do this faster
    uint64_t v = 0;
    pkt_read(in, off, 6, &v);
    return v;
}

static inline uint64_t pkt_read64(struct input *in, pktoff_t off)
{
    CHECK_BOUNDS(in, off, 8, "pkt_read64");
    return *((uint64_t *) ((uint8_t *) in->data + off));
}


/******************************************************************************/
/* Write without conversion from machine order */

static inline void pkt_write8(struct input *in, pktoff_t off, uint8_t val)
{
    CHECK_BOUNDS(in, off, 1, "pkt_write8");
    *((uint8_t *) in->data + off) = val;
}

static inline void pkt_write16(struct input *in, pktoff_t off, uint16_t val)
{
    CHECK_BOUNDS(in, off, 2, "pkt_write16");
    *((uint16_t *) ((uint8_t *) in->data + off)) = val;
}

static inline void pkt_write32(struct input *in, pktoff_t off, uint32_t val)
{
    CHECK_BOUNDS(in, off, 4, "pkt_write32");
    *((uint32_t *) ((uint8_t *) in->data + off)) = val;
}

static inline void pkt_write48(struct input *in, pktoff_t off, uint64_t val)
{
    CHECK_BOUNDS(in, off, 6, "pkt_write48");
    // TODO: We can probably do this faster
    pkt_write(in, off, 6, &val);
}

static inline void pkt_write64(struct input *in, pktoff_t off, uint64_t val)
{
    CHECK_BOUNDS(in, off, 8, "pkt_write64");
    *((uint64_t *) ((uint8_t *) in->data + off)) = val;
}


/******************************************************************************/
/* Reading, convert from big endian to machine order */

static inline uint16_t pkt_read16be(struct input *in, pktoff_t off)
{
    uint16_t v = pkt_read16(in, off);
    // __builtin_bswap16 is missing currently (gcc #52624)
    return (v << 8) | (v >> 8);
}

static inline uint32_t pkt_read32be(struct input *in, pktoff_t off)
{
    return __builtin_bswap32(pkt_read32(in, off));
}

static inline uint64_t pkt_read48be(struct input *in, pktoff_t off)
{
    return __builtin_bswap64(pkt_read64(in, off) << 16);
}

static inline uint64_t pkt_read64be(struct input *in, pktoff_t off)
{
    return __builtin_bswap64(pkt_read64(in, off));
}


/******************************************************************************/
/* Write, convert from machine order to big endian */

static inline void pkt_write16be(struct input *in, pktoff_t off, uint16_t val)
{
    pkt_write16(in, off, (val << 8) | (val >> 8));
}

static inline void pkt_write32be(struct input *in, pktoff_t off, uint32_t val)
{
    pkt_write32(in, off, __builtin_bswap32(val));
}

static inline void pkt_write48be(struct input *in, pktoff_t off, uint64_t val)
{
    pkt_write48(in, off, __builtin_bswap64(val << 16));
}

static inline void pkt_write64be(struct input *in, pktoff_t off, uint64_t val)
{
    pkt_write64(in, off, __builtin_bswap64(val));
}


/******************************************************************************/
/* Reading, convert from little endian to machine order */

static inline uint16_t pkt_read16le(struct input *in, pktoff_t off)
{
    return pkt_read16(in, off);
}

static inline uint32_t pkt_read32le(struct input *in, pktoff_t off)
{
    return pkt_read32(in, off);
}

static inline uint64_t pkt_read48le(struct input *in, pktoff_t off)
{
    return pkt_read48(in, off);
}

static inline uint64_t pkt_read64le(struct input *in, pktoff_t off)
{
    return pkt_read64(in, off);
}


/******************************************************************************/
/* Write, convert from machine order to little endian */

static inline void pkt_write16le(struct input *in, pktoff_t off, uint16_t val)
{
    pkt_write16(in, off, val);
}

static inline void pkt_write32le(struct input *in, pktoff_t off, uint32_t val)
{
    pkt_write32(in, off, val);
}

static inline void pkt_write48le(struct input *in, pktoff_t off, uint64_t val)
{
    pkt_write48(in, off, val);
}

static inline void pkt_write64le(struct input *in, pktoff_t off, uint64_t val)
{
    pkt_write64(in, off, val);
}


#endif // ndef PACKET_ACCESS_H_

