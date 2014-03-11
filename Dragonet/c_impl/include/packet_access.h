#ifndef PACKET_ACCESS_H_
#define PACKET_ACCESS_H_

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <implementation.h>

#define CHECK_BOUNDS(in,o,l,f) do { if (o + l > in->len) \
    panic("Out of bounds in "#f"\n") } while (0)

/** Read `len' bytes from offset `off' to buffer `d' */
static inline void pkt_read(struct input *in, size_t off, size_t len, void *d)
{
    CHECK_BOUNDS(in, off, len, "pkt_read");
    memcpy(d, (uint8_t *) in->data + off, len);
}

/******************************************************************************/
/* Read without conversion to machine order */

static inline uint8_t pkt_read8(struct input *in, size_t off)
{
    CHECK_BOUNDS(in, off, 1, "pkt_read8");
    return *((uint8_t *) in->data + off);
}

static inline uint16_t pkt_read16(struct input *in, size_t off)
{
    CHECK_BOUNDS(in, off, 2, "pkt_read16");
    return *((uint16_t *) ((uint8_t *) in->data + off));
}

static inline uint32_t pkt_read32(struct input *in, size_t off)
{
    CHECK_BOUNDS(in, off, 4, "pkt_read32");
    return *((uint32_t *) ((uint8_t *) in->data + off));
}

static inline uint64_t pkt_read48(struct input *in, size_t off)
{
    CHECK_BOUNDS(in, off, 6, "pkt_read48");
    // TODO: We can probably do this faster
    uint64_t v = 0;
    pkt_read(in, off, 6, &v);
    return v;
}

static inline uint64_t pkt_read64(struct input *in, size_t off)
{
    CHECK_BOUNDS(in, off, 8, "pkt_read64");
    return *((uint64_t *) ((uint8_t *) in->data + off));
}


/******************************************************************************/
/* Reading, convert from big endian to machine order */

static inline uint16_t pkt_read16be(struct input *in, size_t off)
{
    uint16_t v = pkt_read16(in, off);
    // __builtin_bswap16 is missing currently (gcc #52624)
    return (v << 8) | (v >> 8);
}

static inline uint32_t pkt_read32be(struct input *in, size_t off)
{
    return __builtin_bswap32(pkt_read32(in, off));
}

static inline uint64_t pkt_read48be(struct input *in, size_t off)
{
    return __builtin_bswap64(pkt_read64(in, off) << 16);
}

static inline uint64_t pkt_read64be(struct input *in, size_t off)
{
    return __builtin_bswap64(pkt_read64(in, off));
}


/******************************************************************************/
/* Reading, convert from little endian to machine order */

static inline uint16_t pkt_read16le(struct input *in, size_t off)
{
    return pkt_read16(in, off);
}

static inline uint32_t pkt_read32le(struct input *in, size_t off)
{
    return pkt_read32(in, off);
}

static inline uint64_t pkt_read48le(struct input *in, size_t off)
{
    return pkt_read48(in, off);
}

static inline uint64_t pkt_read64le(struct input *in, size_t off)
{
    return pkt_read64(in, off);
}


#endif // ndef PACKET_ACCESS_H_

