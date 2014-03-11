#ifndef PACKET_ACCESS_H_
#define PACKET_ACCESS_H_

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <implementation.h>

/** Read `len' bytes from offset `off' to buffer `d' */
static inline void pkt_read(struct input *in, size_t off, size_t len, void *d)
{
    if (off + len > in->len) {
        panic("Out of bounds in pkt_read\n");
    }

    memcpy(d, (uint8_t *) in->data + off, len);
}

/******************************************************************************/
/* Read without conversion to machine order */

static inline uint8_t pkt_read8(struct input *in, size_t off)
{
    return 0;
}

static inline uint16_t pkt_read16(struct input *in, size_t off)
{
    return 0;
}

static inline uint32_t pkt_read32(struct input *in, size_t off)
{
    return 0;
}

static inline uint64_t pkt_read48(struct input *in, size_t off)
{
    return 0;
}

static inline uint64_t pkt_read64(struct input *in, size_t off)
{
    return 0;
}


/******************************************************************************/
/* Reading, convert from big endian to machine order */

static inline uint16_t pkt_read16be(struct input *in, size_t off)
{
    return 0;
}

static inline uint32_t pkt_read32be(struct input *in, size_t off)
{
    return 0;
}

static inline uint64_t pkt_read48be(struct input *in, size_t off)
{
    return 0;
}

static inline uint64_t pkt_read64be(struct input *in, size_t off)
{
    return 0;
}


/******************************************************************************/
/* Reading, convert from little endian to machine order */

static inline uint16_t pkt_read16le(struct input *in, size_t off)
{
    return 0;
}

static inline uint32_t pkt_read32le(struct input *in, size_t off)
{
    return 0;
}

static inline uint64_t pkt_read48le(struct input *in, size_t off)
{
    return 0;
}

static inline uint64_t pkt_read64le(struct input *in, size_t off)
{
    return 0;
}


#endif // ndef PACKET_ACCESS_H_

