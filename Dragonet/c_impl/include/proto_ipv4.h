#ifndef PROTO_IPV4_H_
#define PROTO_IPV4_H_

#include <packet_access.h>



#define IPV4_HDRLEN_MIN 20
#define IPV4_FLAGS_DF   0x02
#define IPV4_FLAGS_MF   0x04 // TODO: verify
#define IPV4_PROTO_ICMP 0x01
#define IPV4_PROTO_TCP  0x06
#define IPV4_PROTO_UDP  0x11

static inline uint8_t ipv4_ihl_rd(struct input *in);
static inline uint16_t ipv4_length_rd(struct input *in);

static inline pktoff_t ipv4_hdroff(struct input *in)
{
    return in->offset_l3;
}

static inline pktoff_t ipv4_hdrlen(struct input *in)
{
    return 4 * ipv4_ihl_rd(in);
}

static inline pktoff_t ipv4_payload_off(struct input *in)
{
    return ipv4_hdroff(in) + ipv4_hdrlen(in);
}

static inline pktoff_t ipv4_payload_len(struct input *in)
{
    return ipv4_length_rd(in) - ipv4_hdrlen(in);
}

/** Generic IP checksum, note the return value is in host order */
static inline uint16_t ipv4_checksum(struct input *in, pktoff_t off,
                                     pktoff_t len)
{
    uint32_t checksum = 0;
    while (len > 1) {
        checksum += pkt_read16be(in, off);
        len -= 2;
        off += 2;
    }
    if (len > 0) {
        checksum += (uint32_t) pkt_read8(in, off) << 8;
    }
    checksum = ((checksum & 0xffff) + (checksum >> 16));
    checksum = ((checksum & 0xffff) + (checksum >> 16));
    checksum = checksum ^ 0xffff;
    return checksum;
}


/******************************************************************************/
/* Field offsets in packet */

static inline pktoff_t ipv4_off_version(struct input *in)
{
    return ipv4_hdroff(in) + 0;
}

static inline pktoff_t ipv4_off_ihl(struct input *in)
{
    return ipv4_hdroff(in) + 0;
}

static inline pktoff_t ipv4_off_length(struct input *in)
{
    return ipv4_hdroff(in) + 2;
}

static inline pktoff_t ipv4_off_identification(struct input *in)
{
    return ipv4_hdroff(in) + 4;
}

static inline pktoff_t ipv4_off_flags(struct input *in)
{
    return ipv4_hdroff(in) + 6;
}

static inline pktoff_t ipv4_off_fragment(struct input *in)
{
    return ipv4_hdroff(in) + 6;
}

static inline pktoff_t ipv4_off_ttl(struct input *in)
{
    return ipv4_hdroff(in) + 8;
}

static inline pktoff_t ipv4_off_protocol(struct input *in)
{
    return ipv4_hdroff(in) + 9;
}

static inline pktoff_t ipv4_off_checksum(struct input *in)
{
    return ipv4_hdroff(in) + 10;
}

static inline pktoff_t ipv4_off_srcIP(struct input *in)
{
    return ipv4_hdroff(in) + 12;
}

static inline pktoff_t ipv4_off_dstIP(struct input *in)
{
    return ipv4_hdroff(in) + 16;
}


/******************************************************************************/
/* Read Fields */

static inline uint8_t ipv4_version_rd(struct input *in)
{
    return (pkt_read8(in, ipv4_off_version(in)) & 0xf0) >> 4;
}

static inline uint8_t ipv4_ihl_rd(struct input *in)
{
    return pkt_read8(in, ipv4_off_ihl(in)) & 0xf;
}

static inline uint16_t ipv4_length_rd(struct input *in)
{
    return pkt_read16be(in, ipv4_off_length(in));
}

static inline uint16_t ipv4_identification_rd(struct input *in)
{
    return pkt_read16be(in, ipv4_off_identification(in));
}

static inline uint8_t ipv4_flags_rd(struct input *in)
{
    return (pkt_read8(in, ipv4_off_flags(in)) & 0xe0) >> 5;
}

static inline uint16_t ipv4_fragment_rd(struct input *in)
{
    return pkt_read16be(in, ipv4_off_flags(in)) & 0x1fff;
}

static inline uint8_t ipv4_ttl_rd(struct input *in)
{
    return pkt_read8(in, ipv4_off_ttl(in));
}

static inline uint8_t ipv4_protocol_rd(struct input *in)
{
    return pkt_read8(in, ipv4_off_protocol(in));
}

static inline uint16_t ipv4_checksum_rd(struct input *in)
{
    return pkt_read16be(in, ipv4_off_checksum(in));
}

static inline uint32_t ipv4_srcIP_rd(struct input *in)
{
    return pkt_read32be(in, ipv4_off_srcIP(in));
}

static inline uint32_t ipv4_dstIP_rd(struct input *in)
{
    return pkt_read32be(in, ipv4_off_dstIP(in));
}


/******************************************************************************/
/* Write Fields */

static inline void ipv4_version_wr(struct input *in, uint8_t val)
{
    return pkt_write8(in, ipv4_off_version(in),
            (val << 4) | (pkt_read8(in, ipv4_off_version(in)) & 0x0f));
}

static inline void ipv4_ihl_wr(struct input *in, uint8_t val)
{
    return pkt_write8(in, ipv4_off_ihl(in),
            val | (pkt_read8(in, ipv4_off_ihl(in)) & 0xf0));
}

static inline void ipv4_length_wr(struct input *in, uint16_t val)
{
    return pkt_write16be(in, ipv4_off_length(in), val);
}

static inline void ipv4_identification_wr(struct input *in, uint32_t val)
{
    return pkt_write32be(in, ipv4_off_identification(in), val);
}

static inline void ipv4_flags_wr(struct input *in, uint8_t val)
{
    return pkt_write8(in, ipv4_off_flags(in),
            (val << 5) | (pkt_read8(in, ipv4_off_flags(in)) & 0x1f));
}

static inline void ipv4_fragment_wr(struct input *in, uint16_t val)
{
    return pkt_write16be(in, ipv4_off_fragment(in),
            val | (pkt_read16be(in, ipv4_off_fragment(in)) & 0xe000));
}

static inline void ipv4_ttl_wr(struct input *in, uint8_t val)
{
    return pkt_write8(in, ipv4_off_ttl(in), val);
}

static inline void ipv4_protocol_wr(struct input *in, uint8_t val)
{
    return pkt_write8(in, ipv4_off_protocol(in), val);
}

static inline void ipv4_checksum_wr(struct input *in, uint16_t val)
{
    return pkt_write16be(in, ipv4_off_checksum(in), val);
}

static inline void ipv4_srcIP_wr(struct input *in, uint32_t val)
{
    return pkt_write32be(in, ipv4_off_srcIP(in), val);
}

static inline void ipv4_dstIP_wr(struct input *in, uint32_t val)
{
    return pkt_write32be(in, ipv4_off_dstIP(in), val);
}


#endif // ndef PROTO_IPV4_H_

