/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef PROTO_ARP_H_
#define PROTO_ARP_H_

#include <packet_access.h>

#define ARP_HDRLEN_MIN      8
#define ARP_OPER_REQUEST    0x0001
#define ARP_OPER_REPLY      0x0002
#define ARP_HTYPE_ETHERNET  0x0001
#define ARP_PTYPE_IPV4      0x0800

static inline uint8_t arp_hlen_rd(struct input *in);
static inline uint8_t arp_plen_rd(struct input *in);


static inline pktoff_t arp_hdroff(struct input *in)
{
    return in->attr->offset_l3;
}

static inline pktoff_t arp_hdrlen(struct input *in)
{
    return ARP_HDRLEN_MIN + 2 * (arp_hlen_rd(in) + arp_plen_rd(in));
}

static inline pktoff_t arp_alloclen(uint16_t htype, uint16_t ptype) {
    if (htype != ARP_HTYPE_ETHERNET || ptype != ARP_PTYPE_IPV4) {
        panic("arp_alloclen: Unsupported protocol combination\n");
    }
    return ARP_HDRLEN_MIN + 2 * (4 + 6);
}


/******************************************************************************/
/* Field offsets in packet */

static inline pktoff_t arp_off_htype(struct input *in)
{
    return arp_hdroff(in) + 0;
}

static inline pktoff_t arp_off_ptype(struct input *in)
{
    return arp_hdroff(in) + 2;
}

static inline pktoff_t arp_off_hlen(struct input *in)
{
    return arp_hdroff(in) + 4;
}

static inline pktoff_t arp_off_plen(struct input *in)
{
    return arp_hdroff(in) + 5;
}

static inline pktoff_t arp_off_oper(struct input *in)
{
    return arp_hdroff(in) + 6;
}

static inline pktoff_t arp_off_sha(struct input *in)
{
    return arp_hdroff(in) + 8;
}

static inline pktoff_t arp_off_spa(struct input *in)
{
    return arp_hdroff(in) + 8 + arp_hlen_rd(in);
}

static inline pktoff_t arp_off_tha(struct input *in)
{
    return arp_hdroff(in) + 8 + arp_hlen_rd(in) + arp_plen_rd(in);
}

static inline pktoff_t arp_off_tpa(struct input *in)
{
    return arp_hdroff(in) + 8 + 2 * arp_hlen_rd(in) + arp_plen_rd(in);
}


/******************************************************************************/
/* Read Fields */

static inline uint16_t arp_htype_rd(struct input *in)
{
    return pkt_read16be(in, arp_off_htype(in));
}

static inline uint16_t arp_ptype_rd(struct input *in)
{
    return pkt_read16be(in, arp_off_ptype(in));
}

static inline uint8_t arp_hlen_rd(struct input *in)
{
    return pkt_read8(in, arp_off_hlen(in));
}

static inline uint8_t arp_plen_rd(struct input *in)
{
    return pkt_read8(in, arp_off_plen(in));
}

static inline uint16_t arp_oper_rd(struct input *in)
{
    return pkt_read16be(in, arp_off_oper(in));
}

static inline uint64_t arp_sha_eth_rd(struct input *in)
{
    return pkt_read48(in, arp_off_sha(in));
}

static inline uint32_t arp_spa_ipv4_rd(struct input *in)
{
    return pkt_read32be(in, arp_off_spa(in));
}

static inline uint64_t arp_tha_eth_rd(struct input *in)
{
    return pkt_read48(in, arp_off_tha(in));
}

static inline uint32_t arp_tpa_ipv4_rd(struct input *in)
{
    return pkt_read32be(in, arp_off_tpa(in));
}


/******************************************************************************/
/* Write Fields */

static inline void arp_htype_wr(struct input *in, uint16_t val)
{
    return pkt_write16be(in, arp_off_htype(in), val);
}

static inline void arp_ptype_wr(struct input *in, uint16_t val)
{
    return pkt_write16be(in, arp_off_ptype(in), val);
}

static inline void arp_hlen_wr(struct input *in, uint8_t val)
{
    return pkt_write8(in, arp_off_hlen(in), val);
}

static inline void arp_plen_wr(struct input *in, uint8_t val)
{
    return pkt_write8(in, arp_off_plen(in), val);
}

static inline void arp_oper_wr(struct input *in, uint16_t val)
{
    return pkt_write16be(in, arp_off_oper(in), val);
}

static inline void arp_sha_eth_wr(struct input *in, uint64_t val)
{
    return pkt_write48(in, arp_off_sha(in), val);
}

static inline void arp_spa_ipv4_wr(struct input *in, uint32_t val)
{
    return pkt_write32be(in, arp_off_spa(in), val);
}

static inline void arp_tha_eth_wr(struct input *in, uint64_t val)
{
    return pkt_write48(in, arp_off_tha(in), val);
}

static inline void arp_tpa_ipv4_wr(struct input *in, uint32_t val)
{
    return pkt_write32be(in, arp_off_tpa(in), val);
}


#endif // ndef PROTO_ARP_H_

