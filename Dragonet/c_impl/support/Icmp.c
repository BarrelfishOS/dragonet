/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <icmpproto.h>




// ----------------------- ICMP field TYPE -----------
static inline pktoff_t icmp_hdr_type_offset(struct input *pkt)
{
    return icmp_header_field_offset(pkt, 0);
}

uint8_t icmp_hdr_type_read(struct input *pkt)
{
    return pkt_read8(pkt, icmp_hdr_type_offset(pkt));
}

void icmp_hdr_type_write(struct input *pkt, uint8_t val)
{
    return pkt_write8(pkt, icmp_hdr_type_offset(pkt), val);
}


// ----------------------- ICMP field CODE -----------
static inline pktoff_t icmp_hdr_code_offset(struct input *pkt)
{
    return icmp_header_field_offset(pkt, 1);
}

uint8_t icmp_hdr_code_read(struct input *pkt)
{
    return pkt_read8(pkt, icmp_hdr_code_offset(pkt));
}

void icmp_hdr_code_write(struct input *pkt, uint8_t val)
{
    return pkt_write8(pkt, icmp_hdr_code_offset(pkt), val);
}

// ----------------------- ICMP field CHECKSUM -----------
static inline pktoff_t icmp_hdr_checksum_offset(struct input *pkt)
{
    return icmp_header_field_offset(pkt, 2);
}

uint16_t icmp_hdr_checksum_read(struct input *pkt)
{
    return pkt_read16be(pkt, icmp_hdr_checksum_offset(pkt));
}

void icmp_hdr_checksum_write(struct input *pkt, uint16_t val)
{
    return pkt_write16be(pkt, icmp_hdr_checksum_offset(pkt), val);
}

// ----------------------- ICMP field MISC -----------

static inline pktoff_t icmp_hdr_misc_offset(struct input *pkt)
{
    return icmp_header_field_offset(pkt, 4);
}

uint32_t icmp_hdr_misc_read(struct input *pkt)
{
    return pkt_read32be(pkt, icmp_hdr_misc_offset(pkt));
}

void icmp_hdr_misc_write(struct input *pkt, uint32_t val)
{
    return pkt_write32be(pkt, icmp_hdr_misc_offset(pkt), val);
}

