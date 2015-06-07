/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef ETHERNET_PROTOCOL_H_
#define ETHERNET_PROTOCOL_H_

#include <stddef.h>
#include <stdio.h>

#include <implementation.h>
#include <packet_access.h>

// ---------------------------- --------------------
#define eth_type_IPv4       (0x0800)
#define eth_type_IPv6       (0x86DD)
#define eth_type_ARP        (0x0806)

// Ethernet header length
#define ethernet_header_len     (14)

//#define l2Offset(x)                (0)
static inline size_t l2Offset(struct input *pkt)
{
    return pkt->attr->offset_l2;
}

typedef uint64_t mac_t;

// NOTE:when read with pkt_read48
#define eth_broadcast_addr               ((mac_t) 0xffffffffffff)
//#define eth_multicast_bit_mask           ((mac_t) 0x100000000000)
#define eth_multicast_bit_mask           ((mac_t) 0x000000000001)

mac_t eth_src_mac_read(struct input *pkt);
void eth_src_mac_write(struct input *pkt, mac_t val);
mac_t eth_dst_mac_read(struct input *pkt);
void eth_dst_mac_write(struct input *pkt, mac_t val);
uint16_t eth_type_read(struct input *pkt);
void eth_type_write(struct input *pkt, uint16_t val);
uint32_t eth_checksum_read(struct input *pkt);
void eth_checksum_write(struct input *pkt, uint32_t val);

#endif // ETHERNET_PROTOCOL_H_
