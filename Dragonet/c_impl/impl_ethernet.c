/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdint.h>
#include <implementation.h>
#include <ethernetproto.h>

node_out_t do_pg__RxL2EtherClassified(struct ctx_RxL2EtherClassified *context,
        struct state *state, struct input **in)
{
    // P_true, P_false
    // setting the offset of Ethernet packet starting
    (*in)->attr->offset_l2 = 0;
    return P_true;
}

node_out_t do_pg__RxL2EtherValidLength(struct ctx_RxL2EtherValidLength *context,
        struct state *state, struct input **in)
{
    size_t len = (*in)->len;
    return PORT_BOOL(len >= 14);
}

node_out_t do_pg__RxL2EtherValidUnicast(
        struct ctx_RxL2EtherValidUnicast *context, struct state *state,
        struct input **in)
{
    return PORT_BOOL((eth_dst_mac_read(*in) & eth_multicast_bit_mask) == 0);
}

node_out_t do_pg__RxL2EtherValidMulticast(
        struct ctx_RxL2EtherValidMulticast *context, struct state *state,
        struct input **in)
{
    mac_t dst = eth_dst_mac_read(*in);
//    dprint("%s: %lx & %lx == %lx\n", __func__, dst, eth_multicast_bit_mask,
//            (dst & eth_multicast_bit_mask));
    return PORT_BOOL((dst != eth_broadcast_addr) &&
                     (dst & eth_multicast_bit_mask));
}

node_out_t do_pg__RxL2EtherValidBroadcast(
        struct ctx_RxL2EtherValidBroadcast *context, struct state *state,
        struct input **in)
{
    mac_t dst = eth_dst_mac_read(*in);
//    dprint("%s: %lx == %lx\n", __func__, dst, eth_broadcast_addr);
    return PORT_BOOL(dst == eth_broadcast_addr);
}

node_out_t do_pg__RxL2EtherValidSrc(struct ctx_RxL2EtherValidSrc *context,
        struct state *state, struct input **in)
{
    return P_true;
}

node_out_t do_pg__RxL2EtherValidLocalMAC(
        struct ctx_RxL2EtherValidLocalMAC *context, struct state *state,
        struct input **in)
{
    mac_t dst = eth_dst_mac_read(*in);
    mac_t localmac = state->local_mac;
//    dprint("%s: %lx == %lx\n", __func__, dst, localmac);
//    dprint("%s: src mac == %lx\n", __func__, eth_src_mac_read(in));
    return PORT_BOOL(dst == localmac || dst == eth_broadcast_addr);
    //return ((eth_dst_mac_read(in) == get_local_mac(state))?P_true: P_false);
}

node_out_t do_pg__RxL2EtherValidType(struct ctx_RxL2EtherValidType *context,
        struct state *state, struct input **in)
{
    return PORT_BOOL(eth_type_read(*in) >= eth_type_IPv4);
}

node_out_t do_pg__RxL2EtherClassifyL3(struct ctx_RxL2EtherClassifyL3 *context,
        struct state *state, struct input **in)
{
    (*in)->attr->offset_l3 = 14;

    ++debug_pkt_stats.rx_ethv;
    switch (eth_type_read(*in)) {
        case eth_type_IPv4  : dprint("%s: pkt IPv4\n", __func__);
                              ++debug_pkt_stats.rx_ipv4;
                              return P_RxL2EtherClassifyL3_ipv4;
        case eth_type_IPv6  : dprint("%s: pkt IPv6\n", __func__);
                              ++debug_pkt_stats.rx_ipv6;
                              return P_RxL2EtherClassifyL3_ipv6;
        case eth_type_ARP   : dprint("%s: pkt ARP\n", __func__);
                              ++debug_pkt_stats.rx_arp;
                              return P_RxL2EtherClassifyL3_arp;
        default             : dprint("%s: pkt DROP\n", __func__);
                              ++debug_pkt_stats.rx_drop;
                              return P_RxL2EtherClassifyL3_drop;
    }
    ++debug_pkt_stats.rx_drop;
    return P_RxL2EtherClassifyL3_drop;
}

node_out_t do_pg__TxL2EtherAllocateHeader(
        struct ctx_TxL2EtherAllocateHeader *context, struct state *state,
        struct input **in)
{

    pkt_prepend(*in, ethernet_header_len);
    pkt_clear(*in,  l2Offset(*in), ethernet_header_len);

    // Moving L3 and L4 header offset (in case someone still modifies them)
    (*in)->attr->offset_l3 += (ethernet_header_len);
    (*in)->attr->offset_l4 += (ethernet_header_len);
    (*in)->attr->offset_l5 += (ethernet_header_len);
    return P_TxL2EtherAllocateHeader_out;
}

node_out_t do_pg__TxL2EtherFillHeader(struct ctx_TxL2EtherFillHeader *context,
        struct state *state, struct input **in)
{

    ++debug_pkt_stats.tx_ethv;
    mac_t src_mac = (*in)->attr->eth_src_mac;
    mac_t dst_mac = (*in)->attr->eth_dst_mac;
    uint16_t ethType = (*in)->attr->eth_type;
    eth_src_mac_write(*in, src_mac);
    eth_dst_mac_write(*in, dst_mac);
    eth_type_write(*in, ethType);
    return P_TxL2EtherFillHeader_out;
}

