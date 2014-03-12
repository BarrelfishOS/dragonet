#include <stdint.h>
#include <implementation.h>
#include <ethernetproto.h>

node_out_t do_pg__RxL2EtherClassified(struct state *state, struct input *in)
{
    // P_true, P_false
    // setting the offset of Ethernet packet starting
    in->offset_l2 = 0;
    return P_true;
}

node_out_t do_pg__RxL2EtherValidLength(struct state *state, struct input *in)
{
    size_t len = in->len;
    return PORT_BOOL(len >= 14);
}

node_out_t do_pg__RxL2EtherValidUnicast(struct state *state, struct input *in)
{
    return PORT_BOOL((eth_dst_mac_read(in) & eth_multicast_bit_mask) == 0);
}

node_out_t do_pg__RxL2EtherValidMulticast(struct state *state, struct input *in)
{
    mac_t dst = eth_dst_mac_read(in);
    dprint("%s: %lx & %lx == %lx\n", __func__, dst, eth_multicast_bit_mask,
            (dst & eth_multicast_bit_mask));
    return PORT_BOOL((dst != eth_broadcast_addr) &&
                     (dst & eth_multicast_bit_mask));
}

node_out_t do_pg__RxL2EtherValidBroadcast(struct state *state, struct input *in)
{
    mac_t dst = eth_dst_mac_read(in);
    dprint("%s: %lx == %lx\n", __func__, dst, eth_broadcast_addr);
    return PORT_BOOL(dst == eth_broadcast_addr);
}

node_out_t do_pg__RxL2EtherValidSrc(struct state *state, struct input *in)
{
    return P_true;
}

node_out_t do_pg__RxL2EtherValidLocalMAC(struct state *state, struct input *in)
{
    mac_t dst = eth_dst_mac_read(in);
    mac_t localmac = state->local_mac;
    dprint("%s: %lx == %lx\n", __func__, dst, localmac);
    dprint("%s: src mac == %lx\n", __func__, eth_src_mac_read(in));
    return PORT_BOOL(dst == localmac || dst == eth_broadcast_addr);
    //return ((eth_dst_mac_read(in) == get_local_mac(state))?P_true: P_false);
}

node_out_t do_pg__RxL2EtherValidType(struct state *state, struct input *in)
{
    return PORT_BOOL(eth_type_read(in) >= eth_type_IPv4);
}

node_out_t do_pg__RxL2EtherClassifyL3(struct state *state, struct input *in)
{
    in->offset_l3 = 14;
    switch (eth_type_read(in)) {
        case eth_type_IPv4  : dprint("%s: pkt IPv4\n", __func__);
                              return P_RxL2EtherClassifyL3_ipv4;
        case eth_type_IPv6  : dprint("%s: pkt IPv6\n", __func__);
                              return P_RxL2EtherClassifyL3_ipv6;
        case eth_type_ARP   : dprint("%s: pkt ARP\n", __func__);
                              return P_RxL2EtherClassifyL3_arp;
        default             : dprint("%s: pkt DROP\n", __func__);
                              return P_RxL2EtherClassifyL3_drop;
    }
    return P_RxL2EtherClassifyL3_drop;
}

node_out_t do_pg__TxL2EtherAllocateHeader(struct state *state, struct input *in)
{

    pkt_prepend(in, ethernet_header_len);
    pkt_clear(in,  l2Offset(in), ethernet_header_len);

    // Moving L3 and L4 header offset (in case someone still modifies them)
    in->offset_l3 += (ethernet_header_len);
    in->offset_l4 += (ethernet_header_len);
    in->offset_l5 += (ethernet_header_len);
    return P_TxL2EtherAllocateHeader_out;
}

node_out_t do_pg__TxL2EtherFillHeader(struct state *state, struct input *in)
{

    mac_t src_mac = in->eth_src_mac;
    mac_t dst_mac = in->eth_dst_mac;
    uint16_t ethType = in->eth_type;
    eth_src_mac_write(in, src_mac);
    eth_dst_mac_write(in, dst_mac);
    eth_type_write(in, ethType);
    return P_TxL2EtherFillHeader_out;
}

