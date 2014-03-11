#include <implementation.h>
#include <ethernetproto.h>
#include "config.h"

node_out_t do_pg__RxL2EtherClassified(struct state *state, struct input *in)
{
    // P_true, P_false
    // Assuming that it is Ethernet packet
    return P_true;
}

node_out_t do_pg__RxL2EtherValidLength(struct state *state, struct input *in)
{
    size_t len = in->len;
    return ((len >= 14) ? P_true : P_false);
}

node_out_t do_pg__RxL2EtherValidUnicast(struct state *state, struct input *in)
{
    return (((eth_dst_mac_read(in) & eth_multicast_bit_mask) == 0)? P_true : P_false);
}

node_out_t do_pg__RxL2EtherValidMulticast(struct state *state, struct input *in)
{
    mac_t dst = eth_dst_mac_read(in);
    return (((dst != eth_broadcast_addr) && (dst & eth_multicast_bit_mask))?
                P_true : P_false);
}

node_out_t do_pg__RxL2EtherValidBroadcast(struct state *state, struct input *in)
{
    return ((eth_dst_mac_read(in) == eth_broadcast_addr)? P_true : P_false);
}

node_out_t do_pg__RxL2EtherValidSrc(struct state *state, struct input *in)
{
    return P_true;
}

node_out_t do_pg__RxL2EtherValidLocalMAC(struct state *state, struct input *in)
{
    return ((eth_dst_mac_read(in) == get_local_mac(state))?P_true: P_false);
}

node_out_t do_pg__RxL2EtherValidType(struct state *state, struct input *in)
{
    return ((eth_type_read(in) >= eth_type_IPv4) ? P_true : P_false);
}

node_out_t do_pg__RxL2EtherClassifyL3(struct state *state, struct input *in)
{
    switch (eth_type_read(in)) {
        case eth_type_IPv4  : return P_RxL2EtherClassifyL3_ipv4;
        case eth_type_IPv6  : return P_RxL2EtherClassifyL3_ipv6;
        case eth_type_ARP   : return P_RxL2EtherClassifyL3_arp;
        default             : return P_RxL2EtherClassifyL3_drop;
    }
    return P_RxL2EtherClassifyL3_drop;
}

node_out_t do_pg__TxL2EtherAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL2EtherAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL2EtherFillHeader(struct state *state, struct input *in)
{
    // P_TxL2EtherFillHeader_out
    return 0;
}

