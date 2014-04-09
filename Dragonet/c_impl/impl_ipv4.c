#include <implementation.h>
#include <proto_ipv4.h>
#include <ethernetproto.h>
#include <inttypes.h>


struct arp_cache *arp_cache_lookup(struct state *st, uint32_t ip);

node_out_t do_pg__RxL3IPv4ValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    pktoff_t iplen = in->len - ipv4_hdroff(in);
    if (iplen < IPV4_HDRLEN_MIN)
        return P_false;
    return PORT_BOOL(iplen >= ipv4_hdrlen(in) && ipv4_ihl_rd(in) >= 4);
}

node_out_t do_pg__RxL3IPv4ValidReassembly(struct state *state, struct input *in)
{
    // P_true, P_false
    return PORT_BOOL((ipv4_flags_rd(in) & IPV4_FLAGS_MF) == 0 &&
                     ipv4_fragment_rd(in) == 0);
}

node_out_t do_pg__RxL3IPv4ValidVersion(struct state *state, struct input *in)
{
    // P_true, P_false
    return PORT_BOOL(ipv4_version_rd(in) == 4);
}

node_out_t do_pg__RxL3IPv4ValidLength(struct state *state, struct input *in)
{
    // P_true, P_false
    pktoff_t iplen = in->len - ipv4_hdroff(in);
    return PORT_BOOL(iplen >= ipv4_hdrlen(in) + ipv4_payload_len(in));
}

node_out_t do_pg__RxL3IPv4ValidTTL(struct state *state, struct input *in)
{
    // P_true, P_false
    return P_true;
}

node_out_t do_pg__RxL3IPv4ValidChecksum(struct state *state, struct input *in)
{
    // P_true, P_false
    return PORT_BOOL(ipv4_checksum(in, ipv4_hdroff(in), ipv4_hdrlen(in)) == 0);
}

node_out_t do_pg__RxL3IPv4ValidLocalIP(struct state *state, struct input *in)
{
//    dprint("%s: srcIP = %"PRIx32", dstIP = %"PRIx32"\n", __func__,
//            ipv4_srcIP_rd(in), ipv4_dstIP_rd(in));

    /*struct arp_cache *c = arp_cache_lookup(state, ipv4_srcIP_rd(in));
    if (c == NULL) {
        c = malloc(sizeof(*c));
        c->ip = ipv4_srcIP_rd(in);
        c->mac =  eth_src_mac_read(in);
        c->next = state->arp_cache;
        state->arp_cache = c;
    }*/

    // P_true, P_false
    return PORT_BOOL(ipv4_dstIP_rd(in) == state->local_ip);
}

node_out_t do_pg__RxL3IPv4Classify(struct state *state, struct input *in)
{
    // P_RxL3IPv4Classify_udp, P_RxL3IPv4Classify_icmp, P_RxL3IPv4Classify_drop
    in->attr->offset_l4 = ipv4_payload_off(in);
    switch (ipv4_protocol_rd(in)) {
        case IPV4_PROTO_ICMP:   return P_RxL3IPv4Classify_icmp;
        case IPV4_PROTO_UDP:    return P_RxL3IPv4Classify_udp;
        default:                return P_RxL3IPv4Classify_drop;
    }
}

node_out_t do_pg__TxL3IPv4AllocateHeader(struct state *state, struct input *in)
{
    // P_TxL3IPv4AllocateHeader_out
    pktoff_t hlen = IPV4_HDRLEN_MIN;
    pkt_prepend(in, hlen);

    in->attr->offset_l3 = 0;
    in->attr->offset_l4 += hlen;
    in->attr->offset_l5 += hlen;

    ipv4_ihl_wr(in, 5);
    return P_TxL3IPv4AllocateHeader_out;
}

node_out_t do_pg__TxL3IPv4FillHeader(struct state *state, struct input *in)
{
    // P_TxL3IPv4FillHeader_out
    pktoff_t hoff = ipv4_hdroff(in);
    ipv4_version_wr(in, 4);
    ipv4_length_wr(in, in->len - hoff);
    ipv4_identification_wr(in, 0);
    ipv4_flags_wr(in, IPV4_FLAGS_DF);
    ipv4_fragment_wr(in, 0);
    ipv4_ttl_wr(in, 17);
    ipv4_protocol_wr(in, in->attr->ip4_proto);
    ipv4_srcIP_wr(in, in->attr->ip4_src);
    ipv4_dstIP_wr(in, in->attr->ip4_dst);

    ipv4_checksum_wr(in, 0);
    ipv4_checksum_wr(in, ipv4_checksum(in, hoff, ipv4_hdrlen(in)));

    in->attr->eth_type = eth_type_IPv4;
    return 0;
}

node_out_t do_pg__TxL3IPv4Routing(struct state *state, struct input *in)
{
    // P_true, P_false
    in->attr->eth_src_mac = state->local_mac;
    return P_true;
}

