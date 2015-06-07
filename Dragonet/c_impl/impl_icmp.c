/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <implementation.h>
#include <icmpproto.h>
#include <proto_ipv4.h>
#include "config.h"

node_out_t do_pg__RxL3ICMPValidHeaderLength(
        struct ctx_RxL3ICMPValidHeaderLength *context, struct state *state,
        struct input **in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);

    pktoff_t len = (*in)->len;
    pktoff_t off = icmp_header_offset(*in);
    return (((len - off) >= icmp_header_len)? P_true : P_false);
}

node_out_t do_pg__RxL3ICMPValidChecksum(
        struct ctx_RxL3ICMPValidChecksum *context, struct state *state,
        struct input **in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // calculate the checksum without copying out the packet
    pktoff_t off = icmp_header_offset(*in);
    pktoff_t len = (*in)->len;
    uint16_t checksum = ipv4_checksum(*in, off, (len - off));
    return ((checksum == 0)?P_true: P_false);
}

node_out_t do_pg__RxL3ICMPIsTypeRequest(
        struct ctx_RxL3ICMPIsTypeRequest *context, struct state *state,
        struct input **in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    uint8_t icmp_type = icmp_hdr_type_read(*in);
    uint8_t icmp_code = icmp_hdr_code_read(*in);
    if ((icmp_type == icmpTypeEchoRequest) && (icmp_code == 0x0)) {
        ++debug_pkt_stats.rx_icmpv;
        return P_true;
    } else {
        return P_false;
    }
    return P_false;
}

node_out_t do_pg__TxL3ICMPInitiateResponse(
        struct ctx_TxL3ICMPInitiateResponse *context, struct state *state,
        struct input **in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // get packet fields to be sent out.
    uint32_t id = icmp_hdr_misc_read(*in);
    uint32_t srcIP = ipv4_srcIP_rd(*in);
    uint32_t dstIP = ipv4_dstIP_rd(*in);

#if DO_EXPLICIT_COPY
    pktoff_t payload_len = icmp_payload_length(*in);
    uint8_t payload[DEFAULT_BUFFER_SIZE];
    int ret = icmp_copy_payload(*in, payload, sizeof(payload));
    if (ret < 0) {
        return P_TxL4ICMPInitiateResponse_drop;
    }
#endif // DO_EXPLICIT_COPY

    // Throw out all headers up to and including ICMP
    pkt_prepend(*in, - (ssize_t) icmp_payload_offset(*in));
    // clean up the attributes
    input_clean_attrs(*in);

#if DO_EXPLICIT_COPY
    pkt_write(*in, 0, payload_len, payload);
#endif // DO_EXPLICIT_COPY

    (*in)->attr->ip4_dst = srcIP;
    (*in)->attr->ip4_src = dstIP;  // FIXME: maybe I should read it from global state
    (*in)->attr->icmp_id = id;

    ++debug_pkt_stats.tx_icmpv;
    return P_TxL3ICMPInitiateResponse_out;
}

node_out_t do_pg__TxL3ICMPAllocateHeader(
        struct ctx_TxL3ICMPAllocateHeader *context, struct state *state,
        struct input **in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    pktoff_t len = icmp_header_len;
    pkt_prepend(*in, len);
    (*in)->attr->offset_l5 = 0;
    return P_TxL3ICMPAllocateHeader_out;
}

node_out_t do_pg__TxL3ICMPFillHeader(struct ctx_TxL3ICMPFillHeader *context,
        struct state *state, struct input **in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);

    icmp_hdr_type_write(*in, icmpTypeEchoReply);
    icmp_hdr_code_write(*in, 0);
    icmp_hdr_misc_write(*in, (*in)->attr->icmp_id);

    uint16_t checksum = 0;
    icmp_hdr_checksum_write(*in, checksum);

    pktoff_t off = icmp_header_offset(*in);
    pktoff_t len = (*in)->len;

    checksum = ipv4_checksum(*in, off, (len - off));
    icmp_hdr_checksum_write(*in, checksum);

    (*in)->attr->ip4_proto = IPV4_PROTO_ICMP;

    return P_true;
}

