#include <implementation.h>
#include <icmpproto.h>
#include <proto_ipv4.h>
#include "config.h"

node_out_t do_pg__RxL3ICMPValidHeaderLength(struct state *state, struct input *in)
{

    pktoff_t len = in->len;
    pktoff_t off = icmp_header_offset(in);
    return (((len - off) >= icmp_header_len)? P_true : P_false);
}

node_out_t do_pg__RxL3ICMPValidChecksum(struct state *state, struct input *in)
{
    pktoff_t __attribute__((unused)) len = in->len;
    //pktoff_t off = icmp_header_offset(in);
    // FIXME: calculate the checksum without copying out the packet
    //pkt <- readP (len - off) off
    // FIXME: Verify that the checksum is zero.
    //toPort $ pbool (IP4.checksum pkt == 0)

    // P_true, P_false
    return P_true;
}

node_out_t do_pg__RxL3ICMPIsTypeRequest(struct state *state, struct input *in)
{
    uint8_t icmp_type = icmp_hdr_type_read(in);
    uint8_t icmp_code = icmp_hdr_code_read(in);
    if ((icmp_type == icmpTypeEchoRequest) && (icmp_code == 0x0)) {
        return P_true;
    } else {
        return P_false;
    }
    return P_false;
}

node_out_t do_pg__TxL3ICMPInitiateResponse(struct state *state, struct input *in)
{
    // get packet fields to be sent out.
    uint32_t id = icmp_hdr_misc_read(in);
//    pktoff_t payload_off = icmp_payload_offset(in);
    pktoff_t payload_len = icmp_payload_length(in);

    uint32_t srcIP = ipv4_srcIP_rd(in);
    uint32_t dstIP = ipv4_dstIP_rd(in);
    uint8_t payload[DEFAULT_BUFFER_SIZE];

    // copy out the payload
    int ret = icmp_copy_payload(in, payload, sizeof(payload));
    if (ret < 0) {
        return P_TxL3ICMPInitiateResponse_drop;
    }

    // clean up the packet and attributes
    input_clean_packet(in);
    input_clean_attrs(in);

    in->ip4_dst = srcIP;
    in->ip4_src = dstIP;  // FIXME: maybe I should read it from global state
    in->icmp_id = id;

    // copy the payload to packet
    pkt_prepend(in, payload_len);
    pkt_write(in, 0, payload_len, payload);
    return P_TxL3ICMPInitiateResponse_out;
}

node_out_t do_pg__TxL3ICMPAllocateHeader(struct state *state, struct input *in)
{
    pktoff_t len = icmp_header_len;
    pkt_prepend(in, len);
    in->offset_l5 = 0;
    return P_TxL3ICMPAllocateHeader_out;
}

node_out_t do_pg__TxL3ICMPFillHeader(struct state *state, struct input *in)
{

    icmp_hdr_type_write(in, icmpTypeEchoReply);
    icmp_hdr_code_write(in, 0);
    icmp_hdr_misc_write(in, in->icmp_id);

    uint16_t checksum = 0;
    icmp_hdr_checksum_write(in, checksum);

    pktoff_t off = icmp_payload_offset(in);
    pktoff_t len = in->len;

    checksum = ipv4_checksum(in, off, (len - off));
    icmp_hdr_checksum_write(in, checksum);

    in->ip4_proto = IPV4_PROTO_ICMP;

    return P_true;
}

