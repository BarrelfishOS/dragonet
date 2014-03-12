#include <implementation.h>
#include <icmpproto.h>

node_out_t do_pg__RxL3ICMPValidHeaderLength(struct state *state, struct input *in)
{

    size_t len = icmp_header_len;
    size_t off = icmp_header_offset(in);
    return (((len - off) >= icmp_header_len)? P_true : P_false);
    // P_true, P_false
}

node_out_t do_pg__RxL3ICMPValidChecksum(struct state *state, struct input *in)
{
    size_t __attribute__((unused)) len = in->len;
    size_t off = icmp_header_offset(in);
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
    // P_TxL3ICMPInitiateResponse_out, P_TxL3ICMPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL3ICMPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL3ICMPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL3ICMPFillHeader(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

