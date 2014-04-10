#include <stdlib.h>
#include <string.h>
#include <implementation.h>
#include <inttypes.h>

#include <udpproto.h>
#include <proto_ipv4.h>

node_out_t do_pg__RxL4UDPValidHeaderLength(struct state *state, struct input *in)
{
    in->attr->offset_l5 = udp_payload_offset(in);
    return PORT_BOOL(((in->len) - (udp_header_offset(in))) >= udp_header_len);
}

node_out_t do_pg__RxL4UDPValidLength(struct state *state, struct input *in)
{
    return PORT_BOOL((in->len) >=
            ((udp_header_offset(in)) + udp_hdr_pkt_length_read(in)));
}


node_out_t do_pg__RxL4UDPValidChecksum(struct state *state, struct input *in)
{
    pktoff_t off = udp_header_offset(in);
    pktoff_t len = in->len;
    uint16_t checksum = udp_hdr_checksum_read(in);
    if (checksum == 0) {
        dprint("#### checksum is disabled\n");
        return P_true;
    }

    uint8_t buf[PSUDO_IPV4_HEADER_LEN];
    struct input psudo_hdr;
    memset(buf, 0, sizeof(buf));
    memset(&psudo_hdr, 0, sizeof(psudo_hdr));
    psudo_hdr.data = buf;
    psudo_hdr.space_before = 0;
    psudo_hdr.space_after = PSUDO_IPV4_HEADER_LEN;

    psudo_header_rx(in, &psudo_hdr);
    uint32_t checksum_psudo_hdr = ipv4_checksum_incremental(&psudo_hdr, 0,
            PSUDO_IPV4_HEADER_LEN, 0);
    uint16_t checksum_final = ipv4_checksum_incremental_final(in, off,
            (len - off), checksum_psudo_hdr);

    if (checksum_final != 0) {
        printf("ERROR: UDP checksum is invalid: %"PRIu16"\n", checksum_final);
    }
    //return PORT_BOOL((checksum_final == 0));
    return P_true;
}

node_out_t do_pg__RxL4UDPPortClassifyType(struct state *state, struct input *in)
{
    // P_RxL4UDPPortClassifyType_static, P_RxL4UDPPortClassifyType_dynamic
    return P_RxL4UDPPortClassifyType_static;
}

node_out_t do_pg__RxL4UDPPortClassifyStatic(struct state *state, struct input *in)
{
    portno_t dport = udp_hdr_dport_read(in);
    switch(dport) {
        case 51098: return P_RxL4UDPPortClassifyStatic_appDNS;
        case 5556: return P_RxL4UDPPortClassifyStatic_appEcho;
        case 7: return P_RxL4UDPPortClassifyStatic_appEcho;
        default: return P_RxL4UDPPortClassifyStatic_closedPort;
    }
    return P_RxL4UDPPortClassifyStatic_closedPort;
}

node_out_t do_pg__RxL4UDPPortClassifyDynamic(struct state *state, struct input *in)
{
    //
    return 0;
}

node_out_t do_pg__RxL4UDPClosedPortAction(struct state *state, struct input *in)
{
    dprint("#### UDP packet on closed port %"PRIu16"\n",
            udp_hdr_dport_read(in));
    return P_RxL4UDPClosedPortAction_out;
}

node_out_t do_pg__RxEchoAPP(struct state *state, struct input *in)
{
    uint8_t contents[DEFAULT_BUFFER_SIZE];
    int ret = udp_copy_payload(in, contents, sizeof(contents));
    // get packet fields to be sent out.
    portno_t sport = udp_hdr_sport_read(in);
    portno_t dport = udp_hdr_dport_read(in);
    in->attr->udp_sport = dport;
    in->attr->udp_dport = sport;


    contents[ret] = '\0';
    //printf
    //dprint
    //    ("EchoAPP: Packet received ############## [%s]\n", contents);
    // P_RxEchoAPP_out, P_RxEchoAPP_drop
    return P_RxEchoAPP_out;
}

node_out_t do_pg__RxDnsAPP(struct state *state, struct input *in)
{
    uint8_t contents[DEFAULT_BUFFER_SIZE];
    int ret = udp_copy_payload(in, contents, sizeof(contents));
    portno_t sport = udp_hdr_sport_read(in);
    portno_t dport = udp_hdr_dport_read(in);
    in->attr->udp_sport = dport;
    in->attr->udp_dport = (sport + 1);

    contents[ret] = '\0';
    dprint
    //printf
        ("DnsAPP: Packet received ##############\n");
    dprint
    //printf
        ("DnsAPP: reflecting back to %"PRIu16" instead of %"PRIu16"\n",
            in->attr->udp_dport, sport);
    // P_RxDnsAPP_out, P_RxDnsAPP_drop
    return P_RxDnsAPP_out;
}


//#define DO_EXPLICIT_COPY

node_out_t do_pg__TxL4UDPInitiateResponse(struct state *state, struct input *in)
{

    uint32_t srcIP = ipv4_srcIP_rd(in);
    uint32_t dstIP = ipv4_dstIP_rd(in);

    // copy out application set attributes so that they will not get cleared
    // get packet fields to be sent out.
    portno_t sport = in->attr->udp_sport; // udp_hdr_sport_read(in);
    portno_t dport = in->attr->udp_dport; // udp_hdr_dport_read(in);

#if DO_EXPLICIT_COPY
    pktoff_t payload_len = udp_payload_length(in);
    uint8_t payload[DEFAULT_BUFFER_SIZE];
    int ret = udp_copy_payload(in, payload, sizeof(payload));
    if (ret < 0) {
        return P_TxL4UDPInitiateResponse_drop;
    }
#endif // DO_EXPLICIT_COPY

    // Throw out all headers up to and including ICMP
    pkt_prepend(in, - (ssize_t) udp_payload_offset(in));
    // clean up the attributes
    input_clean_attrs(in);

#if DO_EXPLICIT_COPY
    pkt_write(in, 0, payload_len, payload);
#endif // DO_EXPLICIT_COPY

    in->attr->ip4_dst = srcIP;
    in->attr->ip4_src = dstIP;  // FIXME: maybe I should read it from global state
    in->attr->udp_sport = sport;
    in->attr->udp_dport = dport;

    // P_TxL4UDPInitiateResponse_drop
    return P_TxL4UDPInitiateResponse_out;
}

node_out_t do_pg__TxL4UDPAllocateHeader(struct state *state, struct input *in)
{
    pktoff_t len = udp_header_len;
    pkt_prepend(in, len);
    in->attr->offset_l5 = 0;
    // P_TxL4UDPAllocateHeader_out
    return P_TxL4UDPAllocateHeader_out;
}

node_out_t do_pg__TxL4UDPFillHeader(struct state *state, struct input *in)
{

    udp_hdr_sport_write(in, in->attr->udp_sport);
    udp_hdr_dport_write(in, in->attr->udp_dport);
    udp_hdr_pkt_length_write(in, in->len);

    in->attr->ip4_proto = IPV4_PROTO_UDP;

    uint16_t checksum = 0;
    udp_hdr_checksum_write(in, checksum);

//#if 0
    uint8_t buf[PSUDO_IPV4_HEADER_LEN];
    struct input psudo_hdr;
    memset(buf, 0, sizeof(buf));
    memset(&psudo_hdr, 0, sizeof(psudo_hdr));
    psudo_hdr.data = buf;
    psudo_hdr.space_before = 0;
    psudo_hdr.space_after = PSUDO_IPV4_HEADER_LEN;

    psudo_header_tx(in, &psudo_hdr);
    uint32_t checksum_psudo_hdr = ipv4_checksum_incremental(&psudo_hdr, 0,
            PSUDO_IPV4_HEADER_LEN, 0);
    uint16_t checksum_final = ipv4_checksum_incremental_final(in, 0,
            in->len, checksum_psudo_hdr);

    dprint("Calculated checksum for UDP is %"PRIu16"\n", checksum_final);
    udp_hdr_checksum_write(in, checksum_final);
//#endif // 0

    // P_true, P_false
    return P_true;
}


