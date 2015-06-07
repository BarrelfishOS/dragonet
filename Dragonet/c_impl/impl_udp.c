/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdlib.h>
#include <string.h>
#include <implementation.h>
#include <inttypes.h>
#include <demuxstate.h>
#include <pthread.h>
#include <assert.h>

#include <udpproto.h>
#include <proto_ipv4.h>

node_out_t do_pg__RxL4UDPValidHeaderLength(
        struct ctx_RxL4UDPValidHeaderLength *context, struct state *state,
        struct input **in)
{
    dprint("### %s:%s:%d: UDP packet %"PRIu16"\n",
            __FILE__, __FUNCTION__, __LINE__, udp_hdr_dport_read(*in));
    (*in)->attr->offset_l5 = udp_payload_offset(*in);
    (*in)->attr->udp_dport = udp_hdr_dport_read(*in);
    (*in)->attr->udp_sport = udp_hdr_sport_read(*in);
    (*in)->attr->ip4_src =  ipv4_srcIP_rd(*in);
    (*in)->attr->ip4_dst =  ipv4_dstIP_rd(*in);
    return PORT_BOOL((((*in)->len) - (udp_header_offset(*in)))
                        >= udp_header_len);
}

node_out_t do_pg__RxL4UDPValidLength(struct ctx_RxL4UDPValidLength *context,
        struct state *state, struct input **in)
{
    dprint("### %s:%s:%d: UDP packet %"PRIu16"\n",
            __FILE__, __FUNCTION__, __LINE__, udp_hdr_dport_read(*in));
    return PORT_BOOL(((*in)->len) >=
            ((udp_header_offset(*in)) + udp_hdr_pkt_length_read(*in)));
}

//#define ENABLE_UDP_CHECKSUM_FAILURE_NOTIFICATION        1

#if     ENABLE_UDP_CHECKSUM_FAILURE_NOTIFICATION
static uint64_t udp_checksum_failed = 0;
#endif //  ENABLE_UDP_CHECKSUM_FAILURE_NOTIFICATION

node_out_t do_pg__RxL4UDPValidChecksum(struct ctx_RxL4UDPValidChecksum *context,
        struct state *state, struct input **in)
{
    dprint("### %s:%s:%d: UDP packet %"PRIu16"\n",
            __FILE__, __FUNCTION__, __LINE__, udp_hdr_dport_read(*in));
    pktoff_t off = udp_header_offset(*in);
    pktoff_t len = (*in)->len;
    uint16_t checksum = udp_hdr_checksum_read(*in);
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

    psudo_header_rx(*in, &psudo_hdr);
    uint32_t checksum_psudo_hdr = ipv4_checksum_incremental(&psudo_hdr, 0,
            PSUDO_IPV4_HEADER_LEN, 0);
    uint16_t checksum_final = ipv4_checksum_incremental_final(*in, off,
            (len - off), checksum_psudo_hdr);

#if     ENABLE_UDP_CHECKSUM_FAILURE_NOTIFICATION
    if (checksum_final != 0) {
        ++udp_checksum_failed;
        // printing every 10th invalid checksum stats to
        //      reduce the fequency of prints
        if (udp_checksum_failed % 1000 == 0) {
            dbg_printf("ERROR: UDP checksum is invalid: %"PRIu16", count: %"PRIu64"\n",
                    checksum_final, udp_checksum_failed);
        }
    }
#endif  //   ENABLE_UDP_CHECKSUM_FAILURE_NOTIFICATION

    //return PORT_BOOL((checksum_final == 0));
    return P_true;
}

#if 0
node_out_t do_pg__RxL4UDPPortClassifyType(struct state *state, struct input *in)
{
    dprint("### %s:%s:%d: UDP packet %"PRIu16"\n",
            __FILE__, __FUNCTION__, __LINE__, udp_hdr_dport_read(in));
    // P_RxL4UDPPortClassifyType_static, P_RxL4UDPPortClassifyType_dynamic
    return P_RxL4UDPPortClassifyType_static;
}

node_out_t do_pg__RxL4UDPPortClassifyStatic(struct state *state, struct input *in)
{
    dprint("### %s:%s:%d: UDP packet %"PRIu16"\n",
            __FILE__, __FUNCTION__, __LINE__, udp_hdr_dport_read(in));
    portno_t dport = udp_hdr_dport_read(in);
    switch(dport) {
        case 51098: return P_RxL4UDPPortClassifyStatic_appDNS;
        case 5556: return P_RxL4UDPPortClassifyStatic_appEcho;
        case 7: return P_RxL4UDPPortClassifyStatic_appEcho;
        default: return P_RxL4UDPPortClassifyStatic_closedPort;
    }
    return P_RxL4UDPPortClassifyStatic_closedPort;
}

static uint64_t app_selector_dummy = 0;
node_out_t do_pg__RxL4UDPPortClassifyDynamic(struct state *state, struct input *in)
{
    dprint("### %s:%s:%d:HWQ:%d, UDP packet %"PRIu16"\n",
            __FILE__, __FUNCTION__, __LINE__, in->qid, udp_hdr_dport_read(in));
    struct udp_flow_entry *f, *f_ht;
    struct udp_flow_entry f_key;
    struct udp_listen_entry *l, *l_ht;
    uint64_t socketid = -1;
    uint64_t appid = 0;
    int selected_appid = 0;
    node_out_t out = P_RxL4UDPPortClassifyDynamic_appEcho;

    if (pthread_rwlock_rdlock(state->udp_lock) != 0) {
        fprintf(stderr, "Could not acquire udp lock");
        return P_RxL4UDPPortClassifyDynamic_closedPort;
    }

    dprint
    //printf
        ("### %s:%s:%d: UDP packet %"PRIu16": global state lock grabbed\n",
            __FILE__, __FUNCTION__, __LINE__, udp_hdr_dport_read(in));
    f_ht = state->udp_flow_ht;
    l_ht = state->udp_listen_ht;

    f_key.s_port = udp_hdr_sport_read(in);
    f_key.d_port = udp_hdr_dport_read(in);
    f_key.s_ip   = ipv4_srcIP_rd(in);
    f_key.d_ip   = ipv4_dstIP_rd(in);

    HASH_FIND(hh, f_ht, &f_key.s_ip, UDP_FLOW_KEYLEN, f);
    if (f != NULL) {
        assert(f->rss_entries == 1);
        selected_appid = app_selector_dummy % f->rss_entries;
        ++app_selector_dummy;
        socketid = f->lsocketid[selected_appid];
        appid = f->lappid[selected_appid];
        dprint
        //printf
            ("HWQ:%d, Full flow match: selected appid index = %d, selected appid = %"PRIu64", socket-ID: %"PRIu64"\n",
                in->qid, selected_appid, appid, socketid);
        goto out_success;
    }

    // Masking the source port and destination ip for scalability filters
    // FIXME: I am not covering all the possibilities of masking
    f_key.s_port = 0;
    f_key.d_ip = 0;
    HASH_FIND(hh, f_ht, &f_key.s_ip, UDP_FLOW_KEYLEN, f);
    if (f != NULL) {
        assert(f->rss_entries == 1);
        selected_appid = app_selector_dummy % f->rss_entries;
        ++app_selector_dummy;
        socketid = f->lsocketid[selected_appid];
        appid = f->lappid[selected_appid];
        dprint
        //printf
            ("HWQ:%d, Partial flow match: selected appid index = %d, selected appid = %"PRIu64", socket-ID: %"PRIu64"\n",
              in->qid,  selected_appid, appid, socketid);
        goto out_success;
    }

    HASH_FIND(hh, l_ht, &f_key.d_port, UDP_LISTEN_KEYLEN, l);
    if (l != NULL) {
        assert(l->rss_entries > 0);
        //assert(l->rss_entries == 1);
        selected_appid = app_selector_dummy % l->rss_entries;
        ++app_selector_dummy;
        socketid = l->lsocketid[selected_appid];
        appid = l->lappid[selected_appid];
        dprint
        //printf
            ("HWQ:%d, Listen match: selected appid index = %d, selected appid = %"PRIu64", socket-ID: %"PRIu64"\n",
               in->qid, selected_appid, appid, socketid);
        goto out_success;
    }

    // FIXME: If the socket is marked to support RSS then do another round
    //      of hash on full Flow.  Use this hash value as follows:
    //        - Round it based on how many RSS queues are there
    //        - Use rounded value to choose application id.
    //      choose one of the queues specified by

    dprint
    //printf
        ("### %s:%s:%d: UDP packet %"PRIu16": closed port, returning zero\n",
            __FILE__, __FUNCTION__, __LINE__, udp_hdr_dport_read(in));
    out = P_RxL4UDPPortClassifyDynamic_closedPort;
    appid = P_RxL4UDPPortClassifyDynamic_closedPort;
out_success:

    //dprint("### %s:%s:%d: UDP packet %"PRIu16": open port, going to socket\n",
    //        __FILE__, __FUNCTION__, __LINE__, udp_hdr_dport_read(in));
    in->attr->socket_id = socketid;
    pthread_rwlock_unlock(state->udp_lock);
    return appid;
}
#endif

node_out_t do_pg__RxL4UDPClosedPortAction(
        struct ctx_RxL4UDPClosedPortAction *context, struct state *state,
        struct input **in)
{
    dprint("### %s:%s:%d: UDP packet %"PRIu16"\n",
            __FILE__, __FUNCTION__, __LINE__, udp_hdr_dport_read(*in));
    dprint("#### UDP packet on closed port %"PRIu16"\n",
            udp_hdr_dport_read(*in));
    return P_RxL4UDPClosedPortAction_out;
}

node_out_t do_pg__TxL4UDPInitiateResponse(
        struct ctx_TxL4UDPInitiateResponse *context, struct state *state,
        struct input **in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);

#if DO_EXPLICIT_COPY
    pktoff_t payload_len = udp_payload_length(*in);
    uint8_t payload[DEFAULT_BUFFER_SIZE];
    int ret = udp_copy_payload(*in, payload, sizeof(payload));
    if (ret < 0) {
        return P_TxL4UDPInitiateResponse_drop;
    }
#endif // DO_EXPLICIT_COPY

    // Throw out all headers up to and including ICMP
    pkt_prepend(*in, -(*in)->attr->offset_l5);

#if DO_EXPLICIT_COPY
    pkt_write(*in, 0, payload_len, payload);
#endif // DO_EXPLICIT_COPY

    // P_TxL4UDPInitiateResponse_drop
    return P_TxL4UDPInitiateResponse_out;
}

node_out_t do_pg__TxL4UDPAllocateHeader(
        struct ctx_TxL4UDPAllocateHeader *context, struct state *state,
        struct input **in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    pktoff_t len = udp_header_len;
    pkt_prepend(*in, len);
    (*in)->attr->offset_l5 = udp_header_len;
    (*in)->attr->offset_l4 = 0;
    (*in)->attr->offset_l3 = 0;
    (*in)->attr->offset_l2 = 0;
    // P_TxL4UDPAllocateHeader_out
    return P_TxL4UDPAllocateHeader_out;
}

node_out_t do_pg__TxL4UDPFillHeader(struct ctx_TxL4UDPFillHeader *context,
        struct state *state, struct input **in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);

    udp_hdr_sport_write(*in, (*in)->attr->udp_sport);
    udp_hdr_dport_write(*in, (*in)->attr->udp_dport);
    udp_hdr_pkt_length_write(*in, (*in)->len);

    (*in)->attr->ip4_proto = IPV4_PROTO_UDP;

    uint16_t checksum = 0;
    udp_hdr_checksum_write(*in, checksum);

//#if 0
    uint8_t buf[PSUDO_IPV4_HEADER_LEN];
    struct input psudo_hdr;
    memset(buf, 0, sizeof(buf));
    memset(&psudo_hdr, 0, sizeof(psudo_hdr));
    psudo_hdr.data = buf;
    psudo_hdr.space_before = 0;
    psudo_hdr.space_after = PSUDO_IPV4_HEADER_LEN;

    psudo_header_tx(*in, &psudo_hdr);
    uint32_t checksum_psudo_hdr = ipv4_checksum_incremental(&psudo_hdr, 0,
            PSUDO_IPV4_HEADER_LEN, 0);
    uint16_t checksum_final = ipv4_checksum_incremental_final(*in, 0,
            (*in)->len, checksum_psudo_hdr);

    dprint("Calculated checksum for UDP is %"PRIu16"\n", checksum_final);
    udp_hdr_checksum_write(*in, checksum_final);
//#endif // 0

    // P_true, P_false
    return P_true;
}


