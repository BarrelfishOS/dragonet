#include <stdlib.h>
#include <string.h>
#include <implementation.h>
#include <proto_arp.h>
#include <ethernetproto.h>
#include <inttypes.h>

static struct arp_pending *arp_get_pending(struct state *st, uint32_t ip)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    struct arp_pending *pending = st->arp_pending;
    while (pending != NULL) {
        if (pending->ip == ip) {
            return pending;
        }
        pending = pending->next;
    }
    return NULL;
}

static void arp_remove_pending(struct state *st, struct arp_pending *p)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    struct arp_pending *pending = st->arp_pending;
    struct arp_pending *prev = NULL;
    while (pending != NULL) {
        if (pending == p) {
            if (prev != NULL) {
                prev->next = pending->next;
            } else {
                st->arp_pending = pending->next;
            }
            return;
        }
        prev = pending;
        pending = pending->next;
    }
}

struct arp_cache *arp_cache_lookup(struct state *st, uint32_t ip)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    struct arp_cache *cache = st->arp_cache;
    while (cache != NULL) {
        if (cache->ip == ip) {
            return cache;
        }
        cache = cache->next;
    }
    return NULL;
}

node_out_t do_pg__RxL3ARPValidHeaderLength(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_true, P_false
    pktoff_t arplen = in->len - arp_hdroff(in);
    if (arplen < ARP_HDRLEN_MIN)
        return P_false;

    return PORT_BOOL(arplen >= arp_hdrlen(in));
}

node_out_t do_pg__RxL3ARPClassify(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_RxL3ARPClassify_request, P_RxL3ARPClassify_response, P_RxL3ARPClassify_drop
    switch (arp_oper_rd(in)) {
        case ARP_OPER_REQUEST:  return P_RxL3ARPClassify_request;
        case ARP_OPER_REPLY:    return P_RxL3ARPClassify_response;
        default:                return P_RxL3ARPClassify_drop;
    }
}

node_out_t do_pg__RxL3ARPLocalIPDest(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_true, P_false
    return PORT_BOOL(arp_tpa_ipv4_rd(in) == state->local_ip);
}

node_out_t do_pg__RxL3ARPValidRequest(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_true, P_false
    return PORT_BOOL (arp_htype_rd(in) == ARP_HTYPE_ETHERNET &&
                      arp_ptype_rd(in) == ARP_PTYPE_IPV4);
}

node_out_t do_pg__RxL3ARPValidResponse(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_true, P_false
    return PORT_BOOL (arp_htype_rd(in) == ARP_HTYPE_ETHERNET &&
                      arp_ptype_rd(in) == ARP_PTYPE_IPV4);
}

node_out_t do_pg__RxL3ARPIsPending(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_true, P_false
    struct arp_pending *pending = arp_get_pending(state, arp_spa_ipv4_rd(in));
    return PORT_BOOL(pending != NULL);
}

node_out_t do_pg__TxL3ARPProcessPendingResponse(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_TxL3ARPProcessPendingResponse_true, P_TxL3ARPProcessPendingResponse_false, P_TxL3ARPProcessPendingResponse_drop
    struct arp_pending *pending = arp_get_pending(state, arp_spa_ipv4_rd(in));
    struct arp_cache   *cache;
    arp_remove_pending(state, pending);

    cache = malloc(sizeof(*cache));
    cache->ip = pending->ip;
    cache->mac = arp_sha_eth_rd(in);
    cache->next = state->arp_cache;
    state->arp_cache = cache;

    // TODO: Can we avoid this?
    input_xchg(in, pending->input);
    input_free(pending->input);
    free(pending);

    return P_TxL3ARPProcessPendingResponse_true;
}

node_out_t do_pg__TxL3ARPInitiateResponse(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_TxL3ARPInitiateResponse_true, P_TxL3ARPInitiateResponse_false, P_TxL3ARPInitiateResponse_drop
    in->attr->arp_src_mac = state->local_mac;
    in->attr->arp_src_ip = arp_tpa_ipv4_rd(in);
    in->attr->arp_dst_mac = arp_sha_eth_rd(in);
    in->attr->arp_dst_ip = arp_spa_ipv4_rd(in);
    in->attr->arp_oper = ARP_OPER_REPLY;
    in->attr->eth_dst_mac = arp_sha_eth_rd(in);

    // empty packet
    pkt_prepend(in, - (ssize_t) in->len);
    return P_TxL3ARPInitiateResponse_true;
}

node_out_t do_pg__TxL3ARPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL3ARPAllocateHeader_out
    pkt_prepend(in, arp_alloclen(ARP_HTYPE_ETHERNET, ARP_PTYPE_IPV4));
    in->attr->offset_l3 = 0;
    return P_TxL3ARPAllocateHeader_out;
}

node_out_t do_pg__TxL3ARPFillHeader(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_true, P_false
    arp_htype_wr(in, ARP_HTYPE_ETHERNET);
    arp_ptype_wr(in, ARP_PTYPE_IPV4);
    arp_hlen_wr(in, 6);
    arp_plen_wr(in, 4);
    arp_oper_wr(in, in->attr->arp_oper);
    arp_sha_eth_wr(in, in->attr->arp_src_mac);
    arp_spa_ipv4_wr(in, in->attr->arp_src_ip);
    arp_tha_eth_wr(in, in->attr->arp_dst_mac);
    arp_tpa_ipv4_wr(in, in->attr->arp_dst_ip);

    in->attr->eth_src_mac = in->attr->arp_src_mac;
    in->attr->eth_type = eth_type_ARP;
    return P_true;
}

node_out_t do_pg__TxL3ARPLookupRequestIn(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_true, P_false
    return P_true;
}

node_out_t do_pg__TxL3ARPLookup_(struct state *state, struct input *in)
{
    dprint("%s: arp lookup for : %"PRIx32"\n", __func__, in->attr->ip4_dst);
    // P_TxL3ARPLookup__true, P_TxL3ARPLookup__false, P_TxL3ARPLookup__miss
    struct arp_cache *cache = arp_cache_lookup(state, in->attr->ip4_dst);
    if (cache == NULL) {
        return P_TxL3ARPLookup__miss;
    }

    in->attr->eth_dst_mac = cache->mac;
    return P_TxL3ARPLookup__true;
}

node_out_t do_pg__TxL3ARPSendGratuitous(struct state *state, struct input *in)
{
    dprint("%s: %"PRIx32"\n", __func__, state->local_ip);
    in->attr->arp_src_mac = state->local_mac;
    in->attr->arp_dst_mac = 0;
    in->attr->arp_src_ip = state->local_ip;
    in->attr->arp_dst_ip = state->local_ip;
    in->attr->arp_oper = ARP_OPER_REQUEST;
    in->attr->eth_dst_mac = eth_broadcast_addr;
    in->attr->eth_dst_mac = eth_broadcast_addr;
    return P_TxL3ARPSendGratuitous_true;
}

node_out_t do_pg__TxL3ARPSendRequest(struct state *state, struct input *in)
{
    dprint("%s:%s:%d \n", __FILE__, __func__, __LINE__);
    // P_TxL3ARPSendRequest_true, P_TxL3ARPSendRequest_false, P_TxL3ARPSendRequest_drop
    struct arp_pending *pending;
    struct input i;

    dprint("%s: %"PRIx32"\n", __func__, state->local_ip);
    pending = malloc(sizeof(*pending));
    pending->next = state->arp_pending;
    state->arp_pending = pending;
    pending->ip = in->attr->ip4_dst;

    // Allocate new input for pending, and exchange it with current in
    pending->input = input_alloc();
    memcpy(&i, pending->input, sizeof(i));
    memcpy(pending->input, in, sizeof(*in));
    memcpy(in, &i, sizeof(*in));

    in->attr->arp_src_mac = state->local_mac;
    in->attr->arp_dst_mac = 0;
    in->attr->arp_src_ip = pending->input->attr->ip4_src;
    in->attr->arp_dst_ip = pending->input->attr->ip4_dst;
    in->attr->arp_oper = ARP_OPER_REQUEST;
    in->attr->eth_dst_mac = eth_broadcast_addr;
    return P_TxL3ARPSendRequest_true;
}

