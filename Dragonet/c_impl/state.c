/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>
#include <pthread.h>

#include <implementation.h>
#include <demuxstate.h>
#include "config.h"


void pg_state_init(struct state *st)
{
    int r;
    st->local_mac      = 0;
    st->local_ip       = 0;
    st->arp_pending    = NULL;
    st->arp_cache      = NULL;
    st->pkt_counter    = 0;
    st->udp_flow_ht = NULL;
    st->udp_listen_ht = NULL;
    st->udp_lock = malloc(sizeof(pthread_rwlock_t));
    r = pthread_rwlock_init(st->udp_lock, NULL);
    assert(r == 0);
}

/** Allocate/initialize a new input structure including a buffer */
struct input *input_alloc_plh(pipeline_handle_t plh)
{
    return pl_input_alloc(plh);
}

void input_copy_packet(struct input *in, unsigned char *buff, size_t len)
{
    pkt_prepend(in, len);
    memcpy(in->data, buff, len);
}


void input_free_plh(pipeline_handle_t plh, struct input *in)
{
    assert(plh != NULL);
    assert(in != NULL);
    pl_input_free(plh, in);
}

void input_clean_attrs(struct input *in)
{
    memset(in->attr, 0, sizeof(*in->attr));
}

void input_zero(struct input *in)
{
    memset(in, 0, sizeof(*in));
}

void input_clean_packet(struct input *in)
{
    in->data = (void *) ((uintptr_t) in->data + in->space_after + in->len);
    in->phys += in->space_after + in->len;
    in->space_before += in->space_after + in->len;
    in->space_after = 0;
    in->len = 0;
}

void input_dump(struct input *in)
{
    pktoff_t len = in->len;
    printf("input[%"PRIx32"]: ", in->len);
    pktoff_t i = 0;
    while (i < len) {
        printf("%02"PRIx8" ", *((uint8_t *) in->data + i));
        i++;
    }
    printf("\n");

}

int32_t input_muxid(struct input *in)
{
    return in->attr->mux_id;
}

void input_set_muxid(struct input *in, int32_t mux)
{
    in->attr->mux_id = mux;
}

void input_xchg(struct input *a, struct input *b)
{
    struct input tmp;
    memcpy(&tmp, a, sizeof(*a));
    memcpy(a, b, sizeof(*b));
    memcpy(b, &tmp, sizeof(*b));
}

void *udp_state_add_listen(struct state *st, uint64_t appid, uint64_t socketid,
                           uint16_t port)
{

    struct udp_listen_entry *lf,  // listen-found
                            *l_ht;
    struct udp_flow_entry f_key;
    f_key.s_port = 0;
    f_key.d_port = port;
    f_key.s_ip   = 0;
    f_key.d_ip   = 0;

    struct udp_listen_entry *ln,  // listen-new
                            *ht;

    struct udp_listen_entry *lr;  // listen-return

    ln = calloc(1, sizeof(*ln));
    ln->lsocketid[ln->rss_entries] = socketid;
    ln->lappid[ln->rss_entries] = appid;
    ++ln->rss_entries;

    ln->port = port;


    if (pthread_rwlock_wrlock(st->udp_lock) != 0) {
        fprintf(stderr, "udp_state_add_listen: wrlock failed\n");
        abort();
    }

    l_ht = st->udp_listen_ht;

    // Look for existing entry for same hash key
    HASH_FIND(hh, l_ht, &f_key.d_port, UDP_LISTEN_KEYLEN, lf);
    if (lf == NULL) {
        // No existing listen entry, so using newly created listen-entry
        ht = st->udp_listen_ht;
        HASH_ADD(hh, ht, port, UDP_LISTEN_KEYLEN, ln);
        st->udp_listen_ht = ht;
        lr = ln;
    } else {
        // There is existing listen entry.  Either report error
        //      or use this entry as spanned entry.
        assert(lf->rss_entries > 0 && lf->rss_entries < RSS_TABLE_MAX);
        printf("Warning: Listen on already inserted filter by appid %"PRIu64", "
                " treating as span for appid %"PRIu64", %d\n",
               lf->lappid[0], appid, (int)lf->rss_entries);
        lf->lsocketid[lf->rss_entries] = socketid;
        lf->lappid[lf->rss_entries] = appid;
        ++lf->rss_entries;
        lr = lf;
    } // end else:

    pthread_rwlock_unlock(st->udp_lock);
    return lr;
}

void udp_state_remove_listen(struct state *st, void *listen)
{
    struct udp_listen_entry *l, *ht;
    l = listen;

    if (pthread_rwlock_wrlock(st->udp_lock) != 0) {
        fprintf(stderr, "udp_state_add_listen: wrlock failed\n");
        abort();
    }

    ht = st->udp_listen_ht;
    HASH_DEL(ht, l);
    st->udp_listen_ht = ht;

    pthread_rwlock_unlock(st->udp_lock);
}

void *udp_state_add_flow(struct state *st, uint64_t appid, uint64_t socketid,
                         uint32_t s_ip, uint16_t s_port,
                         uint32_t d_ip, uint16_t d_port)
{
    struct udp_flow_entry *f, *ht;

    f = calloc(1, sizeof(*f));
    f->lsocketid[f->rss_entries] = socketid;
    f->lappid[f->rss_entries] = appid;
    ++f->rss_entries;

    f->s_ip = s_ip;
    f->d_ip = d_ip;
    f->s_port = s_port;
    f->d_port = d_port;

    if (pthread_rwlock_wrlock(st->udp_lock) != 0) {
        fprintf(stderr, "udp_state_add_listen: wrlock failed\n");
        abort();
    }

    ht = st->udp_flow_ht;
    HASH_ADD(hh, ht, s_ip, UDP_FLOW_KEYLEN, f);
    st->udp_flow_ht = ht;

    pthread_rwlock_unlock(st->udp_lock);

    return f;
}

void udp_state_remove_flow(struct state *st, void *flow)
{
    struct udp_flow_entry *f, *ht;
    f = flow;

    if (pthread_rwlock_wrlock(st->udp_lock) != 0) {
        fprintf(stderr, "udp_state_add_listen: wrlock failed\n");
        abort();
    }

    ht = st->udp_flow_ht;
    HASH_DEL(ht, f);
    st->udp_flow_ht = ht;

    pthread_rwlock_unlock(st->udp_lock);
}

bool ip_from_string(const char *ip, uint32_t *dst)
{
    if (inet_pton(AF_INET, ip, dst) != 1) {
        return false;
    }
    *dst = __builtin_bswap32(*dst);
    return true;
}

