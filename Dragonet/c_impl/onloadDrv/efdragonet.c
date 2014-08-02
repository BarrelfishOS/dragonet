/*
** Copyright 2005-2014  Solarflare Communications Inc.
**                      7505 Irvine Center Drive, Irvine, CA 92618, USA
** Copyright 2002-2005  Level 5 Networks Inc.
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of version 2 of the GNU General Public License as
** published by the Free Software Foundation.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
*/

/*
** Copyright 2005-2014  Solarflare Communications Inc.
**                      7505 Irvine Center Drive, Irvine, CA 92618, USA
** Copyright 2002-2005  Level 5 Networks Inc.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**
** * Redistributions of source code must retain the above copyright notice,
**   this list of conditions and the following disclaimer.
**
** * Redistributions in binary form must reproduce the above copyright
**   notice, this list of conditions and the following disclaimer in the
**   documentation and/or other materials provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
** IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
** TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
** PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
** HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
** TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
** PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
** LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
** NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/* efforward
 *
 * Forward packets between two interfaces without modification.
 *
 * 2011 Solarflare Communications Inc.
 * Author: David Riddoch
 * Date: 2011/04/13
 */

#include "efvi_sfw.h"

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/time.h>
#include <assert.h>
#include <errno.h>
#include <string.h>

#include <implementation.h>
#include <packet_access.h>


#define LOGE(x)  do{ x; }while(0)
#define LOGW(x)  do{ x; }while(0)
#define LOGI(x)  do{}while(0)

struct vi *alloc_queue(struct net_if *myif);
size_t get_packet(struct dragonet_sf_queue *sfq, char *pkt_out,
    size_t buf_len);
void send_packet(struct dragonet_sf_queue *sfq, char *pkt_tx, size_t len,
        uint8_t qi);
struct vi *alloc_queue(struct net_if *myif)
{
    struct vi* vis;
    vis = vi_alloc(0, myif, EF_VI_FLAGS_DEFAULT);
    return vis;
} // end function: alloc_queue

int alloc_filter_default(struct dragonet_sf_queue *sfq);
int alloc_filter_default(struct dragonet_sf_queue *sfq)
{
    assert(sfq != NULL);
    struct vi *vis = sfq->queue_handle;
    assert(vis != NULL);

    // setting up filter
    ef_filter_spec filter_spec;
    ef_filter_spec_init(&filter_spec, EF_FILTER_FLAG_NONE);
    TRY(ef_filter_spec_set_unicast_all(&filter_spec));
    TRY(ef_vi_filter_add(&vis->vi, vis->dh, &filter_spec, NULL));
    //dprint
    printf
        ("%s:%s:%d: [vq:%p], [qid:%"PRIu8"], done\n",
            __FILE__, __func__, __LINE__, sfq, sfq->qid);
    return 1;
} // end function: alloc_filter_default




size_t get_packet(struct dragonet_sf_queue *sfq, char *pkt_out,
        size_t buf_len)
{

    assert(sfq != NULL);
    struct vi *vif = sfq->queue_handle;
    if (vif == NULL) {
        dprint("%s:%s:%d:ERROR: vif == NULL\n", __FILE__, __func__, __LINE__);
        return 0;
    }

    struct vi* viff;
    int i, j, n, n_ev = 0;
    int idx = 0;
    int pkt_buf_i = 0;
    int len = 0;
    int copylen = 0;
    int pkt_received = 0;
    struct pkt_buf* pkt_buf;
//    char printbuf[PRINTBUFSIZE]; // = {'\0'};
    int pkt_received_count = 0;

    while( 1 ) {
        viff = vif;

        // If there are already received packet,
        // then pop one of them out and return.

        if ( (sfq->evs_bufferd_rx_total - sfq->evs_bufferd_rx_last) > 0) {
//            printf("Warning: returning already buffered packet: "
//                    "%d from total %d packets\n",
//                    sfq->evs_bufferd_rx_last, sfq->evs_bufferd_rx_total);

            idx = sfq->evs_rx_buffered_indexes[sfq->evs_bufferd_rx_last];
            ++sfq->evs_bufferd_rx_last;


            /* This code does not handle jumbos. */
            assert(EF_EVENT_RX_SOP(sfq->evs[idx]) != 0);
            assert(EF_EVENT_RX_CONT(sfq->evs[idx]) == 0);

            pkt_buf_i = EF_EVENT_RX_RQ_ID(sfq->evs[idx]);
            pkt_buf = pkt_buf_from_id(viff, pkt_buf_i);
            // Every incoming packet should have n_refs set to 1.
            assert(pkt_buf->n_refs == 1);
            assert(pkt_buf->is_tx == 0);
            len = EF_EVENT_RX_BYTES(sfq->evs[idx]) - viff->frame_off;

            copylen = len;
            if (len > buf_len) {
                copylen = buf_len;
                dprint("buffer too small to copy full packet."
                        "Ignoring  %d byte data\n", (len - copylen));
            }
            dprint("RX event: trying to copy %d bytes at location %p\n",
                    copylen, pkt_out);
            memcpy(pkt_out, RX_PKT_PTR(pkt_buf), copylen);
            ++pkt_received_count;
            pkt_buf_release(pkt_buf);


            pkt_received = copylen;
            ++sfq->refill_counter_local;
            if (sfq->refill_counter_local >=  REFILL_BATCH_SIZE) {
                vi_refill_rx_ring(viff, REFILL_BATCH_SIZE);
                sfq->refill_counter_local = 0;
            }

            // FIXME: print the size of packet and qid
            dprint
                ("%s:%s:%d: [vq:%p], [qid:%"PRIu8"], packet received %d\n",
                    __FILE__, __func__, __LINE__, sfq, sfq->qid,
                    pkt_received);
            return pkt_received;
        } // end if: buffered packets

        // OK, there are no bufferd packets, lets buffer some packets!
        // starting new buffer cycle
        sfq->evs_bufferd_rx_total = 0;
        sfq->evs_bufferd_rx_last = 0;

        //n_ev = ef_eventq_poll(&viff->vi, sfq->evs, sizeof(sfq->evs) / sizeof(sfq->evs[0]));
        n_ev = ef_eventq_poll(&viff->vi, sfq->evs, EF_VI_RX_BATCH);
        //++sfq->event_count;
        if( n_ev <= 0 ) {
            //++sfq->no_event_count;
            continue;
        }

        //if (n_ev > 1) {
        //    printf("WARNING: no. of events received = %d\n", n_ev);
        //}

        for( i = 0; i < n_ev; ++i ) {
            switch( EF_EVENT_TYPE(sfq->evs[i]) ) {
                case EF_EVENT_TYPE_RX:
                    sfq->evs_rx_buffered_indexes[sfq->evs_bufferd_rx_total] = i;
                    ++sfq->evs_bufferd_rx_total;
                    assert(sfq->evs_bufferd_rx_total <= EF_VI_RX_BATCH);

                    ++sfq->rx_event_count;

                    dprint("status: %s:%s:%d: %d,RX event arrived, "
                            "no events: %d, TX:%d, RX=%d, DROP=%d\n",
                            __FILE__, __FUNCTION__, __LINE__,
                            sfq->event_count, sfq->no_event_count,
                            sfq->tx_event_count, sfq->rx_event_count,
                            sfq->tx_discard_event_count);
                    break;

                case EF_EVENT_TYPE_TX:
                    ++sfq->tx_event_count;
                    dprint("status: %s:%s:%d: %d, no events: %d, TX:%d, "
                            "RX=%d, DROP=%d\n",
                            __FILE__, __FUNCTION__, __LINE__,
                            sfq->event_count, sfq->no_event_count,
                            sfq->tx_event_count, sfq->rx_event_count,
                            sfq->tx_discard_event_count);
                    dprint("TX event arrived\n");
                    n = ef_vi_transmit_unbundle(&viff->vi, &sfq->evs[i], sfq->ids);
                    for( j = 0; j < n; ++j ) {
                        pkt_buf = pkt_buf_from_id(viff, TX_RQ_ID_PB(sfq->ids[j]));
                        assert(pkt_buf->is_tx == 1);
                        if (pkt_buf->n_refs != 1) {
                            printf("tx_packet with ref id %d, instead of 1\n", (int) pkt_buf->n_refs);
                            if (pkt_buf->n_refs > 1) {
                                pkt_buf_release(pkt_buf);
                                printf("WARNING: couldn't this buffer, so letting it leak with ref_count = %d\n",
                                        (int) pkt_buf->n_refs);
                            }
                        } else {
                            pkt_buf_release(pkt_buf);
                        }
                    } // end for:
                    break;

                case EF_EVENT_TYPE_RX_DISCARD:
                    ++sfq->tx_discard_event_count;
                    if (sfq->tx_discard_event_count % 10 == 0) {
                        dprint("status: %s:%s:%d: %d, no events: %d, TX:%d, RX=%d, DROP=%d\n",
                            __FILE__, __FUNCTION__, __LINE__,
                            sfq->event_count, sfq->no_event_count, sfq->tx_event_count, sfq->rx_event_count,
                            sfq->tx_discard_event_count);
                    }
                    pkt_buf = pkt_buf_from_id(viff,
                                    EF_EVENT_RX_DISCARD_RQ_ID(sfq->evs[i]));
                        //buf_details(pkt_buf, printbuf, sizeof(printbuf));
                    printf("%s:%s:%d: RX_DISCARD, before released, ref = %d\n",
                                __FILE__, __func__, __LINE__, pkt_buf->n_refs);

                    assert(pkt_buf->n_refs == 1);
                    pkt_buf_release(pkt_buf);
                    break;

                default:
                    printf("ERROR: unexpected event type=%d\n",
                            (int) EF_EVENT_TYPE(sfq->evs[i]));
                    LOGE(fprintf(stderr, "ERROR: unexpected event type=%d\n",
                                (int) EF_EVENT_TYPE(sfq->evs[i])));
                    break;
            } // end switch
        } // end for : i

    } // end while: infinite
    return 0;
} // end function: get_packet


void send_packet(struct dragonet_sf_queue *sfq, char *pkt_tx, size_t len,
        uint8_t qi)
{
    assert(sfq != NULL);
    struct vi *vif = sfq->queue_handle;
    assert(vif != NULL);
    struct pkt_buf* pkt_buf;
    int rc;
    int offset = 0;

    dprint("%s:%s:%d: ###### packet %p, len %zu, using vif %p \n",
            __FILE__, __func__, __LINE__,
            pkt_tx, len, vif);
    if (vif == NULL) {
        dprint("%s:%s:%d:ERROR: vif == NULL\n", __FILE__, __func__, __LINE__);
        return;
    }

    pkt_buf = vi_get_free_pkt_buf_tx(vif);
    if (pkt_buf == NULL) {
        dprint("%s:%s:%d: No free tx_pkt buffers\n",
                __FILE__, __func__, __LINE__);
        return;
    }

    // print details of buffer which is being sent
//    char printbuf[PRINTBUFSIZE]; // = {'\0'};
    //buf_details(pkt_buf, printbuf, sizeof(printbuf));
    //dprint("%s:%s:%d: ###### 1 %s\n", __FILE__, __func__, __LINE__, printbuf);

    offset = RX_PKT_OFF(vif);
    assert(pkt_buf != NULL);
    assert(pkt_buf->vi_owner != NULL);
    assert(pkt_buf->vi_owner->net_if != NULL);
    assert(pkt_buf->n_refs == 1);
    assert(pkt_buf->is_tx);

    void * buf_addr = RX_PKT_PTR(pkt_buf);
    //buf_details(pkt_buf, printbuf, sizeof(printbuf));
    //dprint("%s:%s:%d: ###### 2 %s\n", __FILE__, __func__, __LINE__, printbuf);

    // FIXME: make sure that len is smaller than buffer length
    memcpy(buf_addr, pkt_tx, len);


    dprint("%s:%s:%d: calling vi_send\n", __FILE__, __func__, __LINE__);
    rc = vi_send(vif, pkt_buf, offset, len);
    if( rc != 0 ) {
        assert(rc == -EAGAIN);
        /* TXQ is full.  A real app might consider implementing an overflow
         * queue in software.  We simply choose not to send.
         */
        dprint("%s:%s:%d: send queue full, so not sending\n", __FILE__, __func__, __LINE__);
        LOGW(fprintf(stderr, "WARNING: [%s] dropped send on queue %"PRIu8"\n",
                    vif->net_if->name, qi));
        assert(pkt_buf->n_refs == 1);
        pkt_buf_release(pkt_buf);
        return;
    }

} // end function: send_packet

void *init_and_alloc_default_queue(char *name)
{

    struct dragonet_sf *sf_nic = NULL;
    uint8_t k = 0;
    // Allocating memory to store the device configuration handle
    sf_nic = (struct dragonet_sf *) malloc (sizeof(struct dragonet_sf));
    if (sf_nic == NULL) {
        printf("ERROR: %s:%s:%d: malloc failed in allocating memory\n",
                __FILE__, __FUNCTION__, __LINE__);
        abort();
        return NULL;
    }
    assert(sf_nic != NULL);
    memset(sf_nic, 0, sizeof(struct dragonet_sf));

    // Allocate memory to hold the actual queue elements
    sf_nic->queues = (struct dragonet_sf_queue *)
                calloc(SF_MAX_QUEUES, sizeof(struct dragonet_sf_queue));
    assert(sf_nic->queues != NULL);

    // connecting to device with given name using openonalod library and storing the handle
    sf_nic->sfif = net_if_alloc(0, name, 0);

    if(sf_nic->sfif == NULL) {
        LOGE(fprintf(stderr, "ERROR: Bad interface '%s' or unable to allocate "
                    "resources\n", name));
        printf("ERROR: Bad interface '%s' or unable to allocate resources\n",
                name);
        exit(1);
    }

    // Initialize the queues
    struct dragonet_sf_queue *iq = sf_nic->queues;
    for (k = 0; k < SF_MAX_QUEUES; k++) {
        iq[k].sf = sf_nic;
        iq[k].populated = false;
        iq[k].queue_handle = alloc_queue(sf_nic->sfif);
        iq[k].qid = k;
        iq[k].evs_bufferd_rx_total = 0;
        iq[k].evs_bufferd_rx_last = 0;
        iq[k].queue =  (void *) iq[k].queue_handle;
    }

    // Create a default filter to make sure that all traffic ends up in queue-0 by default.
    alloc_filter_default(&sf_nic->queues[0]);
//    vis_local = sf_nic->queues[0].queue_handle;

    dprint("%s:%s:%d: dragonet_nic = %p,  sf_if = %p, (q0 [%p], q1 [%p], q2[%p])\n",
            __FILE__, __func__, __LINE__,
            sf_nic, sf_nic->sfif,
            sf_nic->queues[0].queue_handle,
            sf_nic->queues[1].queue_handle,
            sf_nic->queues[2].queue_handle);
    return (void *)sf_nic;
}

// ######################  ###########################


struct vi;

static
uint64_t sf_mac_read(device_t ttd) {
    return (CONFIG_LOCAL_MAC_sf);
}

static
uint32_t sf_ip_read(device_t ttd) {
    return (CONFIG_LOCAL_IP_sf);
}

static
void *
init_onload_wrapper(char *dev_name)
{
    void *ret;
    ret = init_and_alloc_default_queue(dev_name);
    dprint("%s:%s:%d: dev =  %p\n", __FILE__, __func__, __LINE__, ret);
    return (ret);
}

static
pktoff_t onload_rx_wrapper(struct dragonet_sf_queue *selected_vqueue,
        uint8_t *data, pktoff_t len)
{
    return ((pktoff_t)get_packet(selected_vqueue, (char *)data, len));
}

static
int onload_tx_wrapper(struct dragonet_sf_queue *selected_vqueue,
        uint8_t *data, pktoff_t len, uint8_t qi)
{
    dprint("calling onload_tx_wrapper...................\n");
    send_packet(selected_vqueue, (char *)data, len, qi);
    return len;
}

static void tap_init(struct state *state, char *dev_name)
{
    if (state->tap_handler != NULL) {
        printf("SF driver Already intialized\n");
        return;
    }

    void *onload_dev = init_onload_wrapper(dev_name);
    state->tap_handler = onload_dev;
//    dprint("%s:%s:%d: %p == %p ##########\n",
//              __FILE__,  __func__, __LINE__, onload_dev, state->tap_handler);
}
// ############################################################
// ####################### FOR RX and TX  #####################
// ############################################################

#define MAX_QUEUES                     128
static uint64_t qstat[MAX_QUEUES] = {0, 0};  // for per queue packets stats

static node_out_t rx_queue_new_v1(struct ctx_E10kRxQueue0 *context,
    struct state *state, struct input **in, uint8_t qi)
{
    node_out_t out_decision = P_RxQueue_drop;
    assert(qi < MAX_QUEUES);
    struct dragonet_sf *sf_driver = (struct dragonet_sf *) state->tap_handler;
    struct dragonet_sf_queue *q;

    //pktoff_t maxlen;
    if (sf_driver == NULL) {
        if (qi != 0) {

            // We'll do the intialization on queue 0
            dprint("%s:%s:%d: [QID:%"PRIu8"], "
                "initialization will be done on queue-0, returning\n",
              __FILE__,  __func__, __LINE__, qi);

            out_decision =  P_RxQueue_drop;
            goto spawn_and_return;
        }
        tap_init(state, IFNAME);

        // clear up the stats array
        memset(qstat, 0, sizeof(qstat));

        state->local_mac = CONFIG_LOCAL_MAC_sf;
        state->local_ip = CONFIG_LOCAL_IP_sf;
        dprint("%s:%s:%d: ############## Initializing driver %p done\n",
              __FILE__,  __func__, __LINE__, state->tap_handler);

        // FIXME: enable following line.  I don't know why it generates compiliation error
        //declare_dragonet_initialized(DN_READY_FNAME, "SF driver started!\n");
        *in = input_alloc();  // FIXME: uncomment this!!!
        printf("Initialized\n");

        out_decision = P_RxQueue_init;
        goto spawn_and_return;
    }


    // get handle on queue, and make sure that it is correct
    assert(qi < SF_MAX_QUEUES);
    assert(sf_driver != NULL);
    q = sf_driver->queues + qi;
    assert(q->queue_handle != NULL);
    ++q->rx_pkts;
    dprint
    //printf
        ("%s:%s:%d: [QID:%"PRIu8"], [pktid:%d], dragonet_nic = %p, "
            "sf_if = %p, (vq0 [%p, %p], [vq-%"PRIu8": %p, %p], "
            "######## Trying to RX packet\n",
            __FILE__,  __func__, __LINE__, qi, q->rx_pkts,
            state->tap_handler, sf_driver->sfif,
            &sf_driver->queues[0], sf_driver->queues[0].queue_handle,
            qi, q, q->queue_handle);


    *in = input_alloc();  // FIXME: uncomment this!!!

    // start working on RX space
    pkt_prepend(*in, (*in)->space_before);
    ssize_t len = onload_rx_wrapper(q, (uint8_t *)(*in)->data, (*in)->len);
    if (len == 0) {
        dprint("%s:%d: [QID: %"PRIu8"], [pktid:%d], pkt with zero len\n",
            __func__, __LINE__, qi, q->rx_pkts);
        pkt_append(*in, -((*in)->len - len));

        out_decision = P_RxQueue_drop;
        goto spawn_and_return;
    }

#if SHOW_INTERVAL_STATS
    if (qstat[qi] % INTERVAL_STAT_FREQUENCY == 0) {
        //dprint
        printf
            ("QueueID:%"PRIu8":[TID:%d]: has handled %"PRIu64" packets\n",
               qi, (int)pthread_self(), qstat[qi]);
    }
#endif // SHOW_INTERVAL_STATS
    ++qstat[qi];

    (*in)->qid = qi;
    pkt_append(*in, -((*in)->len - len));



    dprint
    //printf
        ("%s:%d: [QID:%"PRIu8"], [pktid:%d]: ############## pkt received, data: %p, len:%zu\n",
            __func__, __LINE__, qi, q->rx_pkts, (*in)->data, len);

    out_decision = P_RxQueue_out;
    goto spawn_and_return;

spawn_and_return:
    // Respawn this node
    // FIXME: shouldn't  value S_E10kRxQueue0_poll should depend which queue-id?
    spawn(context, NULL, S_E10kRxQueue0_poll, SPAWNPRIO_LOW);
    return out_decision;
} // end function: rx_queue_new_v1

static node_out_t tx_queue(struct state *state, struct input **in, uint8_t qi)
{
    struct dragonet_sf *sf_driver = (struct dragonet_sf *) state->tap_handler;
    struct dragonet_sf_queue *q;

    // get handle on queue, and make sure that it is correct
    assert(qi < SF_MAX_QUEUES);
    assert(sf_driver != NULL);
    q = sf_driver->queues + qi;
    assert(q->queue_handle != NULL);

    ++q->tx_pkts;
    dprint("%s:%s:%d: [QID:%"PRIu8"], [pktid:%d], dragonet_nic = %p, "
            "sf_if = %p, (vq0 [%p, %p], [vq-%"PRIu8": %p, %p], "
            "######## Trying to send packet, data: %p, len:%"PRIu32"\n",
            __FILE__,  __func__, __LINE__, qi, q->tx_pkts,
            state->tap_handler, sf_driver->sfif,
            &sf_driver->queues[0], sf_driver->queues[0].queue_handle,
            qi, q, q->queue_handle, (*in)->data, (*in)->len);

    onload_tx_wrapper(q, (*in)->data, (*in)->len, qi);

    dprint("%s:%s:%d: [QID:%"PRIu8"], [pktid:%d]:"
            "##############  packet sent, data: %p, len:%"PRIu32"\n",
            __FILE__, __func__, __LINE__, qi, q->tx_pkts, (*in)->data, (*in)->len);
    //return 0;
    return P_RxQueue_out;
} // end function: tx_queue




// ################# Implementation based on Intel driver ###########


node_out_t do_pg__E10kRxQueue0(struct ctx_E10kRxQueue0 *context,
        struct state *state, struct input **in)
{
    return rx_queue_new_v1(context, state, in, 0);
}

node_out_t do_pg__E10kRxQueue1(struct ctx_E10kRxQueue1 *context,
        struct state *state, struct input **in)
{
    return rx_queue_new_v1((struct ctx_E10kRxQueue0 *) context, state, in, 1);
}

node_out_t do_pg__E10kRxQueue2(struct ctx_E10kRxQueue2 *context,
        struct state *state, struct input **in)
{
    return rx_queue_new_v1((struct ctx_E10kRxQueue0 *) context, state, in, 2);
}

node_out_t do_pg__E10kRxQueue3(struct ctx_E10kRxQueue3 *context,
        struct state *state, struct input **in)
{
    return rx_queue_new_v1((struct ctx_E10kRxQueue0 *) context, state, in, 3);
}


node_out_t do_pg__E10kTxQueue0(struct ctx_E10kTxQueue0 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 0);
}

node_out_t do_pg__E10kTxQueue1(struct ctx_E10kTxQueue1 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 1);
}

node_out_t do_pg__E10kTxQueue2(struct ctx_E10kTxQueue2 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 2);
}

node_out_t do_pg__E10kTxQueue3(struct ctx_E10kTxQueue3 *context,
        struct state *state, struct input **in)
{
    return tx_queue(state, in, 3);
}

