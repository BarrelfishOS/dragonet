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

struct vi* vis_local = NULL;



// ############################ from e10kControl #########################

#define QUEUES 4
#define QUEUE_INDEX(q) ((q) - (q)->sf->queues)

#define EF_VI_TRANSMIT_BATCH    64

typedef void * sf_queue_t;

struct dragonet_sf;

struct dragonet_sf_queue {
    struct dragonet_sf *sf;
    bool populated;
    bool chained;
    uint8_t qid;
    sf_queue_t queue;
    struct vi *queue_handle;
    int refill_counter_local;
    int tx_event_count;
    int tx_discard_event_count;
    int rx_event_count;
    int no_event_count;
    int event_count;
    int rx_pkts;
    int tx_pkts;
    ef_request_id ids[EF_VI_TRANSMIT_BATCH];
    ef_event evs[16];
};

struct dragonet_sf {
//    struct usp_pci_desc dev;
//    struct sf_card card;
    struct net_if *sfif;
    struct dragonet_sf_queue queues[QUEUES];
};



// ############################ end: from e10kControl #####################

// ###################### MY CODE ###########################

/*
typedef uint8_t  lcoreid_t;
typedef uint8_t  portid_t;
typedef uint16_t queueid_t;

// TODO: Implement these as these are used in DPDK
size_t get_packetV2(int core_id, int port_id, int queue_id,
        char *pkt_out, size_t buf_len);
void send_packetV2(int core_id, int port_id, int queue_id,
        char *pkt_tx, size_t len);
int init_dpdk_setupV2(void);
*/

#include <arpa/inet.h>

struct vi *alloc_queue(struct net_if *myif);

int alloc_filter_default(struct dragonet_sf_queue *sfq);
int alloc_filter_full_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip, uint16_t localport,
            uint32_t remoteip, uint16_t remoteport);
int alloc_filter_listen_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip, uint16_t localport);

size_t get_packet(struct dragonet_sf_queue *sfq, char *pkt_out,
    size_t buf_len);
void send_packet(struct dragonet_sf_queue *sfq, char *pkt_tx, size_t len);

struct vi *alloc_queue(struct net_if *myif)
{
    struct vi* vis;
    vis = vi_alloc(0, myif, EF_VI_FLAGS_DEFAULT);
    return vis;
} // end function: alloc_queue

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
    dprint("%s:%s:%d: [vq:%p], [qid:%"PRIu8"], done\n",
            __FILE__, __func__, __LINE__, sfq, sfq->qid);
    return 1;
} // end function: alloc_filter_default


#define IPROTO_IP       0
#define IPROTO_ICMP     1
#define IPROTO_IGMP     2
#define IPROTO_TCP      6
#define IPROTO_UDP      17

int alloc_filter_listen_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip1,
            uint16_t localport1)
{
    assert(sfq != NULL);
    struct vi *vis = sfq->queue_handle;
    assert(vis != NULL);

    uint16_t localport = htons(localport1);
    uint32_t localip = htonl(localip1);
    ef_filter_spec filter_spec;
    ef_filter_spec_init(&filter_spec, EF_FILTER_FLAG_NONE);
    dprint("%s:%s:%d: [vq:%p], [qid:%"PRIu8"], inserting listen filter proto [%d], "
            "localip [%"PRIx32"] localport[%"PRIx16"]\n",
            __FILE__, __func__, __LINE__, sfq, sfq->qid,
            protocol, localip, localport);

    TRY(ef_filter_spec_set_ip4_local(&filter_spec, protocol, localip,
                localport));
    TRY(ef_vi_filter_add(&vis->vi, vis->dh, &filter_spec, NULL));
    dprint("%s:%s:%d: [vq:%p], [qid:%"PRIu8"] done\n",
            __FILE__, __func__, __LINE__, sfq, sfq->qid);
    return 1;
} // end function: alloc_filter_listen_ipv4


int alloc_filter_full_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip, uint16_t localport,
            uint32_t remoteip, uint16_t remoteport
            )
{
    assert(sfq != NULL);
    struct vi *vis = sfq->queue_handle;
    assert(vis != NULL);

    ef_filter_spec filter_spec;
    ef_filter_spec_init(&filter_spec, EF_FILTER_FLAG_NONE);
    dprint("%s:%s:%d: [vq:%p], [qid:%"PRIu8"] inserting listen filter proto [%d], "
            "localip [%"PRIx32"] localport[%"PRIx16"]\n",
            __FILE__, __func__, __LINE__, sfq, sfq->qid,
            protocol, localip, localport);

    TRY(ef_filter_spec_set_ip4_full(&filter_spec, protocol,
                localip, localport, remoteip, remoteport));
    TRY(ef_vi_filter_add(&vis->vi, vis->dh, &filter_spec, NULL));
    dprint("%s:%s:%d: [vq:%p], [qid:%"PRIu8"] done\n",
            __FILE__, __func__, __LINE__, sfq, sfq->qid);
    return 1;
} // end function: alloc_filter_full_ipv4



#define PRINTBUFSIZE    (1023)
static void buf_details(struct pkt_buf* pkt_buf, char *buff, int l)
{
    assert(pkt_buf != NULL);
    assert(pkt_buf->vi_owner != NULL);
    assert(pkt_buf->vi_owner->net_if != NULL);
    assert(buff != NULL);

    //snprintf(buff, l,
    dprint(
           "BUF[id=%d,owner=%d,ref=%d,if_id=%d,addr=%p]\n",
                    pkt_buf->id,
                    pkt_buf->vi_owner->id,
                    pkt_buf->n_refs,
                    pkt_buf->vi_owner->net_if->id,
                    RX_PKT_PTR(pkt_buf)
                    );

}


// FIXME: remove these state variables as now they are part of the struct
/*
static int refill_counter_local = 0;
static int tx_event_count = 0;
static int tx_discard_event_count = 0;
static int rx_event_count = 0;
static int no_event_count = 0;
static int event_count = 0;
*/

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
    int pkt_buf_i = 0;
    int len = 0;
    int copylen = 0;
    int pkt_received = 0;
    struct pkt_buf* pkt_buf;
    char printbuf[PRINTBUFSIZE] = {'\0'};
    int pkt_received_count = 0;

    while( 1 ) {
        viff = vif;

        n_ev = ef_eventq_poll(&viff->vi, sfq->evs, sizeof(sfq->evs) / sizeof(sfq->evs[0]));
        ++sfq->event_count;
        if( n_ev <= 0 ) {
            ++sfq->no_event_count;
            continue;
        }

        if (n_ev > 1) {
            printf("WARNING: no. of events received = %d\n", n_ev);
        }

        for( i = 0; i < n_ev; ++i ) {
            switch( EF_EVENT_TYPE(sfq->evs[i]) ) {
                case EF_EVENT_TYPE_RX:
                    ++sfq->rx_event_count;
                        dprint("status: %s:%s:%d: %d, no events: %d, TX:%d, RX=%d, DROP=%d\n",
                            __FILE__, __FUNCTION__, __LINE__,
                            sfq->event_count, sfq->no_event_count, sfq->tx_event_count, sfq->rx_event_count,
                            sfq->tx_discard_event_count);
                    dprint("%s:%d: RX event arrived\n", __func__, __LINE__);
                    /* This code does not handle jumbos. */
                    assert(EF_EVENT_RX_SOP(sfq->evs[i]) != 0);
                    assert(EF_EVENT_RX_CONT(sfq->evs[i]) == 0);

                    pkt_buf_i = EF_EVENT_RX_RQ_ID(sfq->evs[i]);
                    pkt_buf = pkt_buf_from_id(viff, pkt_buf_i);
                    len = EF_EVENT_RX_BYTES(sfq->evs[i]) - viff->frame_off;

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
                    if (pkt_buf->n_refs != 0) {
                        dprint("%s:%s:%d: ################### BUF LEAK ####\n", __FILE__, __func__, __LINE__);
                    }
                    //assert(pkt_buf->n_refs == 0);

                    pkt_received = copylen;
                    ++sfq->refill_counter_local;
                    if (sfq->refill_counter_local >= 16 ) {
                        vi_refill_rx_ring(viff);
                        sfq->refill_counter_local = 0;
                    }

                    // NOTE: I am assuming that we are reading only one event
                    //      in each iteration
//                    return pkt_received;
                    break;

                case EF_EVENT_TYPE_TX:
                    ++sfq->tx_event_count;
                    dprint("status: %s:%s:%d: %d, no events: %d, TX:%d, RX=%d, DROP=%d\n",
                            __FILE__, __FUNCTION__, __LINE__,
                            sfq->event_count, sfq->no_event_count, sfq->tx_event_count, sfq->rx_event_count,
                            sfq->tx_discard_event_count);
                    dprint("TX event arrived\n");
                    n = ef_vi_transmit_unbundle(&viff->vi, &sfq->evs[i], sfq->ids);
                    for( j = 0; j < n; ++j ) {
                        pkt_buf = pkt_buf_from_id(viff, TX_RQ_ID_PB(sfq->ids[j]));
                        buf_details(pkt_buf, printbuf, sizeof(printbuf));
                        dprint("%s:%s:%d: TX, before released %s\n", __FILE__, __func__, __LINE__, printbuf);
                        pkt_buf_release(pkt_buf);
                        if (pkt_buf->n_refs != 0) {
                            dprint("%s:%s:%d: ################### BUF LEAK %d ####\n", __FILE__, __func__, __LINE__,j);
                            buf_details(pkt_buf, printbuf, sizeof(printbuf));
                            dprint("%s:%s:%d: TX, after released %s\n", __FILE__, __func__, __LINE__, printbuf);
                        }
                        //assert(pkt_buf->n_refs == 0);
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
                    dprint("RX_discard event arrived\n");
                        pkt_buf = pkt_buf_from_id(viff,
                                    EF_EVENT_RX_DISCARD_RQ_ID(sfq->evs[i]));
                        buf_details(pkt_buf, printbuf, sizeof(printbuf));
                        dprint("%s:%s:%d: RX_DISCARD, before released %s\n", __FILE__, __func__, __LINE__, printbuf);
                        pkt_buf_release(pkt_buf);
                        buf_details(pkt_buf, printbuf, sizeof(printbuf));
                        dprint("%s:%s:%d: RX_DISCARD, after released %s\n", __FILE__, __func__, __LINE__, printbuf);
                    break;

                default:
                    LOGE(fprintf(stderr, "ERROR: unexpected event type=%d\n",
                                (int) EF_EVENT_TYPE(sfq->evs[i])));
                    break;
            } // end switch
        } // end for : i
        //vi_refill_rx_ring(viff);

        if (pkt_received_count == 1) {
            dprint("Exactly one packet received, returning it\n");
            return pkt_received;
        } else if (pkt_received_count > 1) {
            printf("Warning: %d packets received, returning first out of it\n",
                   pkt_received_count);
            return pkt_received;
        }
    }
    return 0;
} // end function: get_packet


void send_packet(struct dragonet_sf_queue *sfq, char *pkt_tx, size_t len)
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

    pkt_buf = vi_get_free_pkt_buf(vif);
    if (pkt_buf == NULL) {
        dprint("%s:%s:%d: No free pkt buffers\n", __FILE__, __func__, __LINE__);
        return;
    }

    // print details of buffer which is being sent
    char printbuf[PRINTBUFSIZE] = {'\0'};
    //buf_details(pkt_buf, printbuf, sizeof(printbuf));
    dprint("%s:%s:%d: ###### 1 %s\n", __FILE__, __func__, __LINE__, printbuf);

    offset = RX_PKT_OFF(vif);
    assert(pkt_buf != NULL);
    assert(pkt_buf->vi_owner != NULL);
    assert(pkt_buf->vi_owner->net_if != NULL);

    void * buf_addr = RX_PKT_PTR(pkt_buf);
    //buf_details(pkt_buf, printbuf, sizeof(printbuf));
    dprint("%s:%s:%d: ###### 2 %s\n", __FILE__, __func__, __LINE__, printbuf);

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
        LOGW(fprintf(stderr, "WARNING: [%s] dropped send\n",
                    vif->net_if->name));
    }

    // marking that sending is partially done.  The ref counter should still be 1
    pkt_buf_release(pkt_buf);
    //assert(pkt_buf->n_refs == 1);

    dprint("%s:%s:%d: vi_send done\n", __FILE__, __func__, __LINE__);
//    buf_details(pkt_buf, printbuf, sizeof(printbuf));
    dprint("%s:%s:%d: ###### 3 %s\n", __FILE__, __func__, __LINE__, printbuf);

    // FIXME: Wait for send ACK
    dprint("%s:%s:%d: buf[] send done\n", __FILE__, __func__, __LINE__);
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

    // connecting to device with given name using openonalod library and storing the handle
    sf_nic->sfif = net_if_alloc(0, name, 0);

    if(sf_nic->sfif == NULL) {
        LOGE(fprintf(stderr, "ERROR: Bad interface '%s' or unable to allocate "
                    "resources\n", name));
        printf("ERROR: Bad interface '%s' or unable to allocate resources\n",
                name);
        exit(1);
    }

    struct dragonet_sf_queue *iq = sf_nic->queues;

    for (k = 0; k < QUEUES; k++) {
        iq[k].sf = sf_nic;
        iq[k].populated = false;
        iq[k].queue_handle = alloc_queue(sf_nic->sfif);
        iq[k].qid = k;
        iq[k].queue =  (void *) iq[k].queue_handle;
    }
    alloc_filter_default(&sf_nic->queues[0]);
    vis_local = sf_nic->queues[0].queue_handle;
    dprint("%s:%s:%d: dragonet_nic = %p,  sf_if = %p, (q0 [%p], q1 [%p], q2[%p])\n",
            __FILE__, __func__, __LINE__,
            sf_nic, sf_nic->sfif,
            sf_nic->queues[0].queue_handle,
            sf_nic->queues[1].queue_handle,
            sf_nic->queues[2].queue_handle);
    return (void *)sf_nic;
}

// ######################  ###########################



// this one is connected via switch on appenzeller
#define IFNAME              "p6p2"
#define CONFIG_LOCAL_MAC_sf  0x495107530f00ULL  // "00:0f:53:07:51:49"
#define CONFIG_LOCAL_IP_sf   0x0a710447         // "10.113.4.71"


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
        uint8_t *data, pktoff_t len)
{
    dprint("calling onload_tx_wrapper...................\n");
    send_packet(selected_vqueue, (char *)data, len);
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
    dprint("%s:%s:%d: %p == %p ##########\n",
              __FILE__,  __func__, __LINE__, onload_dev, state->tap_handler);
}


//node_out_t do_pg__SFRxQueue(struct state *state, struct input *in)
static
node_out_t rx_queue(struct state *state, struct input *in, uint8_t qi)
{

    struct dragonet_sf *sf_driver = (struct dragonet_sf *) state->tap_handler;
    struct dragonet_sf_queue *q;

    //pktoff_t maxlen;
    if (sf_driver == NULL) {
        tap_init(state, IFNAME);
        state->local_mac = CONFIG_LOCAL_MAC_sf;
        state->local_ip = CONFIG_LOCAL_IP_sf;
        dprint("%s:%s:%d: ############## Initializing driver %p done\n",
              __FILE__,  __func__, __LINE__, state->tap_handler);
        printf("Initialized\n");
        return P_Queue_init;
    }


    // get handle on queue, and make sure that it is correct
    assert(qi < QUEUES);
    assert(sf_driver != NULL);
    q = sf_driver->queues + qi;
    assert(q->queue_handle != NULL);
    int p_id = ++q->rx_pkts;
    dprint("%s:%s:%d: [QID:%"PRIu8"], [pktid:%d], dragonet_nic = %p, "
            "sf_if = %p, (vq0 [%p, %p], [vq-%"PRIu8": %p, %p], "
            "######## Trying to RX packet\n",
            __FILE__,  __func__, __LINE__, qi, p_id,
            state->tap_handler, sf_driver->sfif,
            &sf_driver->queues[0], sf_driver->queues[0].queue_handle,
            qi, q, q->queue_handle);


    // start working on RX space
    pkt_prepend(in, in->space_before);
    ssize_t len = onload_rx_wrapper(q, (uint8_t *)in->data, in->len);
    if (len == 0) {
        dprint("%s:%d: [QID: %"PRIu8"], [pktid:%d], pkt with zero len\n",
            __func__, __LINE__, qi, p_id);
        pkt_prepend(in, -in->len);
        return P_Queue_drop;
    }

    pkt_append(in, -(in->len - len));
    dprint("%s:%d: [QID:%"PRIu8"], [pktid:%d]: ############## pkt received, data: %p, len:%zu\n",
            __func__, __LINE__, qi, p_id, in->data, len);
    return P_Queue_out;
}

//node_out_t do_pg__SFTxQueue(struct state *state, struct input *in)
node_out_t tx_queue(struct state *state, struct input *in, uint8_t qi)
{
    struct dragonet_sf *sf_driver = (struct dragonet_sf *) state->tap_handler;
    struct dragonet_sf_queue *q;

    // get handle on queue, and make sure that it is correct
    assert(qi < QUEUES);
    assert(sf_driver != NULL);
    q = sf_driver->queues + qi;
    assert(q->queue_handle != NULL);

    int p_id = ++q->tx_pkts;
    dprint("%s:%s:%d: [QID:%"PRIu8"], [pktid:%d], dragonet_nic = %p, "
            "sf_if = %p, (vq0 [%p, %p], [vq-%"PRIu8": %p, %p], "
            "######## Trying to send packet, data: %p, len:%"PRIu32"\n",
            __FILE__,  __func__, __LINE__, qi, p_id,
            state->tap_handler, sf_driver->sfif,
            &sf_driver->queues[0], sf_driver->queues[0].queue_handle,
            qi, q, q->queue_handle, in->data, in->len);

    onload_tx_wrapper(q, in->data, in->len);

    dprint("%s:%s:%d: [QID:%"PRIu8"], [pktid:%d]:"
            "##############  packet sent, data: %p, len:%"PRIu32"\n",
            __FILE__, __func__, __LINE__, qi, p_id, in->data, in->len);
    return 0;
}


node_out_t do_pg__TapTxQueue(struct state *state, struct input *in) {
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return  tx_queue(state, in, 0);
}


node_out_t do_pg__TapRxQueue(struct state *state, struct input *in) {
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return  rx_queue(state, in, 0);
}



// ############################ from e10kControl #########################


void sf_ctrl_waitready(struct state *state)
{
    struct dragonet_sf *sf;
    do {
        sf = (struct dragonet_sf *) state->tap_handler;
        sched_yield();
    } while (sf == NULL);
}

bool sf_ctrl_5tuple_unset(struct state *state, uint8_t index)
{
    assert(!"NYI");
    // FIXME: figure out a way to lookup filters so that they can be deleted
    /*
    struct dragonet_sf *sf_driver = (struct dragonet_sf *) state->tap_handler;
    struct dragonet_sf_queue *q;
    assert(qi < QUEUES);
    assert(sf_driver != NULL);
    q = sf_driver->queues + qi;
    assert(q->queue_handle != NULL);
    dprint("%s:%s:%d: [QID:%"PRIu8"], [pktid:%d], dragonet_nic = %p, "
            "sf_if = %p, (vq0 [%p, %p], [vq-%"PRIu8": %p, %p], "
            "######## unsetting filter\n",
            __FILE__,  __func__, __LINE__, qi, p_id,
            state->tap_handler, sf_driver->sfif,
            &sf_driver->queues[0], sf_driver->queues[0].queue_handle,
            qi, q, q->queue_handle);
    */

}

bool sf_ctrl_5tuple_set(struct state *state,
        uint8_t index, uint8_t priority, uint8_t queue,
        uint32_t src_ip, uint32_t dst_ip, uint16_t src_port, uint16_t dst_port,
        uint16_t l4_type, uint16_t mask)
{
    struct dragonet_sf *sf_driver = (struct dragonet_sf *) state->tap_handler;
    struct dragonet_sf_queue *q;
    uint8_t qi = queue;
    assert(qi < QUEUES);
    assert(sf_driver != NULL);
    q = sf_driver->queues + qi;
    assert(q->queue_handle != NULL);
    dprint("%s:%s:%d: [QID:%"PRIu8"], dragonet_nic = %p, "
            "sf_if = %p, (vq0 [%p, %p], [vq-%"PRIu8": %p, %p], "
            "######## setting up 5tuple filter\n",
            __FILE__,  __func__, __LINE__, qi, state->tap_handler,
            sf_driver->sfif, &sf_driver->queues[0],
            sf_driver->queues[0].queue_handle,
            qi, q, q->queue_handle);

    //int ret = alloc_filter_listen_ipv4(l4_type, dst_ip, dst_port);
    int ret = alloc_filter_full_ipv4(q, l4_type, dst_ip, dst_port,
            src_ip, src_port);
    assert(ret == 1);
    return true;
}


// ################# from impl_sf.c ########################

//node_out_t do_pg__RxE10kQueue000(struct state *state, struct input *in)
node_out_t do_pg__RxE10kQueue000(struct state *state, struct input *in)
{
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return rx_queue(state, in, 0);
}

node_out_t do_pg__RxE10kQueue001(struct state *state, struct input *in)
{
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return rx_queue(state, in, 1);
}

node_out_t do_pg__RxE10kQueue002(struct state *state, struct input *in)
{
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return rx_queue(state, in, 2);
}

node_out_t do_pg__RxE10kQueue003(struct state *state, struct input *in)
{
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return rx_queue(state, in, 3);
}


node_out_t do_pg__TxE10kQueue000(struct state *state, struct input *in)
{
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return tx_queue(state, in, 0);
}

node_out_t do_pg__TxE10kQueue001(struct state *state, struct input *in)
{
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return tx_queue(state, in, 1);
}

node_out_t do_pg__TxE10kQueue002(struct state *state, struct input *in)
{
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return tx_queue(state, in, 2);
}

node_out_t do_pg__TxE10kQueue003(struct state *state, struct input *in)
{
    dprint("%s:%s:%d: called\n", __FILE__,  __func__, __LINE__);
    return tx_queue(state, in, 3);
}


