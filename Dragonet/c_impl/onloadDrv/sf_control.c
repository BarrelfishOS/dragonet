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


#if 0
struct vi* vis_local = NULL;


// ############################ from e10kControl #########################

#define QUEUE_INDEX(q) ((q) - (q)->sf->queues)

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
    ef_event evs[EF_VI_RX_BATCH];

    // buffered RX packets (as we process one packet at time)
    int evs_rx_buffered_indexes[EF_VI_RX_BATCH];
    int evs_bufferd_rx_total;       // total buffered RX packet
    int evs_bufferd_rx_last;     // last packet that was reported to userspace
};

struct dragonet_sf {
//    struct usp_pci_desc dev;
//    struct sf_card card;
    struct net_if *sfif;
    struct dragonet_sf_queue *queues;
};


// ############################ end: from e10kControl #####################

#endif // 0



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


int alloc_filter_full_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip, uint16_t localport,
            uint32_t remoteip, uint16_t remoteport);
int alloc_filter_listen_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip, uint16_t localport);



#define IPROTO_IP       0
#define IPROTO_ICMP     1
#define IPROTO_IGMP     2
#define IPROTO_TCP      6
#define IPROTO_UDP      17


static int convert_u32_ip4_to_string(uint32_t ipAddress, char *addr, int len)
{
    uint8_t  octet[4] = {0,0,0,0};
    int i, ret;
    assert(len >= 16);
    for (i = 0; i < 4; i++)
    {
        octet[i] = ( ipAddress >> (i*8) ) & 0xFF;
    }
    ret = snprintf(addr, len, "%d.%d.%d.%d", octet[3], octet[2],
            octet[1], octet[0]);
    printf("IP address %"PRIu32" is converted to %s\n", ipAddress, addr);
    return ret;
}



int alloc_filter_listen_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip1,
            uint16_t localport1)
{
    assert(sfq != NULL);
    struct vi *vis = sfq->queue_handle;
    assert(vis != NULL);

    char localIPStr[16];
    char filterStr[1024];

    if (localip1 == 0) {
        localip1 = CONFIG_LOCAL_IP_sf;
    }

    convert_u32_ip4_to_string(localip1, localIPStr, sizeof(localIPStr));


 //{udp|tcp}:[vid=<vlan>,]<local-host>:<local-port>"
// "[,<remote-host>:<remote-port>]");
    snprintf(filterStr, sizeof(filterStr), "%s:%s:%d",
            "udp", localIPStr, localport1);
    printf("The created Listen filter is [%s]\n", filterStr);

    ef_filter_spec filter_spec;
    if(filter_parse(&filter_spec, filterStr) != 0) {
        printf("Error in filter parsing: %s\n", filterStr);
        abort();
        return 0;
    }

    //dprint
    printf
        ("%s:%s:%d: [vq:%p, vis:%p], [qid:%"PRIu8"], inserting listen filter proto [%d], "
            "localip [%"PRIx32"] localport[%"PRIx16"]\n",
            __FILE__, __func__, __LINE__, sfq, vis, sfq->qid,
            protocol, localip1, localport1);

//    TRY(ef_filter_spec_set_ip4_local(&filter_spec, protocol, localip,
//                localport));
    TRY(ef_vi_filter_add(&vis->vi, vis->dh, &filter_spec, NULL));
    //dprint
    printf
        ("%s:%s:%d: [vq:%p], [qid:%"PRIu8"] done\n",
            __FILE__, __func__, __LINE__, sfq, sfq->qid);
    return 1;
} // end function: alloc_filter_listen_ipv4


int alloc_filter_full_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip1, uint16_t localport1,
            uint32_t remoteip1, uint16_t remoteport1
            )
{
    assert(sfq != NULL);
    struct vi *vis = sfq->queue_handle;
    assert(vis != NULL);

    char localIPStr[16];
    char remoteIPStr[16];
    char filterStr[1024];

    convert_u32_ip4_to_string(localip1, localIPStr, sizeof(localIPStr));
    convert_u32_ip4_to_string(remoteip1, remoteIPStr, sizeof(remoteIPStr));

 //{udp|tcp}:[vid=<vlan>,]<local-host>:<local-port>"
// "[,<remote-host>:<remote-port>]");
    snprintf(filterStr, sizeof(filterStr), "%s:%s:%d,%s:%d",
            "udp", localIPStr, localport1, remoteIPStr, remoteport1);
    printf("The created filter is [%s]\n", filterStr);

    ef_filter_spec filter_spec;
    if(filter_parse(&filter_spec, filterStr) != 0) {
        printf("Error in filter parsing: %s\n", filterStr);
        abort();
        return 0;
    }

    //dprint
    printf
        ("%s:%s:%d: [vq:%p, vis:%p], [qid:%"PRIu8"] inserting full filter proto [%d], "
            "localip [%"PRIx32"] localport[%"PRIx16"], "
            "RemoteIP [%"PRIx32"] RemotePort[%"PRIx16"], error = %d\n",
            __FILE__, __func__, __LINE__, sfq, vis, sfq->qid,
            protocol, localip1, localport1,
            remoteip1, remoteport1, EINVAL);

//    TRY(ef_filter_spec_set_ip4_full(&filter_spec, protocol,
//                localip, localport, remoteip, remoteport));

    TRY(ef_vi_filter_add(&vis->vi, vis->dh, &filter_spec, NULL));
    //dprint
    printf
        ("%s:%s:%d: [vq:%p], [qid:%"PRIu8"] done\n",
            __FILE__, __func__, __LINE__, sfq, sfq->qid);
    return 1;
} // end function: alloc_filter_full_ipv4



// ############################ from e10kControl #########################


void sf_ctrl_waitready(struct state *state);
void sf_ctrl_waitready(struct state *state)
{
    struct dragonet_sf *sf;
    do {
        sf = (struct dragonet_sf *) state->tap_handler;
        sched_yield();
    } while (sf == NULL);
}

// FIXME: moved to impl_sf.c file.  should be deleted from here
bool sf_ctrl_5tuple_unset(struct state *state, uint8_t index)
{
    assert(!"NYI");
    // FIXME: figure out a way to lookup filters so that they can be deleted
    /*
    struct dragonet_sf *sf_driver = (struct dragonet_sf *) state->tap_handler;
    struct dragonet_sf_queue *q;
    assert(qi < SF_MAX_QUEUES);
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
    int ret;
    uint8_t qi = queue;
    assert(qi < SF_MAX_QUEUES);
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

    if (src_ip == 0 && src_port == 0) {
        ret = alloc_filter_listen_ipv4(q, l4_type, dst_ip, dst_port);
    } else {
        ret = alloc_filter_full_ipv4(q, l4_type, dst_ip, dst_port,
            src_ip, src_port);
    }
    assert(ret == 1);
    return true;
}



