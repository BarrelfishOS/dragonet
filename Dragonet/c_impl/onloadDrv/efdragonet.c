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


struct thread {
  struct vi** vis;
  int         vis_n;
  struct vi** fwd_map;
  int         n_rx_pkts;
  uint64_t    n_rx_bytes;
};


struct vi* vis_local = NULL;

static void handle_rx(struct thread* thread, struct vi* vi,
                      int pkt_buf_i, int len)
{
  struct pkt_buf* pkt_buf;
  struct vi* send_vi;
  int rc;

  LOGI(fprintf(stderr, "INFO: [%s] received pkt=%d len=%d\n",
               vi->interface, pkt_buf_i, len));

  pkt_buf = pkt_buf_from_id(vi, pkt_buf_i);

  send_vi = thread->fwd_map[vi->id];
  rc = vi_send(send_vi, pkt_buf, RX_PKT_OFF(vi), len);
  if( rc != 0 ) {
    assert(rc == -EAGAIN);
    /* TXQ is full.  A real app might consider implementing an overflow
     * queue in software.  We simply choose not to send.
     */
    LOGW(fprintf(stderr, "WARNING: [%s] dropped send\n",
                 send_vi->net_if->name));
  }
  pkt_buf_release(pkt_buf);
  ++thread->n_rx_pkts;
  thread->n_rx_bytes += len;
}


static void handle_rx_discard(struct thread* thread, struct vi* vi,
                              int pkt_buf_i, int discard_type)
{
  struct pkt_buf* pkt_buf;

  LOGE(fprintf(stderr, "ERROR: [%s] discard type=%d\n",
               vi->net_if->name, discard_type));

  pkt_buf = pkt_buf_from_id(vi, pkt_buf_i);
  pkt_buf_release(pkt_buf);
}


static void complete_tx(struct thread* thread, int vi_i, int pkt_buf_i)
{
  struct pkt_buf* pkt_buf;
  assert(vi_i < thread->vis_n);
  pkt_buf = pkt_buf_from_id(thread->vis[vi_i], pkt_buf_i);
  pkt_buf_release(pkt_buf);
}

static void thread_main_loop(struct thread* thread)
{
  ef_request_id ids[EF_VI_TRANSMIT_BATCH];
  ef_event evs[16];
  struct vi* vi;
  int i, j, n, n_ev, vi_i = 0;

  while( 1 ) {
    vi = thread->vis[vi_i];
    vi_i = (vi_i + 1) % thread->vis_n;

    n_ev = ef_eventq_poll(&vi->vi, evs, sizeof(evs) / sizeof(evs[0]));
    //n_ev = ef_eventq_poll(&vi->vi, evs, 1);
    if( n_ev <= 0 )
      continue;

    for( i = 0; i < n_ev; ++i ) {
      switch( EF_EVENT_TYPE(evs[i]) ) {
      case EF_EVENT_TYPE_RX:
        /* This code does not handle jumbos. */
        assert(EF_EVENT_RX_SOP(evs[i]) != 0);
        assert(EF_EVENT_RX_CONT(evs[i]) == 0);
        handle_rx(thread, vi, EF_EVENT_RX_RQ_ID(evs[i]),
                  EF_EVENT_RX_BYTES(evs[i]) - vi->frame_off);
        break;
      case EF_EVENT_TYPE_TX:
        n = ef_vi_transmit_unbundle(&vi->vi, &evs[i], ids);
        for( j = 0; j < n; ++j )
          complete_tx(thread, TX_RQ_ID_VI(ids[j]), TX_RQ_ID_PB(ids[j]));
        break;
      case EF_EVENT_TYPE_RX_DISCARD:
        handle_rx_discard(thread, vi, EF_EVENT_RX_DISCARD_RQ_ID(evs[i]),
                          EF_EVENT_RX_DISCARD_TYPE(evs[i]));
        break;
      default:
        LOGE(fprintf(stderr, "ERROR: unexpected event type=%d\n",
                     (int) EF_EVENT_TYPE(evs[i])));
        break;
      }
    }
    vi_refill_rx_ring(vi);
  }
}

/**********************************************************************/

static void monitor(struct thread* thread)
{
  /* Print approx packet rate and bandwidth every second. */

  uint64_t now_bytes, prev_bytes;
  struct timeval start, end;
  int prev_pkts, now_pkts;
  int ms, pkt_rate, mbps;

  prev_pkts = thread->n_rx_pkts;
  prev_bytes = thread->n_rx_bytes;
  gettimeofday(&start, NULL);

  while( 1 ) {
    sleep(1);
    now_pkts = thread->n_rx_pkts;
    now_bytes = thread->n_rx_bytes;
    gettimeofday(&end, NULL);
    ms = (end.tv_sec - start.tv_sec) * 1000;
    ms += (end.tv_usec - start.tv_usec) / 1000;
    pkt_rate = (int) ((int64_t) (now_pkts - prev_pkts) * 1000 / ms);
    mbps = (int) ((now_bytes - prev_bytes) * 8 / 1000 / ms);
    printf("%8d %10d\n", pkt_rate, mbps);
    fflush(stdout);
    prev_pkts = now_pkts;
    prev_bytes = now_bytes;
    start = end;
  }
}


static void* monitor_fn(void* arg)
{
  struct thread* thread = arg;
  monitor(thread);
  return NULL;
}


static void usage(void)
{
  fprintf(stderr, "usage:\n");
  fprintf(stderr, "  efforward <intf1> <intf2>\n");
  exit(1);
}


int dummy_main(int argc, char* argv[]);
int dummy_main(int argc, char* argv[])
{
  pthread_t thread_id;
  struct vi* vis[2];
  struct vi* fwd_map[2];
  struct thread* thread;
  struct net_if* net_if;
  int i, j;

  if( argc != 3 )
    usage();
  ++argv;
  --argc;

  thread = calloc(1, sizeof(*thread));
  thread->vis = vis;
  thread->vis_n = 2;
  thread->fwd_map = fwd_map;

  for( i = 0; i < thread->vis_n; ++i ) {
    if( (net_if = net_if_alloc(i, argv[i], 0)) == NULL ) {
      LOGE(fprintf(stderr, "ERROR: Bad interface '%s' or unable to allocate "
                   "resources\n", argv[i]));
      exit(1);
    }
    vis[i] = vi_alloc(i, net_if, EF_VI_FLAGS_DEFAULT);
  }
  for( i = 0; i < thread->vis_n; ++i )
    for( j = 0; j < thread->vis_n; ++j )
      if( i != j )
        net_if_map_vi_pool(vis[i]->net_if, vis[j]);
  fwd_map[0] = vis[1];
  fwd_map[1] = vis[0];

  for( i = 0; i < thread->vis_n; ++i ) {
    ef_filter_spec filter_spec;
    ef_filter_spec_init(&filter_spec, EF_FILTER_FLAG_NONE);
    TRY(ef_filter_spec_set_unicast_all(&filter_spec));
    TRY(ef_vi_filter_add(&vis[i]->vi, vis[i]->dh, &filter_spec, NULL));
    ef_filter_spec_init(&filter_spec, EF_FILTER_FLAG_NONE);
    TRY(ef_filter_spec_set_multicast_all(&filter_spec));
    TRY(ef_vi_filter_add(&vis[i]->vi, vis[i]->dh, &filter_spec, NULL));
  }

  TEST(pthread_create(&thread_id, NULL, monitor_fn, thread) == 0);
  thread_main_loop(thread);

  return 0;
}


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

struct net_if *init_openonload_setup(char *name);
struct vi *alloc_queue(struct net_if *myif);

int alloc_filter_default(struct vi *vis);
int alloc_filter_full_ipv4(struct vi *vis, int protocol,
            uint32_t localip, uint16_t localport,
            uint32_t remoteip, uint16_t remoteport);
int alloc_filter_listen_ipv4(struct vi *vis, int protocol,
            uint32_t localip, uint16_t localport);

size_t get_packet(struct vi *vif, char *pkt_out, size_t buf_len);
void send_packet(struct vi *vif, char *pkt_tx, size_t len);

static struct net_if* net_if = NULL;

//struct vi *init_openonload_setup(char *name)
struct net_if *init_openonload_setup(char *name)
{

    dprint("%s:%s:%d: called\n", __FILE__, __func__, __LINE__);
    if( (net_if = net_if_alloc(0, name, 0)) == NULL ) {
        LOGE(fprintf(stderr, "ERROR: Bad interface '%s' or unable to allocate "
                    "resources\n", name));
        exit(1);
    }
    return net_if;
} // end function : init_openonload_setup




struct vi *alloc_queue(struct net_if *myif)
{
    struct vi* vis;
    vis = vi_alloc(0, myif, EF_VI_FLAGS_DEFAULT);
    return vis;
} // end function: alloc_queue

int alloc_filter_default(struct vi *vis)
{
    // setting up filter
    ef_filter_spec filter_spec;
    ef_filter_spec_init(&filter_spec, EF_FILTER_FLAG_NONE);
    TRY(ef_filter_spec_set_unicast_all(&filter_spec));
    TRY(ef_vi_filter_add(&vis->vi, vis->dh, &filter_spec, NULL));

    dprint("%s:%s:%d: %p: done\n", __FILE__, __func__, __LINE__, vis);
    return 1;
} // end function: alloc_filter_default

int alloc_filter_listen_ipv4(struct vi *vis, int protocol,
            uint32_t localip1,
            uint16_t localport1)
{
    uint16_t localport = htons(localport1);
    uint32_t localip = htonl(localip1);
    ef_filter_spec filter_spec;
    ef_filter_spec_init(&filter_spec, EF_FILTER_FLAG_NONE);
    dprint("%s:%s:%d: inserting listen filter proto [%d], "
            "localip [%"PRIx32"] localport[%"PRIx16"]\n",
            __FILE__, __func__, __LINE__,
            protocol, localip, localport);

    TRY(ef_filter_spec_set_ip4_local(&filter_spec, protocol, localip,
                localport));
    TRY(ef_vi_filter_add(&vis->vi, vis->dh, &filter_spec, NULL));
    dprint("%s:%s:%d: done\n", __FILE__, __func__, __LINE__);
    return 1;
} // end function: alloc_filter_listen_ipv4

int alloc_filter_full_ipv4(struct vi *vis, int protocol,
            uint32_t localip, uint16_t localport,
            uint32_t remoteip, uint16_t remoteport
            )
{
    ef_filter_spec filter_spec;
    ef_filter_spec_init(&filter_spec, EF_FILTER_FLAG_NONE);
    dprint("%s:%s:%d: inserting listen filter proto [%d], "
            "localip [%"PRIx32"] localport[%"PRIx16"]\n",
            __FILE__, __func__, __LINE__,
            protocol, localip, localport);

    TRY(ef_filter_spec_set_ip4_full(&filter_spec, protocol,
                localip, localport, remoteip, remoteport));
    TRY(ef_vi_filter_add(&vis->vi, vis->dh, &filter_spec, NULL));
    dprint("%s:%s:%d: done\n", __FILE__, __func__, __LINE__);
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


#define EF_VI_TRANSMIT_BATCH    64
static int refill_counter_local = 0;
static int tx_event_count = 0;
static int tx_discard_event_count = 0;
static int rx_event_count = 0;
static int no_event_count = 0;
static int event_count = 0;
size_t get_packet(struct vi *vif, char *pkt_out, size_t buf_len)
{
    if (vif == NULL) {
        dprint("%s:%s:%d:ERROR: vif == NULL\n", __FILE__, __func__, __LINE__);
        return 0;
    }

    assert(vis_local != NULL);
     if (vif != vis_local) {
        dprint("%s:%s:%d:WARNING: vif (%p) != vis_local(%p)\n",
                __FILE__, __func__, __LINE__, vif, vis_local);
        // FIXME : This should work!!!!
//        assert(vif == vis_local);
    }
    // FIXME : This should not be needed !!!!!
//    vif = vis_local;

    ef_request_id ids[EF_VI_TRANSMIT_BATCH];
    //ef_request_id ids[1];
    ef_event evs[16];
    //ef_event evs[1];

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

//        dprint("%s:%d: %p: polling event queue\n", __func__, __LINE__, viff);
        n_ev = ef_eventq_poll(&viff->vi, evs, sizeof(evs) / sizeof(evs[0]));
        //n_ev = ef_eventq_poll(&viff->vi, evs, 1);
        ++event_count;
//        if (event_count % 10 == 0) {
//            printf("status: %s:%s:%d: %d, no events: %d, TX:%d, RX=%d, DROP=%d\n",
//                    __FILE__, __FUNCTION__, __LINE__,
//                    event_count, no_event_count, tx_event_count, rx_event_count,
//                    tx_discard_event_count);
//        }

//        dprint("%s:%d: polling event queue returned\n", __func__, __LINE__);

        if( n_ev <= 0 ) {
            ++no_event_count;
//            if (no_event_count % 10 == 0) {
//                printf("status: %s:%s:%d: %d, no events: %d, TX:%d, RX=%d, DROP=%d\n",
//                        __FILE__, __FUNCTION__, __LINE__,
//                        event_count, no_event_count, tx_event_count, rx_event_count,
//                        tx_discard_event_count);
//            }
            continue;
        }

        //assert(n_ev == 1);
        if (n_ev > 1) {
            printf("WARNING: no. of events received = %d\n", n_ev);
        }

        for( i = 0; i < n_ev; ++i ) {
            switch( EF_EVENT_TYPE(evs[i]) ) {
                case EF_EVENT_TYPE_RX:
                    ++rx_event_count;
                    //if (rx_event_count % 10 == 0) {
                        dprint("status: %s:%s:%d: %d, no events: %d, TX:%d, RX=%d, DROP=%d\n",
                            __FILE__, __FUNCTION__, __LINE__,
                            event_count, no_event_count, tx_event_count, rx_event_count,
                            tx_discard_event_count);
                    //}
                    dprint("%s:%d: RX event arrived\n", __func__, __LINE__);
                    /* This code does not handle jumbos. */
                    assert(EF_EVENT_RX_SOP(evs[i]) != 0);
                    assert(EF_EVENT_RX_CONT(evs[i]) == 0);

                    pkt_buf_i = EF_EVENT_RX_RQ_ID(evs[i]);
                    pkt_buf = pkt_buf_from_id(viff, pkt_buf_i);
                    len = EF_EVENT_RX_BYTES(evs[i]) - viff->frame_off;

                    copylen = len;
                    if (len > buf_len) {
                        copylen = buf_len;
                        dprint("buffer too small to copy full packet."
                                "Ignoring  %d byte data\n", (len - copylen));
                    }
                    dprint("RX event: trying to copy %d bytes at location %p\n",
                            copylen, pkt_out);

                    //buf_details(pkt_buf, printbuf, sizeof(printbuf));
                    //dprint("%s:%s:%d: reveived %s\n", __FILE__, __func__, __LINE__, printbuf);

                    //int myoffset = RX_PKT_OFF(viff);
                    //memcpy(pkt_out, pkt_buf->addr[viff->net_if->id] + myoffset, copylen);
                    //hexdump(RX_PKT_PTR(pkt_buf), len);
                    memcpy(pkt_out, RX_PKT_PTR(pkt_buf), copylen);
                    ++pkt_received_count;
                    pkt_buf_release(pkt_buf);
                    if (pkt_buf->n_refs != 0) {
                        dprint("%s:%s:%d: ################### BUF LEAK ####\n", __FILE__, __func__, __LINE__);
                        //buf_details(pkt_buf, printbuf, sizeof(printbuf));
                        //dprint("%s:%s:%d: RX, after released %s\n", __FILE__, __func__, __LINE__, printbuf);
                    }
                    //assert(pkt_buf->n_refs == 0);

                    pkt_received = copylen;
                    ++refill_counter_local;
                    if (refill_counter_local >= 16 ) {
                        vi_refill_rx_ring(viff);
                        refill_counter_local = 0;
                    }

                    // NOTE: I am assuming that we are reading only one event
                    //      in each iteration
//                    return pkt_received;
                    break;

                case EF_EVENT_TYPE_TX:
                    ++tx_event_count;
                    //if (tx_event_count % 10 == 0) {
                        dprint("status: %s:%s:%d: %d, no events: %d, TX:%d, RX=%d, DROP=%d\n",
                            __FILE__, __FUNCTION__, __LINE__,
                            event_count, no_event_count, tx_event_count, rx_event_count,
                            tx_discard_event_count);
                    //}
                    dprint("TX event arrived\n");
                    //break;
                    n = ef_vi_transmit_unbundle(&viff->vi, &evs[i], ids);
                    for( j = 0; j < n; ++j ) {
                        //complete_tx(thread, TX_RQ_ID_VI(ids[j]), TX_RQ_ID_PB(ids[j]));
                        pkt_buf = pkt_buf_from_id(viff, TX_RQ_ID_PB(ids[j]));
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
                    ++tx_discard_event_count;
                    if (tx_discard_event_count % 10 == 0) {
                        dprint("status: %s:%s:%d: %d, no events: %d, TX:%d, RX=%d, DROP=%d\n",
                            __FILE__, __FUNCTION__, __LINE__,
                            event_count, no_event_count, tx_event_count, rx_event_count,
                            tx_discard_event_count);
                    }
                    dprint("RX_discard event arrived\n");
                    //handle_rx_discard(thread, viff, EF_EVENT_RX_DISCARD_RQ_ID(evs[i]),
                    //        EF_EVENT_RX_DISCARD_TYPE(evs[i]));
                        pkt_buf = pkt_buf_from_id(viff,
                                    EF_EVENT_RX_DISCARD_RQ_ID(evs[i]));
                        buf_details(pkt_buf, printbuf, sizeof(printbuf));
                        dprint("%s:%s:%d: RX_DISCARD, before released %s\n", __FILE__, __func__, __LINE__, printbuf);
                        pkt_buf_release(pkt_buf);
                        buf_details(pkt_buf, printbuf, sizeof(printbuf));
                        dprint("%s:%s:%d: RX_DISCARD, after released %s\n", __FILE__, __func__, __LINE__, printbuf);
                    break;

                default:
                    LOGE(fprintf(stderr, "ERROR: unexpected event type=%d\n",
                                (int) EF_EVENT_TYPE(evs[i])));
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


void send_packet(struct vi *vif, char *pkt_tx, size_t len)
{

    struct pkt_buf* pkt_buf;
    int rc;
    int offset = 0;
    int ii;

    dprint("%s:%s:%d: ###### packet %p, len %zu, using vif %p \n",
            __FILE__, __func__, __LINE__,
            pkt_tx, len, vif);
    if (vif == NULL) {
        dprint("%s:%s:%d:ERROR: vif == NULL\n", __FILE__, __func__, __LINE__);
        return;
    }

    //assert(vis_local != NULL);
     if (vif != vis_local) {
        dprint("%s:%s:%d:WARNING: vif (%p) != vis_local(%p)\n",
                __FILE__, __func__, __LINE__, vif, vis_local);
//        assert(vif == vis_local);
    }
//    vif = vis_local;

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

//#define RX_PKT_PTR(pb)   ((char*) (pb) + RX_PKT_OFF((pb)->vi_owner))
//                    memcpy(pkt_out, RX_PKT_PTR(pkt_buf), copylen);
    void * buf_addr = RX_PKT_PTR(pkt_buf);
    //buf_details(pkt_buf, printbuf, sizeof(printbuf));
    dprint("%s:%s:%d: ###### 2 %s\n", __FILE__, __func__, __LINE__, printbuf);

//    void * buf_addr = (void *)(pkt_buf->addr[pkt_buf->vi_owner->net_if->id] + offset);
//    dprint("%s:%s:%d: calling memcpy\n", __FILE__, __func__, __LINE__);
//    volatile char aa = 0;
//    for (ii = 0; ii < len; ++ii) {
//       aa = aa + ((char *) pkt_tx)[ii];
//    }
//    dprint("%s:%s:%d: verified pkt_tx %c\n", __FILE__, __func__, __LINE__, aa);
//
//    for (ii = 0; ii < len; ++ii) {
//       aa = aa + ((char *) buf_addr)[ii];
//    }
//    dprint("%s:%s:%d: verified pkt_tx %c\n", __FILE__, __func__, __LINE__, aa);
//

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
    //assert(!"send_packet: NYI");
} // end function: send_packet

void *init_and_alloc_default_queue(char *name)
{
    struct net_if *myif = init_openonload_setup(name);
    struct vi* vis =  NULL;
    vis = alloc_queue(myif);
    alloc_filter_default(vis);
    vis_local = vis;
    dprint("%s:%s:%d: void * == %d\n", __FILE__, __func__, __LINE__, sizeof(void *));
    dprint("%s:%s:%d: struct vi* == %d\n", __FILE__, __func__, __LINE__, sizeof(struct vi *));
    dprint("%s:%s:%d: if = %p, (vis [%p] == [%p] vis_local)\n",
            __FILE__, __func__, __LINE__, myif, vis, vis_local);
    return (void *)vis;
}

void *
init_onload_wrapper2(char *arg)
{
    //struct vi *ret;
    void *ret;
    ret = init_and_alloc_default_queue(arg);
    dprint("%s:%s:%d: void * == %d\n", __FILE__, __func__, __LINE__, sizeof(void *));
    dprint("%s:%s:%d: dev =  %p\n",
            __FILE__, __func__, __LINE__, ret);
    return (ret);
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
//struct vi *
void *
init_onload_wrapper(char *arg)
{
    //struct vi *ret;
    void *ret;
    //ret = init_and_alloc_default_queue(IFNAME);
    ret = init_onload_wrapper2(IFNAME);
    dprint("%s:%s:%d: void * == %d\n", __FILE__, __func__, __LINE__, sizeof(void *));
    dprint("%s:%s:%d: dev =  %p\n",
            __FILE__, __func__, __LINE__, ret);
    return (ret);
}

static
pktoff_t onload_rx_wrapper(device_t dev, uint8_t *data, pktoff_t len)
{
    struct vi *onload = (struct vi *)dev;
    return ((pktoff_t)get_packet(onload, (char *)data, len));
}

static
int onload_tx_wrapper(device_t dev, uint8_t *data, pktoff_t len)
{
    struct vi *onload = (struct vi *)dev;
    dprint("calling onload_tx_wrapper...................\n");
    send_packet(onload, (char *)data, len);
    return len;
}

static struct driver onload_driver = {
    .drv_handle = NULL,
//    .drv_init = init_onload_wrapper,
    .drv_rx = onload_rx_wrapper,
    .drv_tx = onload_tx_wrapper,
    .drv_mac_read = sf_mac_read,
    .drv_ip_read = sf_ip_read,
};

static
struct driver *get_sf_driver(void)
{
    return &onload_driver;
}

#if 0
int main(int argc, char *argv[])
{
    struct driver *drv = NULL;
    drv = get_sf_driver();
    return main_loop(drv);
}
#endif // 0

static void tap_init(struct state *state)
{
    if (state->tap_handler != NULL) {
        printf("SF driver Already intialized\n");
        return;
    }

    //struct vi *onload_dev = init_onload_wrapper(NULL);
    void *onload_dev = init_onload_wrapper(NULL);
    state->tap_handler = onload_dev;
    onload_driver.drv_handle = onload_dev;
    dprint("%s:%s:%d: %p == %p == %p ##########\n",
              __FILE__,  __func__, __LINE__,
              onload_dev, state->tap_handler, onload_driver.drv_handle);
}


static int rx_pkts = 0;
static int tx_pkts = 0;
node_out_t do_pg__SFRxQueue(struct state *state, struct input *in)
{
    int p_id = ++rx_pkts;
    pktoff_t maxlen;
    if (state->tap_handler == NULL) {
        tap_init(state);

        state->local_mac = CONFIG_LOCAL_MAC_sf;
        state->local_ip = CONFIG_LOCAL_IP_sf;
        dprint("%s:%s:%d: [pktid:%d]: ############## initialized queue\n",
              __FILE__,  __func__, __LINE__, p_id);
        printf("Initialized\n");
        return P_Queue_init;
    }

    pkt_prepend(in, in->space_before);

//    struct driver *onload_ptr = (struct driver *)state->tap_handler;
    struct vi *onload_dev = (struct vi *) state->tap_handler;
    dprint("%s:%s:%d: [pktid:%d]: [drv: %p, %p], ############## Trying to receive packet\n",
           __FILE__,  __func__, __LINE__, p_id, onload_dev, state->tap_handler);
    ssize_t len = onload_rx_wrapper(
            //onload_driver.drv_handle,
            //onload_ptr->drv_handle,
            onload_dev,
            (char *)in->data, in->len);
    if (len == 0) {
        dprint("%s:%d: [pktid:%d]: pkt with zero len\n", __func__, __LINE__, p_id);
        pkt_prepend(in, -in->len);
        return P_Queue_drop;
    }

    pkt_append(in, -(in->len - len));
    dprint("%s:%d: [pktid:%d]: ############## pkt received, data: %p, len:%"PRIu32"\n",
            __func__, __LINE__, p_id, in->data, len);
    return P_Queue_out;
}

node_out_t do_pg__SFTxQueue(struct state *state, struct input *in)
{
    int p_id = ++tx_pkts;
    dprint("%s:%s:%d: [pktid:%d]: ############## Trying to send packet, data: %p, len:%"PRIu32"\n",
            __FILE__, __func__, __LINE__, p_id, in->data, in->len);
    struct vi *onload_dev = (struct vi *) state->tap_handler;
    //struct driver *onload_ptr = (struct driver *)state->tap_handler;
    onload_tx_wrapper(
            //onload_driver.drv_handle,
            //onload_ptr->drv_handle,
            onload_dev,
            in->data, in->len);
    dprint("%s:%s:%d: [pktid:%d]: ##############  packet sent\n",
            __FILE__, __func__, __LINE__, p_id);
    return 0;
}


node_out_t do_pg__TapTxQueue(struct state *state, struct input *in) {
    return  do_pg__SFTxQueue(state, in);
}


node_out_t do_pg__TapRxQueue(struct state *state, struct input *in) {
    return  do_pg__SFRxQueue(state, in);
}















