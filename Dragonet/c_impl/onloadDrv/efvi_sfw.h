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


#ifndef __EFVI_SFW_H__
#define __EFVI_SFW_H__

#include <etherfabric/vi.h>
#include <etherfabric/pd.h>
#include <etherfabric/memreg.h>
#include <pthread.h>
#include <stdbool.h>

#define ROUND_UP(p, align)   (((p)+(align)-1u) & ~((align)-1u))


/* Maximum number of network interfaces. */
#define MAX_NET_IFS          8


/* Hardware delivers at most ef_vi_receive_buffer_len() bytes to each
 * buffer (default 1792), and for best performance buffers should be
 * aligned on a 64-byte boundary.  Also, RX DMA will not cross a 4K
 * boundary.  The I/O address space may be discontiguous at 4K boundaries.
 * So easiest thing to do is to make buffers always be 2K in size.
 */
#define PKT_BUF_SIZE         2048

//#define REFILL_BATCH_SIZE       1
//#define EF_VI_RX_BATCH          1

#define REFILL_BATCH_SIZE         16
#define EF_VI_RX_BATCH            16

#define EF_VI_TRANSMIT_BATCH      64

// No. of buffers reserved for TX operation
//   This is an upper limit on how many outstanding TX operations can there be.
//#define MAX_TX_BUFFERS            128
#define MAX_TX_BUFFERS              256

struct pkt_buf;


struct net_if {
  const char*      name;
  int              id;
  int              ifindex;
  /* Handle for accessing the driver. */
  ef_driver_handle dh;
  /* Protection domain. */
  ef_pd            pd;
  /* vi_set is only used for receive-side scaling (RSS). */
  ef_vi_set        vi_set;
  int              vi_set_size;
};


struct vi {
  /* ID of this VI. */
  int              id;
  /* The network interface this VI is associated with. */
  struct net_if*   net_if;
  /* Handle for accessing the driver. */
  ef_driver_handle dh;
  /* Virtual interface (hardware send/recv interface). */
  ef_vi            vi;
  /* Registered memory. */
  void*            pkt_bufs;
  int              pkt_bufs_n;
  ef_memreg        memreg;
  /* Offset where the ethernet header starts on RX packets. */
  int              frame_off;
  /* Offset where the minor ticks can be found on RX packets. */
  int              minor_ticks_off;
  /* Pool of free packet buffers (LIFO to minimise working set). */
  struct pkt_buf*  free_pkt_bufs;
  int              free_pkt_bufs_n;

  /* Pool of free packet buffers for TX operation (LIFO to minimise working set). */
  /* Currently they are just few buffers (MAX_TX_BUFFERS) from RX queue which are taken
   *        out and kept in separate list */

  struct pkt_buf*  tx_free_pkt_bufs;
  int              tx_free_pkt_bufs_n;
  /* Introducing locks around free_pkt_bufs as we are using it in
   * multi-thread setup */
  pthread_mutex_t      vi_lock;
};


struct pkt_buf {
  struct vi*       vi_owner;
  /* Linked list of [pkt_buf]s. */
  struct pkt_buf*  next;
  /* The I/O address of this buffer in each net_if.  Indexed by [net_if->id] */
  ef_addr          addr[MAX_NET_IFS];
  /* The ID of this buffer within [vi_owner]s pool. */
  int              id;
  /* Reference count.  No concurrency control, so we assume packet buffers
   * are only used in a single thread.
   */
  int              n_refs;
  /* If this buffer is reserved for TX part */
  int              is_tx;
};


#define TRY(x)                                                  \
  do {                                                          \
    int __rc = (x);                                             \
    if( __rc < 0 ) {                                            \
      fprintf(stderr, "ERROR: TRY(%s) failed\n", #x);           \
      fprintf(stderr, "ERROR: at %s:%d\n", __FILE__, __LINE__); \
      fprintf(stderr, "ERROR: rc=%d errno=%d (%s)\n",           \
              __rc, errno, strerror(errno));                    \
      exit(1);                                                  \
    }                                                           \
  } while( 0 )


#define TEST(x)                                                 \
  do {                                                          \
    if( ! (x) ) {                                               \
      fprintf(stderr, "ERROR: TEST(%s) failed\n", #x);          \
      fprintf(stderr, "ERROR: at %s:%d\n", __FILE__, __LINE__); \
      exit(1);                                                  \
    }                                                           \
  } while( 0 )


/* Align address where data is delivered onto EF_VI_DMA_ALIGN boundary,
 * because that gives best performance.
 */
#define RX_DMA_OFF     ROUND_UP(sizeof(struct pkt_buf), EF_VI_DMA_ALIGN)

/* Where does a received packet start?  The hardware usually puts a
 * meta-data prefix in front of the packet data.
 */
#define RX_PKT_OFF(vi)   (RX_DMA_OFF + (vi)->frame_off)

/* Get pointer to the received packet payload. */
#define RX_PKT_PTR(pb)   ((char*) (pb) + RX_PKT_OFF((pb)->vi_owner))

/* Pack VI that owns the packet buffer, and the buffer-id into TX request
 * ID.  NB. We only have 16-bits to play with.
 */
#define MK_TX_RQ_ID(vi_i, pb_i)  (((vi_i) << 12) | (pb_i))
#define TX_RQ_ID_VI(rq_id)       ((rq_id) >> 12)
#define TX_RQ_ID_PB(rq_id)       ((rq_id) & 0xfff)





// #################################### related to sf control #############


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

int alloc_filter_default(struct dragonet_sf_queue *sfq);
int alloc_filter_full_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip, uint16_t localport,
            uint32_t remoteip, uint16_t remoteport);
int alloc_filter_listen_ipv4(struct dragonet_sf_queue *sfq, int protocol,
            uint32_t localip, uint16_t localport);


// #################################### related to sf control #############


struct net_if*
net_if_alloc(int net_if_id, const char* name, int rss_set_size);

extern void
net_if_map_vi_pool(struct net_if*, struct vi*);

extern struct pkt_buf*
pkt_buf_from_id(struct vi* vi, int pkt_buf_i);


extern void
pkt_buf_release(struct pkt_buf* pkt_buf);

extern void
vi_refill_rx_ring(struct vi* vi, int no_bufs);

extern int
filter_parse(ef_filter_spec* fs, const char* s_in);

extern struct vi*
vi_alloc(int id, struct net_if*, enum ef_vi_flags flags);

extern struct vi*
vi_alloc_from_set(int id, struct net_if*, int vi_set_instance);

extern int
vi_send(struct vi* vi, struct pkt_buf* pkt_buf, int off, int len);


extern struct pkt_buf*
vi_get_free_pkt_buf(struct vi* vi);

struct pkt_buf*
vi_get_free_pkt_buf_tx(struct vi* vi);


#define PRINTBUFSIZE    (1023)
int buf_details(struct pkt_buf* pkt_buf, char *buff, int l);

// NOTE: this is quite outdated after indroduction of dprint in implementation.h
//#define MYDEBUG     1
#ifdef MYDEBUG
#ifndef dprint2
#define dprint2(x...)    printf("sfdebug:" x)
#endif // dprint
#else
#define dprint2(x...)   ((void)0)
#endif // MYDEBUG


// This value should be set based on the parameter to llvm-cgen-sf
#define SF_MAX_QUEUES           (10)

// this one is connected via switch on asiago
//#define IFNAME              "p801p1"
#define IFNAME               "p786p1"
#define CONFIG_LOCAL_MAC_sf  0x644d07530f00ULL  // "00:0f:53:07:4d:64"
#define CONFIG_LOCAL_IP_sf   0x0a7104c3         // "10.113.4.195"


// this one is connected via switch on appenzeller
//#define IFNAME              "p6p2"
//#define CONFIG_LOCAL_MAC_sf  0x495107530f00ULL  // "00:0f:53:07:51:49"
//#define CONFIG_LOCAL_IP_sf   0x0a710447         // "10.113.4.71"




#endif  /* __EFVI_SFW_H__ */
