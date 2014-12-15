/*-
 *   BSD LICENSE
 *
 *   Copyright(c) 2010-2013 Intel Corporation. All rights reserved.
 *   All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *     * Neither the name of Intel Corporation nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/******************************************************************
 * FIXME: This file is copied from dpdk-1.5.0r1/app/dpdkDriver/dpdkData.c
 *      which intern was based on some code in  dpdk-1.5.0r1/app/testpmd/
 *      code.  So, essentially old codebase.  You may want to fix it wo
 *      work with newcode.
 *
 ******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <sys/types.h>
#include <sys/queue.h>
#include <netinet/in.h>
#include <setjmp.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>
#include <getopt.h>
#include <assert.h>


#include <rte_common.h>
#include <rte_log.h>
#include <rte_memory.h>
#include <rte_memcpy.h>
#include <rte_memzone.h>
#include <rte_tailq.h>
#include <rte_eal.h>
#include <rte_per_lcore.h>
#include <rte_launch.h>
#include <rte_atomic.h>
#include <rte_cycles.h>
#include <rte_prefetch.h>
#include <rte_lcore.h>
#include <rte_per_lcore.h>
#include <rte_branch_prediction.h>
#include <rte_interrupts.h>
#include <rte_pci.h>
#include <rte_random.h>
#include <rte_debug.h>
#include <rte_ether.h>
#include <rte_ethdev.h>
#include <rte_ring.h>
#include <rte_mempool.h>
#include <rte_mbuf.h>

#include "dpdkControl.h"

#define RTE_LOGTYPE_L2FWD RTE_LOGTYPE_USER1

#define GOTO_FAIL(str, ...) do {					\
		printf("%s FAILED (%s: l.%d): <" str ">\n",		\
		       __func__, __FILE__, __LINE__,  ##__VA_ARGS__);   \
		goto fail;						\
} while(0)


#define MBUF_SIZE (2048 + sizeof(struct rte_mbuf) + RTE_PKTMBUF_HEADROOM)
#define     IFNAMSIZDPDK        1023



static int already_initialized = 0; // checks if the init function is already called

static uint64_t rx_count = 0;
static uint64_t tx_count = 0;
static uint64_t tx_error = 0;
static int64_t count_diff = 0;
static int64_t count_diff_last = 0;

void print_stats_dpdk(void *nic_p, int port_idi)
{
    uint8_t port_id = (uint8_t)port_idi;
    assert(nic_p != NULL);
    struct rte_eth_stats stats;
    memset(&stats, 0, sizeof(struct rte_eth_stats));
    rte_eth_stats_get(port_id, &stats);

    printf
    //dprint
        ("SW (RX:%-10"PRIu64", TX:%-10"PRIu64", DIFF:%-10"PRId64"), "
            "HW-RX (T:%-10"PRIu64", D: %-10"PRIu64" E: %-10"PRIu64"), "
            "HW-TX (T:%-10"PRIu64", D: %-10"PRIu64", E: %-10"PRIu64"), "
            "FDIR (H:%-10"PRIu64" M:%-10"PRIu64"), MBUF-E: %-10"PRIu64"\n",
            rx_count, tx_count, ((uint64_t)rx_count - (uint64_t)tx_count),
            stats.ipackets, stats.imissed, stats.ierrors,
            stats.opackets, stats.oerrors, tx_error,
            stats.fdirmatch,  stats.fdirmiss,
            stats.rx_nombuf
          );
}


void send_packetV2(void *nic_p, int core_id, int port_id, int queue_id,
        char *pkt_tx, size_t len)
{

        struct rte_mbuf *m_tx = NULL;

        dprint("sending pkt-no %"PRIu64" on q:%d, core=%d\n",
                tx_count, queue_id, core_id);
        assert(nic_p != NULL);
        if (m_tx == NULL) {
            //m_tx = rte_pktmbuf_alloc(fwd_lcores[core_id]->mbp);
            m_tx = rte_pktmbuf_alloc(tx_queues_mbp[queue_id]);
        }

	if (m_tx == NULL) {
                ++tx_error;
		printf("ERROR: (%s:%d:%s) Cannot allocate mbuf. core_id: %d, queue=%d\n",
                       __FILE__, __LINE__, __func__, core_id, queue_id);
                print_stats_dpdk(nic_p, port_id);
		GOTO_FAIL("Cannot allocate mbuf");
                return;
        }
	if (rte_pktmbuf_pkt_len(m_tx) != 0) {
                ++tx_error;

                printf("ERROR: buf has len of %"PRIu32", instead of 0\n",
                        rte_pktmbuf_pkt_len(m_tx));

                rte_pktmbuf_reset(m_tx);
                printf("NOTE: buf has len of %"PRIu32" after reset\n",
                        rte_pktmbuf_pkt_len(m_tx));

                print_stats_dpdk(nic_p, port_id);
                abort();

		GOTO_FAIL("Bad length");
//		printf("ERROR: (%s:%d:%s) Bad length of allocated mbuf\n",
//                        __FILE__, __LINE__, __func__);
		rte_pktmbuf_free(m_tx);
                return;
        }

	char *data = rte_pktmbuf_append(m_tx, len);
	if (data == NULL) {
                ++tx_error;
		GOTO_FAIL("Cannot append data");
        }
	if (rte_pktmbuf_pkt_len(m_tx) != len) {
                ++tx_error;
		GOTO_FAIL("Bad pkt length");
        }
//	if (rte_pktmbuf_data_len(m_tx) != MBUF_TEST_DATA_LEN)
//		GOTO_FAIL("Bad data length");
	memcpy(data, pkt_tx, len);
	if (!rte_pktmbuf_is_contiguous(m_tx)) {
                ++tx_error;
		GOTO_FAIL("Buffer should be continuous");
            }


//	struct ether_hdr *eth;
//	eth = rte_pktmbuf_mtod(m_tx, struct ether_hdr *);

	/* src addr */
        // ether_addr_copy(&l2fwd_ports_eth_addr[port_id], &eth->s_addr);
//	ether_addr_copy(&ports[port_id].eth_addr, &eth->s_addr);

	struct rte_mbuf *m_table[2];
        m_table[0] = m_tx;
        m_table[1] = NULL;
        uint16_t nb_pkts = 1;
	unsigned ret = rte_eth_tx_burst(port_id, (uint16_t) queue_id,
                m_table, nb_pkts);

	if (unlikely(ret < nb_pkts)) {
                printf("failed to send packet\n");
                assert(ret == nb_pkts); // FIXME: I need better way to handle errors
		do {
			//rte_pktmbuf_reset(m_table[ret]);
			rte_pktmbuf_free(m_table[ret]);
		} while (++ret < nb_pkts);
                ++tx_error;
	} else {
            tx_count =  tx_count + ret;

            count_diff_last = count_diff;
            count_diff = (int64_t)rx_count - (int64_t)tx_count;
#ifdef SHOW_INTERVAL_STATS
            if (tx_count % INTERVAL_STAT_FREQUENCY == 0) {
                dprint("## TX ##\n");
                print_stats_dpdk(nic_p, port_id);
            }
#endif // SHOW_INTERVAL_STATS
/*
            if (count_diff > count_diff_last) {
                printf("## DIFF %"PRId64" , last-diff %"PRId64" ##\n",
                        count_diff, count_diff_last);
                print_stats_dpdk(nic_p, port_id);
            }
*/
        }

        dprint("sending done: pkt-no %"PRIu64" on q:%d, core=%d\n",
                tx_count, queue_id, core_id);
//        tx_lock = 0;
        return;

fail:
        if (m_tx) {
            printf("ERROR: %s:%s: %p Packet not sent!\n",
                    __FILE__, __FUNCTION__, m_tx);
            rte_pktmbuf_free(m_tx);

        }
	return;
} // end function: send_packetV2



size_t get_packet_blocking(void *nic_p, int core_id, int port_id, int queue_id,
        char *pkt_out, size_t buf_len)
{
    struct rte_mbuf *pkts_burst[2];
    struct rte_mbuf *m;
    unsigned portid, nb_rx;
    size_t pkt_size = 0;

    assert(nic_p != NULL);
/*
    struct rte_mbuf *pkts_burst2[2];
    unsigned nb_rx_other_q = 0;
*/

    dprint("get_packet_blocking on queue_id %d, port_id %d, core_id %d\n", queue_id,
            port_id, core_id);

    portid = port_id;

    do {
        nb_rx = rte_eth_rx_burst((uint8_t) portid, (uint16_t) queue_id,
                pkts_burst, 1);

/*
        nb_rx_other_q = rte_eth_rx_burst((uint8_t) portid, (uint16_t) (queue_id + 1),
                pkts_burst2, 1);

        if (nb_rx_other_q > 0) {
            printf("Other queue %d received %d packets\n", (queue_id + 1), nb_rx_other_q);
        }
*/
    } while (nb_rx <= 0);

    assert(nb_rx == 1);

    if (nb_rx > 1) {
        printf("ERROR: (%s:%d:%s) Multiple pkts (%d) in queue %d"
                "for core %d, in brust, processing only one pkt\n",
                __FILE__, __LINE__, __func__,
                nb_rx, port_id, core_id);
    }

    //printf("Received %d packets on queue %d\n", nb_rx, queue_id);

    // Taking out the fist packet from the brust
    m = pkts_burst[0];
    rte_prefetch0(rte_pktmbuf_mtod(m, void *));

    //printf("Packet of len %d received at %p\n", m->pkt.pkt_len, m->pkt.data);
    if (m->pkt.pkt_len > 0 && m->pkt.pkt_len < buf_len) {
        // rte_pktmbuf_dump(m, 0);
        pkt_size = m->pkt.pkt_len;
    } else {
        printf("ERROR: (%s:%d:%s) too small buffer to copy packet."
                "pkt_len %"PRIu32", buf_len = %zd\n",
                __FILE__, __LINE__, __func__,
                m->pkt.pkt_len, buf_len);
        pkt_size =  buf_len;
    }
    memcpy(pkt_out, m->pkt.data, pkt_size);
    rte_pktmbuf_free(m);
    dprint("received packet of size %zu\n", pkt_size);
    return pkt_size;

} // end function: get_packet_blocking



size_t get_packet_nonblock(void *nic_p, int core_id, int port_id, int queue_id,
        char *pkt_out, size_t buf_len)
{
    struct rte_mbuf *pkts_burst[2];
    struct rte_mbuf *m;
    unsigned portid, nb_rx;
    size_t pkt_size = 0;

    assert(nic_p != NULL);

    portid = port_id;

    nb_rx = rte_eth_rx_burst((uint8_t) portid, (uint16_t) queue_id,
                pkts_burst, 1);

    if (nb_rx == 0) {
       // no packets to read yet.
       return 0;
    }

    assert(nb_rx == 1);

    if (nb_rx > 1) {
        printf("ERROR: (%s:%d:%s) Multiple pkts (%d) in queue %d"
                "for core %d, in brust, processing only one pkt\n",
                __FILE__, __LINE__, __func__,
                nb_rx, port_id, core_id);
    }

    //printf("Received %d packets on queue %d\n", nb_rx, queue_id);

    // Taking out the fist packet from the brust
    m = pkts_burst[0];

    rte_prefetch0(rte_pktmbuf_mtod(m, void *));

    //printf("Packet of len %d received at %p\n", m->pkt.pkt_len, m->pkt.data);
    if (m->pkt.pkt_len > 0 && m->pkt.pkt_len < buf_len) {
        // rte_pktmbuf_dump(m, 0);
        pkt_size = m->pkt.pkt_len;
    } else {
        printf("ERROR: (%s:%d:%s) too small buffer to copy packet."
                "pkt_len %"PRIu32", buf_len = %zd\n",
                __FILE__, __LINE__, __func__,
                m->pkt.pkt_len, buf_len);
        pkt_size =  buf_len;
    }
    memcpy(pkt_out, m->pkt.data, pkt_size);
    rte_pktmbuf_free(m);

    dprint("queue_id %d, port_id %d, core_id %d:"
            " received pkt of len %zu\n", queue_id, port_id, core_id, pkt_size);

    ++rx_count;
#ifdef SHOW_INTERVAL_STATS
    if (rx_count % INTERVAL_STAT_FREQUENCY == 0) {
        print_stats_dpdk(nic_p, port_id);
    }
#endif // SHOW_INTERVAL_STATS
    return pkt_size;
} // end function: get_packet_nonblock


#define ARGNOS (15)
int init_dpdk_setupV2(int queues)
{

    if (already_initialized == 1) {
        printf("ERROR: Already initialized\n");
        assert(!"ERROR: Already initialized\n");
        return -1;
    }

    const char *myArgs[ARGNOS] = {"./stack-dpdk",
        "-c", "0xff",  // coremask
        "-n", "1",  // no of mem chanels // FIXME: figure out proper value here
        "--file-prefix=dnetHuge",
//        "--no-huge",
        "-m", "700",
        "--",
        "--pkt-filter-mode=perfect",
//        "--pkt-filter-mode=signature",
	"--burst=16",
        "--disable-rss",
        "--disable-hw-vlan",
        "", ""}; // 13 arguments

    char *myArgs2[ARGNOS + 4];
    int i, j;
    for (i = 0, j = 0; i < ARGNOS ; ++i, ++j) {
        myArgs2[j] = malloc(127);
        if (strcmp("--", myArgs[i]) == 0) {
            printf("Inserting special agruments at location %d, "
                    "after seprator: [%s]\n", i, myArgs[i]);
            snprintf(myArgs2[j], 126, "--");

            ++j;
            myArgs2[j] = malloc(127);
            snprintf(myArgs2[j], 126, "--rxq=%d", queues);

            ++j;
            myArgs2[j] = malloc(127);
            snprintf(myArgs2[j], 126, "--txq=%d", queues);

        } else {
            if (myArgs[i] == NULL) {
                printf("Found NULL for location %d, so putting null\n", i);
                myArgs2[j] = NULL;
            } else {
                printf("copying %dth string [%s]\n", i, myArgs[i]);
                strncpy(myArgs2[j], myArgs[i], 126);
            }
        }
    } // end for:  marshalling cmdline args

    printf("Hello world from DPDK...., V3\n");
    int ret = init_device(j, myArgs2);
    if (ret < 0) {
        printf("ERROR: %s: Initialization failed (ret val=%d)\n",
                __func__, ret);
    }

    printf("\nInitialization successful.\n");

    return ret;
} // end function:  init_dpdk_setupV2

#if 0
int main(int __attribute__((unused)) argc, char __attribute__((unused)) *argv[])
{
    return init_dpdk_setupV2();
}
#endif // 0


void *init_dpdk_setup_and_get_default_queue2(char *ifAddr,
        int queues)
{
    printf("WARNING: Ignoring %s interface address suggestion\n", ifAddr);
    printf("    going for the NIC which is connected to uio\n");
    printf("Initializing %d queues\n", queues);
    int ret = init_dpdk_setupV2(queues);
    if (ret < 0) {
        printf("ERROR: %s:%d:%s init_dpdk_setupV2 failed\n",
                    __FILE__, __LINE__, __func__);
        return NULL;
    }

    struct dpdk_info *dev = NULL;
    dev = (struct dpdk_info *) malloc (sizeof(struct dpdk_info));
    if (dev == NULL) {
        printf("ERROR: %s:%d:%s malloc failed\n",
                    __FILE__, __LINE__, __func__);
        return NULL;
    }
    memset(dev, 0, sizeof(struct dpdk_info));
    dev->core_id = 0;
    dev->port_id = 0;
    dev->queue_id = 0;
    dev->ptr = NULL;
    // FIXME: send some useful pointer here instead of sending empty box!!!
    return (void *)dev;
}

