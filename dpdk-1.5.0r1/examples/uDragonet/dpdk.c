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

//#include "main.h"

#define RTE_LOGTYPE_L2FWD RTE_LOGTYPE_USER1

#define GOTO_FAIL(str, ...) do {					\
		printf("%s FAILED (%s: l.%d): <" str ">\n",		\
		       __func__, __FILE__, __LINE__,  ##__VA_ARGS__);			\
		goto fail;						\
} while(0)


#define MBUF_SIZE (2048 + sizeof(struct rte_mbuf) + RTE_PKTMBUF_HEADROOM)
#define     IFNAMSIZDPDK        1023

struct dpdk_handler {
	int dpdk_fd;
	int ctl_fd;
	char name[IFNAMSIZDPDK];
        char pkt_data_buff[MBUF_SIZE]; // packet will be stored here
        uint32_t portid_tx;   // port id to be used for sending
        struct lcore_queue_conf *qconf_rx;
};

size_t get_packet(struct dpdk_handler *dpdk, char *pkt_out, size_t buf_len);
void send_packet(struct dpdk_handler *dpdk, char *pkt_tx, size_t len);
struct dpdk_handler *init_dpdk_setup(const char *name);

int fdir_add_perfect_filter_wrapper(int queue_id, char *srcIP, int srcPort,
        char *dstIP, int dstPort, int type);
int fdir_add_perfect_filter2_wrapper(int queue_id);
int fdir_del_perfect_filter_wrapper(int queue_id);
int fdir_add_flow_filter_wrapper(int queue_id);
int fdir_del_flow_filter_wrapper(int queue_id);

struct dpdk_handler *g_dpdk_if;

static char pkt_data_buff[MBUF_SIZE]; // packet will be stored here
//static char pkt_data_buff_tx[MBUF_SIZE]; // packet to be sent will be stored here
static unsigned portid_tx = 0;   // port id to be used for sending
static struct lcore_queue_conf *qconf_rx = NULL;
    // qconf ds which will be used for receiving

//#define NB_MBUF   8192
//#define NB_MBUF  64   //  does not initialize
//#define NB_MBUF  128  // initializes, but does not accept packets
#define NB_MBUF  256    // initializes, accepts packet
//#define NB_MBUF  512    // initializes, accepts packets

/*
 * RX and TX Prefetch, Host, and Write-back threshold values should be
 * carefully set for optimal performance. Consult the network
 * controller's datasheet and supporting DPDK documentation for guidance
 * on how these parameters should be set.
 */
#define RX_PTHRESH 8 /**< Default values of RX prefetch threshold reg. */
#define RX_HTHRESH 8 /**< Default values of RX host threshold reg. */
#define RX_WTHRESH 4 /**< Default values of RX write-back threshold reg. */

/*
 * These default values are optimized for use with the Intel(R) 82599 10 GbE
 * Controller and the DPDK ixgbe PMD. Consider using other values for other
 * network controllers and/or network drivers.
 */
#define TX_PTHRESH 36 /**< Default values of TX prefetch threshold reg. */
#define TX_HTHRESH 0  /**< Default values of TX host threshold reg. */
#define TX_WTHRESH 0  /**< Default values of TX write-back threshold reg. */

//#define MAX_PKT_BURST 32
#define MAX_PKT_BURST 1
#define BURST_TX_DRAIN_US 100 /* TX drain every ~100us */

/*
 * Configurable number of RX/TX ring descriptors
 */
#define RTE_TEST_RX_DESC_DEFAULT 128
#define RTE_TEST_TX_DESC_DEFAULT 512
static uint16_t nb_rxd = RTE_TEST_RX_DESC_DEFAULT;
static uint16_t nb_txd = RTE_TEST_TX_DESC_DEFAULT;

/* ethernet addresses of ports */
static struct ether_addr l2fwd_ports_eth_addr[RTE_MAX_ETHPORTS];

/* mask of enabled ports */
static uint32_t l2fwd_enabled_port_mask = 0;

/* list of enabled ports */
static uint32_t l2fwd_dst_ports[RTE_MAX_ETHPORTS];

static unsigned int l2fwd_rx_queue_per_lcore = 1;

struct mbuf_table {
	unsigned len;
	struct rte_mbuf *m_table[MAX_PKT_BURST];
};

#define MAX_RX_QUEUE_PER_LCORE 16
#define MAX_TX_QUEUE_PER_PORT 16
struct lcore_queue_conf {
	unsigned n_rx_port;
	unsigned rx_port_list[MAX_RX_QUEUE_PER_LCORE];
	struct mbuf_table tx_mbufs[RTE_MAX_ETHPORTS];

} __rte_cache_aligned;
struct lcore_queue_conf lcore_queue_conf[RTE_MAX_LCORE];

static const struct rte_eth_conf port_conf = {
	.rxmode = {
		.split_hdr_size = 0,
		.header_split   = 0, /**< Header Split disabled */
		.hw_ip_checksum = 0, /**< IP checksum offload disabled */
		.hw_vlan_filter = 0, /**< VLAN filtering disabled */
		.jumbo_frame    = 0, /**< Jumbo Frame Support disabled */
		.hw_strip_crc   = 0, /**< CRC stripped by hardware */
	},
	.txmode = {
		.mq_mode = ETH_MQ_TX_NONE,
	},
};

static const struct rte_eth_rxconf rx_conf = {
	.rx_thresh = {
		.pthresh = RX_PTHRESH,
		.hthresh = RX_HTHRESH,
		.wthresh = RX_WTHRESH,
	},
};

static const struct rte_eth_txconf tx_conf = {
	.tx_thresh = {
		.pthresh = TX_PTHRESH,
		.hthresh = TX_HTHRESH,
		.wthresh = TX_WTHRESH,
	},
	.tx_free_thresh = 0, /* Use PMD default values */
	.tx_rs_thresh = 0, /* Use PMD default values */
};

struct rte_mempool * l2fwd_pktmbuf_pool = NULL;

/* Per-port statistics struct */
struct l2fwd_port_statistics {
	uint64_t tx;
	uint64_t rx;
	uint64_t dropped;
} __rte_cache_aligned;
struct l2fwd_port_statistics port_statistics[RTE_MAX_ETHPORTS];

/* A tsc-based timer responsible for triggering statistics printout */
#define TIMER_MILLISECOND 2000000ULL /* around 1ms at 2 Ghz */
#define MAX_TIMER_PERIOD 86400 /* 1 day max */
static int64_t timer_period = 10 * TIMER_MILLISECOND * 1000; /* default period is 10 seconds */

/* Print out statistics on packets dropped */
//static
void print_stats(void);
void print_stats(void)
{
	uint64_t total_packets_dropped, total_packets_tx, total_packets_rx;
	unsigned portid;

	total_packets_dropped = 0;
	total_packets_tx = 0;
	total_packets_rx = 0;

		/* Clear screen and move to top left */
	//const char clr[] = { 27, '[', '2', 'J', '\0' };
	//const char topLeft[] = { 27, '[', '1', ';', '1', 'H','\0' };
	//printf("%s%s", clr, topLeft);

	printf("\nPort statistics ====================================");

	for (portid = 0; portid < RTE_MAX_ETHPORTS; portid++) {
		/* skip disabled ports */
		if ((l2fwd_enabled_port_mask & (1 << portid)) == 0)
			continue;
		printf("\nStatistics for port %u ------------------------------"
			   "\nPackets sent: %24"PRIu64
			   "\nPackets received: %20"PRIu64
			   "\nPackets dropped: %21"PRIu64,
			   portid,
			   port_statistics[portid].tx,
			   port_statistics[portid].rx,
			   port_statistics[portid].dropped);

		total_packets_dropped += port_statistics[portid].dropped;
		total_packets_tx += port_statistics[portid].tx;
		total_packets_rx += port_statistics[portid].rx;
	}
	printf("\nAggregate statistics ==============================="
		   "\nTotal packets sent: %18"PRIu64
		   "\nTotal packets received: %14"PRIu64
		   "\nTotal packets dropped: %15"PRIu64,
		   total_packets_tx,
		   total_packets_rx,
		   total_packets_dropped);
	printf("\n====================================================\n");
}

/* Send the burst of packets on an output interface */
static int
l2fwd_send_burst(struct lcore_queue_conf *qconf, unsigned n, uint8_t port)
{
	struct rte_mbuf **m_table;
	unsigned ret;
	unsigned queueid =0;

	m_table = (struct rte_mbuf **)qconf->tx_mbufs[port].m_table;

	ret = rte_eth_tx_burst(port, (uint16_t) queueid, m_table, (uint16_t) n);
	port_statistics[port].tx += ret;
	if (unlikely(ret < n)) {
		port_statistics[port].dropped += (n - ret);
		do {
			rte_pktmbuf_free(m_table[ret]);
		} while (++ret < n);
	}

	return 0;
}

/* Enqueue packets for TX and prepare them to be sent */
static int
l2fwd_send_packet(struct rte_mbuf *m, uint8_t port)
{
	unsigned lcore_id, len;
	struct lcore_queue_conf *qconf;

	lcore_id = rte_lcore_id();

	qconf = &lcore_queue_conf[lcore_id];
	len = qconf->tx_mbufs[port].len;
	qconf->tx_mbufs[port].m_table[len] = m;
	len++;

	/* enough pkts to be sent */
	if (unlikely(len == MAX_PKT_BURST)) {
		l2fwd_send_burst(qconf, MAX_PKT_BURST, port);
		len = 0;
	}

	qconf->tx_mbufs[port].len = len;
	return 0;
}



void send_packet(struct dpdk_handler *dpdk, char *pkt_tx, size_t len)
{
        assert(dpdk != NULL);
        printf("%s:sending packet of size %zu\n", dpdk->name, len);
	struct ether_hdr *eth;
//	void *tmp;

	unsigned dst_port;
        struct rte_mbuf *m = rte_pktmbuf_alloc(l2fwd_pktmbuf_pool);

	if (m == NULL) {
		GOTO_FAIL("Cannot allocate mbuf");
//		printf("ERROR: (%s:%d:%s) Cannot allocate mbuf\n",
//                        __FILE__, __LINE__, __func__);
//                return;
        }
	if (rte_pktmbuf_pkt_len(m) != 0) {
		GOTO_FAIL("Bad length");
//		printf("ERROR: (%s:%d:%s) Bad length of allocated mbuf\n",
//                        __FILE__, __LINE__, __func__);
//		rte_pktmbuf_free(m);
                return;
        }

	char *data = rte_pktmbuf_append(m, len);
	if (data == NULL)
		GOTO_FAIL("Cannot append data");
	if (rte_pktmbuf_pkt_len(m) != len)
		GOTO_FAIL("Bad pkt length");
//	if (rte_pktmbuf_data_len(m) != MBUF_TEST_DATA_LEN)
//		GOTO_FAIL("Bad data length");
	memcpy(data, pkt_tx, len);
	if (!rte_pktmbuf_is_contiguous(m))
		GOTO_FAIL("Buffer should be continuous");


        // PS: assuming last used portid
	dst_port = l2fwd_dst_ports[portid_tx];

	eth = rte_pktmbuf_mtod(m, struct ether_hdr *);

	/* 02:00:00:00:00:xx */
	//tmp = &eth->d_addr.addr_bytes[0];
	//*((uint64_t *)tmp) = 0x000000000002 + ((uint64_t)dst_port << 40);

        // PS: for time being, I will keep this code which adds our address
        // instead of original address
	/* src addr */
	ether_addr_copy(&l2fwd_ports_eth_addr[dst_port], &eth->s_addr);

	l2fwd_send_packet(m, (uint8_t) dst_port);

fail:
	if (m)
		rte_pktmbuf_free(m);
	return;
}


size_t get_packet(struct dpdk_handler *dpdk, char *pkt_out, size_t buf_len)
{
    struct rte_mbuf *pkts_burst[MAX_PKT_BURST];
    struct rte_mbuf *m;
    unsigned i, j, portid, nb_rx;
    size_t pkt_size = 0;

    assert(dpdk != NULL);
    if (qconf_rx == NULL) {
        printf("ERROR: (%s:%d:%s) %s: RX side not initialized\n",
                __FILE__, __LINE__, __func__, dpdk->name);
        return pkt_size;
    }

    while (1) {
        /*
         * Read packet from RX queues
         */
        for (i = 0; i < qconf_rx->n_rx_port; i++) {

            portid = qconf_rx->rx_port_list[i];
            nb_rx = rte_eth_rx_burst((uint8_t) portid, 0,
                    pkts_burst, MAX_PKT_BURST);

            port_statistics[portid].rx += nb_rx;
            if (nb_rx > 1) {
                printf("ERROR: (%s:%d:%s) Multiple pkts (%d) in queue %d"
                        "in brust, processing only one pkt\n",
                        __FILE__, __LINE__, __func__,
                        nb_rx, i);
            }
            //        printf("Received %d packets on queue %d\n", nb_rx, i);
            for (j = 0; j < nb_rx; j++) {
                m = pkts_burst[j];
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
                printf("received packet of size %zu\n", pkt_size);
                return pkt_size;
            } // end for : for each packet in packet_brust
        } // end for: for each port, receive

    } // end while: infinite

    return pkt_size;
} // end function : get_packet



/* main processing loop */
//static
void l2fwd_main_loop(struct dpdk_handler *dpdk);
void l2fwd_main_loop(struct dpdk_handler *dpdk)
{

    assert(dpdk != NULL);
        int in_pkt_len = 0, in_pkt_count = 0;
	while (1) {

            // get an incoming packet
            in_pkt_len = get_packet(dpdk, pkt_data_buff, sizeof(pkt_data_buff));
            printf("%d:incoming packet of len %d\n", in_pkt_count, in_pkt_len);
            ++in_pkt_count;

            // send out the same packet
            send_packet(dpdk, pkt_data_buff, in_pkt_len);
	} // end while: infinite while loop

} // end function: l2fwd_main_loop

static int
l2fwd_launch_one_lcore(__attribute__((unused)) void *dummy)
{
	unsigned lcore_id;

	lcore_id = rte_lcore_id();
	qconf_rx = &lcore_queue_conf[lcore_id];

	if (qconf_rx->n_rx_port == 0) {
		RTE_LOG(INFO, L2FWD, "lcore %u has nothing to do\n", lcore_id);
		return -1;
	}

	RTE_LOG(INFO, L2FWD, "entering main loop on lcore %u for %u queues\n",
                lcore_id, qconf_rx->n_rx_port);

        unsigned i = 0;
	for (i = 0; i < qconf_rx->n_rx_port; i++) {
		portid_tx = qconf_rx->rx_port_list[i];
		RTE_LOG(INFO, L2FWD, " -- lcoreid=%u portid=%u\n", lcore_id,
			portid_tx);
	}

//	l2fwd_main_loop();
	return 0;
}

/* display usage */
static void
l2fwd_usage(const char *prgname)
{
	printf("%s [EAL options] -- -p PORTMASK [-q NQ]\n"
	       "  -p PORTMASK: hexadecimal bitmask of ports to configure\n"
	       "  -q NQ: number of queue (=ports) per lcore (default is 1)\n"
		   "  -T PERIOD: statistics will be refreshed each PERIOD seconds (0 to disable, 10 default, 86400 maximum)\n",
	       prgname);
}

static int
l2fwd_parse_portmask(const char *portmask)
{
	char *end = NULL;
	unsigned long pm;

	/* parse hexadecimal string */
	pm = strtoul(portmask, &end, 16);
	if ((portmask[0] == '\0') || (end == NULL) || (*end != '\0'))
		return -1;

	if (pm == 0)
		return -1;

	return pm;
}

static unsigned int
l2fwd_parse_nqueue(const char *q_arg)
{
	char *end = NULL;
	unsigned long n;

	/* parse hexadecimal string */
	n = strtoul(q_arg, &end, 10);
	if ((q_arg[0] == '\0') || (end == NULL) || (*end != '\0'))
		return 0;
	if (n == 0)
		return 0;
	if (n >= MAX_RX_QUEUE_PER_LCORE)
		return 0;

	return n;
}

static int
l2fwd_parse_timer_period(const char *q_arg)
{
	char *end = NULL;
	int n;

	/* parse number string */
	n = strtol(q_arg, &end, 10);
	if ((q_arg[0] == '\0') || (end == NULL) || (*end != '\0'))
		return -1;
	if (n >= MAX_TIMER_PERIOD)
		return -1;

	return n;
}

/* Parse the argument given in the command line of the application */
static int
l2fwd_parse_args(int argc, char **argv)
{
	int opt, ret;
	char **argvopt;
	int option_index;
	char *prgname = argv[0];
	static struct option lgopts[] = {
		{NULL, 0, 0, 0}
	};

	argvopt = argv;

	while ((opt = getopt_long(argc, argvopt, "p:q:T:",
				  lgopts, &option_index)) != EOF) {

		switch (opt) {
		/* portmask */
		case 'p':
			l2fwd_enabled_port_mask = l2fwd_parse_portmask(optarg);
			if (l2fwd_enabled_port_mask == 0) {
				printf("invalid portmask\n");
				l2fwd_usage(prgname);
				return -1;
			}
			break;

		/* nqueue */
		case 'q':
			l2fwd_rx_queue_per_lcore = l2fwd_parse_nqueue(optarg);
			if (l2fwd_rx_queue_per_lcore == 0) {
				printf("invalid queue number\n");
				l2fwd_usage(prgname);
				return -1;
			}
			break;

		/* timer period */
		case 'T':
			timer_period = l2fwd_parse_timer_period(optarg) * 1000 * TIMER_MILLISECOND;
			if (timer_period < 0) {
				printf("invalid timer period\n");
				l2fwd_usage(prgname);
				return -1;
			}
			break;

		/* long options */
		case 0:
			l2fwd_usage(prgname);
			return -1;

		default:
			l2fwd_usage(prgname);
			return -1;
		}
	}

	if (optind >= 0)
		argv[optind-1] = prgname;

	ret = optind-1;
	optind = 0; /* reset getopt lib */
	return ret;
}

/* Check the link status of all ports in up to 9s, and print them finally */
static void
check_all_ports_link_status(uint8_t port_num, uint32_t port_mask)
{
#define CHECK_INTERVAL 100 /* 100ms */
#define MAX_CHECK_TIME 90 /* 9s (90 * 100ms) in total */
	uint8_t portid, count, all_ports_up, print_flag = 0;
	struct rte_eth_link link;

	printf("\nChecking link status");
	fflush(stdout);
	for (count = 0; count <= MAX_CHECK_TIME; count++) {
		all_ports_up = 1;
		for (portid = 0; portid < port_num; portid++) {
			if ((port_mask & (1 << portid)) == 0)
				continue;
			memset(&link, 0, sizeof(link));
			rte_eth_link_get_nowait(portid, &link);
			/* print link status if flag set */
			if (print_flag == 1) {
				if (link.link_status)
					printf("Port %d Link Up - speed %u "
						"Mbps - %s\n", (uint8_t)portid,
						(unsigned)link.link_speed,
				(link.link_duplex == ETH_LINK_FULL_DUPLEX) ?
					("full-duplex") : ("half-duplex\n"));
				else
					printf("Port %d Link Down\n",
						(uint8_t)portid);
				continue;
			}
			/* clear all_ports_up flag if any link down */
			if (link.link_status == 0) {
				all_ports_up = 0;
				break;
			}
		}
		/* after finally printing all link status, get out */
		if (print_flag == 1)
			break;

		if (all_ports_up == 0) {
			printf(".");
			fflush(stdout);
			rte_delay_ms(CHECK_INTERVAL);
		}

		/* set the print_flag if all ports up or timeout */
		if (all_ports_up == 1 || count == (MAX_CHECK_TIME - 1)) {
			print_flag = 1;
			printf("done\n");
		}
	}
}

int MAIN(int argc, char **argv);
int MAIN(int argc, char **argv)
{
	struct lcore_queue_conf *qconf;
	struct rte_eth_dev_info dev_info;
	int ret;
	uint8_t nb_ports;
	uint8_t nb_ports_available;
	uint8_t portid, last_port;
	unsigned lcore_id, rx_lcore_id;
	unsigned nb_ports_in_mask = 0;

	/* init EAL */
	ret = rte_eal_init(argc, argv);
	if (ret < 0)
		rte_exit(EXIT_FAILURE, "Invalid EAL arguments\n");
	argc -= ret;
	argv += ret;

	/* parse application arguments (after the EAL ones) */
	ret = l2fwd_parse_args(argc, argv);
	if (ret < 0)
		rte_exit(EXIT_FAILURE, "Invalid L2FWD arguments\n");

	/* create the mbuf pool */
	l2fwd_pktmbuf_pool =
		rte_mempool_create("mbuf_pool", NB_MBUF,
				   MBUF_SIZE, 32,
				   sizeof(struct rte_pktmbuf_pool_private),
				   rte_pktmbuf_pool_init, NULL,
				   rte_pktmbuf_init, NULL,
				   rte_socket_id(), 0);
	if (l2fwd_pktmbuf_pool == NULL)
		rte_exit(EXIT_FAILURE, "Cannot init mbuf pool\n");

	/* init driver(s) */
	if (rte_pmd_init_all() < 0)
		rte_exit(EXIT_FAILURE, "Cannot init pmd\n");

	if (rte_eal_pci_probe() < 0)
		rte_exit(EXIT_FAILURE, "Cannot probe PCI\n");

	nb_ports = rte_eth_dev_count();
	if (nb_ports == 0)
		rte_exit(EXIT_FAILURE, "No Ethernet ports - bye\n");

	if (nb_ports > RTE_MAX_ETHPORTS)
		nb_ports = RTE_MAX_ETHPORTS;

	/* reset l2fwd_dst_ports */
	for (portid = 0; portid < RTE_MAX_ETHPORTS; portid++)
		l2fwd_dst_ports[portid] = 0;
	last_port = 0;

	/*
	 * Each logical core is assigned a dedicated TX queue on each port.
	 */
	for (portid = 0; portid < nb_ports; portid++) {
		/* skip ports that are not enabled */
		if ((l2fwd_enabled_port_mask & (1 << portid)) == 0)
			continue;

		if (nb_ports_in_mask % 2) {
			l2fwd_dst_ports[portid] = last_port;
			l2fwd_dst_ports[last_port] = portid;
		}
		else
			last_port = portid;

		nb_ports_in_mask++;

		rte_eth_dev_info_get(portid, &dev_info);
	}
	if (nb_ports_in_mask % 2) {
		printf("Notice: odd number of ports in portmask.\n");
		l2fwd_dst_ports[last_port] = last_port;
	}

	rx_lcore_id = 0;
	qconf = NULL;

	/* Initialize the port/queue configuration of each logical core */
	for (portid = 0; portid < nb_ports; portid++) {
		/* skip ports that are not enabled */
		if ((l2fwd_enabled_port_mask & (1 << portid)) == 0)
			continue;

		/* get the lcore_id for this port */
		while (rte_lcore_is_enabled(rx_lcore_id) == 0 ||
		       lcore_queue_conf[rx_lcore_id].n_rx_port ==
		       l2fwd_rx_queue_per_lcore) {
			rx_lcore_id++;
			if (rx_lcore_id >= RTE_MAX_LCORE)
				rte_exit(EXIT_FAILURE, "Not enough cores\n");
		}

		if (qconf != &lcore_queue_conf[rx_lcore_id])
			/* Assigned a new logical core in the loop above. */
			qconf = &lcore_queue_conf[rx_lcore_id];

		qconf->rx_port_list[qconf->n_rx_port] = portid;
		qconf->n_rx_port++;
		printf("Lcore %u: RX port %u\n", rx_lcore_id, (unsigned) portid);
	}

	nb_ports_available = nb_ports;

	/* Initialise each port */
	for (portid = 0; portid < nb_ports; portid++) {
		/* skip ports that are not enabled */
		if ((l2fwd_enabled_port_mask & (1 << portid)) == 0) {
			printf("Skipping disabled port %u\n", (unsigned) portid);
			nb_ports_available--;
			continue;
		}
		/* init port */
		printf("Initializing port %u... ", (unsigned) portid);
		fflush(stdout);
		ret = rte_eth_dev_configure(portid, 1, 1, &port_conf);
		if (ret < 0)
			rte_exit(EXIT_FAILURE, "Cannot configure device: err=%d, port=%u\n",
				  ret, (unsigned) portid);

		rte_eth_macaddr_get(portid,&l2fwd_ports_eth_addr[portid]);

		/* init one RX queue */
		fflush(stdout);
		ret = rte_eth_rx_queue_setup(portid, 0, nb_rxd,
					     rte_eth_dev_socket_id(portid), &rx_conf,
					     l2fwd_pktmbuf_pool);
		if (ret < 0)
			rte_exit(EXIT_FAILURE, "rte_eth_rx_queue_setup:err=%d, port=%u\n",
				  ret, (unsigned) portid);

		/* init one TX queue on each port */
		fflush(stdout);
		ret = rte_eth_tx_queue_setup(portid, 0, nb_txd,
				rte_eth_dev_socket_id(portid), &tx_conf);
		if (ret < 0)
			rte_exit(EXIT_FAILURE, "rte_eth_tx_queue_setup:err=%d, port=%u\n",
				ret, (unsigned) portid);

		/* Start device */
		ret = rte_eth_dev_start(portid);
		if (ret < 0)
			rte_exit(EXIT_FAILURE, "rte_eth_dev_start:err=%d, port=%u\n",
				  ret, (unsigned) portid);

		printf("done: \n");

		rte_eth_promiscuous_enable(portid);

		printf("Port %u, MAC address: %02X:%02X:%02X:%02X:%02X:%02X\n\n",
				(unsigned) portid,
				l2fwd_ports_eth_addr[portid].addr_bytes[0],
				l2fwd_ports_eth_addr[portid].addr_bytes[1],
				l2fwd_ports_eth_addr[portid].addr_bytes[2],
				l2fwd_ports_eth_addr[portid].addr_bytes[3],
				l2fwd_ports_eth_addr[portid].addr_bytes[4],
				l2fwd_ports_eth_addr[portid].addr_bytes[5]);

		/* initialize port stats */
		memset(&port_statistics, 0, sizeof(port_statistics));
	}

	if (!nb_ports_available) {
		rte_exit(EXIT_FAILURE,
			"All available ports are disabled. Please set portmask.\n");
	}

	check_all_ports_link_status(nb_ports, l2fwd_enabled_port_mask);

	/* launch per-lcore init on every lcore */
	rte_eal_mp_remote_launch(l2fwd_launch_one_lcore, NULL, CALL_MASTER);
	RTE_LCORE_FOREACH_SLAVE(lcore_id) {
		if (rte_eal_wait_lcore(lcore_id) < 0)
			return -1;
	}

	return 0;
}

static struct dpdk_handler dpdk_if_1;

struct dpdk_handler *init_dpdk_setup(const char *name)
{
        g_dpdk_if = &dpdk_if_1;
	if (name)
            strncpy(g_dpdk_if->name, name, sizeof(g_dpdk_if->name));
        else
            g_dpdk_if->name[0] = '\0';

        const char *myArgs[13] = {"./a.out",
                    "-c", "1", "-n",
                    "1", "--", "-q",
                    "1", "-p", "1", ""};

        char *myArgs2[13];
        int i;
        for (i = 0; i < 11; ++i) {
            printf("copying %dth string [%s]\n", i, myArgs[i]);
            myArgs2[i] = malloc(12);
            strncpy(myArgs2[i], myArgs[i], 12);
        }

        printf("Hello world from DPDK....\n");
        MAIN(11, myArgs2);
        return g_dpdk_if;
}

//#define DPDK_MAIN  1

#if defined(DPDK_MAIN)
int main(__attribute__((unused))int argc, __attribute__((unused))const char *argv[])
{


        const char *myArgs[13] = {"./a.out",
                    "-c", "1", "-n",
                    "1", "--", "-q",
                    "1", "-p", "1", ""};

        char *myArgs2[13];
        int i;
        for (i = 0; i < 11; ++i) {
            printf("copying %dth string [%s]\n", i, myArgs[i]);
            myArgs2[i] = malloc(12);
            strncpy(myArgs2[i], myArgs[i], 12);
        }

        printf("Hello world from DPDK....\n");
        MAIN(11, myArgs2);
	return 0;
}
#endif  // DPDK_MAIN

typedef uint8_t  lcoreid_t;
typedef uint8_t  portid_t;
typedef uint16_t queueid_t;
typedef uint16_t streamid_t;

#if 0
static void __attribute__((unused))
fdir_add_perfect_filter(portid_t port_id, uint16_t soft_id, uint8_t queue_id,
			uint8_t drop, struct rte_fdir_filter *fdir_filter)
{
	int diag;

	if (port_id_is_invalid(port_id))
		return;

	diag = rte_eth_dev_fdir_add_perfect_filter(port_id, fdir_filter,
						   soft_id, queue_id, drop);
	if (diag == 0)
		return;

	printf("rte_eth_dev_fdir_add_perfect_filter for port_id=%d failed "
	       "diag=%d\n", port_id, diag);
}
#endif // 0



int
fdir_add_perfect_filter_wrapper(int queue_id, char *srcIP, int srcPort,
        char *dstIP, int dstPort, int type)
{
    printf("%s:%s: for queue %d filter [srcIP=%s, scrPort=%d, dstIP=%s, dstPort =%d, type =%d]\n",
            __FILE__, __func__,
            queue_id, srcIP, srcPort, dstIP, dstPort, type);

    return 0;
}

int
fdir_add_perfect_filter2_wrapper(int queue_id)
{
    printf("%s:%s: for queue %d filter add\n",
            __FILE__, __func__,
            queue_id);
    return 0;
}

int
fdir_del_perfect_filter_wrapper(int queue_id)
{
    printf("%s:%s: for queue %d filter del\n",
            __FILE__, __func__,
            queue_id);
    return 0;
}

int
fdir_add_flow_filter_wrapper(int queue_id)
{
    printf("%s:%s: for queue %d flow-filter add\n",
            __FILE__, __func__,
            queue_id);
    return 0;
}



int
fdir_del_flow_filter_wrapper(int queue_id)
{
    printf("%s:%s: for queue %d flow-filter del\n",
            __FILE__, __func__,
            queue_id);
    return 0;
}




