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

#include "dpdkControl.h"

#define RTE_LOGTYPE_L2FWD RTE_LOGTYPE_USER1

#define GOTO_FAIL(str, ...) do {					\
		printf("%s FAILED (%s: l.%d): <" str ">\n",		\
		       __func__, __FILE__, __LINE__,  ##__VA_ARGS__);			\
		goto fail;						\
} while(0)


#define MBUF_SIZE (2048 + sizeof(struct rte_mbuf) + RTE_PKTMBUF_HEADROOM)
#define     IFNAMSIZDPDK        1023

//#define MYDEBUG     1
#ifdef MYDEBUG
#define dprint(x...)    printf("dpdkData:" x)
#else
#define dprint(x...)   ((void)0)
#endif // MYDEBUG



typedef uint8_t  lcoreid_t;
typedef uint8_t  portid_t;
typedef uint16_t queueid_t;

struct dpdk_info {
    int core_id;
    int port_id;
    int queue_id;
    void *ptr;
};

size_t get_packetV2(int core_id, int port_id, int queue_id,
        char *pkt_out, size_t buf_len);
void send_packetV2(int core_id, int port_id, int queue_id,
        char *pkt_tx, size_t len);
int init_dpdk_setupV2(void);


// Simplified wrapper functions
struct dpdk_info *init_dpdk_setup_and_get_default_queue2(char *ifAddr);
int send_packet_wrapper(struct dpdk_info *dinf, char *pkt_tx, size_t len);
int get_packet_wrapper(struct dpdk_info *dinf, char *pkt_tx, size_t len);

static struct cmdline *virtual_cl = NULL; // virtual cmdline for Dragonet
//int exec_control_command(const char *cmd);

//  ###################### TO DELETE ########################

int fdir_add_perfect_filter_wrapper_dummy(int queue_id, char *srcIP, int srcPort,
        char *dstIP, int dstPort, int type);
int fdir_add_perfect_filter2_wrapper_dummy(int queue_id);
int fdir_del_perfect_filter_wrapper_dummy(int queue_id);
int fdir_add_flow_filter_wrapper_dummy(int queue_id);
int fdir_del_flow_filter_wrapper_dummy(int queue_id);


void tx_cksum_set_dummy(portid_t port_id, uint8_t cksum_mask);
void fdir_add_signature_filter_dummy(portid_t port_id, uint8_t queue_id,
			       struct rte_fdir_filter *fdir_filter);
void fdir_update_signature_filter_dummy(portid_t port_id, uint8_t queue_id,
				  struct rte_fdir_filter *fdir_filter);
void fdir_remove_signature_filter_dummy(portid_t port_id,
				  struct rte_fdir_filter *fdir_filter);
void fdir_get_infos_dummy(portid_t port_id);
void fdir_add_perfect_filter_dummy(portid_t port_id, uint16_t soft_id,
			     uint8_t queue_id, uint8_t drop,
			     struct rte_fdir_filter *fdir_filter);
void fdir_update_perfect_filter_dummy(portid_t port_id, uint16_t soft_id,
				uint8_t queue_id, uint8_t drop,
				struct rte_fdir_filter *fdir_filter);
void fdir_remove_perfect_filter_dummy(portid_t port_id, uint16_t soft_id,
				struct rte_fdir_filter *fdir_filter);
void fdir_set_masks_dummy(portid_t port_id, struct rte_fdir_masks *fdir_masks);


//  ######################  ########################


void send_packetV2(int core_id, int port_id, int queue_id,
        char *pkt_tx, size_t len)
{
	struct ether_hdr *eth;

        struct rte_mbuf *m = rte_pktmbuf_alloc(fwd_lcores[core_id]->mbp);

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


	eth = rte_pktmbuf_mtod(m, struct ether_hdr *);

	/* src addr */
        // ether_addr_copy(&l2fwd_ports_eth_addr[port_id], &eth->s_addr);
	ether_addr_copy(&ports[port_id].eth_addr, &eth->s_addr);

	struct rte_mbuf *m_table[2];
        m_table[0] = m;
        m_table[1] = NULL;
        uint16_t nb_pkts = 1;
	unsigned ret = rte_eth_tx_burst(port_id, (uint16_t) queue_id,
                m_table, nb_pkts);

	if (unlikely(ret < nb_pkts)) {
                printf("failed to send packet\n");
                assert(ret == nb_pkts); // FIXME: I need better way to handle errors
		do {
			rte_pktmbuf_free(m_table[ret]);
		} while (++ret < nb_pkts);
	}

fail:
	if (m)
		rte_pktmbuf_free(m);
	return;
} // end function: send_packetV2


int send_packet_wrapper(struct dpdk_info *dinf, char *pkt_tx, size_t len)
{
    assert(dinf != NULL);
    send_packetV2(dinf->core_id, dinf->port_id, dinf->queue_id, pkt_tx, len);
    return len;
}

size_t get_packetV2(int core_id, int port_id, int queue_id,
        char *pkt_out, size_t buf_len)
{
    struct rte_mbuf *pkts_burst[2];
    struct rte_mbuf *m;
    unsigned portid, nb_rx;
    size_t pkt_size = 0;

/*
    struct rte_mbuf *pkts_burst2[2];
    unsigned nb_rx_other_q = 0;
*/

    dprint("get_packetV2 on queue_id %d\n", queue_id);
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

} // end function: get_packetV2


int get_packet_wrapper(struct dpdk_info *dinf, char *pkt_tx, size_t len)
{
    assert(dinf != NULL);
    return get_packetV2(dinf->core_id, dinf->port_id, dinf->queue_id,
        pkt_tx, len);
}

#define ARGNOS 15
int init_dpdk_setupV2(void)
{

    if (virtual_cl != NULL) {
        printf("ERROR: Already initialized\n");
        return -1;
    }
    const char *myArgs[ARGNOS] = {"./a.out",
        "-c", "0x18",  // coremask
        "-n", "1",  // no of ports
        "--",
        "--pkt-filter-mode=perfect",
//        "--pkt-filter-mode=signature",
        "--rxq=2",
        "--txq=2",
	"--burst=1",
        "--disable-rss",
        "--disable-hw-vlan",
        "", ""}; // 13 arguments

    char *myArgs2[ARGNOS];
    int i;
    for (i = 0; i < ARGNOS; ++i) {
        printf("copying %dth string [%s]\n", i, myArgs[i]);
        myArgs2[i] = malloc(127);
        if (myArgs[i] == NULL) {
            myArgs2[i] = NULL;
        } else {
            strncpy(myArgs2[i], myArgs[i], 126);
        }
    }

    printf("Hello world from DPDK...., V3\n");
    int ret = init_device(13, myArgs2);
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


struct dpdk_info *init_dpdk_setup_and_get_default_queue2(char *ifAddr)
{
    printf("WARNING: Ignoring %s interface address suggestion\n", ifAddr);
    printf("    going for default hardcoded value\n");
    int ret = init_dpdk_setupV2();
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
    return dev;
}

//  ###################### TO DELETE ########################

typedef uint8_t  lcoreid_t;
typedef uint8_t  portid_t;
typedef uint16_t queueid_t;
typedef uint16_t streamid_t;

static int
port_id_is_invalid_dummy(portid_t port_id)
{
	if (port_id < nb_ports)
		return 0;
	printf("Invalid port %d (must be < nb_ports=%d)\n", port_id, nb_ports);
	return 1;
}


void
fdir_add_signature_filter_dummy(portid_t port_id, uint8_t queue_id,
			  struct rte_fdir_filter *fdir_filter)
{
	int diag;

	if (port_id_is_invalid_dummy(port_id))
		return;

	diag = rte_eth_dev_fdir_add_signature_filter(port_id, fdir_filter,
						     queue_id);
	if (diag == 0)
		return;

	printf("rte_eth_dev_fdir_add_signature_filter for port_id=%d failed "
	       "diag=%d\n", port_id, diag);
}

void
fdir_update_signature_filter_dummy(portid_t port_id, uint8_t queue_id,
			     struct rte_fdir_filter *fdir_filter)
{
	int diag;

	if (port_id_is_invalid_dummy(port_id))
		return;

	diag = rte_eth_dev_fdir_update_signature_filter(port_id, fdir_filter,
							queue_id);
	if (diag == 0)
		return;

	printf("rte_eth_dev_fdir_update_signature_filter for port_id=%d failed "
	       "diag=%d\n", port_id, diag);
}

void
fdir_remove_signature_filter_dummy(portid_t port_id,
			     struct rte_fdir_filter *fdir_filter)
{
	int diag;

	if (port_id_is_invalid_dummy(port_id))
		return;

	diag = rte_eth_dev_fdir_remove_signature_filter(port_id, fdir_filter);
	if (diag == 0)
		return;

	printf("rte_eth_dev_fdir_add_signature_filter for port_id=%d failed "
	       "diag=%d\n", port_id, diag);

}

void
fdir_get_infos_dummy(portid_t port_id)
{
	struct rte_eth_fdir fdir_infos;

	static const char *fdir_stats_border = "########################";

	if (port_id_is_invalid_dummy(port_id))
		return;

	rte_eth_dev_fdir_get_infos(port_id, &fdir_infos);

	printf("\n  %s FDIR infos for port %-2d     %s\n",
	       fdir_stats_border, port_id, fdir_stats_border);

	printf("  collision: %-10"PRIu64"  free:     %"PRIu64"\n"
	       "  maxhash:   %-10"PRIu64"  maxlen:   %"PRIu64"\n"
	       "  add:       %-10"PRIu64"  remove:   %"PRIu64"\n"
	       "  f_add:     %-10"PRIu64"  f_remove: %"PRIu64"\n",
	       (uint64_t)(fdir_infos.collision), (uint64_t)(fdir_infos.free),
	       (uint64_t)(fdir_infos.maxhash), (uint64_t)(fdir_infos.maxlen),
	       fdir_infos.add, fdir_infos.remove,
	       fdir_infos.f_add, fdir_infos.f_remove);
	printf("  %s############################%s\n",
	       fdir_stats_border, fdir_stats_border);
}

void
fdir_add_perfect_filter_dummy(portid_t port_id, uint16_t soft_id, uint8_t queue_id,
			uint8_t drop, struct rte_fdir_filter *fdir_filter)
{
	int diag;

	if (port_id_is_invalid_dummy(port_id))
		return;

	diag = rte_eth_dev_fdir_add_perfect_filter(port_id, fdir_filter,
						   soft_id, queue_id, drop);
	if (diag == 0)
		return;

	printf("rte_eth_dev_fdir_add_perfect_filter for port_id=%d failed "
	       "diag=%d\n", port_id, diag);
}

void
fdir_update_perfect_filter_dummy(portid_t port_id, uint16_t soft_id, uint8_t queue_id,
			   uint8_t drop, struct rte_fdir_filter *fdir_filter)
{
	int diag;

	if (port_id_is_invalid_dummy(port_id))
		return;

	diag = rte_eth_dev_fdir_update_perfect_filter(port_id, fdir_filter,
						      soft_id, queue_id, drop);
	if (diag == 0)
		return;

	printf("rte_eth_dev_fdir_update_perfect_filter for port_id=%d failed "
	       "diag=%d\n", port_id, diag);
}

void
fdir_remove_perfect_filter_dummy(portid_t port_id, uint16_t soft_id,
			   struct rte_fdir_filter *fdir_filter)
{
	int diag;

	if (port_id_is_invalid_dummy(port_id))
		return;

	diag = rte_eth_dev_fdir_remove_perfect_filter(port_id, fdir_filter,
						      soft_id);
	if (diag == 0)
		return;

	printf("rte_eth_dev_fdir_update_perfect_filter for port_id=%d failed "
	       "diag=%d\n", port_id, diag);
}

void
fdir_set_masks_dummy(portid_t port_id, struct rte_fdir_masks *fdir_masks)
{
	int diag;

	if (port_id_is_invalid_dummy(port_id))
		return;

	diag = rte_eth_dev_fdir_set_masks(port_id, fdir_masks);
	if (diag == 0)
		return;

	printf("rte_eth_dev_set_masks_filter for port_id=%d failed "
	       "diag=%d\n", port_id, diag);
}


/*
void set_filter_values()
{

    memset(&fdir_filter, 0, sizeof(struct rte_fdir_filter));

    // PS: Added to parse the IP address................
    cmdline_parse_token_ipaddr_t token;
    char buf[CMDLINE_TEST_BUFSIZE];
    cmdline_ipaddr_t result;

    // clear out everything
    memset(buf, 0, sizeof(buf));
    memset(&result, 0, sizeof(result));
    memset(&token, 0, sizeof(token));
    token.ipaddr_data.flags = CMDLINE_IPADDR_V4;
    ret = cmdline_parse_ipaddr((cmdline_parse_token_hdr_t*)&token,
        ipaddr_valid_strs[i].str, (void*)&result);
    // ####################################################


    if (res->ip_src.family == AF_INET)
        fdir_filter.ip_src.ipv4_addr = res->ip_src.addr.ipv4.s_addr;
    else
        memcpy(&(fdir_filter.ip_src.ipv6_addr),
                &(res->ip_src.addr.ipv6),
                sizeof(struct in6_addr));

    if (res->ip_dst.family == AF_INET)
        fdir_filter.ip_dst.ipv4_addr = res->ip_dst.addr.ipv4.s_addr;
    else
        memcpy(&(fdir_filter.ip_dst.ipv6_addr),
                &(res->ip_dst.addr.ipv6),
                sizeof(struct in6_addr));

    fdir_filter.port_dst = rte_cpu_to_be_16(res->port_dst);
    fdir_filter.port_src = rte_cpu_to_be_16(res->port_src);

    if (!strcmp(res->protocol, "udp"))
        fdir_filter.l4type = RTE_FDIR_L4TYPE_UDP;
    else if (!strcmp(res->protocol, "tcp"))
        fdir_filter.l4type = RTE_FDIR_L4TYPE_TCP;
    else if (!strcmp(res->protocol, "sctp"))
        fdir_filter.l4type = RTE_FDIR_L4TYPE_SCTP;
    else //  default only IP
        fdir_filter.l4type = RTE_FDIR_L4TYPE_NONE;

    if (res->ip_dst.family == AF_INET6)
        fdir_filter.iptype = RTE_FDIR_IPTYPE_IPV6;
    else
        fdir_filter.iptype = RTE_FDIR_IPTYPE_IPV4;

    fdir_filter.vlan_id    = rte_cpu_to_be_16(res->vlan_id);
    fdir_filter.flex_bytes = rte_cpu_to_be_16(res->flexbytes_value);


} // end function: set_filter_values

*/

int fdir_add_perfect_filter_wrapper_dummy(int queue_id, char *srcIP,
        int srcPort, char *dstIP, int dstPort, int type)
{
/*
    printf("%s:%s: for queue %d filter [srcIP=%s, scrPort=%d, dstIP=%s, "
            "dstPort =%d, type =%d]\n",
            __FILE__, __func__,
            queue_id, srcIP, srcPort, dstIP, dstPort, type);
*/
    return 0;
    struct rte_fdir_filter fdir_filter;
    memset(&fdir_filter, 0, sizeof(struct rte_fdir_filter));

    int diag;

    diag = rte_eth_dev_fdir_add_perfect_filter(0, NULL,
            0, queue_id, 0);
    if (diag == 0)
        return 0;
    else {
        printf("%s:%s: failed: for queue %d filter "
                "[srcIP=%s, scrPort=%d, dstIP=%s, dstPort =%d, type =%d]\n",
                __FILE__, __func__,
                queue_id, srcIP, srcPort, dstIP, dstPort, type);

        return -1;
    }
    return 0;
}

int
fdir_add_perfect_filter2_wrapper_dummy(int queue_id)
{
    return fdir_add_perfect_filter_wrapper_dummy(queue_id, NULL, 0, NULL, 0, 0);
}

int
fdir_del_perfect_filter_wrapper_dummy(int queue_id)
{
/*
    printf("%s:%s: for queue %d filter del\n",
            __FILE__, __func__,
            queue_id);
*/

//    const char *cmd = "help ports\r\n";
//    const char *cmd = "add_perfect_filter 0 udp src 0.0.0.0 0 "
//        "dst 0.0.0.0 0 flexbytes 0 vlan 0 queue 0 soft 0\r\n";

/*    printf("%s:%s: for queue %d filter add\n",
            __FILE__, __func__,
            queue_id);
*/

/*
    printf("###############################\n");
    printf("## calling cmdline\n");
    struct cmdline *cl = create_virtual_cmdline();
    printf("\n## executing command [%s] \n", cmd);
    int ret = exec_virtual_cmd(cl, cmd);
    printf("############################### return = %d ####\n", ret);
    */
    return queue_id; // 0; // FIXME: to avoid unused queue_id error.
}


#if 0
// Execute the command sent on the NIC.
// To be used by Dragonet remotely.
// Example commands:
//    const char *cmd = "help ports\r\n";
//    const char *cmd = "add_perfect_filter 0 udp src 0.0.0.0 0 "
//        "dst 0.0.0.0 0 flexbytes 0 vlan 0 queue 0 soft 0\r\n";
int exec_control_command(const char *cmd)
{


    int ret = 0;
    if (virtual_cl == NULL) {
        printf("ERROR: command called before initializing the cmdline\n");
        return -1;
    }

    printf("\n## executing command [%s] \n", cmd);
    ret = exec_virtual_cmd(virtual_cl, cmd);

    if (ret < 0) {
        printf("ERROR: %s: Execution failed for command [%s] (ret val=%d)\n",
                __func__, cmd, ret);
    }

    return ret;
} // end function: exec_control_command
#endif // 0
int
fdir_add_flow_filter_wrapper_dummy(int queue_id)
{
    /*
    printf("%s:%s: for queue %d flow-filter add\n",
            __FILE__, __func__,
            queue_id);
    */
    return queue_id; // 0; // FIXME: to avoid unused queue_id error.
}



int
fdir_del_flow_filter_wrapper_dummy(int queue_id)
{
    printf("%s:%s: for queue %d flow-filter del\n",
            __FILE__, __func__,
            queue_id);
    return 0;
}


//  ###################### ########################
