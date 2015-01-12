#include <string.h>
#include <stdio.h>
#include <unistd.h>

#include <implementation.h>
#include <assert.h>
#include "config.h"

pipeline_handle_t pipeline_handle;

void set_pipeline_handle(pipeline_handle_t h)
{
    pipeline_handle = h;
}

/** Allocate/initialize a new input structure including a buffer */
struct input *input_alloc(void)
{
    return input_alloc_plh(pipeline_handle);
}

void input_free(struct input *in)
{
    assert(pipeline_handle != NULL);
    assert(in != NULL);
    input_free_plh(pipeline_handle, in);
}

void declare_dragonet_initialized(char *fname, char *msg)
{
    printf("##################### creating file [%s] ############\n", fname);
   int fid = creat(fname, 0644);
   assert(fid >= 0);
   int ret = write(fid, msg, strlen(msg));
   assert(ret >= 0);
   ret = fsync(fid);
   assert(ret >= 0);
   close(fid);
}



struct pkt_stats debug_pkt_stats;

void show_pkt_stats(struct pkt_stats *s)
{
    assert(s != NULL);
    assert(s == &debug_pkt_stats);
    printf(
            "ETH  : (RX:%-10"PRIu64", TX:%-10"PRIu64", DIFF:%-10"PRId64") "
            "ETHV : (RX:%-10"PRIu64", TX:%-10"PRIu64", DIFF:%-10"PRId64") \n"
            "ARP  : (RX:%-10"PRIu64", TX:%-10"PRIu64", DIFF:%-10"PRId64") "
            "IPV4 : (RX:%-10"PRIu64", TX:%-10"PRIu64", DIFF:%-10"PRId64") \n"
            "ICMP : (RX:%-10"PRIu64", TX:%-10"PRIu64", DIFF:%-10"PRId64") "
            "ICMPV: (RX:%-10"PRIu64", TX:%-10"PRIu64", DIFF:%-10"PRId64") \n"
            "DROP : (RX:%-10"PRIu64", TX:%-10"PRIu64", DIFF:%-10"PRId64") \n",
            s->rx_eth,   s->tx_eth,   ((int64_t)s->rx_eth -   (int64_t)s->tx_eth),
            s->rx_ethv,  s->tx_ethv,  ((int64_t)s->rx_ethv -  (int64_t)s->tx_ethv),
            s->rx_arp,   s->tx_arp,   ((int64_t)s->rx_arp -   (int64_t)s->tx_arp),
            s->rx_ipv4,  s->tx_ipv4,  ((int64_t)s->rx_ipv4 -  (int64_t)s->tx_ipv4),
            s->rx_icmp,  s->tx_icmp,  ((int64_t)s->rx_icmp -  (int64_t)s->tx_icmp),
            s->rx_icmpv, s->tx_icmpv, ((int64_t)s->rx_icmpv - (int64_t)s->tx_icmpv),
            s->rx_drop,  s->tx_drop,  ((int64_t)s->rx_drop -  (int64_t)s->tx_drop)
            );
}

