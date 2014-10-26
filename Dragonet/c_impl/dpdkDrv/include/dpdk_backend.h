#ifndef DPDK_BACKEND_H_
#define DPDK_BACKEND_H_

#include <stdbool.h>
#include <stddef.h>
#include <implementation.h>
#include <packet_access.h>
#include <pipelines.h>

#define IFNAME              "eth7"

#define DPDK_MAX_QUEUES     128

typedef void * dpdk_queue_t;

struct dragonet_dpdk;

// struct holding information about hw queue state
struct dragonet_dpdk_queue {
    struct dragonet_dpdk *dpdk_e10k;    // ptr to device to which queue belongs
    uint8_t qid;                        // qid
    dpdk_queue_t queue;                 // queue handle (assuming its needed by dpdk)
    int qstate;                         // state of queue (unused, free,
    struct input *pkt_holder;           //  Buffer where RX packet will be received.
                                        //      in-use, initialized, etc...)

    // variables maintaining counters, stats, state and other info needed
    //      to RX, TX packets
    int refill_counter_local;
    int tx_event_count;
    int rx_pkts;
    int tx_pkts;
};

// struct holding the device info
struct dragonet_dpdk {
    device_t e10kif; // handle to the card  (dpdk specific ptr)
    struct dragonet_dpdk_queue *queues; // list of queue data-struct
};


// functions which will be used by dpdkDrv for init, RX, TX
//      This indirection is there to avoid adding DPDK specific
//      code
struct dragonet_dpdk *init_dpdk_wrapper_deleteme(char *name, int queues);

// External function prototypes to avoid compile-time warning
//  copied from dpdk-1.7.1/app/dpdkDriver/dpdkData.c
//  This is essentially the interface that is being used between
//          Dragonet and DPDK
void send_packetV2(void *nic_p, int core_id, int port_id, int queue_id,
    char *pkt_tx, size_t len);
size_t get_packet_nonblock(void *nic_p,  int core_id, int port_id,
        int queue_id, char *pkt_out, size_t buf_len);

//int init_dpdk_setupV2(int queues);
//int init_device(int argc, char **argv);

void *init_dpdk_setup_and_get_default_queue2(char *ifAddr, int queues);


// filter management
int set_5tuple_filter(void *e10k_nic, uint32_t dst_ip, uint32_t src_ip,
        uint16_t dst_port, uint16_t src_port, uint16_t protocol, uint16_t mask,
        uint8_t priority, uint8_t queue_id, uint8_t index_value);

/**
 * set flow director filter
 *  NOTE: It is assumed that you are *adding* a *perfect matching filter*.
 *      Other type of filters are supported, but not used here yet.
 *
 * @param protocol
 *      should hold actual protocol field value
 * @return
 *   - <0: error
 *   -  otherwise: success
 */
bool set_fdir_filter( void *nic_p, uint32_t dst_ip, uint32_t src_ip,
        uint16_t dst_port, uint16_t src_port, uint16_t protocol,
        uint16_t mask, uint8_t queue_id, uint16_t soft_id);

#endif // DPDK_BACKEND_H_
