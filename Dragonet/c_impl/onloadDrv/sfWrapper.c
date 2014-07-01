#include <implementation.h>
#include <packet_access.h>

#if 0
// this one is connected directly
//#define IFNAME              "eth7"
//#define CONFIG_LOCAL_MAC_sf  0x485107530f00ULL  // "00:0f:53:07:51:48"
//#define CONFIG_LOCAL_IP_sf   0x0a160426          // "10.22.4.38"
        // NOTE: Can be generated in python with ``print "%02x%02x%02x%02x" % (10,113,4,71)``

// FIXME: this one is connected directly
//#define IFNAME              "eth5"
//#define CONFIG_LOCAL_MAC_sf  0x654d07530f00ULL  //  "00:0f:53:07:4d:65"
//#define CONFIG_LOCAL_IP_sf   0x0a170415         // "10.23.4.21"
        // NOTE: Can be generated in python with ``print "%02x%02x%02x%02x" % (10,113,4,71)``


// this one is connected via switch
//#define IFNAME              "eth7"
//#define CONFIG_LOCAL_MAC_sf  0x495107530f00ULL  // "00:0f:53:07:51:49"
//#define CONFIG_LOCAL_IP_sf   0x0a710447         // "10.113.4.71"

// this one is connected via switch on asiago
//#define IFNAME              "p801p1"
//#define CONFIG_LOCAL_MAC_sf  0x644d07530f00ULL  // "00:0f:53:07:4d:64"
//#define CONFIG_LOCAL_IP_sf   0x0a7104c3         // "10.113.4.195"

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
    return (send_packet(onload, (char *)data, len));
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

        declare_dragonet_initialized(DN_READY_FNAME, "sf driver started!\n");
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

#endif // 0
