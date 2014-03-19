#include <implementation.h>


// I need PCI address and not the interface name
//#define IFNAME              "eth2"
#define IFNAME              "eth2"

#define CONFIG_LOCAL_MAC_sf    0x64188f211b00ULL  //    "00:1b:21:8f:18:64"
#define CONFIG_LOCAL_IP_sf   0x0a160425          //   "10.22.4.37"
        // NOTE: Can be generated in python with ``print "%02x%02x%02x%02x" % (10,113,4,71)``

struct dpdk_info;
static
uint64_t dpdk_mac_read(device_t ttd) {
    return (CONFIG_LOCAL_MAC_sf);
}

static
uint32_t dpdk_ip_read(device_t ttd) {
    return (CONFIG_LOCAL_IP_sf);
}

static
void *init_dpdk_wrapper(char *arg)
{
    return ((void *) init_dpdk_setup_and_get_default_queue(IFNAME));
}

static
pktoff_t dpdk_rx_wrapper(device_t dev, uint8_t *data, pktoff_t len)
{
    struct dpdk_info *dpdk = (struct dpdk_info *)dev;
    return ((pktoff_t)get_packet_wrapper(dpdk, (char *)data, len));
}

static
int dpdk_tx_wrapper(device_t dev, uint8_t *data, pktoff_t len)
{
    struct dpdk_info *dpdk = (struct dpdk_info *)dev;
    return (send_packet_wrapper(dpdk, (char *)data, len));
}

static struct driver dpdk_driver = {
    .drv_handle = NULL,

    .drv_init = init_dpdk_wrapper,
    .drv_rx = dpdk_rx_wrapper,
    .drv_tx = dpdk_tx_wrapper,

    .drv_mac_read = dpdk_mac_read,
    .drv_ip_read = dpdk_ip_read,
};

static
struct driver *get_dpdk_driver(void)
{
    return &dpdk_driver;
}

int main(int argc, char *argv[])
{
    struct driver *drv = NULL;
    drv = get_dpdk_driver();
    return main_loop(drv);
}

