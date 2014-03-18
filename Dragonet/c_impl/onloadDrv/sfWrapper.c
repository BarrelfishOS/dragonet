#include <implementation.h>

// this one is connected directly
#define IFNAME              "eth5"
#define CONFIG_LOCAL_MAC_sf  0x485107530f00ULL  // "00:0f:53:07:51:48"
#define CONFIG_LOCAL_IP_sf   0x0a160426          // "10.22.4.38"
        // NOTE: Can be generated in python with ``print "%02x%02x%02x%02x" % (10,113,4,71)``

// this one is connected via switch
//#define IFNAME              "eth7"
//#define CONFIG_LOCAL_MAC_sf  0x495107530f00ULL  // "00:0f:53:07:51:49"
//#define CONFIG_LOCAL_IP_sf   0x0a710447         // "10.113.4.71"

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
void *init_onload_wrapper(char *arg)
{
    return ((void *) init_and_alloc_default_queue(IFNAME));
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
    return (send_packet(onload, (char *)data, len));
}

static struct driver onload_driver = {
    .drv_handle = NULL,
    .drv_init = init_onload_wrapper,
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

int main(int argc, char *argv[])
{
    struct driver *drv = NULL;
    drv = get_sf_driver();
    return main_loop(drv);
}

