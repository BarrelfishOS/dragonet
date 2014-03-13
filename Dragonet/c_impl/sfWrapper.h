
#include <implementation.h>

#define CONFIG_LOCAL_MAC_sf    0xf86954221b00ULL   // 00:1b:22:54:69:f8
#define CONFIG_LOCAL_IP_sf    0xc0a87b01          // 192.168.123.1

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
    init_and_alloc_default_queue(char *name)
    return ((void *) init_and_alloc_default_queue("eth7"));
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

struct driver *get_sf_driver(void)
{
    return &onload_driver;
}

