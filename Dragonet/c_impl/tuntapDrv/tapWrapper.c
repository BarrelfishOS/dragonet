// #############################################################
// FIXME: following this the direct copy-paste from dragonet/Dragonet/Util/tap.c
//          we need better way to reuse the code and avoid this ugly copy.
#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <linux/if.h>
#include <linux/if_tun.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <err.h>

#include <implementation.h>

#define TUNDEV "/dev/net/tun"

struct tap_handler {
	int tun_fd;
	int ctl_fd;
	char name[IFNAMSIZ];
};

static void
tap_open(struct tap_handler *tap, char *name)
{
	struct ifreq ifr = {{{0}}};

	if (name)
		strncpy(tap->name, name, sizeof(tap->name));
	strncpy(ifr.ifr_name, tap->name, sizeof(ifr.ifr_name));
	ifr.ifr_flags = IFF_TAP | IFF_NO_PI;

	tap->tun_fd = open(TUNDEV, O_RDWR);
	if (tap->tun_fd < 0)
		err(1, TUNDEV);

	if (ioctl(tap->tun_fd, TUNSETIFF, &ifr) < 0)
		err(1, "TUNSETIFF");

	if ((tap->ctl_fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_IP)) < 0)
		err(1, "socket");
}

static void
tap_up(struct tap_handler *tap)
{
	struct ifreq ifr = {{{0}}};

	strncpy(ifr.ifr_name, tap->name, sizeof(ifr.ifr_name));
	if (ioctl(tap->ctl_fd, SIOCGIFFLAGS, &ifr) < 0)
		err(1, "ioctl: SIOCGIFFLAGS");

	ifr.ifr_flags |= IFF_UP | IFF_RUNNING;

	if (ioctl(tap->ctl_fd, SIOCSIFFLAGS, &ifr) < 0)
		err(1, "ioctl: SIOCSIFFLAGS");
}

static int
tap_set_addr(struct tap_handler *tap, int cmd, const char *addr_str)
{
	struct ifreq ifr = {{{0}}};
	struct sockaddr_in addr;

	strncpy(ifr.ifr_name, tap->name, sizeof(ifr.ifr_name));

	addr.sin_family = AF_INET;
	if (!inet_aton(addr_str, &addr.sin_addr))
		err(1, "inet_aton: %s\n", addr_str);
	memcpy(&ifr.ifr_addr, &addr, sizeof(addr));

	return ioctl(tap->ctl_fd, cmd, &ifr);
}

static void
tap_set_ip(struct tap_handler *tap, const char *ip)
{
	if (tap_set_addr(tap, SIOCSIFADDR, ip) < 0)
		err(1, "SIOCGIFFLAGS: %s", ip);
}

void
tap_set_mask(struct tap_handler *tap, const char *mask)
{
	if (tap_set_addr(tap, SIOCSIFNETMASK, mask) < 0)
		err(1, "SIOCSIFNETMASK: %s", mask);
}

static ssize_t
tap_read(struct tap_handler *tap, uint8_t *buff, pktoff_t len)
{
	ssize_t ret;

	/*
	struct tun_pi *pi;
	pi = (struct tun_pi *)buff;
	pi->flags = 0;
	pi->proto = 666;
	*/

	// we will want to handle some (e.g., EAGAIN), but for now just die
	if ((ret = read(tap->tun_fd, buff, (len))) < 0)
		err(1, "read failed");
	else if (ret == 0)    // ditto for EOF
		err(1, "read returned 0");

	return ret;
}

static int
tap_write(struct tap_handler *tap, uint8_t *buff, size_t len)
{
	ssize_t ret;

	if ((ret = write(tap->tun_fd, buff, len)) < 0) {
		err(1, "write failed");
                return ret;
        }
	else if (ret < len) {
		err(1, "short write");
                return -1;
        }
        return ret;
}

struct tap_handler *
tap_create(char *name)
{
	struct tap_handler *tap;

	tap = malloc(sizeof(*tap));
	if (!tap)
		err(1, "malloc");

	tap_open(tap, name);
	return tap;
}

static
struct tap_handler *init_tap_network(char *ifname)
{
    char *ipaddr = "192.168.123.100";
    char *ipmask = "255.255.255.0";
    struct tap_handler *tap = NULL;
    tap = tap_create(ifname);
    tap_up(tap);
    tap_set_ip(tap, ipaddr);
    tap_set_mask(tap, ipmask);
    return tap;
}

#define CONFIG_LOCAL_MAC_TUNTAP    0xf86954221b00ULL   // 00:1b:22:54:69:f8
#define CONFIG_LOCAL_IP_TUNTAP    0xc0a87b01          // 192.168.123.1

static
uint64_t tuntap_mac_read(device_t ttd) {
    return (CONFIG_LOCAL_MAC_TUNTAP);
}

static
uint32_t tuntap_ip_read(device_t ttd) {
    return (CONFIG_LOCAL_IP_TUNTAP);
}

static
void *init_tap_wrapper(char *arg)
{
    return ((void *)init_tap_network("dragonet0"));
}

static
pktoff_t tap_rx_wrapper(device_t dev, uint8_t *data, pktoff_t len)
{
    struct tap_handler *tap = (struct tap_handler *)dev;
    return ((pktoff_t)tap_read(tap, data, len));
}

static
int tap_tx_wrapper(device_t dev, uint8_t *data, pktoff_t len)
{
    struct tap_handler *tap = (struct tap_handler *)dev;
    return (tap_write(tap, data, len));
}

static struct driver tap_driver = {
    .drv_handle = NULL,
    .drv_init = init_tap_wrapper,
    .drv_rx = tap_rx_wrapper,
    .drv_tx = tap_tx_wrapper,
    .drv_mac_read = tuntap_mac_read,
    .drv_ip_read = tuntap_ip_read,
};

static
struct driver *get_tuntap_driver(void)
{
    return &tap_driver;
}

int main(int argc, char *argv[])
{
    struct driver *drv = NULL;
    drv = get_tuntap_driver();
    return main_loop(drv);
}
