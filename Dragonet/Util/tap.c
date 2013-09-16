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

#define TUNDEV "/dev/net/tun"

struct tap_handler {
	int tun_fd;
	int ctl_fd;
	char name[IFNAMSIZ];
};

void
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

void
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

int
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

void
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

ssize_t
tap_read(struct tap_handler *tap, char *buff, size_t len)
{
	ssize_t ret;

	/*
	struct tun_pi *pi;
	pi = (struct tun_pi *)buff;
	pi->flags = 0;
	pi->proto = 666;
	*/

	// we will want to handle some (e.g., EAGAIN), but for now just die
	if ((ret = read(tap->tun_fd, buff, len)) < 0)
		err(1, "read failed");
	else if (ret == 0)    // ditto for EOF
		err(1, "read returned 0");

	return ret;
}

void
tap_write(struct tap_handler *tap, char *buff, size_t len)
{
	ssize_t ret;

	if ((ret = write(tap->tun_fd, buff, len)) < 0)
		err(1, "write failed");
	else if (ret < len)
		err(1, "short write");
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

#if defined(TUNTAP_MAIN)
int main(int argc, const char *argv[])
{
	struct tap_handler tap;
	char buf[4096];

	tap_open(&tap, "dragonet");
	tap_set_ip(&tap, "192.168.10.100");
	tap_set_mask(&tap, "255.255.255.0");
	tap_up(&tap);
	for (;;)
		tap_read(&tap, buf, sizeof(buf));
	return 0;
}
#endif
