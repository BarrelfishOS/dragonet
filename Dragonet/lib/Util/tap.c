/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#define _GNU_SOURCE
#include <ctype.h> // isprint
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <linux/if.h>
#include <linux/if_tun.h>
#include <sys/select.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <err.h>

#include "tap.h"


#define TUNDEV "/dev/net/tun"


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
    // printf("IP: %s: %s\n", __FUNCTION__, ip);
    // NB: That's not the stack's IP, that's the linux tap IP
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
tap_read(struct tap_handler *tap, char *buff, size_t len, long timeout)
{
	ssize_t ret;

	/*
	struct tun_pi *pi;
	pi = (struct tun_pi *)buff;
	pi->flags = 0;
	pi->proto = 666;
	*/

    if (timeout != 0) {
        fd_set reads;
        struct timeval tv;

        FD_ZERO(&reads);
        FD_SET(tap->tun_fd, &reads);
        tv.tv_sec = timeout / 1000;
        tv.tv_usec = (timeout % 1000) * 1000;
        ret = select(tap->tun_fd + 1, &reads, NULL, NULL, &tv);
        if (ret == 0) {
            return 0;
        }
    }

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

static void
tap_dump_packet(char *buf, size_t len)
{
    for (int i=0; i<len; i+=16) {
        printf("%06x: ", i);
        int j;
        for (j=0; j<16; j++) 
            if (i+j < len)
                printf("%02x ", buf[i+j]);
            else
                printf("   ");
        printf(" ");
        for (j=0; j<16; j++) 
        if (i+j < len)
            printf("%c", isprint(buf[i+j]) ? buf[i+j] : '.');
        printf("\n");
    }
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
