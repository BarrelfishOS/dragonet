#ifndef TAP_H_
#define TAP_H_

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

void tap_open(struct tap_handler *tap, char *name);
void tap_up(struct tap_handler *tap);
int tap_set_addr(struct tap_handler *tap, int cmd, const char *addr_str);
void tap_set_ip(struct tap_handler *tap, const char *ip);
void tap_set_mask(struct tap_handler *tap, const char *mask);
ssize_t tap_read(struct tap_handler *tap, char *buff, size_t len);
void tap_write(struct tap_handler *tap, char *buff, size_t len);
struct tap_handler *tap_create(char *name);

#endif
