#ifndef DRAGONET_CONFIG_H_
#define DRAGONET_CONFIG_H_

#include <implementation.h>
#include <ethernetproto.h>

#define CONFIG_LOCAL_MAC    0xf86954221b00ULL   // 00:1b:22:54:69:f8
#define CONFIG_LOCAL_IP     0xc0a87b01          // 192.168.123.1

#define DEFAULT_BUFFER_SIZE 2048


struct driver *get_tuntap_driver(void);

#endif // DRAGONET_CONFIG_H_
