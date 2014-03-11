#ifndef DRAGONET_CONFIG_H_
#define DRAGONET_CONFIG_H_

#include <implementation.h>
#include <ethernetproto.h>

static inline mac_t get_local_mac(struct state *state)
{
    uint64_t macaddr = 0x001b225469f80000;
    return macaddr;
};

//cfgLocalMAC = fromJust $ ETH.macFromString "00:1b:22:54:69:f8"
//cfgLocalIP = fromJust $ IP4.ipFromString "192.168.123.1"


#endif // DRAGONET_CONFIG_H_
