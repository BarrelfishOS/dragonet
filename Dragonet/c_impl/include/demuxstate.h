#ifndef DEMUX_STATE_H_
#define DEMUX_STATE_H_

#include <stdint.h>
#include <stddef.h>
#include <uthash.h>

#define FIELDSZ(t,f) (sizeof(((t*) 0)->f))

#define UDP_LISTEN_KEYLEN (FIELDSZ(struct udp_listen_entry, port))
struct udp_listen_entry {
    UT_hash_handle hh;
    uint64_t socketid;
    uint64_t appid;

    // Key
    uint16_t port;
};

#define UDP_FLOW_KEYLEN (offsetof(struct udp_flow_entry, d_port) + \
                         FIELDSZ(struct udp_flow_entry, d_port) - \
                         offsetof(struct udp_flow_entry, s_ip))
struct udp_flow_entry {
    UT_hash_handle hh;
    uint64_t socketid;
    uint64_t appid;

    // The following elements are the key
    uint32_t s_ip;
    uint32_t d_ip;
    uint16_t s_port;
    uint16_t d_port;
};


#endif // ndef DEMUX_STATE_H_

