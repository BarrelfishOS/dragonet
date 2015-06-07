/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

// Test registering/unregistering flows

#include <stdint.h>
#include <unistd.h>
#include <dragonet/app_lowlevel.h>
//#include <implementation.h> // ip_from_string()

// UDP endpoint
struct dnet_udpep {
    uint32_t loc_ip, rem_ip;
    uint16_t loc_port, rem_port;
};


#define BIND_PORT           6666
#define INITIAL_FLOWS       100
#define TOTAL_FLOWS         200
#define REGISTER_FLOW_DELAY 1000

int main(int argc, char *argv[])
{
    const char stackname[] = "dragonet";
    const char appname[]   = "testflows";
    errval_t err;

    struct dnal_app_queue     *aq;
    struct dnal_socket_handle *sh;
    struct dnal_net_destination ep;

    ep.type                    = DNAL_NETDSTT_IP4UDP;
    ep.data.ip4udp.ip_local    = 0;
    ep.data.ip4udp.ip_remote   = 0;
    ep.data.ip4udp.port_local  = BIND_PORT;
    ep.data.ip4udp.port_remote = 0;

    err = dnal_aq_create(stackname, appname, &aq, 0);
    if (err_is_fail(err)) {
        fprintf(stderr, "dnal_aq_create failed: %s\n", err_str(err));
        abort();
    }

    err = dnal_socket_create(aq, &sh);
    if (err_is_fail(err)) {
        fprintf(stderr, "dnal_socket_create failed: %s\n", err_str(err));
        abort();
    }

    err = dnal_socket_bind(sh, &ep, 0);
    if (err_is_fail(err)) {
        fprintf(stderr, "dnal_socket_bind failed: %s\n", err_str(err));
        abort();
    }

    unsigned next_rport = 100;
    unsigned long dnal_flags = DNAL_FLAGS_MORE;
    for (unsigned i=0; i<TOTAL_FLOWS; i++) {

        if (i == INITIAL_FLOWS) {
            dnal_flags = 0;
            dnal_noop(aq, 0); // SYNC
        }

        if (i >= INITIAL_FLOWS) {
            usleep(REGISTER_FLOW_DELAY);
        }

        struct dnal_net_destination flow;

        // NOTE: e10k CFD filters do not suport wildcards, so we need fully
        // specify the flows
        flow.type                    = DNAL_NETDSTT_IP4UDP;
        flow.data.ip4udp.port_local  = BIND_PORT;
        flow.data.ip4udp.port_remote = next_rport++;
        ip_from_string("10.1.1.1", &flow.data.ip4udp.ip_local);
        ip_from_string("10.1.1.111", &flow.data.ip4udp.ip_remote);

        err = dnal_socket_register_flow(sh, &flow, dnal_flags);
        if (err_is_fail(err)) {
            fprintf(stderr, "dnal_socket_register_flow failed: %s\n", err_str(err));
            abort();
        }
    }

    return 0;
}
