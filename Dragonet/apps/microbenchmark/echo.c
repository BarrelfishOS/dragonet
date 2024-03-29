/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdio.h>
#include <helpers.h>
#include <inttypes.h>
#include <stdint.h>

#include <udpproto.h>
#include <proto_ipv4.h>

static void recv_cb(socket_handle_t sh, struct input *in, void *data)
{
//    printf("Packet received from %"PRIx32": %"PRIx16" with data %s\n",
//            ipv4_dstIP_rd(in), udp_hdr_dport_read(in), (char *)data);
    socket_send_udp(sh, in, ipv4_dstIP_rd(in), udp_hdr_dport_read(in),
            ipv4_srcIP_rd(in), udp_hdr_sport_read(in));
}

int main(int argc, char *argv[])
{
    struct stack_handle *stack;
    socket_handle_t sh;
    const char *name;
    uint16_t port;

    if (argc == 1) {
        name = "AppEcho";
        port = 7;
    } else if (argc == 3) {
        name = argv[1];
        port = atoi(argv[2]);
    } else {
        fprintf(stderr, "Usage: bench-echo [app-name port]\n");
        return 1;
    }

    stack = stack_init("dragonet", name);
    printf("###################### stack init done!!!!!!\n");

    sh = socket_create(stack, recv_cb, NULL);
    if (!socket_bind_udp_listen(sh, 0, port)) {
        fprintf(stderr, "socket_bind_udp_listen failed\n");
        return 1;
    }

    printf("socket_bind_udp_listen succeeded\n");
    while (1) {
        stack_process_event(stack);
    }
    return 0;
}
