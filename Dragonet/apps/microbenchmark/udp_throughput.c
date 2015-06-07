/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <helpers.h>
#include <packet_access.h>

#define PACKET_LEN 1024

static struct stack_handle *stack;
socket_handle_t sh;
static uint32_t dstIP;
static uint16_t dstPort, srcPort;
static struct state *state;
size_t total, win, received;

static inline void send_request(struct input *in)
{
    pkt_prepend(in, PACKET_LEN);
    memset(in->data, 0, PACKET_LEN);
    socket_send_udp(sh, in, state->local_ip, srcPort, dstIP, dstPort);
}

static void recv_cb(socket_handle_t sh, struct input *in, void *data)
{
    pktoff_t len;

    len = in->len - in->attr->offset_l5;
    assert(len == PACKET_LEN);

    received++;
    total++;
    input_clean_attrs(in);
    input_clean_packet(in);
    send_request(in);
}

int main(int argc, char *argv[])
{
    struct timespec start, end;
    uint64_t micros, total_micros;
    char *name, *ipstr;
    struct input *in;

    if (argc != 7) {
        fprintf(stderr, "Usage: %s app-name srcPort dstPort dstIP "
                        "window time\n", argv[0]);
        return 1;
    }

    name = argv[1];
    srcPort = atoi(argv[2]);
    dstPort = atoi(argv[3]);
    ipstr = argv[4];
    win = atoi(argv[5]);
    total_micros = atoi(argv[6]);
    total_micros *= 1000000;

    if (!ip_from_string(ipstr, &dstIP)) {
        fprintf(stderr, "IP address could not be parsed\n");
        return 1;
    }

    stack = stack_init("dragonet", name);
    state = stack_get_state(stack);

    sh = socket_create(stack, recv_cb, NULL);
    if (!socket_bind_udp_listen(sh, 0, srcPort)) {
        fprintf(stderr, "socket_bind_udp_listen failed\n");
        return 1;
    }

    clock_gettime(CLOCK_MONOTONIC, &start);
    end = start;
    micros = 0;
    received = 0;
    // Fill send window
    for (total = 0; total < win; total++) {
        in = stack_input_alloc(stack);
        send_request(in);
    }

    do {
        stack_process_event(stack);
        if (total % 1000 == 0) {
            clock_gettime(CLOCK_MONOTONIC, &end);
            micros = (end.tv_nsec - start.tv_nsec) / 1000;
            micros += (end.tv_sec - start.tv_sec) * 1000000;
        }
    } while (micros < total_micros);

    long double through = received * PACKET_LEN;
    through /= micros / 1000000.L;
    puts("       Time [μs],"
         "    Data [bytes],"
         "Throughput [MB/s]");
    printf("%16"PRId64",%16"PRId64",%17Lf\n",
           micros, received * PACKET_LEN, through / 1024 / 1024);
    return 0;
}
