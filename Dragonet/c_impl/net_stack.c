/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#define EXECUTE_GRAPH
#include <implementation.h>
#include <stddef.h>
#include <stdio.h>
#include <inttypes.h>
#include <packet_access.h>
#include "config.h"

static uint8_t arp_request_rx[] = {
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xd6, 0xc8, 0x7f, 0xdd,
        0xe3, 0xab, 0x8, 0x6, 0x0, 0x1, 0x8, 0x0, 0x6, 0x4, 0x0, 0x1, 0xd6,
        0xc8, 0x7f, 0xdd, 0xe3, 0xab, 0xc0, 0xa8, 0x7b, 0x64, 0x0, 0x0, 0x0,
        0x0, 0x0, 0x0, 0xc0, 0xa8, 0x7b, 0x1
    };

//static  // FIXME: commenting out to avoid warning of unused variable
uint8_t arp_response_tx[] = {
         0xd6, 0xc8, 0x7f, 0xdd, 0xe3,
         0xab, 0x0, 0x1b, 0x22, 0x54, 0x69, 0xf8, 0x8, 0x6, 0x0, 0x1, 0x8, 0x0,
         0x6, 0x4, 0x0, 0x2, 0x0, 0x1b, 0x22, 0x54, 0x69, 0xf8, 0xc0, 0xa8,
         0x7b, 0x1, 0xd6, 0xc8, 0x7f, 0xdd, 0xe3, 0xab, 0xc0, 0xa8, 0x7b, 0x64
    };

static uint8_t pkt_icmp_echo_rx[] = {
         0x0, 0x1b, 0x22, 0x54, 0x69, 0xf8, 0xd6, 0xc8, 0x7f, 0xdd,
         0xe3, 0xab, 0x8, 0x0, 0x45, 0x0, 0x0, 0x54, 0x2b, 0xdd, 0x40, 0x0,
         0x40, 0x1, 0x97, 0x15, 0xc0, 0xa8, 0x7b, 0x64, 0xc0, 0xa8, 0x7b, 0x1,
         0x8, 0x0, 0x28, 0x59, 0xd, 0x94, 0x0, 0x1, 0x9, 0x5a, 0x1f, 0x53, 0x0,
         0x0, 0x0, 0x0, 0xd1, 0x91, 0x9, 0x0, 0x0, 0x0, 0x0, 0x0, 0x10, 0x11,
         0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
         0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29,
         0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35,
         0x36, 0x37
    };

//static  // FIXME: commenting out to avoid warning of unused variable
uint8_t pkt_icmp_echo_response_tx[] = {
         0xd6, 0xc8, 0x7f, 0xdd, 0xe3, 0xab, 0x0, 0x1b, 0x22, 0x54,
         0x69, 0xf8, 0x8, 0x0, 0x45, 0x0, 0x0, 0x54, 0x0, 0x0, 0x40, 0x0, 0x40,
         0x1, 0xc2, 0xf2, 0xc0, 0xa8, 0x7b, 0x1, 0xc0, 0xa8, 0x7b, 0x64, 0x0,
         0x0, 0x30, 0x59, 0xd, 0x94, 0x0, 0x1, 0x9, 0x5a, 0x1f, 0x53, 0x0, 0x0,
         0x0, 0x0, 0xd1, 0x91, 0x9, 0x0, 0x0, 0x0, 0x0, 0x0, 0x10, 0x11, 0x12,
         0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e,
         0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a,
         0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36,
         0x37
    };


static uint8_t pkt_unsupported_proto_rx[] = {
         0x1, 0x0, 0x5e, 0x0, 0x0, 0xfb, 0xd6, 0xc8, 0x7f, 0xdd, 0xe3,
         0xab, 0x8, 0x0, 0x45, 0x0, 0x0, 0x49, 0xa9, 0x40, 0x40, 0x0, 0xff,
         0x11, 0xb5, 0x5a, 0xc0, 0xa8, 0x7b, 0x64, 0xe0, 0x0, 0x0, 0xfb, 0x14,
         0xe9, 0x14, 0xe9, 0x0, 0x35, 0xb7, 0x2e, 0x0, 0x0, 0x0, 0x0, 0x0, 0x2,
         0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x5, 0x5f, 0x69, 0x70, 0x70, 0x73, 0x4,
         0x5f, 0x74, 0x63, 0x70, 0x5, 0x6c, 0x6f, 0x63, 0x61, 0x6c, 0x0, 0x0,
         0xc, 0x0, 0x1, 0x4, 0x5f, 0x69, 0x70, 0x70, 0xc0, 0x12, 0x0, 0xc, 0x0,
         0x1
    };


static void show_hex_dump(void *data, size_t len)
{
    int i = 0;
    printf("[");
    uint8_t *ptr = data;
    for (i = 0; i < len; ++i) {
        printf("0x%x,", ptr[i]);
    }
    printf("]\n");
}

static void run_packet(struct state *st, void *buffer, size_t len)
{
    struct input *in = input_alloc();
    assert(in != NULL);
    input_copy_packet(in, buffer, len);
    executeGraph(st, in);
    input_free(in);
}



struct arp_cache hardcoded_cache = {
    .ip = 0xc0a87b64,
    .mac = 0x1d366fc109a2ULL,
    .next = NULL,
};


void pg_state_init(struct state *st);

int main_loop(struct driver *drv)
{
    struct state st;

    pg_state_init(&st);
    st.driver_handler = drv;

    if (st.driver_handler == NULL
            || st.driver_handler->drv_init == NULL
            || st.driver_handler->drv_rx == NULL
            || st.driver_handler->drv_tx == NULL
            || st.driver_handler->drv_mac_read == NULL
            || st.driver_handler->drv_ip_read == NULL
          ) {
        panic("No driver handler configured, quitting\n");
        return -1;
    }

    st.driver_handler->drv_handle = st.driver_handler->drv_init(NULL);

    if (st.driver_handler->drv_handle == NULL) {
        panic("driver initialization failed, quitting\n");
        return -1;
    }

    st.local_mac = st.driver_handler->drv_mac_read(st.driver_handler->drv_handle);
    st.local_ip = st.driver_handler->drv_ip_read(st.driver_handler->drv_handle);

    uint8_t buf[4096];
    int pktsize;
    uint32_t pktcount = 0;
    // looping with tuntap device
    for (;;) {
        //pktsize = tap_read(tap_dn, buf, sizeof(buf));
        pktsize =  st.driver_handler->drv_rx(st.driver_handler->drv_handle, buf, sizeof(buf));
        if (pktsize < 0) {
            panic("drv_rx failed and returned %d\n", pktsize);
            return -1;
        }
        run_packet(&st, buf, pktsize);
        dprint("\n### handled %"PRIu32"th packet!!!!\n", pktcount);
        ++pktcount;
    }
    return 0;

    // Testing incoming arp request packet
    printf("\nTesting:arp_request_rx\n");
    run_packet(&st, arp_request_rx, sizeof(arp_request_rx));
    printf("\nVerify: Should match with following expected packet\n");
    show_hex_dump(arp_response_tx, sizeof(arp_response_tx));

    // Testing incoming icmp packet
    printf("\nTesting:ICMP packet\n");
    run_packet(&st, pkt_icmp_echo_rx, sizeof(pkt_icmp_echo_rx));
    printf("\nVerify: Should match with following expected packet\n");
    show_hex_dump(pkt_icmp_echo_response_tx, sizeof(pkt_icmp_echo_response_tx));

    // Testing incoming packet for unsupported protocol
    printf("\nTesting incoming packet for unsupported protocol\n");
    run_packet(&st, pkt_unsupported_proto_rx, sizeof(pkt_unsupported_proto_rx));

    return 0;
}

