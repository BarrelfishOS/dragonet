#include <stdio.h>
#include <helpers.h>

#include <udpproto.h>
#include <proto_ipv4.h>

static void recv_cb(socket_handle_t sh, struct input *in, void *data)
{
    socket_send_udp(sh, in, ipv4_dstIP_rd(in), udp_hdr_dport_read(in),
            ipv4_srcIP_rd(in), udp_hdr_sport_read(in));
}

static void recv_foo_cb(socket_handle_t sh, struct input *in, void *data)
{
    printf("dropping\n");
}


int main(int argc, char *argv[])
{
    struct input *in;
    socket_handle_t sh, si;
    stack_init("AppEcho", "Rx_to_AppEcho", "AppEcho_to_Tx");

    sh = socket_create(recv_cb, NULL);
    si = socket_create(recv_foo_cb, NULL);
    if (!socket_bind_udp_listen(sh, 0, 7)) {
        fprintf(stderr, "socket_bind_udp_listen failed\n");
        return 1;
    }
    if (!socket_bind_udp_listen(si, 0, 8)) {
        fprintf(stderr, "socket_bind_udp_listen(8) failed\n");
        return 1;
    }

    printf("socket_bind_udp_listen succeeded\n");
    while (1) {
        stack_process_event();
    }
    return 0;
}
