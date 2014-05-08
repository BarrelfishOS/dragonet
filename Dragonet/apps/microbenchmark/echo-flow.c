#include <stdio.h>
#include <helpers.h>

#include <udpproto.h>
#include <proto_ipv4.h>

static void recv_cb(socket_handle_t sh, struct input *in, void *data)
{
    socket_send_udp(sh, in, ipv4_dstIP_rd(in), udp_hdr_dport_read(in),
            ipv4_srcIP_rd(in), udp_hdr_sport_read(in));
}

int main(int argc, char *argv[])
{
    struct stack_handle *stack;
    socket_handle_t sh;
    const char *name;
    uint32_t s_ip, d_ip;
    uint16_t s_port, d_port;

    if (argc == 6) {
        name = argv[1];
        if (!ip_from_string(argv[2], &s_ip)) {
            fprintf(stderr, "Couldn't parse src ip\n");
            return 1;
        }
        if (!ip_from_string(argv[4], &d_ip)) {
            fprintf(stderr, "Couldn't parse dst ip\n");
            return 1;
        }
        s_port = atoi(argv[3]);
        d_port = atoi(argv[5]);
    } else {
        fprintf(stderr, "Usage: bench-echo-flow app-name"
                " src-ip src-port dst-ip dst-port\n");
        return 1;
    }

    stack = stack_init("dragonet", name);

    sh = socket_create(stack, recv_cb, NULL);
    if (!socket_bind_udp_flow(sh, s_ip, s_port, d_ip, d_port)) {
        fprintf(stderr, "socket_bind_udp_listen failed\n");
        return 1;
    }

    printf("socket_bind_udp_listen succeeded\n");
    while (1) {
        stack_process_event(stack);
    }
    return 0;
}
