#include <stdio.h>
#include <helpers.h>

#include <udpproto.h>
#include <proto_ipv4.h>

static void recv_sink_cb(socket_handle_t sh, struct input *in, void *data)
{
    printf("dropping\n");
}


int main(int argc, char *argv[])
{
    struct stack_handle *stack;
    socket_handle_t si;
    const char *name;
    uint16_t port;

    if (argc == 1) {
        name = "AppEcho";
        port = 8;
    } else if (argc == 3) {
        name = argv[1];
        port = atoi(argv[2]);
    } else {
        fprintf(stderr, "Usage: bench-sink [app name] [port [port [...]]]\n");
        return 1;
    }

    stack = stack_init("dragonet", name);
    si = socket_create(stack, recv_sink_cb, NULL);
    if (!socket_bind_udp_listen(si, 0, port)) {
        fprintf(stderr, "socket_bind_udp_listen(%d) failed\n", port);
        return 1;
    }

    printf("socket_bind_udp_listen succeeded\n");
    while (1) {
        stack_process_event(stack);
    }
    return 0;
}
