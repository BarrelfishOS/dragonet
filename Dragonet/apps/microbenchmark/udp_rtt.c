#include <stdio.h>
#include <time.h>
#include <helpers.h>
#include <packet_access.h>

//#include <udpproto.h>
//#include <proto_ipv4.h>

static volatile bool received;
static struct timespec end;
static size_t size;
static char templ[1024];
static struct stack_handle *stack;

static void recv_cb(socket_handle_t sh, struct input *in, void *data)
{
    int res;
    pktoff_t len;

    len = in->len - in->attr->offset_l5;
    if (len <= 1024) {
        res = memcmp((uint8_t *) in->data + in->attr->offset_l5, templ,
                len);
    } else {
        res = -1;
    }
    clock_gettime(CLOCK_MONOTONIC, &end);

    if (len != size) {
        printf("invalid payload len\n");
        abort();
    }
    if (res != 0) {
        printf("payload not as expected\n");
        printf("l5off=%d\n", in->attr->offset_l5);
        input_dump(in);
        abort();
    }
    stack_input_free(stack, in);

    received = true;
}

int main(int argc, char *argv[])
{
    struct input *in;
    struct state *state;
    const char *name, *ipstr;
    uint32_t dstIP;
    uint16_t dstPort, srcPort;
    unsigned long i;
    unsigned long n = 10000;
    struct timespec start;
    socket_handle_t sh;

    if (argc == 5 || argc == 6) {
        name = argv[1];
        srcPort = atoi(argv[2]);
        dstPort = atoi(argv[3]);
        ipstr = argv[4];
        if (argc == 6) {
            n = atol(argv[5]);
        }
    } else {
        fprintf(stderr,
                "Usage: bench-udp-rtt app-name srcPort dstPort dstIP [n]\n");
        fprintf(stderr, "  n: Number of iterations with one packet size\n");
        fprintf(stderr, "     (default: 10000)\n");
        return 1;
    }

    if (!ip_from_string(ipstr, &dstIP)) {
        fprintf(stderr, "IP address could not be parsed\n");
        return 1;
    }

    stack = stack_init("dragonet", name);

    sh = socket_create(stack, recv_cb, NULL);
    if (!socket_bind_udp_listen(sh, 0, srcPort)) {
        fprintf(stderr, "socket_bind_udp_listen failed\n");
        return 1;
    }

    memset(templ, 'a', sizeof(templ));

    state = stack_get_state(stack);
    for (size = 64; size <= 1024; size <<= 1) {
        for (i = 0; i < n; i++) {

            clock_gettime(CLOCK_MONOTONIC, &start);
            in = stack_input_alloc(stack);
            pkt_prepend(in, size);
            memset(in->data, 'a', size);

            received = false;
            socket_send_udp(sh, in, state->local_ip, srcPort, dstIP, dstPort);

            while (!received) {
                stack_process_event(stack);
            }

            long double t = (end.tv_sec - start.tv_sec) * 1000000.L;
            t += (end.tv_nsec - start.tv_nsec) / 1000.L;
            printf("%zu %Lf\n", size, t);
        }
    }

    return 0;
}
