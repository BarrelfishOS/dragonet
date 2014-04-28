#include <stdio.h>
#include <time.h>
#include <helpers.h>

#include <udpproto.h>
#include <proto_ipv4.h>

static volatile bool received;
static struct timespec end;
static size_t size;
static char templ[1024];

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
    input_free(in);

    received = true;
}

int main(int argc, char *argv[])
{
    struct input *in;
    struct state *state;
    uint32_t dstIP = 0xc0a87b64; // 192.168.123.100
    uint16_t dstPort = 7;
    unsigned long i;
    unsigned long n = 10000;
    struct timespec start;
    socket_handle_t sh;

    stack_init("AppEcho", "Rx_to_AppEcho", "AppEcho_to_Tx");
    // make sure tap device is initialized
    usleep(1000*1000);

    sh = socket_create(recv_cb, NULL);
    if (!socket_bind_udp_listen(sh, 0, 7)) {
        fprintf(stderr, "socket_bind_udp_listen failed\n");
        return 1;
    }

    memset(templ, 'a', sizeof(templ));

    state = stack_get_state();
    for (size = 64; size <= 1024; size <<= 1) {
        for (i = 0; i < n; i++) {

            clock_gettime(CLOCK_MONOTONIC, &start);
            in = input_alloc();
            pkt_prepend(in, size);
            memset(in->data, 'a', size);

            received = false;
            socket_send_udp(sh, in, state->local_ip, 7, dstIP, dstPort);

            while (!received) {
                stack_process_event();
            }

            long double t = (end.tv_sec - start.tv_sec) * 1000000.L;
            t += (end.tv_nsec - start.tv_nsec) / 1000.L;
            printf("%d %Lf\n", (int) size, t);
        }
    }

    return 0;
}
