#include <stdio.h>
#include <time.h>
#include <helpers.h>
#include <packet_access.h>

//#include <udpproto.h>
//#include <proto_ipv4.h>

int main(int argc, char *argv[])
{
    struct input *in;
    struct state *state;
    uint32_t dstIP = 0xc0a87b64; // 192.168.123.100
    uint16_t dstPort = 7;
    pktoff_t len;
    int res;
    size_t size;
    unsigned long i;
    unsigned long n = 8;
    struct timespec start, end;
    char templ[1024];

    stack_init("AppEcho", "Rx_to_AppEcho", "AppEcho_to_Tx");
    // make sure tap device is initialized
    usleep(1000*1000);

    memset(templ, 'a', sizeof(templ));

    state = stack_get_state();
    for (size = 64; size <= 1024; size <<= 1) {
        for (i = 0; i < n; i++) {

            clock_gettime(CLOCK_MONOTONIC, &start);
            in = input_alloc();
            pkt_prepend(in, size);
            memset(in->data, 'a', size);

            in->attr->udp_sport = 7;
            in->attr->udp_dport = dstPort;
            in->attr->ip4_dst   = dstIP;
            in->attr->ip4_src   = state->local_ip;

            stack_send_udp_packet(in);


            in = stack_get_packet();
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
                goto out;
            }
            if (res != 0) {
                printf("payload not as expected\n");
                printf("l5off=%d\n", in->attr->offset_l5);
                input_dump(in);
                goto out;
            }
            input_free(in);

            long double t = (end.tv_sec - start.tv_sec) * 1000000.L;
            t += (end.tv_nsec - start.tv_nsec) / 1000.L;
            printf("%zu %Lf\n", size, t);
        }
    }

out:
    return 0;
}
