#include <stdio.h>
#include <helpers.h>

#include <udpproto.h>
#include <proto_ipv4.h>

int main(int argc, char *argv[])
{
    struct input *in;
    stack_init("AppEcho", "Rx_to_AppEcho", "AppEcho_to_Tx");
    while (1) {
        in = stack_get_packet();
        printf("got packet! len=%d  l5off=%d\n", in->len, in->attr->offset_l5);

        portno_t sport = udp_hdr_sport_read(in);
        portno_t dport = udp_hdr_dport_read(in);
        in->attr->udp_sport = dport;
        in->attr->udp_dport = sport;

        stack_send_udp_packet(in);
    }
    return 0;
}
