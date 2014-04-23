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
        //printf("got packet! len=%d  l5off=%d\n", in->len, in->attr->offset_l5);

        in->attr->udp_sport = udp_hdr_dport_read(in);
        in->attr->udp_dport = udp_hdr_sport_read(in);
        in->attr->ip4_dst   = ipv4_srcIP_rd(in);
        in->attr->ip4_src   = ipv4_dstIP_rd(in);

        stack_send_udp_packet(in);
    }
    return 0;
}
