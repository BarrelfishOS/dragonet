#include <implementation.h>
#include <udpproto.h>

/*node_out_t do_pg__Queue(struct state *state, struct input *in)
{
    // P_Queue_out
    return P_Queue_out;
}*/

node_out_t do_pg__PacketDrop(struct state *state, struct input *in)
{
    //
    dprint("PacketDrop!\n");
    return 0;
}

node_out_t do_pg__NotSupported(struct state *state, struct input *in)
{
    //
    dprint("NotSupported!\n");
    return 0;
}

node_out_t do_pg__RxTagTxARPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    in->mux_id = ATTR_MUX_ARPIR;
    return P_true;
}

node_out_t do_pg__RxTagTxARPLu(struct state *state, struct input *in)
{
    // P_true, P_false
    in->mux_id = ATTR_MUX_ARPLU;
    return P_true;
}

node_out_t do_pg__RxTagTxICMPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    in->mux_id = ATTR_MUX_ICMPIR;
    return P_true;
}

node_out_t do_pg__RxTagTxUDPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    in->mux_id = ATTR_MUX_UDPIR;
    return P_true;
}



node_out_t do_pg__TxDemux(struct state *state, struct input *in)
{
    // P_TxDemux_ICMPIR, P_TxDemux_ARPLu, P_TxDemux_ARPIR, P_TxDemux_drop
    switch (in->mux_id) {
        case ATTR_MUX_ARPIR:    return P_TxDemux_ARPIR;
        case ATTR_MUX_ARPLU:    return P_TxDemux_ARPLu;
        case ATTR_MUX_ICMPIR:   return P_TxDemux_ICMPIR;
        case ATTR_MUX_UDPIR:    return P_TxDemux_UDPIR;
        default:                return P_TxDemux_drop;
    }
}

node_out_t do_pg__RxL3IPv6ValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}


#include "../lib/Util/tap.h"

static struct tap_handler *tap_handler = NULL;

static void tap_init(void)
{
    if (tap_handler != NULL) {
        printf("TAP Already intialized\n");
        return;
    }
    tap_handler = tap_create("dragonet0");
    tap_set_ip(tap_handler, "192.168.123.100");
    tap_set_mask(tap_handler, "255.255.255.0");
    tap_up(tap_handler);
}

node_out_t do_pg__TapRxQueue(struct state *state, struct input *in)
{
    if (tap_handler == NULL) {
        tap_init();

        state->local_mac = 0xf86954221b00ULL;
        state->local_ip = 0xc0a87b01;
    }

    static uint8_t tmpbuf[2048];
    size_t len = tap_read(tap_handler, (char *) tmpbuf, sizeof(tmpbuf));
    input_copy_packet(in, tmpbuf, len);
    return P_Queue_out;
}

node_out_t do_pg__TapTxQueue(struct state *state, struct input *in)
{
    tap_write(tap_handler, in->data, in->len);
    return 0;

}


