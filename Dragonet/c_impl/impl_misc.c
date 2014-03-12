#include <implementation.h>

node_out_t do_pg__Queue(struct state *state, struct input *in)
{
    // P_Queue_out
    return P_Queue_out;
}

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

node_out_t do_pg__TxDemux(struct state *state, struct input *in)
{
    // P_TxDemux_ICMPIR, P_TxDemux_ARPLu, P_TxDemux_ARPIR, P_TxDemux_drop
    switch (in->mux_id) {
        case ATTR_MUX_ARPIR:    return P_TxDemux_ARPIR;
        case ATTR_MUX_ARPLU:    return P_TxDemux_ARPLu;
        case ATTR_MUX_ICMPIR:   return P_TxDemux_ICMPIR;
        default:                return P_TxDemux_drop;
    }
}

node_out_t do_pg__TxQueue(struct state *state, struct input *in)
{
    // 
    printf("Send!\n");
    return 0;
}

