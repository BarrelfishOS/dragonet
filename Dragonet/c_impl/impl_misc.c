#include "implementation.h"

node_out_t do_pg__Queue(struct state *state, struct input *in)
{
    // P_Queue_out
    return 0;
}

node_out_t do_pg__PacketDrop(struct state *state, struct input *in)
{
    // 
    return 0;
}

node_out_t do_pg__NotSupported(struct state *state, struct input *in)
{
    // 
    return 0;
}

node_out_t do_pg__RxTagTxARPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxTagTxARPLu(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxTagTxICMPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxDemux(struct state *state, struct input *in)
{
    // P_TxDemux_ICMPIR, P_TxDemux_ARPLu, P_TxDemux_ARPIR, P_TxDemux_drop
    return 0;
}

node_out_t do_pg__TxQueue(struct state *state, struct input *in)
{
    // 
    return 0;
}

