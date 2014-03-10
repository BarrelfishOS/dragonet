#include "implementation.h"

node_out_t do_pg__RxL3ICMPValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ICMPValidChecksum(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ICMPIsTypeRequest(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxL3ICMPInitiateResponse(struct state *state, struct input *in)
{
    // P_TxL3ICMPInitiateResponse_out, P_TxL3ICMPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL3ICMPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL3ICMPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL3ICMPFillHeader(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

