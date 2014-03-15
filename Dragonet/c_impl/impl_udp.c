#include <stdlib.h>
#include <string.h>
#include <implementation.h>
#include <inttypes.h>

node_out_t do_pg__RxL4UDPValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4UDPValidLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4UDPValidChecksum(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4UDPPortClassifyType(struct state *state, struct input *in)
{
    // P_RxL4UDPPortClassifyType_static, P_RxL4UDPPortClassifyType_dynamic
    return 0;
}

node_out_t do_pg__RxL4UDPPortClassifyStatic(struct state *state, struct input *in)
{
    // P_RxL4UDPPortClassifyStatic_appDNS, P_RxL4UDPPortClassifyStatic_appEcho, P_RxL4UDPPortClassifyStatic_closedPort
    return 0;
}

node_out_t do_pg__RxL4UDPPortClassifyDynamic(struct state *state, struct input *in)
{
    //
    return 0;
}

node_out_t do_pg__RxL4UDPClosedPortAction(struct state *state, struct input *in)
{
    // P_RxL4UDPClosedPortAction_out
    return 0;
}


node_out_t do_pg__RxTagTxUDPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}








node_out_t do_pg__TxL4UDPInitiateResponse(struct state *state, struct input *in)
{
    // P_TxL4UDPInitiateResponse_out, P_TxL4UDPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL4UDPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL4UDPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL4UDPFillHeader(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}


