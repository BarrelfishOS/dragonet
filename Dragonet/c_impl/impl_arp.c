#include "implementation.h"

node_out_t do_pg__RxL3ARPValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPClassify(struct state *state, struct input *in)
{
    // P_RxL3ARPClassify_request, P_RxL3ARPClassify_response, P_RxL3ARPClassify_drop
    return 0;
}

node_out_t do_pg__RxL3ARPLocalIPDest(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPValidRequest(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPValidResponse(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPIsPending(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3ARPProcessPendingResponse(struct state *state, struct input *in)
{
    // P_RxL3ARPProcessPendingResponse_true, P_RxL3ARPProcessPendingResponse_false, P_RxL3ARPProcessPendingResponse_drop
    return 0;
}

node_out_t do_pg__TxL3ARPInitiateResponse(struct state *state, struct input *in)
{
    // P_TxL3ARPInitiateResponse_true, P_TxL3ARPInitiateResponse_false, P_TxL3ARPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL3ARPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL3ARPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL3ARPFillHeader(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxL3ARPLookup_(struct state *state, struct input *in)
{
    // P_TxL3ARPLookup__true, P_TxL3ARPLookup__false, P_TxL3ARPLookup__miss
    return 0;
}

node_out_t do_pg__TxL3ARPSendRequest(struct state *state, struct input *in)
{
    // P_TxL3ARPSendRequest_true, P_TxL3ARPSendRequest_false, P_TxL3ARPSendRequest_drop
    return 0;
}

