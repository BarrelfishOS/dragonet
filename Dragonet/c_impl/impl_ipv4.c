#include "implementation.h"

node_out_t do_pg__RxL3IPv4ValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidReassembly(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidVersion(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidTTL(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidChecksum(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4ValidLocalIP(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL3IPv4Classify(struct state *state, struct input *in)
{
    // P_RxL3IPv4Classify_udp, P_RxL3IPv4Classify_icmp, P_RxL3IPv4Classify_drop
    return 0;
}

node_out_t do_pg__TxL3IPv4AllocateHeader(struct state *state, struct input *in)
{
    // P_TxL3IPv4AllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL3IPv4FillHeader(struct state *state, struct input *in)
{
    // P_TxL3IPv4FillHeader_out
    return 0;
}

node_out_t do_pg__TxL3IPv4Routing(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

