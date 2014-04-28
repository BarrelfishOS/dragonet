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

node_out_t do_pg__RxL3IPv6ValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__TxQueueDemux(struct state *state, struct input *in)
{
    // Just use queue 0 for now
    return 0;
}

node_out_t do_pg__RxQueueMux(struct state *state, struct input *in)
{
    return 0;
}


