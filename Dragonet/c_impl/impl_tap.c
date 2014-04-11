#include <implementation.h>
#include <packet_access.h>

static void tap_init(struct state *state)
{
    if (state->tap_handler != NULL) {
        printf("TAP Already intialized\n");
        return;
    }
    state->tap_handler = tap_create("dragonet0");
    tap_set_ip(state->tap_handler, "192.168.123.100");
    tap_set_mask(state->tap_handler, "255.255.255.0");
    tap_up(state->tap_handler);
}

node_out_t do_pg__TapRxQueue(struct state *state, struct input *in)
{
    pktoff_t maxlen;
    if (state->tap_handler == NULL) {
        tap_init(state);

        state->local_mac = 0xf86954221b00ULL;
        state->local_ip = 0xc0a87b01;
        printf("Initialized\n");
    }

    pkt_prepend(in, in->space_before);
    ssize_t len = tap_read(state->tap_handler, (char *) in->data, in->len, 500);
    if (len == 0) {
        pkt_prepend(in, -in->len);
        return P_Queue_drop;
    }
    pkt_append(in, -(in->len - len));
    return P_Queue_out;
}

node_out_t do_pg__TapTxQueue(struct state *state, struct input *in)
{
    tap_write(state->tap_handler, in->data, in->len);
    return 0;

}


