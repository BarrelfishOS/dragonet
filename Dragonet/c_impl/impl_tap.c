#include <implementation.h>

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
    if (state->tap_handler == NULL) {
        tap_init(state);

        state->local_mac = 0xf86954221b00ULL;
        state->local_ip = 0xc0a87b01;
        printf("Initialized\n");
    }

    static uint8_t tmpbuf[2048];
    size_t len = tap_read(state->tap_handler, (char *) tmpbuf, sizeof(tmpbuf));
    /*puts("\n\n\n---------------------------------------------------------");
    printf("Got packet! :-D\n");*/
    input_copy_packet(in, tmpbuf, len);
    return P_Queue_out;
}

node_out_t do_pg__TapTxQueue(struct state *state, struct input *in)
{
    tap_write(state->tap_handler, in->data, in->len);
    return 0;

}


