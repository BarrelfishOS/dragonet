#include <implementation.h>

#define CONFIG_DPDK_IFNAME  "eth2"
#define CONFIG_LOCAL_MAC    0x64188f211b00ULL   //   "00:1b:21:8f:18:64"
#define CONFIG_LOCAL_IP     0x0a160425          //   "10.22.4.37"

struct dpdk_info;
struct dpdk_info *init_dpdk_setup_and_get_default_queue(char *);
int get_packet_wrapper(struct dpdk_info *dinf, char *pkt_tx, size_t len);
int send_packet_wrapper(struct dpdk_info *dinf, char *pkt_tx, size_t len);


static void dpdk_if_init(struct state *state)
{
    if (state->tap_handler != NULL) {
        printf("DPDK Already intialized\n");
        return;
    }
    state->tap_handler = (struct tap_handler *)
        init_dpdk_setup_and_get_default_queue(CONFIG_DPDK_IFNAME);
}

node_out_t do_pg__TapRxQueue(struct state *state, struct input *in)
{
    if (state->tap_handler == NULL) {
        dpdk_if_init(state);

        state->local_mac = CONFIG_LOCAL_MAC;
        state->local_ip = CONFIG_LOCAL_IP;
        printf("Initialized\n");
    }

    static uint8_t tmpbuf[2048];
    size_t len = get_packet_wrapper((struct dpdk_info *) state->tap_handler,
            (char *) tmpbuf, sizeof(tmpbuf));
    puts("\n\n\n---------------------------------------------------------");
    printf("Got packet! :-D\n");
    input_copy_packet(in, tmpbuf, len);
    return P_Queue_out;
}

node_out_t do_pg__TapTxQueue(struct state *state, struct input *in)
{
    send_packet_wrapper((struct dpdk_info *) state->tap_handler, in->data,
            in->len);
    printf("Sent packet! :-D\n");
    puts("\n\n\n---------------------------------------------------------");
    return 0;
}


