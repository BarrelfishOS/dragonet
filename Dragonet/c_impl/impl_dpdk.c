
#define _GNU_SOURCE
#include <implementation.h>
#include <dlfcn.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// Gottardo to switch
#if 0
#define CONFIG_DPDK_IFNAME  "eth1"
#define CONFIG_LOCAL_MAC    0x201b8f211b00ULL   //   " 00:1b:21:8f:1b:20 "
#define CONFIG_LOCAL_IP     0x0a6e0431          //   "10.110.4.49"
#endif


// asiago directly to burrata
#define CONFIG_DPDK_IFNAME  "eth3"
#define CONFIG_LOCAL_MAC    0x06459F671E00ULL   //  "00:1E:67:9F:45:06 "
#define CONFIG_LOCAL_IP     0x0a16040b //   "10.22.4.11"


//   // Ziger1 to switch
//#define CONFIG_DPDK_IFNAME  "eth3"
//   //#define CONFIG_LOCAL_MAC    0x65188f211b00ULL   //   "00:1b:21:8f:18:65"
//#define CONFIG_LOCAL_MAC    0x64188f211b00ULL   //   "00:1b:21:8f:18:64"
//    //#define CONFIG_LOCAL_IP     0x0a6e0426          //   "10.110.4.38"
//#define CONFIG_LOCAL_IP     0x0a160427          //   "10.22.4.39"

struct dpdk_info;
/*struct dpdk_info *init_dpdk_setup_and_get_default_queue(char *);
int get_packet_wrapper(struct dpdk_info *dinf, char *pkt_tx, size_t len);
int send_packet_wrapper(struct dpdk_info *dinf, char *pkt_tx, size_t len);*/


static void dpdk_if_init(struct state *state)
{
    if (state->tap_handler != NULL) {
        printf("DPDK Already intialized\n");
        return;
    }

    void *handle = dlopen("libintel_dpdk.so", RTLD_LAZY | RTLD_GLOBAL);
    if (handle == NULL) {
         char *err = dlerror();
        if (err != NULL)
            puts(err);
        abort();
    }

    handle = dlopen("libdpdk_driver.so", RTLD_LAZY | RTLD_GLOBAL);
    if (handle == NULL) {
         char *err = dlerror();
        if (err != NULL)
            puts(err);
        abort();
    }

    struct dpdk_info *(*init_dpdk_setup_and_get_default_queue)(char *);
    init_dpdk_setup_and_get_default_queue = dlsym(handle,
        "init_dpdk_setup_and_get_default_queue");
    if (init_dpdk_setup_and_get_default_queue == NULL) {
        char *err = dlerror();
        if (err != NULL)
            puts(err);
    }

    assert(init_dpdk_setup_and_get_default_queue != NULL);

    state->tap_handler = (struct tap_handler *)
        init_dpdk_setup_and_get_default_queue(CONFIG_DPDK_IFNAME);
}

node_out_t do_pg__DPDKRxQueue(struct state *state, struct input *in)
{
    if (state->tap_handler == NULL) {
        dpdk_if_init(state);

        state->local_mac = CONFIG_LOCAL_MAC;
        state->local_ip = CONFIG_LOCAL_IP;
        printf("Initialized\n");
    }

    static uint8_t tmpbuf[2048];
    int (*get_packet_wrapper)(struct dpdk_info *dinf, char *pkt_tx, size_t len);
    get_packet_wrapper = dlsym(RTLD_DEFAULT, "get_packet_wrapper");

    size_t len = get_packet_wrapper((struct dpdk_info *) state->tap_handler,
            (char *) tmpbuf, sizeof(tmpbuf));
    /*puts("\n\n\n---------------------------------------------------------");
    printf("Got packet! :-D\n");*/
    input_copy_packet(in, tmpbuf, len);
    return P_Queue_out;
}

node_out_t do_pg__DPDKTxQueue(struct state *state, struct input *in)
{
    int (*send_packet_wrapper)(struct dpdk_info *dinf, char *pkt_tx, size_t len);
    send_packet_wrapper = dlsym(RTLD_DEFAULT, "send_packet_wrapper");
    send_packet_wrapper((struct dpdk_info *) state->tap_handler, in->data,
            in->len);
    /*printf("Sent packet! :-D\n");
    puts("---------------------------------------------------------\n\n\n");*/
    return 0;
}


