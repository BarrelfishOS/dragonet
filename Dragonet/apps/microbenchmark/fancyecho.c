#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <pthread.h>
#include <sched.h>

#include <udpproto.h>
#include <proto_ipv4.h>
#include <dragonet/app_lowlevel.h>
#include <implementation.h>

// NOTE: Moved to c_impl/include/implementation.h
//#define SHOW_INTERVAL_STATS  1
//#define INTERVAL_STAT_FREQUENCY     1000

#define IDLE_BEFORE_YIELD 100


struct cfg_udpep {
    uint32_t             l_ip;
    uint32_t             r_ip;
    uint16_t             l_port;
    uint16_t             r_port;
};

struct cfg_endpoint {
    struct cfg_udpep     ep;
    pthread_mutex_t      mutex;
    void                *opaque;
    struct cfg_endpoint *next;
};

struct cfg_socket {
    struct cfg_endpoint *ep;
    void                *opaque;

    struct cfg_socket   *next;
};

struct cfg_appq {
    const char        *label;
    pthread_mutex_t    mutex;
    unsigned int       num_threads;
    struct cfg_socket *sockets;
    void              *opaque;

    struct cfg_appq   *next;
};

struct cfg_thread {
    struct cfg_appq     **aqs;
    size_t              num_aqs;
    pthread_t           thread;
    int                 localtid;
    uint64_t            packetCount;

    struct cfg_thread   *next;
};

static struct cfg_endpoint *ceps = NULL;
static struct cfg_appq *caqs = NULL;
static struct cfg_thread *cthreads = NULL;

static pthread_mutex_t    stack_init_sequencer_mutex;
static int thread_count = 0;

static void print_usage(void)
{
    fprintf(stderr, "fancyecho [-a label [-p port | -f lip:lport/rip:rport]+]+ "
            "[-t [-q label]+]+\n");

    // For one thread listening on one socket
    //  sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0 -p 7 -t -q t0
    // For two threads listening on same socket
    //  sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0 -p 7 -a t1 -p 7 -t -q t0 -t -q t1
}

static void parse_flow(struct cfg_udpep *udp, char *str)
{
    char *sep;
    char *rem;
    char *port;

    // Split local/remote
    sep = index(str, '/');
    if (sep == NULL) {
        goto parse_err;
    }
    rem = sep + 1;
    *sep = 0;

    // Split lip/lport
    sep = index(str, ':');
    if (sep == NULL) {
        goto parse_err;
    }
    port = sep + 1;
    *sep = 0;
    udp->l_port = atoi(port);
    if (!ip_from_string(str, &udp->l_ip)) {
        goto parse_err;
    }

    // Split rip/rport
    sep = index(rem, ':');
    if (sep == NULL) {
        goto parse_err;
    }
    port = sep + 1;
    *sep = 0;
    udp->r_port = atoi(port);
    if (!ip_from_string(rem, &udp->r_ip)) {
        goto parse_err;
    }

    printf("lIP: %"PRIu32", lPort: %"PRIu32", rIP: %"PRIu32", rPort: %"PRIu32",\n",
            udp->l_ip, udp->l_port, udp->r_ip, udp->r_port);
    return;
parse_err:
    fprintf(stderr, "Parse error for flow specification (expect "
            "lip:lport/rip:rport)\n");
    exit(-1);
}

static struct cfg_endpoint *cep_find(struct cfg_udpep *udp)
{
    struct cfg_endpoint *ep = ceps;
    while (ep != NULL) {
        if (!memcmp(&ep->ep, udp, sizeof(*udp))) {
            return ep;
        }
        ep = ep->next;
    }
    return NULL;
}

static void mksocket(struct cfg_appq  *caq, struct cfg_udpep *udp)
{
    struct cfg_socket *cs = malloc(sizeof(*cs));
    struct cfg_endpoint *cep;
    int res;

    cep = cep_find(udp);
    if (cep == NULL) {
        cep = malloc(sizeof(*cep));
        cep->ep = *udp;
        cep->opaque = NULL;
        cep->next = ceps;
        res = pthread_mutex_init(&cep->mutex, NULL);
        if (res != 0) {
            fprintf(stderr, "pttrhead_mutex_init failed: %s\n", strerror(res));
            abort();
        }
        ceps = cep;
    }
    cs->ep = cep;
    cs->next = caq->sockets;
    caq->sockets = cs;

}
static struct cfg_appq *caq_find(const char *label)
{
    struct cfg_appq *caq = caqs;
    while (caq != NULL) {
        if (!strcmp(caq->label, label)) {
            return caq;
        }
        caq = caq->next;
    }
    return NULL;
}



static void parse_params(int argc, char *argv[])
{
    int res;
    struct cfg_appq *caq = NULL;
    struct cfg_udpep udp;
    struct cfg_thread *th = NULL;

    while ((res = getopt(argc, argv, "a:p:f:tq:")) != -1) {
        // Check state
        switch (res) {
            case 'p':
            case 'f':
                if (caq == NULL) {
                    fprintf(stderr, "Error: Port specified before application "
                            "queue\n");
                    exit(-1);
                }
                // FALLTHRU
            case 'a':
                if (th != NULL) {
                    fprintf(stderr, "Error: AppQ/Port/Flow parameter after "
                            "thread parameter\n");
                    exit(-1);
                }
                break;

            case 'q':
                if (th == NULL) {
                    fprintf(stderr, "Error: AppQ reference before thread "
                            "parameter\n");
                    exit(-1);
                }
                break;
        }

        switch (res) {
            case 'a':
                caq = malloc(sizeof(*caq));
                caq->label = strdup(optarg);
                caq->num_threads = 0;
                caq->sockets = NULL;
                caq->opaque = NULL;

                res = pthread_mutex_init(&caq->mutex, NULL);
                if (res != 0) {
                    fprintf(stderr, "pttrhead_mutex_init failed: %s\n", strerror(res));
                    abort();
                }

                caq->next = caqs;
                caqs = caq;
                break;

            case 'p':
                udp.l_ip = 0;
                udp.l_port = atoi(optarg);
                udp.r_ip = 0;
                udp.r_port = 0;
                mksocket(caq, &udp);
                break;

            case 'f':
                parse_flow(&udp, optarg);
                mksocket(caq, &udp);
                break;

            case 't':
                th = malloc(sizeof(*th));
                th->aqs = NULL;
                th->num_aqs = 0;
                th->next = cthreads;
                cthreads = th;
                break;

            case 'q':
                caq = caq_find(optarg);
                if (caq == NULL) {
                    fprintf(stderr, "Invalid appqueue reference: %s\n", optarg);
                    exit(-1);
                }
                caq->num_threads++;
                th->aqs = realloc(th->aqs,
                        (th->num_aqs + 1) * sizeof(*th->aqs));
                th->aqs[th->num_aqs] = caq;
                th->num_aqs++;
                break;

            case '?':
                print_usage();
                exit(-1);
                break;

            default:
                fprintf(stderr, "Unexpected return value from getopt: %d\n",
                        res);
                exit(-1);
        }
    }
    if (caqs == NULL) {
        fprintf(stderr, "No appqueues specified on commandline\n");
        exit(-1);
    }
    if (cthreads == NULL) {
        fprintf(stderr, "No threads specified on commandline\n");
        exit(-1);
    }

    caq = caqs;
    while (caq != NULL) {
        if (caq->sockets == NULL) {
            fprintf(stderr, "Appqueue without sockets: %s\n", caq->label);
            exit(-1);
        }
        if (caq->num_threads == 0) {
            fprintf(stderr, "Appqueue not assigned to any thread: %s\n",
                    caq->label);
            exit(-1);
        }
        caq = caq->next;
    }

    th = cthreads;
    while (th != NULL) {
        if (th->num_aqs == 0) {
            fprintf(stderr, "Thread without appqueues\n");
            exit(-1);
        }
        th = th->next;
    }

}


//#define TX_LOAD_GEN     (1)
#define TX_STR      "ttttttttttttttttttttttttttttttttttttttttttttttttttttttt"

static void handle_event(struct dnal_aq_event *event, struct cfg_thread *th,
                dnal_appq_t stack)
{
    struct input *in;
    struct dnal_net_destination dest;

    assert(event->type == DNAL_AQET_INPACKET);
    in = event->data.inpacket.buffer;

    dest.type = DNAL_NETDSTT_IP4UDP;
    dest.data.ip4udp.ip_local = ipv4_dstIP_rd(in);
    dest.data.ip4udp.ip_remote = ipv4_srcIP_rd(in);
    dest.data.ip4udp.port_local = udp_hdr_dport_read(in);
    dest.data.ip4udp.port_remote = udp_hdr_sport_read(in);

#if SHOW_INTERVAL_STATS
    // Print the stats after every 1K packets.
    if ((th->packetCount % INTERVAL_STAT_FREQUENCY) == 0) {
        printf("TID:%d:%d: Echo-back to dest: %"PRIu64"\n",
           th->localtid, (int)pthread_self(), th->packetCount);
   }
#endif // SHOW_INTERVAL_STATS



    ++th->packetCount;

/*
    printf("TID:%d:%d: Echo-back to dest: %"PRIu16": %"PRIu16", length: %d\n",
            th->localtid, (int)pthread_self(), dest.data.ip4udp.ip_remote,
            dest.data.ip4udp.port_remote,
            (int)in->len);
*/

#if TX_LOAD_GEN
    for(;;) {
        struct input *in_tmp;
        int ret = dnal_aq_buffer_alloc(stack, &in_tmp);
        assert(ret == SYS_ERR_OK);
        assert(in_tmp != NULL);
        int bufsize =  strlen(TX_STR);
        pkt_prepend(in_tmp, bufsize);
        memcpy(in_tmp->data, TX_STR, bufsize);
        dnal_socket_send(event->data.inpacket.socket, in_tmp, &dest);
    }
#endif  // TX_LOAD_GEN

    // usual echo back
    dnal_socket_send(event->data.inpacket.socket, in, &dest);

}



static void *run_thread(void *arg)
{
    struct cfg_thread *th = arg;
    struct cfg_appq *caq;
    struct cfg_socket *cs;
    size_t i;
    dnal_appq_t daq;
    dnal_sockh_t dsh;
    struct dnal_net_destination dnd;
    struct dnal_aq_event event;
    errval_t err;
    bool locked;
    unsigned int idle = 0;

    while (1) {
        pthread_mutex_lock(&stack_init_sequencer_mutex);
        if ( th->localtid == thread_count) {
            printf("Thread with id %d starting\n", th->localtid);
            break;
        }
        // not youre turn, release the lock and wait
        pthread_mutex_unlock(&stack_init_sequencer_mutex);
        sched_yield();
    } // end while: your turn



    // Make sure everything is initialized
    for (i = 0; i < th->num_aqs; i++) {
        caq = th->aqs[i];
        pthread_mutex_lock(&caq->mutex);
        if (caq->opaque == NULL) {
            err_expect_ok(dnal_aq_create("dragonet", caq->label, &daq));
            caq->opaque = daq;

            // Create sockets
            cs = caq->sockets;
            while (cs != NULL) {
                err_expect_ok(dnal_socket_create(daq, &dsh));
                pthread_mutex_lock(&cs->ep->mutex);
                if (cs->ep->opaque == NULL) {
                    dnd.type = DNAL_NETDSTT_IP4UDP;
                    dnd.data.ip4udp.ip_local = cs->ep->ep.l_ip;
                    dnd.data.ip4udp.ip_remote = cs->ep->ep.r_ip;
                    dnd.data.ip4udp.port_local = cs->ep->ep.l_port;
                    dnd.data.ip4udp.port_remote = cs->ep->ep.r_port;
                    err_expect_ok(dnal_socket_bind(dsh, &dnd));
                    cs->ep->opaque = dsh;
                } else {
                    err_expect_ok(dnal_socket_span(cs->ep->opaque, daq, dsh));
                }
                pthread_mutex_unlock(&cs->ep->mutex);
                cs->opaque = dsh;
                cs = cs->next;
            }
        }
        pthread_mutex_unlock(&caq->mutex);
    }

    printf("Thread with id %d is done\n", th->localtid);
    ++thread_count;
    pthread_mutex_unlock(&stack_init_sequencer_mutex);


    while (true) {
        for (i = 0; i < th->num_aqs; i++) {
            caq = th->aqs[i];
            if (caq->num_threads > 1) {
                pthread_mutex_lock(&caq->mutex);
                locked = true;
            } else {
                locked = false;
            }

            err = dnal_aq_poll(caq->opaque, &event);
            if (err == SYS_ERR_OK) {
                // This needs to be inside the critical section, since we'll
                // send out data through the AQ
                // FIXME: NOW: pass dnal_appq_t daq; // dragonet application endpoint
                    // so that it can do allocation for sending out packet
                handle_event(&event, th, caq->opaque);
                idle = 0;
            } else {
                idle++;
            }

            if (locked) {
                pthread_mutex_unlock(&caq->mutex);
            }

            if (idle >= IDLE_BEFORE_YIELD) {
                sched_yield();
                idle = 0;
            }

        }
    }
    return NULL;
}

static void start_threads(void)
{
    int count = 0;
    struct cfg_thread *th = cthreads;
    int res;
    while (th != NULL) {
        th->localtid = count++;
        res = pthread_create(&th->thread, NULL, run_thread, th);
        if (res != 0) {
            fprintf(stderr, "pthread_create failed: %s\n", strerror(res));
            abort();
        }
        th = th->next;
    }

}

static void wait_threads(void)
{
    struct cfg_thread *th = cthreads;
    while (th != NULL) {
        pthread_join(th->thread, NULL);
        th = th->next;
    }
}

int main(int argc, char *argv[])
{

    int res = pthread_mutex_init(&stack_init_sequencer_mutex, NULL);
    if (res != 0) {
        fprintf(stderr, "pttrhead_mutex_init failed: %s\n", strerror(res));
        abort();
    }


    parse_params(argc, argv);
    start_threads();
    wait_threads();
    return 0;
}
