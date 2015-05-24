#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <pthread.h>
#include <assert.h>


//#define DEBUG_ECHOSRV 1

static bool serialze_socket_access = false; // should socket access should be serialzed?

static void die(const char *msg)
{
    perror(msg);
    exit(EXIT_FAILURE);
}

struct cfg_udpep {
    uint32_t             l_ip;
    uint32_t             r_ip;
    uint16_t             l_port;
    uint16_t             r_port;
    struct sockaddr_in   si;
    struct sockaddr_in   si_r;
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
    int                 sockfd;

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

#define MAX_SOCKS       127
struct cfg_thread {
    struct cfg_appq     **aqs;
    size_t              num_aqs;
    pthread_t           thread;
    int                 local_tid;
    uint64_t            packet_count;
    struct cfg_socket   *socket_list[MAX_SOCKS];
    int                 sock_count;
    int                 highest_fd;
    fd_set              sset;
    struct cfg_thread   *next;
};

static struct cfg_endpoint *ceps = NULL;
static struct cfg_appq *caqs = NULL;
static struct cfg_thread *cthreads = NULL;


static void print_usage(void)
{
    fprintf(stderr, "fancyecho [-a label [-p port | -f lip:lport/rip:rport]+]+ "
            "[-t [-q label]+]+\n");

    // For one thread listening on one socket
    //  sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0 -p 7 -t -q t0
    // For two threads listening on same socket
    //  sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0 -p 7 -a t1 -p 7 -t -q t0 -t -q t1
}

bool ip_from_string(const char *ip, uint32_t *dst)
{
    if (inet_pton(AF_INET, ip, dst) != 1) {
        return false;
    }
    *dst = __builtin_bswap32(*dst);
    return true;
}

static void parse_flow(struct cfg_udpep *udp, char *str)
{
    char *sep;
    char *rem;
    char *port;

    udp->si.sin_family = AF_INET;
    udp->si_r.sin_family = AF_INET;

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
    if (inet_pton(AF_INET, str, &udp->si.sin_addr) != 1) {
        goto parse_err;
    }
    if (!ip_from_string(str, &udp->l_ip)) {
        goto parse_err;
    }
    udp->si.sin_port = htons(udp->l_port);


    // Split rip/rport
    sep = index(rem, ':');
    if (sep == NULL) {
        goto parse_err;
    }
    port = sep + 1;
    *sep = 0;
    udp->r_port = atoi(port);
    if (inet_pton(AF_INET, rem, &udp->si_r.sin_addr) != 1) {
        goto parse_err;
    }
    if (!ip_from_string(rem, &udp->r_ip)) {
        goto parse_err;
    }
    udp->si_r.sin_port = htons(udp->r_port);

    printf("lIP: %"PRIu32", lPort: %"PRIu32", rIP: %"PRIu32", rPort: %"PRIu32",\n",
            udp->l_ip, udp->l_port, udp->r_ip, udp->r_port);

    // FIXME: add the in_addr as well.
    return;
parse_err:
    fprintf(stderr, "Parse error for flow specification (expect "
            "lip:lport/rip:rport)\n");
    exit(EXIT_FAILURE);
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
    struct cfg_socket *cs = calloc(sizeof(*cs), 1);
    struct cfg_endpoint *cep;
    int res;

    assert(cs != NULL);
    cep = cep_find(udp);

    if (cep == NULL) {
        cep = calloc(sizeof(*cep), 1);
        assert(cep != NULL);
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
                    exit(EXIT_FAILURE);
                }
                // FALLTHRU
            case 'a':
                if (th != NULL) {
                    fprintf(stderr, "Error: AppQ/Port/Flow parameter after "
                            "thread parameter\n");
                    exit(EXIT_FAILURE);
                }
                break;

            case 'q':
                if (th == NULL) {
                    fprintf(stderr, "Error: AppQ reference before thread "
                            "parameter\n");
                    exit(EXIT_FAILURE);
                }
                break;
        }

        switch (res) {
            case 'a':
                caq = calloc(sizeof(*caq), 1);
                assert(caq != NULL);
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
                memset(&udp, 0, sizeof(udp));
                udp.l_ip = 0;
                udp.l_port = atoi(optarg);
                udp.r_ip = 0;
                udp.r_port = 0;
                udp.si.sin_family = AF_INET;
                udp.si.sin_port = htons(udp.l_port);
                mksocket(caq, &udp);
                break;

            case 'f':
                memset(&udp, 0, sizeof(udp));
                parse_flow(&udp, optarg);
                mksocket(caq, &udp);
                break;

            case 't':
                th = calloc(sizeof(*th), 1);
                assert(th != NULL);
                th->aqs = NULL;
                th->num_aqs = 0;
                th->next = cthreads;
                cthreads = th;
                break;

            case 'q':
                caq = caq_find(optarg);
                if (caq == NULL) {
                    fprintf(stderr, "Invalid appqueue reference: %s\n", optarg);
                    exit(EXIT_FAILURE);
                }
                caq->num_threads++;
                th->aqs = realloc(th->aqs,
                        (th->num_aqs + 1) * sizeof(*th->aqs));
                th->aqs[th->num_aqs] = caq;
                th->num_aqs++;
                if (th->num_aqs != 1) {
                    printf("Warning:Thread is handling more than 1 (%zu) application slots\n",
                            th->num_aqs);
                }
                break;

            case '?':
                print_usage();
                exit(EXIT_FAILURE);
                break;

            default:
                fprintf(stderr, "Unexpected return value from getopt: %d\n",
                        res);
                exit(EXIT_FAILURE);
        }
    }
    if (caqs == NULL) {
        fprintf(stderr, "No appqueues specified on commandline\n");
        exit(EXIT_FAILURE);
    }
    if (cthreads == NULL) {
        fprintf(stderr, "No threads specified on commandline\n");
        exit(EXIT_FAILURE);
    }

    caq = caqs;
    while (caq != NULL) {
        if (caq->sockets == NULL) {
            fprintf(stderr, "Appqueue without sockets: %s\n", caq->label);
            exit(EXIT_FAILURE);
        }
        if (caq->num_threads == 0) {
            fprintf(stderr, "Appqueue not assigned to any thread: %s\n",
                    caq->label);
            exit(EXIT_FAILURE);
        }
        caq = caq->next;
    }

    th = cthreads;
    while (th != NULL) {
        if (th->num_aqs == 0) {
            fprintf(stderr, "Thread without appqueues\n");
            exit(EXIT_FAILURE);
        }
        th = th->next;
    }

}

static void handle_event(struct cfg_thread *th, int sock_index)
{
    int s;
    struct sockaddr_in si_client;
    ssize_t len;
    socklen_t slen;
    char buf[2047];
    struct cfg_socket *sptr;

    // lookup the socket from sock_index which will be used for receiving packet
    sptr = th->socket_list[sock_index];
    s = sptr->sockfd;

    // receive packet
    slen = sizeof(si_client);
    if ((len = recvfrom(s, buf, sizeof(buf), 0,
                    (struct sockaddr *)&si_client, &slen)) < 0) {
        die("recvfrom() failed");
    }
    ++th->packet_count;

#ifdef DEBUG_ECHOSRV
    // Following is for debugging
    if ((th->packet_count % 1000) == 0) {
        char addr_str[127];

/*        printf("TID:%d:%d: lport:%"PRIu16", echo-request [%"PRIu64"] received, size %zu \n",
           th->local_tid, (int)pthread_self(),
           sptr->ep->ep.l_port,
           th->packet_count, len);
*/

        if (inet_ntop(AF_INET, &si_client.sin_addr, addr_str, sizeof(addr_str)) == NULL) {
            strncpy(addr_str, "PARSING.FAILED", sizeof(addr_str));
        }

        printf("TID:%d:%d: lport:%"PRIu16", echo-request [%"PRIu64"] received "
                "from dest: %s: %"PRIu16", length: %zu\n",
                th->local_tid, (int)pthread_self(),
                sptr->ep->ep.l_port, th->packet_count,
                addr_str, ntohs(si_client.sin_port), len);

    } // end if: print stats info

#endif // DEBUG_ECHOSRV

    // send packet
    if (sendto(s, buf, len, 0, (struct sockaddr *) &si_client, slen) == -1) {
        die("sendto() failed");
    }

} // end function: handle_event

static void *run_thread(void *arg)
{
    struct cfg_thread *th = arg;
    struct cfg_appq *caq;
    struct cfg_socket *cs;
    size_t i;
    int dsh;
    bool locked;

    th->sock_count = 0;
    th->highest_fd = 0;
    FD_ZERO(&th->sset);

    // Make sure everything is initialized
    for (i = 0; i < th->num_aqs; i++) {
        caq = th->aqs[i];
        pthread_mutex_lock(&caq->mutex);
        if (caq->opaque == NULL) {
            caq->opaque = (void *) 1;

            // Create sockets
            cs = caq->sockets;
            while (cs != NULL) {

                if ((dsh = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
                    die("socket() failed");
                }

                pthread_mutex_lock(&cs->ep->mutex);
                if (cs->ep->opaque == NULL) {

                    // Makeing sure that this is not a full flow thingy and just normal listen
                    // FIXME: eventually I would like to remove this
                    assert(cs->ep->ep.r_ip == 0);
                    assert(cs->ep->ep.r_port == 0);

                    if (bind(dsh, (struct sockaddr *) &cs->ep->ep.si,
                                sizeof(cs->ep->ep.si)) == -1) {
                        die("bind() failed");
                    }

                    cs->ep->opaque = (void *)(intptr_t)dsh;
                } else {
                    int orig_sockid = (int)(intptr_t)cs->ep->opaque;
                    // socket is already bound, so using dup2 to make a copy
                    if (dup2(orig_sockid, dsh) == -1) {
                        die("dup2() failed");
                    }
                }

                int optval = 0xfff;
                if (setsockopt(dsh, SOL_SOCKET, SO_NO_CHECK,
                            (void*)&optval, sizeof(optval)) == -1) {
                    perror("setsockopt SO_UDPCHECKSUM_IN: ");
                    printf("ERROR SETTING SO_NO_CHECK");
                }
                pthread_mutex_unlock(&cs->ep->mutex);
                cs->opaque = (void *)(intptr_t)dsh;
                cs->sockfd = dsh;

                assert(th->sock_count < MAX_SOCKS);
                th->socket_list[th->sock_count] = cs;

                FD_SET(dsh, &th->sset);

                if (dsh > th->highest_fd) {
                    th->highest_fd = dsh;
                }

                th->sock_count++;
                cs = cs->next;
            } // end while: for every socket
        } // end if: opaque == NULL
        pthread_mutex_unlock(&caq->mutex);
    } // for all threads/application queues

    printf("TID:%d:%d: Thread ready!\n", th->local_tid, (int)pthread_self());

    while (true) {
        for (i = 0; i < th->num_aqs; i++) {
            caq = th->aqs[i];
            //if (caq->num_threads > 1) {
            if ((caq->num_threads > 1) && (serialze_socket_access)) {
                // FIXME: should I explicitly lock it?
                pthread_mutex_lock(&caq->mutex);
                locked = true;
            } else {
                locked = false;
            }

            if (th->sock_count == 1) {
                // FIXME: direclty call recvfrom

                handle_event(th, 0);


            } else {
                // FIXME: call select and process accordingly
                printf("FIXME: support for select call is not yet added\n");
                exit(EXIT_FAILURE);
            }

            if (locked) {
                pthread_mutex_unlock(&caq->mutex);
            }

        }
    } // end while: infinite loop
    return NULL;
} // end function: run_thread

static void start_threads(void)
{
    int count = 0;
    struct cfg_thread *th = cthreads;
    int res;
    while (th != NULL) {
        th->local_tid = count++;
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
    parse_params(argc, argv);
    start_threads();
    wait_threads();
    return 0;
}
