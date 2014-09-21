#include "dnet_interface.h"
#include <inttypes.h>
#include <arpa/inet.h>
#include <netinet/in.h>
//#include <pthread.h>
#include <assert.h>
#include <errno.h>

#ifdef DRAGONET

#define IDLE_BEFORE_YIELD 100

#define MAX_EPS         256
#define MAX_THREAD_COUNT         256
#define THREAD_NAME_LEN         256         // maximum thread name length


struct cfg_udpep {
    uint32_t             l_ip;
    uint32_t             r_ip;
    uint16_t             l_port;
    uint16_t             r_port;
    struct sockaddr_in   si;
    struct sockaddr_in   si_r;
    int                  primary_thread;
    int                  state;  // is it initilized, etc..
    dnal_sockh_t  orig_socket;
    pthread_mutex_t      ep_lock; // to protect initilization and spawning
};

struct cfg_thread {
    int                 tid; // index in thread_list array
    int                 local_tid; // pthead-thread id for this guy (for debug showing)
    char                tname[THREAD_NAME_LEN];  // name passed for thread from cmdline
    uint64_t            packet_count;       // packets received by this thread
    int                 thread_specific_filters;       // filters registered for this thread
    struct cfg_udpep    *eplist[MAX_EPS];   // pointer to those filters in
    int                 eplist_owner[MAX_EPS]; // flag telling if you are owner of the endpoint
    int                 eplist_idx[MAX_EPS]; // index in client_list array of the particular ep
    int                 thread_done;
    pthread_mutex_t     thread_init_lock; // to protect initilization and spawning
};



// We have a thread list, and every thread gets client list

int use_dragonet_stack = 0;
char *use_dragonet_stack_portmap = NULL;
static struct cfg_udpep *client_list = NULL;
static int filter_count = 0;    // number of filters detected


static struct cfg_thread thread_list[MAX_THREAD_COUNT];
static int thread_blk_count = 0;    // number thread blocks detected
/*
bool ip_from_string(const char *ip, uint32_t *dst)
{
    if (inet_pton(AF_INET, ip, dst) != 1) {
        return false;
    }
    *dst = __builtin_bswap32(*dst);
    return true;
}
*/


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



void *
parse_client_list(char *client_list_str, int thread_count)
{

    printf("Assuming that there are %d threads\n", thread_count);
    int i = 0;
    for (i = 0; client_list_str[i] != '\0'; ++i)  {
        if (client_list_str[i] == ']') {
            ++filter_count;
        }
    }

    printf("Dragonet: %d filters detected\n", filter_count);
    if (filter_count == 0) {
        printf("Dragonet: continuing without parsing\n");
        return client_list;
    }

    client_list = (struct cfg_udpep *)calloc
        ((filter_count + 1), sizeof(struct cfg_udpep));
    printf("before my myAssert\n");
    myAssert(client_list != NULL);
    printf("after my myAssert\n");

// |t1|p[7777]|t2|f[10.113.4.195:7777/10.113.4.20:9000]
//
    struct cfg_udpep *udp = NULL; // current filter
    struct cfg_thread  *ctb = NULL; // current_thread_block
    int fid = 0; // fiter count
    int tid = 0; // thread count
    char *str1, *str2, *token, *subtoken;
    char *saveptr1, *saveptr2;
    int j;

    char f_type, *f_data;

    int first_thread = 1; // flag to manage first thread specially

    // cleanup the struct
    memset(&thread_list[tid], 0, sizeof(struct cfg_thread));
    thread_list[tid].tid = tid;

    udp = &client_list[fid];
    ctb = &thread_list[tid];

    for (j = 1, str1 = client_list_str ;  ; j++, str1 = NULL) {
        token = strtok_r(str1, "]" , &saveptr1);
        if (token == NULL)
            break;
        printf("[tid:%d][fid:%d]: %d: %s\n", tid, fid, j, token);

        myAssert(fid < filter_count);
        if (fid >= filter_count) {
           printf("%s:%s:%d: ERROR: Too many filters (%d) reported, instead of %d\n",
                   __FILE__, __FUNCTION__, __LINE__,
                   fid, filter_count);
           exit(1);
        }
/*
        myAssert(fid < thread_count);
        if (fid >= thread_count) {
           printf("%s:%s:%d: ERROR:  More filters (%d) reported than threads %d\n",
                   __FILE__, __FUNCTION__, __LINE__,
                   fid, thread_count);
           exit(1);
        }
*/

        f_type = '\0';
        f_data = NULL;

        for (str2 = token; ; str2 = NULL) {
            subtoken = strtok_r(str2, "[", &saveptr2);
            if (subtoken == NULL) {
                break;
            }

            printf(" --> %s\n", subtoken);

            if (f_type == '\0') {
                char *tmpptr = &subtoken[0];

                if (strlen(subtoken) > 1) {
                    myAssert(subtoken[0] == 'T');
                    // assuming this is start of new thread block
                    // example: t3.f -> type flow, thread 3
                    // example: t2.p -> type listen, thread 2
                    // example: t12.p -> type listen, thread 12
                    int ii;

                    // ** setup the thread DS

                    // cleanup the struct
                    if (first_thread == 0) {
                        ++tid;
                    }
                    first_thread = 0;

                    printf("Thread block %d detected, parsing [%s]\n", tid,
                            subtoken);
                    ctb = &thread_list[tid];
                    thread_list[tid].tid = tid;

                    for (ii = 0; subtoken[ii] != '.'; ++ii) {
                        myAssert(subtoken[ii] != '\0');
                        thread_list[tid].tname[ii] = subtoken[ii];
                        myAssert(ii < THREAD_NAME_LEN);
                    }
                    thread_list[tid].tname[ii] = '\0';

                    ctb->thread_done = 0;
                    pthread_mutex_init(&ctb->thread_init_lock, NULL);//        = PTHREAD_MUTEX_INITIALIZER;

                    // adjust the pointer for parsing out filter type
                    tmpptr = &subtoken[ii + 1];
                } // end if : if new tid start

                // assuming first char is filter type
                myAssert(strlen(tmpptr) == 1);
                f_type = tmpptr[0];
            } else {
                f_data = strdup(subtoken);
            }
        }
        myAssert(f_type != '\0' && f_data != NULL);

        printf("Parsing [%c-->%s]\n", f_type, f_data);
        switch(f_type) {
            case 'p':
                memset(udp, 0, sizeof(struct cfg_udpep));
                udp->l_ip = 0;
                udp->l_port = atoi(f_data);
                udp->r_ip = 0;
                udp->r_port = 0;
                udp->primary_thread = tid;
                udp->state          = 0;
                pthread_mutex_init(&udp->ep_lock, NULL);//        = PTHREAD_MUTEX_INITIALIZER;
                printf("flowid: %d: Listen port: %"PRIu16" -> to thread %d:%s\n",
                        fid, udp->l_port, tid, ctb->tname);
                ctb->eplist[ctb->thread_specific_filters] = udp;
                ctb->eplist_owner[ctb->thread_specific_filters] = 1;
                ctb->eplist_idx[ctb->thread_specific_filters] = fid;
                ctb->thread_specific_filters++;
                ++fid;

                udp = &client_list[fid];
                break;

            case 'f':
                memset(udp, 0, sizeof(struct cfg_udpep));
                parse_flow(udp, f_data);
                udp->primary_thread = tid;
                udp->state          = 0;
                pthread_mutex_init(&udp->ep_lock, NULL);//        = PTHREAD_MUTEX_INITIALIZER;
                printf("flowid: %d: connect lIP: %"PRIu32", lPort: %"PRIu32","
                        " rIP: %"PRIu32", rPort: %"PRIu32" -> to thread %d:%s\n",
                    fid, udp->l_ip, udp->l_port, udp->r_ip, udp->r_port,
                    tid, ctb->tname);

                ctb->eplist[ctb->thread_specific_filters] = udp;
                ctb->eplist_owner[ctb->thread_specific_filters] = 1;
                ctb->eplist_idx[ctb->thread_specific_filters] = fid;
                ctb->thread_specific_filters++;

                ++fid;
                udp = &client_list[fid];

                break;

            default:
                printf("ERROR: invalid type (%c)i in specifying flow: %s\n",
                        f_type, client_list_str);
                exit(1);
            } // end switch:

    } // end for: tokenizing the client str
    thread_blk_count = tid + 1;
    printf("Total %d thread-blocks found, and system is running with %d threads\n",
            thread_blk_count, thread_count);
    myAssert(thread_count >= thread_blk_count);
    if (thread_count < thread_blk_count)  {
        printf("ERROR: not enough threads to support all threadblocks\n");
        exit(1);
    }
    return client_list;
} // end function: parse_client_list


// Lock to protect dragonet initialization
static pthread_mutex_t dn_init_lock = PTHREAD_MUTEX_INITIALIZER; // dragonet lock
static int threads_initialized_count = 0;


/*
 * Register a callback function which will be called when packet is received
 */
int register_callback_dn(void *dn_state, event_handler_fun_ptr fun, int fd,
        short which, void *arg)
{

    myAssert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;

    // every thread register one
    if (dnt_state->callback_memcached_fn != NULL) {
        printf("dragonet: callback already registerd\n");
        exit(1);
        return -1;
    }
    dnt_state->callback_memcached_fn = fun;
    dnt_state->callback_memcached_fd = fd;
    dnt_state->callback_memcached_which = which;
    dnt_state->callback_memcached_arg = arg;
    mmprint("debug: %s:%s:%d: [TID:%d], callback registered\n",
                __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);
    return 0;
}


static void handle_single_event(struct dn_thread_state *dnt_state,
        struct dnal_aq_event *event, int socket_number)
{
    struct input *in;

    myAssert(event->type == DNAL_AQET_INPACKET);
    in = event->data.inpacket.buffer;
    myAssert(in != NULL);

    mmprint("debug: %s:%s:%d: [TID:%d], new packet arrived\n",
            __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);


   dnt_state->current_packet = in;

    mmprint("debug: %s:%s:%d: [TID:%d] [%p] packet len [%d] = in->len [%"PRIu16"] "
            " - in->->attr->offset_l5 [%"PRIu16"] "
            "formal payload len = %"PRIu16"\n",
            __FILE__, __FUNCTION__, __LINE__,
            dnt_state->tindex, in,
            (int)(in->len - in->attr->offset_l5),
            in->len,  in->attr->offset_l5,
            udp_payload_length(in));




    // calling callback function which will do processing in memcached
    dnt_state->callback_memcached_fn(
                dnt_state->callback_memcached_fd,
                dnt_state->callback_memcached_which,
                dnt_state->callback_memcached_arg);
    // NOTE: struct in will be freed in recvfrom_dn call
}


void event_handle_loop_dn(void *dn_state)
{
    unsigned int idle = 0;
    myAssert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;
    myAssert(dnt_state->callback_memcached_fn);

    mmprint("debug: %s:%s:%d: [TID:%d], looping for incoming packets\n",
            __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);

    while (true) {
        for (int i = 0; i < dnt_state->socket_count; i++) {

            /*
            if (caq->num_threads > 1) {
                pthread_mutex_lock(&caq->mutex);
                locked = true;
            } else {
                locked = false;
            }
            */

            errval_t err = dnal_aq_poll(dnt_state->daq, &dnt_state->event);
            if (err == SYS_ERR_OK) {
                // This needs to be inside the critical section, since we'll
                // send out data through the AQ
                dnt_state->current_socket = i;
                mmprint("%s:%s:%d: [TID:%d, socket:%d], new event arrived\n",
                    __FILE__, __func__, __LINE__, dnt_state->tindex, i);
                handle_single_event(dnt_state, &dnt_state->event, i);
                idle = 0;

            } else {
                idle++;
            }

            /*
            if (locked) {
                pthread_mutex_unlock(&caq->mutex);
            }
            */

            //if (idle >= IDLE_BEFORE_YIELD) {
            //    sched_yield();
            //    idle = 0;
            //}

        } // end for: for each socket in the thread
    }
}

#if 0
void event_handle_loop_dn(void *dn_state)
{
    myAssert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;
    myAssert(dnt_state->callback_memcached_fn);

    // Each thread will call this with its own dn_state structure.

    mmprint("debug: %s:%s:%d: [TID:%d], looping for incoming packets\n",
            __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);
//    pthread_mutex_lock(&dnt_state->dn_lock);


    while (1) {
        //mmprint("waiting for next event\n");
        errval_t err = dnal_aq_poll(dnt_state->daq, &dnt_state->event);
        if (err == SYS_ERR_OK) {
            mmprint("%s:%s:%d: [TID:%d], new event arrived\n",
                __FILE__, __func__, __LINE__, dnt_state->tindex);
            handle_single_event(dnt_state, &dnt_state->event);
        }

        //stack_process_event(dnt_state->stack);
//        mmprint("%s:%s:%d: [TID:%d], stack_process_event done! looping back\n",
//                __FILE__, __func__, __LINE__, dnt_state->tindex);
    } // end while: infinite
} // end function: event_handle_loop_dn
#endif // 0

int recvfrom_dn(void *dn_state, uint8_t *buff, int bufsize)
{
    myAssert(buff != NULL);
    myAssert(dn_state != NULL);
    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;

    struct input *in = dnt_state->current_packet;

    mmprint("debug: %s:%s:%d: [TID:%d] [in=%p]\n",
            __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex, in);


    int len = in->len - in->attr->offset_l5;

    mmprint("debug: %s:%s:%d: [TID:%d] [SID:%d] packet len [%d] = in->len [%"PRIu16"] "
            " - in->->attr->offset_l5 [%"PRIu16"], read bufsize = %d, "
            "formal payload len = %"PRIu16", %"PRIu16",\n",
            __FILE__, __FUNCTION__, __LINE__,
            dnt_state->tindex, dnt_state->current_socket,
            len, in->len,  in->attr->offset_l5, bufsize,
            udp_payload_length(in),
            len
            );



    dnt_state->udp_sport = in->attr->udp_sport;
    dnt_state->udp_dport = in->attr->udp_dport;
    dnt_state->ip4_src = in->attr->ip4_src;
    dnt_state->ip4_dst = in->attr->ip4_dst;

    dnt_state->dest.type = DNAL_NETDSTT_IP4UDP;
    dnt_state->dest.data.ip4udp.ip_local = in->attr->ip4_dst;
    dnt_state->dest.data.ip4udp.ip_remote = in->attr->ip4_src;
    dnt_state->dest.data.ip4udp.port_local = in->attr->udp_dport;
    dnt_state->dest.data.ip4udp.port_remote = in->attr->udp_sport;

    myAssert(len <= bufsize);

    mmprint("debug: %s:%s:%d,[#### IMP ####] [TID:%d], sport = %"PRIx16", dport=%"PRIx16", "
            "srcip = %"PRIx32" dstip = %"PRIx32", len = %d \n",
            __FILE__,__FILE__, __LINE__, dnt_state->tindex,
            in->attr->udp_sport, in->attr->udp_dport, in->attr->ip4_src,
            in->attr->ip4_dst, len);

    memcpy(buff, (uint8_t *)in->data + in->attr->offset_l5, len);

    mmprint("debug: %s:%s:%d: [TID:%d], copying data of len %d, %d, %d at "
            "location %p of size %d\n",
     __FILE__, __FILE__, __LINE__, dnt_state->tindex, len, in->len,
     in->attr->offset_l5,
     buff, bufsize);

#ifdef SHOW_INTERVAL_STAT

    if((dnt_state->pkt_count) % INTERVAL_STAT_FREQUENCY == 0) {
        //mmprint
        printf
            ("[TID:%d], [pkt_count:%"PRIu64"], sport = %"PRIu16", dport=%"PRIu16", "
            "srcip = %"PRIx32" dstip = %"PRIx32", len = %d \n",
            dnt_state->tindex, dnt_state->pkt_count,
            in->attr->udp_sport, in->attr->udp_dport, in->attr->ip4_src,
            in->attr->ip4_dst, len);
    }
#endif // SHOW_INTERVAL_STAT

    ++dnt_state->pkt_count;

    // Free the buffer used for received packet.
   errval_t err = dnal_aq_buffer_free(dnt_state->daq, in);
   if (err != SYS_ERR_OK) {
        printf("%s:%s:%d: (TID: %d) ERROR: dnal_aq_buffer_failed failed to "
                "allocate buffer\n",
                __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);
   }

    return len;
}

int send_dn(void *dn_state, uint8_t *buff, int bufsize)
{
    myAssert(buff != NULL);
    myAssert(bufsize <= 1410);
    myAssert(dn_state != NULL);

    struct dn_thread_state *dnt_state = (struct dn_thread_state *)dn_state;

    // Allocate buffer to send, and copy the contents
    //
    struct input *in = NULL;
    int ret = dnal_aq_buffer_alloc(dnt_state->daq, &in);
    if (ret != SYS_ERR_OK) {
        printf("%s:%s:%d: (TID: %d) ERROR: dnal_aq_buffer_alloc failed to "
                "allocate buffer\n",
                __FILE__, __FUNCTION__, __LINE__, dnt_state->tindex);
    }
    if (in == 0) {
        //errno = EAGAIN;
        return -1;
    }
    pkt_prepend(in, bufsize);
    memcpy(in->data, buff, bufsize);

    // This is implementation of send call
    dnal_socket_send(dnt_state->dshList[dnt_state->current_socket],
            in, &dnt_state->dest);
    return bufsize;
}


#if 0
// initialize dragonet network stack to listen on udp-port uport
int dn_stack_init_specific(struct dn_thread_state *dn_tstate,
        char *slot_name, uint16_t uport)
{

    pthread_mutex_init(&dn_lock, NULL);
    pthread_mutex_lock(&dn_lock);
    int ret = 1;

    printf("Using %s as name to connect with port %"PRIu16"\n", appName, uport);
    stack = stack_init("dragonet", appName);
    sh = socket_create(stack, recv_cb, NULL);
    if (!socket_bind_udp_listen(sh, 0, uport)) {
        fprintf(stderr, "socket_bind_udp_listen failed\n");
        ret = -1;
    }
    pthread_mutex_unlock(&dn_lock);
    return ret;
} // end function: dn_stack_init

int dn_stack_init(uint16_t uport) {

    int sid = uport - 7777;
    char appName[50];
    snprintf(appName, sizeof(appName), "t%d", sid);
    return dn_stack_init_specific(appName, uport);
}
#endif // 0

int lowlevel_dn_stack_init(struct dn_thread_state *dn_tstate)
{
    int ret = 0;
    char appName[50];

    myAssert(dn_tstate != NULL);

    mmprint("%s:%s:%d:[state:%p], trying to grab locks\n",
            __FILE__, __FUNCTION__, __LINE__, dn_tstate);

    // lock to protect global dn state
    pthread_mutex_lock(&dn_init_lock);
    mmprint("%s:%s:%d:[state:%p], critical section: picking up tid\n",
            __FILE__, __FUNCTION__, __LINE__, dn_tstate);
    // thread level dragonet-stack specific lock
    dn_tstate->tindex = threads_initialized_count;
    ++threads_initialized_count;
//    pthread_mutex_unlock(&dn_init_lock);


    pthread_mutex_lock(&dn_tstate->dn_lock);
    mmprint("%s:%s:%d: [TID:%d], thread local lock grabbed\n", __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex);


    // figure out the application slot name to connect with
    ret = snprintf(appName, sizeof(appName), "t%d", dn_tstate->tindex);
    strncpy(dn_tstate->app_slot, appName, ret);
    mmprint("%s:%s:%d: [TID:%d], connecting with slotname [%s]\n",
            __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex,
            dn_tstate->app_slot);

    // create an application queue with application slot
    ret = dnal_aq_create("dragonet", dn_tstate->app_slot, &dn_tstate->daq);
    err_expect_ok(ret);
    mmprint("%s:%s:%d: [TID:%d], \n", __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex);


    //pthread_mutex_lock(&dn_init_lock);  // FIXME: temararily disabled


    mmprint("%s:%s:%d: [TID:%d], [appName:%s] \n", __FILE__, __FUNCTION__, __LINE__,
            dn_tstate->tindex, dn_tstate->app_slot);

    mmprint("%s:%s:%d: [TID:%d], \n", __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex);

    // If this is first thread then bind, else span
    //if (dn_tstate->tindex < filter_count) {
    if (dn_tstate->tindex < thread_blk_count) {

        mmprint("debug: %s:%s:%d: [TID:%d], directing specified flow to this thread\n",
                __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex);

        struct cfg_thread *sel_thread = &thread_list[dn_tstate->tindex];
        myAssert(sel_thread != NULL);

        pthread_mutex_lock(&sel_thread->thread_init_lock);
        for (int i = 0 ; i < sel_thread->thread_specific_filters; ++i) {

            struct cfg_udpep *udp = sel_thread->eplist[i];
            pthread_mutex_lock(&udp->ep_lock);

            mmprint("lIP: %"PRIu32", lPort: %"PRIu32", rIP: %"PRIu32", rPort: %"PRIu32",\n",
                    udp->l_ip, udp->l_port, udp->r_ip, udp->r_port);

            // create a socket
            ret = dnal_socket_create(dn_tstate->daq, &dn_tstate->dshList[i]);
            err_expect_ok(ret);


            dn_tstate->dndList[i].type = DNAL_NETDSTT_IP4UDP;
            dn_tstate->dndList[i].data.ip4udp.ip_remote = udp->r_ip;
            dn_tstate->dndList[i].data.ip4udp.port_remote = udp->r_port;

            // FIXME: get the actual listen IP address
            dn_tstate->dndList[i].data.ip4udp.ip_local = udp->l_ip;

            // FIXME: get the actual specified port number
            dn_tstate->dndList[i].data.ip4udp.port_local = udp->l_port;

            mmprint("debug: %s:%s:%d: [TID:%d],  Binding, using listen port %"PRIu16"\n",
                    __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex,
                    dn_tstate->dndList[i].data.ip4udp.port_local
                  );
            //print_socket_details(dn_tstate->dsh);
            err_expect_ok(dnal_socket_bind(dn_tstate->dshList[i],
                        &dn_tstate->dndList[i]));

            ++dn_tstate->socket_count;

            // FIXME: maybe I should have lock around this as well.
            //dsh_connected_socket_list[dn_tstate->tindex] = dn_tstate->dsh;
            udp->orig_socket = dn_tstate->dshList[i];
            // mark that socket is set and ready
            udp->state = 1;
            pthread_mutex_unlock(&udp->ep_lock);

        } // end for : for each filter in thread block

        // marking that this thread is done
        sel_thread->thread_done = 1;
        pthread_mutex_unlock(&sel_thread->thread_init_lock);


    } else {
        // These are extra threads which don't have any explicit work
        // So, we just send them to same filter set again in round robin fashion

        int td_owner_index = dn_tstate->tindex % thread_blk_count;
        struct cfg_thread *owner_thread = &thread_list[td_owner_index];

        pthread_mutex_lock(&owner_thread->thread_init_lock);

        if (thread_list[td_owner_index].thread_done < 1) {
            printf("%s:%s:%d: ERROR: Prolem in initialization order.\n"
                    "This thread %d should have been already set.\n",
                   __FILE__, __FUNCTION__, __LINE__, td_owner_index);
            exit(1);
        }

        mmprint("debug: %s:%s:%d: [TID:%d], This is not first thread, "
                "so using existing socket for spanning\n",
                __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex);

        for (int i = 0 ; i < owner_thread->thread_specific_filters; ++i) {

            // create a socket
            ret = dnal_socket_create(dn_tstate->daq, &dn_tstate->dshList[i]);
            err_expect_ok(ret);


            struct cfg_udpep *udp = owner_thread->eplist[i];

            pthread_mutex_lock(&udp->ep_lock);
            myAssert(udp->state > 0);

            err_expect_ok(dnal_socket_span(udp->orig_socket,
                        dn_tstate->daq, dn_tstate->dshList[i]));

            ++udp->state;

            ++dn_tstate->socket_count;
            pthread_mutex_unlock(&udp->ep_lock);
        } // end for: for each filter in orginal thread block

        ++owner_thread->thread_done;
        pthread_mutex_unlock(&owner_thread->thread_init_lock);

    } // end for:  each non-orginal thread

    pthread_mutex_unlock(&dn_init_lock);

    mmprint("%s:%s:%d: [TID:%d], releasing locks \n", __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex);
    pthread_mutex_unlock(&dn_tstate->dn_lock);


    mmprint("%s:%s:%d: [TID:%d], \n", __FILE__, __FUNCTION__, __LINE__, dn_tstate->tindex);
    return ret;
}

#endif // DRAGONET

