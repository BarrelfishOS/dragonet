/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <signal.h>
#include <unistd.h>
#include <pthread.h>
#include <dpdk_backend.h>

struct dragonet_dpdk
*init_dpdk_wrapper_deleteme(char *name, int queues)
{
    struct dragonet_dpdk *e10k_nic;
    int k;

    e10k_nic = (struct dragonet_dpdk *) malloc (sizeof(struct dragonet_dpdk));
    if (e10k_nic == NULL) {
        printf("ERROR: %s:%s:%d: malloc failed in allocating memory\n",
                __FILE__, __FUNCTION__, __LINE__);
        abort();
        return NULL;
    }
    assert(e10k_nic != NULL);
    memset(e10k_nic, 0, sizeof(struct dragonet_dpdk));

    // Allocate memory to hold the actual queue elements
    e10k_nic->queues = (struct dragonet_dpdk_queue *)
                calloc(DPDK_MAX_QUEUES, sizeof(struct dragonet_dpdk_queue));
    assert(e10k_nic->queues != NULL);

    // connecting to device with given name using dpdk library and storing the handle
    //  Note that we are currently also allocating a default hardware queue with
    //      catchall filter
    e10k_nic->e10kif = (void *) init_dpdk_setup_and_get_default_queue2(name,
            queues);

    if(e10k_nic->e10kif == NULL) {
        printf("ERROR: Bad interface '%s' or unable to allocate resources\n",
                name);
        exit(1);
    }

    // Initialize the queues
    struct dragonet_dpdk_queue *iq = e10k_nic->queues;
    for (k = 0; k < DPDK_MAX_QUEUES; k++) {
        iq[k].dpdk_e10k = e10k_nic;
        iq[k].queue = NULL; // FIXME: get proper queue handle
        iq[k].qid = k;
        iq[k].qstate = 1;
    } // end for: for each queue

    // Create a default filter to make sure that all traffic ends up in
    //  queue-0 by default.
    //  NOTE: done by above function

    // return handle to the device
    return e10k_nic;
} // end function: init_dpdk_wrapper_deleteme




