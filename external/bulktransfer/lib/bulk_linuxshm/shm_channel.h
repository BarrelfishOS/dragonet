/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#ifndef SHM_CHANNEL_H
#define SHM_CHANNEL_H

#include <assert.h>
#include <bulk_transfer/bulk_transfer.h>
#include <shmchan.h>


enum shm_message_type {
    SHM_MSG_BIND,
    SHM_MSG_BIND_DONE,
    SHM_MSG_ASSIGN,
    SHM_MSG_MOVE,
    SHM_MSG_COPY,
    SHM_MSG_PASS,
    SHM_MSG_RELEASE,
    SHM_MSG_STATUS,
};

struct shm_message {
    enum shm_message_type type;
    union {
        struct {
            uint8_t role;
            uint32_t num_slots;
        } bind_request;
        struct {
            errval_t err;
            uint32_t meta_size;
            uint8_t direction;
        } bind_done;
        struct {
            uint32_t op;
            struct bulk_pool_id pool;
        } assign;
        /* used for move,copy,pass,release */
        struct {
            uint32_t op;
            struct bulk_pool_id pool;
            uint32_t buffer;
            uint32_t meta;
        } buffer;
        struct {
            uint32_t op;
            errval_t err;
        } status;
    } content;
};


SHMCHAN(shm_chan_, struct shm_message)

#endif

