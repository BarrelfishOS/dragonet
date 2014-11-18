#include <implementation.h>
#include <e10k.h>
#include <dragonet_e10k.h>
#include <stdio.h>
#include <sched.h>

/* Hack */
void e10k_qcb_write_queue_tails(struct e10k_card *c,
                                struct e10k_queue_state *q)
{ }

void e10k_ctrl_waitready(struct state *state)
{
    struct dragonet_e10k *e10k;
    do {
        e10k = (struct dragonet_e10k *) state->tap_handler;
        sched_yield();
    } while (e10k == NULL);
}

bool e10k_ctrl_5tuple_unset(struct state *state, uint8_t index)
{
    struct dragonet_e10k *e10k = (struct dragonet_e10k *) state->tap_handler;
    struct e10k_5tfilter f;

    if (e10k == NULL) {
        return false;
    }

    memset(&f, 0, sizeof(f));
    f.enabled = false;
    return e10k_5tfilter_setup(&e10k->card, index, &f);
}

bool e10k_ctrl_fdir_unset(struct state *state, uint8_t index)
{
    assert(!"NYI e10k_ctrl_5tuple_unset");
    return false;
}
bool e10k_ctrl_fdir_set(struct state *state,
        uint32_t index, uint8_t queue,
        uint32_t src_ip, uint32_t dst_ip, uint16_t src_port, uint16_t dst_port,
        uint16_t l4_type, uint16_t mask)
{
    assert(!"NYI e10k_ctrl_fdir_set");
    return false;
}


bool e10k_ctrl_5tuple_set(struct state *state,
        uint8_t index, uint8_t priority, uint8_t queue,
        uint32_t src_ip, uint32_t dst_ip, uint16_t src_port, uint16_t dst_port,
        uint16_t l4_type, uint16_t mask)
{
    struct dragonet_e10k *e10k = (struct dragonet_e10k *) state->tap_handler;
    struct e10k_5tfilter f;

    if (e10k == NULL) {
        return false;
    }

    f.enabled  = true;
    f.priority = priority;
    f.queue    = queue;
    f.src_ip   = src_ip;
    f.dst_ip   = dst_ip;
    f.src_port = src_port;
    f.dst_port = dst_port;
    f.mask     = mask;
    f.l4_type  = l4_type;
    printf("\n\n### %s:%s:%d:  [#### IMP ####]"
            "Priority: %"PRIu8", Queue: %"PRIu8", mask: %"PRIu16", l4Type: %"PRIu16", "
            "srcIP: %"PRIu32", srcPort: %"PRIu16",  dstIP: %"PRIu32", dstPort: %"PRIu16"\n\n",
            __FILE__, __FUNCTION__, __LINE__,
            f.priority,
            f.queue,
            f.mask,
            f.l4_type,
            f.src_ip,
            f.src_port,
            f.dst_ip,
            f.dst_port
            );
    return e10k_5tfilter_setup(&e10k->card, index, &f);
}


// FIXME: This is only for reference/example purpose.  Not to be called by
// any part.  This will be deleted soon.
bool custom_oracle(struct state *state);
bool custom_oracle(struct state *state)
{
    //  index: 0 Priority: 3, Queue: 0,  srcIP: 175178781, dstIP: 175178847, srcPort: 9003,   dstPort: 888, mask: 0, l4Type: 1
     assert(e10k_ctrl_5tuple_set(state, 0, 3, 0,  175178781, 175178847, 9003, 888, 0, 1));

    //  index: 1 Priority: 1, Queue: 1,  srcIP: 0, dstIP: 0, srcPort: 0,   dstPort: 888, mask: 14, l4Type: 1
    assert(e10k_ctrl_5tuple_set(state, 1, 1, 1,  0, 0, 0,   888, 14, 1));

    // index: 2 Priority: 3, Queue: 2,  srcIP: 175178809, dstIP: 175178847, srcPort: 9003,   dstPort: 888, mask: 0, l4Type: 1
    assert(e10k_ctrl_5tuple_set(state, 2, 3, 2,  175178809, 175178847, 9003, 888, 0, 1));

    assert(e10k_ctrl_5tuple_set(state, 3, 3, 3, 175178781, 175178847, 9002, 888, 0, 1));

    assert(e10k_ctrl_5tuple_set(state,  4, 3, 0, 175178809, 175178847, 9002, 888, 0, 1));
    assert(e10k_ctrl_5tuple_set(state,  5, 3, 1, 175178781, 175178847, 9001, 888, 0, 1));
    assert(e10k_ctrl_5tuple_set(state,  7, 3, 3, 175178781, 175178847, 9000, 888, 0, 1));
    assert(e10k_ctrl_5tuple_set(state,  8, 3, 0, 175178809, 175178847, 9000, 888, 0, 1));
    assert(e10k_ctrl_5tuple_set(state,  9, 3, 1, 175178772, 175178847, 9003, 888, 0, 1));
    assert(e10k_ctrl_5tuple_set(state, 10, 3, 2, 175178772, 175178847, 9002, 888, 0, 1));
    assert(e10k_ctrl_5tuple_set(state, 11, 3, 3, 175178772, 175178847, 9001, 888, 0, 1));
    assert(e10k_ctrl_5tuple_set(state, 12, 3, 0, 175178772, 175178847, 9000, 888, 0, 1));
    return true;
}


