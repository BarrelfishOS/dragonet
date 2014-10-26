#include <implementation.h>
#include <stdio.h>
#include <sched.h>
#include <assert.h>
#include <dpdk_backend.h>

void e10k_ctrl_waitready(struct state *state)
{
    void *drv;
    do {
        drv = (void *) state->tap_handler;
        sched_yield();
    } while (drv == NULL);
}

bool e10k_ctrl_5tuple_unset(struct state *state, uint8_t index)
{
    assert(!"NYI");
    return false;
}

bool e10k_ctrl_5tuple_set(struct state *state,
        uint8_t index, uint8_t priority, uint8_t queue,
        uint32_t src_ip, uint32_t dst_ip, uint16_t src_port, uint16_t dst_port,
        uint16_t l4_type, uint16_t mask)
{

    struct dragonet_dpdk *e10k_nic = (struct dragonet_dpdk *)state->tap_handler;

    if (e10k_nic == NULL) {
        return false;
    }

    l4_type = 0x11; // FIXME: hardcoding the type to UDP

    printf("\n\n### %s:%s:%d:  [#### IMP ####]"
            "Priority: %"PRIu8", Queue: %"PRIu8", mask: %"PRIu16", l4Type: %"PRIu16", "
            "srcIP: %"PRIu32", srcPort: %"PRIu16",  dstIP: %"PRIu32", dstPort: %"PRIu16"\n\n",
            __FILE__, __FUNCTION__, __LINE__,
            priority,
            queue,
            mask,
            l4_type,
            src_ip,
            src_port,
            dst_ip,
            dst_port
            );

    int ret = set_5tuple_filter(e10k_nic, dst_ip, src_ip, dst_port, src_port,
            l4_type, mask, priority, queue, index);
    if (ret < 0) return false;
    return true;
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


