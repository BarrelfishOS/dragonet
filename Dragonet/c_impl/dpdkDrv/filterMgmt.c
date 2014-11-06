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
    assert(!"NYI e10k_ctrl_5tuple_unset");
    return false;
}

bool e10k_ctrl_fdir_unset(struct state *state, uint8_t index)
{
    assert(!"NYI e10k_ctrl_5tuple_unset");
    return false;
}


bool e10k_ctrl_fdir_set(struct state *state,
        uint16_t index, uint8_t queue,
        uint32_t src_ip, uint32_t dst_ip, uint16_t src_port, uint16_t dst_port,
        uint16_t l4_type, uint16_t mask)
{
    struct dragonet_dpdk *e10k_nic = (struct dragonet_dpdk *)state->tap_handler;

    if (e10k_nic == NULL) {
        return false;
    }

    l4_type = 0x11; // FIXME: hardcoding the type to UDP

    printf("\n\n### %s:%s:%d:  [#### IMP ####]"
            "index: %"PRIu16", Queue: %"PRIu8", mask: %"PRIu16", l4Type: %"PRIu16", "
            "srcIP: %"PRIu32", srcPort: %"PRIu16",  dstIP: %"PRIu32", dstPort: %"PRIu16"\n\n",
            __FILE__, __FUNCTION__, __LINE__,
            index,
            queue,
            mask,
            l4_type,
            src_ip,
            src_port,
            dst_ip,
            dst_port
            );

    int ret = set_fdir_filter(e10k_nic, dst_ip, src_ip, dst_port, src_port,
            l4_type, mask, queue, index);
    if (ret < 0) return false;
    return true;
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

    // FIXME: just for testing fdir filters.  Remove this line after testing
//    return e10k_ctrl_fdir_set(state, (uint16_t)index, queue,
//            src_ip, dst_ip, src_port, dst_port, l4_type, mask);

    int ret = set_5tuple_filter(e10k_nic, dst_ip, src_ip, dst_port, src_port,
            l4_type, mask, priority, queue, index);
    if (ret < 0) return false;
    return true;
}


