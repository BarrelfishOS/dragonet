#include <implementation.h>
#include <stdio.h>
#include <sched.h>
#include <assert.h>
#include <dpdk_backend.h>

static char BIG_MSG_STR[4000];
void e10k_ctrl_waitready(struct state *state)
{
    void *drv;
    do {
        drv = (void *) state->st_driver_handle;
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
        uint32_t index, uint8_t queue,
        uint32_t src_ip, uint32_t dst_ip, uint16_t src_port, uint16_t dst_port,
        uint16_t l4_type, uint16_t mask)
{
    struct dragonet_dpdk *e10k_nic = (struct dragonet_dpdk *)state->st_driver_handle;
    char src_ip_str[IPv4_ADDR_STR_SIZE];
    convert_ipv4(src_ip, src_ip_str);
    char dst_ip_str[IPv4_ADDR_STR_SIZE];
    convert_ipv4(dst_ip, dst_ip_str);


    if (e10k_nic == NULL) {
        printf(" ################## NIC Not ready ############ \n");
        return false;
    }

    l4_type = 0x11; // FIXME: hardcoding the type to UDP

    snprintf(BIG_MSG_STR, sizeof(BIG_MSG_STR),
            "ID %"PRIu8", Q: %"PRIu8", "
            "sIP: %s, sPort: %"PRIu16",  dIP: %s,  dPort: %"PRIu16", "
            "M: %"PRIu16", l4Type: %"PRIu16"\n",
            index,
            queue,
            src_ip_str,
            src_port,
            dst_ip_str,
            dst_port,
            mask,
            l4_type
            );

    //printf
    dprint
        ("### %s:%s:%d:  [####-- IMP --####] %s",
            __FILE__, __FUNCTION__, __LINE__,
            BIG_MSG_STR);

    int ret = set_fdir_filter(e10k_nic, dst_ip, src_ip, dst_port, src_port,
            l4_type, mask, queue, index);
    if (ret < 0) return false;
    filter_manipulation_log(FILTER_MAN_FNAME, BIG_MSG_STR);
    return true;
}

bool e10k_ctrl_5tuple_set(struct state *state,
        uint8_t index, uint8_t priority, uint8_t queue,
        uint32_t src_ip, uint32_t dst_ip, uint16_t src_port, uint16_t dst_port,
        uint16_t l4_type, uint16_t mask)
{

    struct dragonet_dpdk *e10k_nic = (struct dragonet_dpdk *)state->st_driver_handle;

    char src_ip_str[IPv4_ADDR_STR_SIZE];
    convert_ipv4(src_ip, src_ip_str);
    char dst_ip_str[IPv4_ADDR_STR_SIZE];
    convert_ipv4(dst_ip, dst_ip_str);

    if (e10k_nic == NULL) {
        printf(" ################## NIC Not ready ############ \n");
        return false;
    }

    l4_type = 0x11; // FIXME: hardcoding the type to UDP

    snprintf(BIG_MSG_STR, sizeof(BIG_MSG_STR),
            "ID %"PRIu8", Q: %"PRIu8", "
            "sIP: %s, sPort: %"PRIu16",  dIP: %s,  dPort: %"PRIu16", "
            "Pri: %"PRIu8",  M: %"PRIu16", l4Type: %"PRIu16"\n",
            index,
            queue,
            src_ip_str,
            src_port,
            dst_ip_str,
            dst_port,
            priority,
            mask,
            l4_type
            );

    //printf
    dprint
        ("### %s:%s:%d:  [####-- IMP --####] %s",
            __FILE__, __FUNCTION__, __LINE__,
            BIG_MSG_STR);

    // FIXME: just for testing fdir filters.  Remove this line after testing
//    return e10k_ctrl_fdir_set(state, (uint16_t)index, queue,
//            src_ip, dst_ip, src_port, dst_port, l4_type, mask);

    int ret = set_5tuple_filter(e10k_nic, dst_ip, src_ip, dst_port, src_port,
            l4_type, mask, priority, queue, index);
    if (ret < 0) {
        assert(!"Failed in inserting 5tuple filter");
        return false;
    }
    filter_manipulation_log(FILTER_MAN_FNAME, BIG_MSG_STR);
    return true;
}


