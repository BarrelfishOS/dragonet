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
    return e10k_5tfilter_setup(&e10k->card, index, &f);
}

