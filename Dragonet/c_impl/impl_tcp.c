/*
 * Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
 *
 * Released under a dual BSD 3-clause/GPL 2 license. When using or
 * redistributing this file, you may do so under either license.
 *
 * See LICENCE.Dragonet for details.
 */

#include <stdlib.h>
#include <string.h>
#include <implementation.h>
#include <inttypes.h>

node_out_t do_pg__RxL4TCPValidHeaderLength(
        struct ctx_RxL4TCPValidHeaderLength *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPPortClassifyType(
        struct ctx_RxL4TCPPortClassifyType *context, struct state *state,
        struct input **in)
{
    // P_RxL4TCPPortClassifyType_static, P_RxL4TCPPortClassifyType_dynamic
    return 0;
}

node_out_t do_pg__RxL4TCPPortClassifyStatic(
        struct ctx_RxL4TCPPortClassifyStatic *context, struct state *state,
        struct input **in)
{
    // P_RxL4TCPPortClassifyStatic_toSocket, P_RxL4TCPPortClassifyStatic_noSocket
    return 0;
}

node_out_t do_pg__RxL4TCPPortClassifyDynamic(
        struct ctx_RxL4TCPPortClassifyDynamic *context, struct state *state,
        struct input **in)
{
    //
    return 0;
}

node_out_t do_pg__RxL4TCPClosedPortAction(
        struct ctx_RxL4TCPClosedPortAction *context, struct state *state,
        struct input **in)
{
    // P_RxL4TCPClosedPortAction_out
    return 0;
}

node_out_t do_pg__RxL4TCPSocketClassify(
        struct ctx_RxL4TCPSocketClassify *context, struct state *state,
        struct input **in)
{
    // P_RxL4TCPSocketClassify_srvSocket, P_RxL4TCPSocketClassify_cliSocket
    return 0;
}

node_out_t do_pg__RxL4TCPSocketServerSide(
        struct ctx_RxL4TCPSocketServerSide *context, struct state *state,
        struct input **in)
{
    // P_RxL4TCPSocketServerSide_isListen, P_RxL4TCPSocketServerSide_isSynRecv, P_RxL4TCPSocketServerSide_isEstablished, P_RxL4TCPSocketServerSide_isCloseWait, P_RxL4TCPSocketServerSide_isLastAck, P_RxL4TCPSocketServerSide_isClosed
    return 0;
}

node_out_t do_pg__RxL4TCPSocketInClosed(
        struct ctx_RxL4TCPSocketInClosed *context, struct state *state,
        struct input **in)
{
    // P_RxL4TCPSocketInClosed_out
    return 0;
}

node_out_t do_pg__RxL4TCPSocketClientSide(
        struct ctx_RxL4TCPSocketClientSide *context, struct state *state,
        struct input **in)
{
    // P_RxL4TCPSocketClientSide_out
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidSyn(
        struct ctx_RxL4TCPSocketIsValidSyn *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketAddHalfOpenConn(struct
        ctx_RxL4TCPSocketAddHalfOpenConn *context, struct state *state,
        struct input **in)
{
    // P_RxL4TCPSocketAddHalfOpenConn_success, P_RxL4TCPSocketAddHalfOpenConn_failure
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSendSyn(struct ctx_RxL4TCPSocketSendSyn *context,
        struct state *state, struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidSynAckS(
        struct ctx_RxL4TCPSocketIsValidSynAckS *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidFinAck(
        struct ctx_RxL4TCPSocketIsValidFinAck *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidFinAck2(
        struct ctx_RxL4TCPSocketIsValidFinAck2 *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeEstablished(
        struct ctx_RxL4TCPSocketSChangeEstablished *context,
        struct state *state, struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsDataPacket(
        struct ctx_RxL4TCPSocketIsDataPacket *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsFinSet(
        struct ctx_RxL4TCPSocketIsFinSet *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeLastAck(
        struct ctx_RxL4TCPSocketSChangeLastAck *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeToClosed(
        struct ctx_RxL4TCPSocketSChangeToClosed *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeToClosed2(
        struct ctx_RxL4TCPSocketSChangeToClosed2 *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketCopyData(
        struct ctx_RxL4TCPSocketCopyData *context, struct state *state,
        struct input **in)
{
    // P_true, P_false
    return 0;
}








node_out_t do_pg__TxL4TCPInitiateResponse(
        struct ctx_TxL4TCPInitiateResponse *context, struct state *state,
        struct input **in)
{
    // P_TxL4TCPInitiateResponse_out, P_TxL4TCPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL4TCPAllocateHeader(
        struct ctx_TxL4TCPAllocateHeader *context, struct state *state,
        struct input **in)
{
    // P_TxL4TCPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL4TCPFillHeader(struct ctx_TxL4TCPFillHeader *context,
        struct state *state, struct input **in)
{
    // P_true, P_false
    return 0;
}

