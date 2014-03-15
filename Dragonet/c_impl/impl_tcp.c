#include <stdlib.h>
#include <string.h>
#include <implementation.h>
#include <inttypes.h>

node_out_t do_pg__RxL4TCPValidHeaderLength(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPPortClassifyType(struct state *state, struct input *in)
{
    // P_RxL4TCPPortClassifyType_static, P_RxL4TCPPortClassifyType_dynamic
    return 0;
}

node_out_t do_pg__RxL4TCPPortClassifyStatic(struct state *state, struct input *in)
{
    // P_RxL4TCPPortClassifyStatic_toSocket, P_RxL4TCPPortClassifyStatic_noSocket
    return 0;
}

node_out_t do_pg__RxL4TCPPortClassifyDynamic(struct state *state, struct input *in)
{
    //
    return 0;
}

node_out_t do_pg__RxL4TCPClosedPortAction(struct state *state, struct input *in)
{
    // P_RxL4TCPClosedPortAction_out
    return 0;
}

node_out_t do_pg__RxL4TCPSocketClassify(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketClassify_srvSocket, P_RxL4TCPSocketClassify_cliSocket
    return 0;
}

node_out_t do_pg__RxL4TCPSocketServerSide(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketServerSide_isListen, P_RxL4TCPSocketServerSide_isSynRecv, P_RxL4TCPSocketServerSide_isEstablished, P_RxL4TCPSocketServerSide_isCloseWait, P_RxL4TCPSocketServerSide_isLastAck, P_RxL4TCPSocketServerSide_isClosed
    return 0;
}

node_out_t do_pg__RxL4TCPSocketInClosed(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketInClosed_out
    return 0;
}

node_out_t do_pg__RxL4TCPSocketClientSide(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketClientSide_out
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidSyn(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketAddHalfOpenConn(struct state *state, struct input *in)
{
    // P_RxL4TCPSocketAddHalfOpenConn_success, P_RxL4TCPSocketAddHalfOpenConn_failure
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSendSyn(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidSynAckS(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidFinAck(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsValidFinAck2(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeEstablished(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsDataPacket(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketIsFinSet(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeLastAck(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeToClosed(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketSChangeToClosed2(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxL4TCPSocketCopyData(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

node_out_t do_pg__RxTagTxTCPIR(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}








node_out_t do_pg__TxL4TCPInitiateResponse(struct state *state, struct input *in)
{
    // P_TxL4TCPInitiateResponse_out, P_TxL4TCPInitiateResponse_drop
    return 0;
}

node_out_t do_pg__TxL4TCPAllocateHeader(struct state *state, struct input *in)
{
    // P_TxL4TCPAllocateHeader_out
    return 0;
}

node_out_t do_pg__TxL4TCPFillHeader(struct state *state, struct input *in)
{
    // P_true, P_false
    return 0;
}

