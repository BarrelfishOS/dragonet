/*
** Copyright 2005-2014  Solarflare Communications Inc.
**                      7505 Irvine Center Drive, Irvine, CA 92618, USA
** Copyright 2002-2005  Level 5 Networks Inc.
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of version 2 of the GNU General Public License as
** published by the Free Software Foundation.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
*/

/**************************************************************************\
*//*! \file
** <L5_PRIVATE L5_SOURCE>
** \author  djr/ctk/stg
**  \brief  Sockets interface to user level UDP
**   \date  2004/06/02 (UDP version)
**    \cop  (c) Level 5 Networks Limited.
** </L5_PRIVATE>
*//*
\**************************************************************************/

/*! \cidoxg_lib_transport_unix */

#define _GNU_SOURCE /* for recvmmsg */

#include "internal.h"
#include "ul_poll.h"
#include "ul_select.h"
#include <ci/internal/cplane_ops.h>
#include <ci/internal/cplane_handle.h>
#include <onload/ul/tcp_helper.h>
#include <onload/tcp_poll.h>


#define VERB(x) Log_VTC(x)

#define LPF "citp_udp_"

#ifndef MSG_CONFIRM
#define MSG_CONFIRM 0   /* so we never see it in our flags */
#endif


/* ****************************************************
 * Module utils
 */

#ifndef NDEBUG
ci_inline char * __decode_flags(int fl)
{
  static char buf[32];
  char * t = buf;
  size_t n = sizeof(buf);

  *buf = 0;
  if( fl & MSG_OOB) {
    t += snprintf(t, n, "OOB ");
    n = t - buf;
  }

  if( fl & MSG_PEEK) {
    t += snprintf(t, n, "PEEK ");
    n = t - buf;
  }
  if( fl & MSG_NOSIGNAL) {
    t += snprintf(t, n, "NSIG ");
    n = t - buf;
  }
  if( fl & MSG_TRUNC) {
    t += snprintf(t, n, "TRNC ");
    n = t - buf;
  }
  if( fl & MSG_DONTWAIT) {
    t += snprintf(t, n, "NWT ");
    n = t - buf;
  }
  if( fl & MSG_WAITALL) {
    t += snprintf(t, n, "WALL ");
    n = t - buf;
  }
  return buf;
}
#endif

/* ****************************************************
 * Intercept handlers
 */

static int citp_udp_socket(int domain, int type, int protocol)
{
  citp_fdinfo* fdi;
  citp_sock_fdi* epi;
  ef_driver_handle fd;
  int rc;
  ci_netif* ni;

  Log_V(log(LPF "socket(%d, %d, %d)", domain, type, protocol));

  epi = CI_ALLOC_OBJ(citp_sock_fdi);
  if( ! epi ) {
    Log_U(ci_log(LPF "socket: failed to allocate epi"));
    errno = ENOMEM;
    goto fail1;
  }
  fdi = &epi->fdinfo;
  citp_fdinfo_init(fdi, &citp_udp_protocol_impl);

  rc = citp_netif_alloc_and_init(&fd, &ni);
  if( rc != 0 ) {
    if( rc == CI_SOCKET_HANDOVER ) {
      /* This implies EF_DONT_ACCELERATE is set, so we handover
       * regardless of CITP_OPTS.no_fail */
      CI_FREE_OBJ(epi);
      return rc;
    }
    goto fail2;
  }

  /* Protect the fdtable entry until we're done initialising. */
  if( fdtable_strict() )  CITP_FDTABLE_LOCK();
  if((fd = ci_udp_ep_ctor(&epi->sock, ni, domain, type)) < 0) {
    /*! ?? \TODO unpick the ci_udp_ep_ctor according to how failed */
    Log_U(ci_log(LPF "socket: udp_ep_ctor failed"));
    errno = -fd;
    goto fail3;
  }

  citp_fdtable_new_fd_set(fd, fdip_busy, fdtable_strict());
  if( fdtable_strict() )  CITP_FDTABLE_UNLOCK();

  CI_DEBUG(epi->sock.s->pid = getpid());

  /* We're ready.  Unleash us onto the world! */
  ci_assert(epi->sock.s->b.sb_aflags & CI_SB_AFLAG_NOT_READY);
  ci_atomic32_and(&epi->sock.s->b.sb_aflags, ~CI_SB_AFLAG_NOT_READY);
  citp_fdtable_insert(fdi, fd, 0);

  Log_VSS(log(LPF "socket(%d, %d, %d) = "EF_FMT, domain, type, protocol,
              EF_PRI_ARGS(epi,fd)));
  return fd;

 fail3:
  ef_onload_driver_close(fd);
  if( CITP_OPTS.no_fail && errno != ELIBACC )
    CITP_STATS_NETIF(++ni->state->stats.udp_handover_socket);
  citp_netif_release_ref(ni, 0);
 fail2:
  CI_FREE_OBJ(epi);
 fail1:
  /* BUG1408: Graceful failure. We'll only fail outright if there's a
   * driver/library mismatch */
  if( CITP_OPTS.no_fail && errno != ELIBACC ) {
    Log_U(ci_log("%s: failed (errno:%d) - PASSING TO OS", __FUNCTION__, errno));
    return CI_SOCKET_HANDOVER;
  }
  return -1;
}


static citp_fdinfo* citp_udp_dup(citp_fdinfo* orig_fdi)
{
  citp_socket* orig_sock = fdi_to_socket(orig_fdi);
  citp_sock_fdi* sock_fdi = CI_ALLOC_OBJ(citp_sock_fdi);
  if( sock_fdi ) {
    citp_fdinfo_init(&sock_fdi->fdinfo, &citp_udp_protocol_impl);
    sock_fdi->sock = *orig_sock;
    citp_netif_add_ref(orig_sock->netif);
    return &sock_fdi->fdinfo;
  }
  return 0;
}


static void citp_udp_dtor(citp_fdinfo* fdinfo, int fdt_locked)
{
  citp_netif_release_ref(fdi_to_socket(fdinfo)->netif, fdt_locked);
}


static int citp_udp_bind(citp_fdinfo* fdinfo, const struct sockaddr* sa,
			 socklen_t sa_len)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdinfo);
  int rc;

  Log_V(log(LPF "bind(%d, sa, %d)", fdinfo->fd, sa_len));

  ci_netif_lock_fdi(epi);
  rc = ci_udp_bind(&epi->sock, fdinfo->fd, sa, sa_len);
  ci_netif_unlock_fdi(epi);

  if( rc == CI_SOCKET_HANDOVER ) {
    CITP_STATS_NETIF(++epi->sock.netif->state->stats.udp_handover_bind);
    citp_fdinfo_handover(fdinfo, -1);
    return 0;
  }

  citp_fdinfo_release_ref( fdinfo, 0 );
  return rc;
}


static int citp_udp_connect(citp_fdinfo* fdinfo,
			    const struct sockaddr* sa, socklen_t sa_len,
                            citp_lib_context_t* lib_context)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdinfo);
  int rc;

  Log_V(log(LPF "connect(%d, sa, %d)", fdinfo->fd, sa_len));

  ci_netif_lock_fdi(epi);
  rc = ci_udp_connect(&epi->sock, fdinfo->fd, sa, sa_len);
  ci_netif_unlock_fdi(epi);

  if( rc == CI_SOCKET_HANDOVER ) {
    CITP_STATS_NETIF(++epi->sock.netif->state->stats.udp_handover_connect);
    citp_fdinfo_handover(fdinfo, -1);
    return 0;
  }

  citp_fdinfo_release_ref( fdinfo, 0 );
  return rc;
}


static int citp_udp_close(citp_fdinfo* fdinfo, int may_cache)
{
  return 0;
}


static int citp_udp_shutdown(citp_fdinfo* fdinfo, int how)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdinfo);
  int rc;

  Log_V(ci_log("%s("EF_FMT", %d)", __FUNCTION__, EF_PRI_ARGS(epi,fdinfo->fd), how));

  ci_netif_lock_fdi(epi);
  rc = ci_udp_shutdown(&epi->sock, fdinfo->fd, how);
  ci_netif_unlock_fdi(epi);
  Log_V(log(LPF "shutdown: fd=%d rc=%d", fdinfo->fd, rc));
  return rc;
}


static int citp_udp_getsockname(citp_fdinfo* fdinfo,
				struct sockaddr* sa, socklen_t* p_sa_len)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdinfo);

  Log_VSC(log(LPF "getsockname("EF_FMT")", EF_PRI_ARGS(epi, fdinfo->fd)));

  __citp_getsockname(epi->sock.s, sa, p_sa_len);
  return 0;
}


static int citp_udp_getpeername(citp_fdinfo* fdinfo,
				struct sockaddr* sa, socklen_t* p_sa_len)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdinfo);
  int rc;

  Log_V(log("%s("EF_FMT")", __FUNCTION__, EF_PRI_ARGS(epi,fdinfo->fd)));

  ci_netif_lock_fdi(epi);
  rc = ci_udp_getpeername(&epi->sock, sa, p_sa_len);
  ci_netif_unlock_fdi(epi);
  return rc;
}


static int citp_udp_getsockopt(citp_fdinfo* fdinfo, int level,
			       int optname, void* optval, socklen_t* optlen)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdinfo);
  int rc;

  Log_V(log("%s("EF_FMT", %d, %d)", __FUNCTION__, EF_PRI_ARGS(epi,fdinfo->fd),
            level, optname ));

  ci_netif_lock_fdi(epi);
  rc = ci_udp_getsockopt(&epi->sock, fdinfo->fd,
			 level, optname, optval, optlen);
  ci_netif_unlock_fdi(epi);
  return rc;
}


static int citp_udp_setsockopt(citp_fdinfo* fdinfo, int level,
		       int optname, const void* optval, socklen_t optlen)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdinfo);
  int rc;

  Log_VSC(log("%s("EF_FMT", %d, %d)", __FUNCTION__,
	      EF_PRI_ARGS(epi, fdinfo->fd),  level, optname));

  rc = ci_udp_setsockopt(&epi->sock, fdinfo->fd,
			 level, optname, optval, optlen);

  Log_V(log(LPF "setsockopt: fd=%d rc=%d", fdinfo->fd, rc));

  if( rc == CI_SOCKET_HANDOVER ) {
    CITP_STATS_NETIF(++epi->sock.netif->state->stats.udp_handover_setsockopt);
    citp_fdinfo_handover(fdinfo, -1);
    return 0;
  }

  citp_fdinfo_release_ref(fdinfo, 0);
  return rc;
}

#if CI_CFG_RECVMMSG
static int citp_udp_recvmmsg(citp_fdinfo* fdinfo, struct mmsghdr* msg, 
                             unsigned vlen, int flags, 
                             const struct timespec *timeout)
{
  citp_sock_fdi* epi = fdi_to_sock_fdi(fdinfo);
  ci_udp_iomsg_args a;

  Log_V(log(LPF "recvmmsg(%d, msg, %u, %#x)", fdinfo->fd, vlen, 
            (unsigned) flags));

  a.fd = fdinfo->fd;
  a.ep = &epi->sock;
  a.ni = epi->sock.netif;
  a.us = SOCK_TO_UDP(epi->sock.s);

  return ci_udp_recvmmsg(&a, msg, vlen, flags, timeout);
}
#endif

static int citp_udp_recv(citp_fdinfo* fdinfo, struct msghdr* msg, int flags)
{
  citp_sock_fdi* epi = fdi_to_sock_fdi(fdinfo);
  ci_udp_iomsg_args a;

  Log_V(log(LPF "recv(%d, msg, %#x)", fdinfo->fd, (unsigned) flags));

  a.fd = fdinfo->fd;
  a.ep = &epi->sock;
  a.ni = epi->sock.netif;
  a.us = SOCK_TO_UDP(epi->sock.s);

  return ci_udp_recvmsg( &a, msg, flags);
}


static int citp_udp_send(citp_fdinfo* fdinfo, const struct msghdr * msg,
			 int flags)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdinfo);
  ci_udp_iomsg_args a;
  int rc;

  ci_assert(msg != NULL);

  a.ep = &epi->sock;
  a.fd = fdinfo->fd;
  a.ni = epi->sock.netif;
  a.us = SOCK_TO_UDP(epi->sock.s);

  /* NB. msg_name[len] validated in ci_udp_sendmsg(). */
  if(CI_LIKELY( msg->msg_iov != NULL || msg->msg_iovlen == 0 )) {
    rc = ci_udp_sendmsg( &a, msg, flags);
  }
  else {
    rc = -1;
    errno = EFAULT;
  }
  return rc;
}


#if CI_CFG_SENDMMSG
static int citp_udp_sendmmsg(citp_fdinfo* fdinfo, struct mmsghdr* mmsg, 
                             unsigned vlen, int flags)
{
  citp_sock_fdi* epi = fdi_to_sock_fdi(fdinfo);
  ci_udp_iomsg_args a;
  int i, rc;

  Log_V(log(LPF "sendmmsg(%d, msg, %u, %#x)", fdinfo->fd, vlen, 
            (unsigned) flags));

  if( vlen == 0 ) 
    return 0;

  a.ep = &epi->sock;
  a.fd = fdinfo->fd;
  a.ni = epi->sock.netif;
  a.us = SOCK_TO_UDP(epi->sock.s);

  i = 0;

  do {
    rc = ci_udp_sendmsg(&a, &mmsg[i].msg_hdr, flags);
    if(CI_LIKELY( rc >= 0 ) )
      mmsg[i].msg_len = rc;
    ++i;
  } while( rc >= 0 && i < vlen );
  return (rc>=0) ? i : rc;
}
#endif


static int citp_udp_fcntl(citp_fdinfo* fdinfo, int cmd, long arg)
{
  return citp_sock_fcntl(fdi_to_sock_fdi(fdinfo),
			fdinfo->fd, cmd, arg);
}


static int citp_udp_ioctl(citp_fdinfo* fdinfo, int request, void* arg)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdinfo);
  int rc;

  Log_V(log("%s("EF_FMT", %d, 0x%lx)", __FUNCTION__,
            EF_PRI_ARGS(epi, fdinfo->fd), request, (long) arg));

  rc = ci_udp_ioctl(&epi->sock, fdinfo->fd, request, arg);
  Log_V(log(LPF "ioctl()=%d", rc));
  if( rc < 0 )
    CI_SET_ERROR(rc, -rc);
  return rc;
}


#if CI_CFG_USERSPACE_SELECT

static int citp_udp_select(citp_fdinfo* fdi, int* n, int rd, int wr, int ex,
                           struct oo_ul_select_state*__restrict__ ss)
{
  citp_sock_fdi* epi;
  ci_udp_state* us;
  unsigned mask;
  ci_netif* ni;

  epi = fdi_to_sock_fdi(fdi);
  us = SOCK_TO_UDP(epi->sock.s);
  ni = epi->sock.netif;

  citp_poll_if_needed(ni, ss->now_frc, ss->ul_select_spin);
  mask = ci_udp_poll_events(ni, us);

  if( rd && (mask & SELECT_RD_SET) ) {
    FD_SET(fdi->fd, ss->rdu);
    ++*n;
  }
  if( wr && (mask & SELECT_WR_SET) ) {
    FD_SET(fdi->fd, ss->wru);
    ++*n;
  }

  return 1;
}


static int citp_udp_poll(citp_fdinfo*__restrict__ fdi,
                         struct pollfd*__restrict__ pfd,
                         struct oo_ul_poll_state*__restrict__ ps)
{
  citp_sock_fdi *epi = fdi_to_sock_fdi(fdi);
  ci_udp_state* us = SOCK_TO_UDP(epi->sock.s);
  ci_netif* ni = epi->sock.netif;
  unsigned mask;

  mask = ci_udp_poll_events(ni, us);
  pfd->revents = mask & (pfd->events | POLLERR | POLLHUP);
  if( pfd->revents == 0 )
    if( citp_poll_if_needed(ni, ps->this_poll_frc, ps->ul_poll_spin) ) {
      mask = ci_udp_poll_events(ni, us);
      pfd->revents = mask & (pfd->events | POLLERR | POLLHUP);
    }

  return 1;
}

#endif


#if CI_CFG_USERSPACE_EPOLL
#include "ul_epoll.h"
/* More-or-less copy of citp_udp_poll */
static void citp_udp_epoll(citp_fdinfo*__restrict__ fdi,
                           struct citp_epoll_member*__restrict__ eitem,
                           struct oo_ul_epoll_state*__restrict__ eps)
{
  citp_sock_fdi* epi = fdi_to_sock_fdi(fdi);
  ci_udp_state* us = SOCK_TO_UDP(epi->sock.s);
  ci_netif* ni = epi->sock.netif;
  ci_uint64 sleep_seq;
  unsigned mask;

  /* Try to return a result without polling if we can. */
  sleep_seq = us->s.b.sleep_seq.all;
  mask = ci_udp_poll_events(ni, us);
  if( ! citp_ul_epoll_set_ul_events(eps, eitem, mask, sleep_seq) )
    if( citp_poll_if_needed(ni, eps->this_poll_frc, eps->ul_epoll_spin) ) {
      sleep_seq = us->s.b.sleep_seq.all;
      mask = ci_udp_poll_events(ni, us);
      citp_ul_epoll_set_ul_events(eps, eitem, mask, sleep_seq);
    }
}
#endif


static int citp_udp_listen(citp_fdinfo* fdinfo, int backlog)
{
  Log_V(log(LPF "listen: not supported by dg protocol"));
  citp_fdinfo_release_ref( fdinfo, 0 );
  RET_WITH_ERRNO( EOPNOTSUPP );
}

static int citp_udp_accept(citp_fdinfo* fdinfo,
			   struct sockaddr* sa, socklen_t* p_sa_len,
                           int flags,
                           citp_lib_context_t* lib_context)
{
  Log_V(log(LPF "accept: not supported by dg protocol"));
  RET_WITH_ERRNO( EOPNOTSUPP );
}


static int citp_udp_zc_send(citp_fdinfo* fdi, struct onload_zc_mmsg* msg, 
                            int flags)
{
  msg->rc = -EOPNOTSUPP;
  return 1;
}


static int citp_udp_zc_recv(citp_fdinfo* fdi, struct onload_zc_recv_args* args)
{
  citp_sock_fdi* epi = fdi_to_sock_fdi(fdi);
  ci_udp_iomsg_args a;

  if( args->flags & ~ONLOAD_ZC_RECV_FLAGS_MASK ) 
    return -EINVAL;

  a.fd = fdi->fd;
  a.ep = &epi->sock;
  a.ni = epi->sock.netif;
  a.us = SOCK_TO_UDP(epi->sock.s);

  return ci_udp_zc_recv(&a, args);
}


static int citp_udp_zc_recv_filter(citp_fdinfo* fdi, 
                                   onload_zc_recv_filter_callback filter,
                                   void* cb_arg, int flags)
{
#if CI_CFG_ZC_RECV_FILTER
  citp_sock_fdi* epi = fdi_to_sock_fdi(fdi);
  ci_udp_state* us = SOCK_TO_UDP(epi->sock.s);

  ci_assert_nequal(filter, NULL);
  /* flags not yet used */
  ci_assert_equal(flags, 0);

  us->recv_q_filter = (ci_uintptr_t)filter;
  us->recv_q_filter_arg = (ci_uintptr_t)cb_arg;
  return 0;
#else
  return -ENOSYS;
#endif
}


static int citp_udp_recvmsg_kernel(citp_fdinfo* fdi, struct msghdr* msg, 
                                   int flags)
{
  citp_sock_fdi* epi = fdi_to_sock_fdi(fdi);

  return ci_udp_recvmsg_kernel(fdi->fd, epi->sock.netif, 
                               SOCK_TO_UDP(epi->sock.s),
                               msg, flags);
}


int citp_udp_tmpl_alloc(citp_fdinfo* fdi, struct iovec* initial_msg,
                        int mlen, struct oo_msg_template** omt_pp,
                        unsigned flags)
{
  return -EOPNOTSUPP;
}


int citp_udp_tmpl_update(citp_fdinfo* fdi, struct oo_msg_template* omt,
                         struct onload_template_msg_update_iovec* updates,
                         int ulen, unsigned flags)
{
  return -EOPNOTSUPP;
}


int citp_udp_tmpl_abort(citp_fdinfo* fdi, struct oo_msg_template* omt)
{
  return -EOPNOTSUPP;
}


citp_protocol_impl citp_udp_protocol_impl = {
  .type        = CITP_UDP_SOCKET,
  .ops         = {
    .socket      = citp_udp_socket,
    .dtor        = citp_udp_dtor,
    .dup         = citp_udp_dup,
    .bind        = citp_udp_bind,
    .listen      = citp_udp_listen,
    .accept      = citp_udp_accept,
    .connect     = citp_udp_connect,
    .close       = citp_udp_close,
    .shutdown    = citp_udp_shutdown,
    .getsockname = citp_udp_getsockname,
    .getpeername = citp_udp_getpeername,
    .getsockopt  = citp_udp_getsockopt,
    .setsockopt  = citp_udp_setsockopt,
    .recv        = citp_udp_recv,
#if CI_CFG_RECVMMSG
    .recvmmsg    = citp_udp_recvmmsg,
#endif
    .send        = citp_udp_send,
#if CI_CFG_SENDMMSG
    .sendmmsg    = citp_udp_sendmmsg,
#endif
    .fcntl       = citp_udp_fcntl,
    .ioctl       = citp_udp_ioctl,
#if CI_CFG_USERSPACE_SELECT
    .select	 = citp_udp_select,
    .poll	 = citp_udp_poll,
#if CI_CFG_USERSPACE_EPOLL
    .epoll       = citp_udp_epoll,
#endif
#endif
#if CI_CFG_SENDFILE
    .sendfile_post_hook = NULL,
#endif
    .zc_send     = citp_udp_zc_send,
    .zc_recv     = citp_udp_zc_recv,
    .zc_recv_filter = citp_udp_zc_recv_filter,
    .recvmsg_kernel = citp_udp_recvmsg_kernel,
    .tmpl_alloc     = citp_udp_tmpl_alloc,
    .tmpl_update    = citp_udp_tmpl_update,
    .tmpl_abort     = citp_udp_tmpl_abort,
  }
};

/*! \cidoxg_end */
