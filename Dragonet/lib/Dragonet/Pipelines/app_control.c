#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "app_control.h"

#define MAX_APPS 20

void app_control_init(
    const char *stackname,
    void (*new_application)(int),
    void (*register_app)(int,const char *),
    void (*stop_application)(int,bool),
    void (*socket_udplisten)(int,uint32_t,uint16_t),
    void (*socket_udpflow)(int,uint32_t,uint16_t,uint32_t,uint16_t),
    void (*socket_span)(int,socket_id_t),
    void (*socket_close)(int,socket_id_t))
{
    int appfds[MAX_APPS];
    int num_apps = 0, s, i, fd, maxfd, cnt;
    fd_set reads, reads_orig, errs;
    struct sockaddr_un addr;
    struct app_control_message msg;
    ssize_t len;

    if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("app_control_init: creating socket failed");
        abort();
    }

    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    snprintf(addr.sun_path, sizeof(addr.sun_path)-1, "/tmp/%s_apps", stackname);
    unlink(addr.sun_path);
    if (bind(s, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        perror("app_control_init: bind failed");
        abort();
    }
    if (listen(s, 5) < 0) {
        perror("app_control_init: listen failed");
        abort();
    }

    FD_ZERO(&reads_orig);
    FD_SET(s, &reads_orig);
    maxfd = s;

    printf("Starting event loop\n");
    while (true) {
        reads = reads_orig;
        errs = reads_orig;
        if ((cnt = select(maxfd + 1, &reads, NULL, &errs, NULL)) < 0) {
            if (errno == EINTR) {
                continue;
            }
            perror("app_control_init: select failed");
            abort();
        }

        // handle errors
        if (FD_ISSET(s, &errs)) {
            close(s);
            perror("app_control_init: Error on listening socket :-/");
            abort();
        }
        for (i = 0; cnt > 0 && i < num_apps; i++) {
            if (appfds[i] == -1 || !FD_ISSET(appfds[i], &errs)) {
                continue;
            }
            cnt--;
            close(appfds[i]);
            perror("app_control_init: Application socket error");
            stop_application(appfds[i], false);
            FD_CLR(appfds[i], &reads_orig);
            appfds[i] = -1;
        }

        // handle new connections
        if (cnt > 0 && FD_ISSET(s, &reads) ) {
            cnt--;
            if ((fd = accept(s, NULL, NULL)) < 0) {
                perror("app_control_init: accept() failed, skipping "
                        "connection");
            } else if (num_apps >= MAX_APPS) {
                fprintf(stderr, "app_control_init: connection limit reached\n");
                close(fd);
            } else {
                printf("New application\n");
                appfds[num_apps++] = fd;
                maxfd = (fd > maxfd ? fd : maxfd);
                FD_SET(fd, &reads_orig);
                new_application(fd);
            }
        }

        // handle requests
        for (i = 0; cnt > 0 && i < num_apps; i++) {
            if (appfds[i] == -1 || !FD_ISSET(appfds[i], &reads)) {
                continue;
            }
            cnt--;

            len = recv(appfds[i], &msg, sizeof(msg), MSG_WAITALL);
            if (len < sizeof(msg)) {
                if (len > 0) {
                    fprintf(stderr, "app_control_init: incomplete recv\n");
                } else if (len < 0) {
                    perror("app_control_init: error in recv");
                }
                close(appfds[i]);
                stop_application(appfds[i], len == 0);
                FD_CLR(appfds[i], &reads_orig);
                appfds[i] = -1;
                continue;
            }

            switch (msg.type) {
                case APPCTRL_REGISTER:
                    printf("APPCTRL_REGISTER\n");
                    msg.data.register_app.label[MAX_APPLBL - 1]
                        = 0;
                    register_app(appfds[i], msg.data.register_app.label);
                    break;
                case APPCTRL_SOCKET_UDPLISTEN:
                    printf("APPCTRL_SOCKET_UDPLISTEN\n");
                    socket_udplisten(appfds[i],
                            msg.data.socket_udplisten.ip,
                            msg.data.socket_udplisten.port);
                    break;
                case APPCTRL_SOCKET_UDPFLOW:
                    printf("APPCTRL_SOCKET_UDPFLOW\n");
                    socket_udpflow(appfds[i],
                            msg.data.socket_udpflow.s_ip,
                            msg.data.socket_udpflow.s_port,
                            msg.data.socket_udpflow.d_ip,
                            msg.data.socket_udpflow.d_port);
                    break;
                case APPCTRL_SOCKET_SPAN:
                    printf("APPCTRL_SOCKET_SPAN\n");
                    socket_span(appfds[i], msg.data.socket_span.id);
                    break;
                case APPCTRL_SOCKET_CLOSE:
                    printf("APPCTRL_SOCKET_CLOSE\n");
                    socket_close(appfds[i], msg.data.socket_close.id);
                    break;
                default:
                    fprintf(stderr, "app_control_init: invalid msg type\n");
            }
        }
    }
}

void app_control_send_welcome(int fd, app_id_t id,
                              uint8_t num_inq, uint8_t num_outq)
{
    printf("app_control_send_welcome\n");
    struct app_control_message msg;
    msg.type = APPCTRL_WELCOME;
    msg.data.welcome.id = id;
    msg.data.welcome.num_inq = num_inq;
    msg.data.welcome.num_outq = num_outq;
    if (send(fd, &msg, sizeof(msg), 0) < sizeof(msg)) {
        fprintf(stderr, "app_control_send_welcome: incomplete send\n");
    }
}

void app_control_send_status(int fd, bool success)
{
    printf("app_control_send_status\n");
    struct app_control_message msg;
    msg.type = APPCTRL_STATUS;
    msg.data.status.success = success;
    if (send(fd, &msg, sizeof(msg), 0) < sizeof(msg)) {
        fprintf(stderr, "app_control_send_status: incomplete send\n");
    }
}

void app_control_send_queue(int fd, bool out, const char *label)
{
    printf("app_control_send_queue (o=%d,l=%s)\n", out, label);
    struct app_control_message msg;
    msg.type = (out ? APPCTRL_OUTQUEUE : APPCTRL_INQUEUE);
    if (strlen(label) >= MAX_QUEUELBL) {
        fprintf(stderr, "app_control_send_queue: Queue label too long\n");
        return;
    }
    strcpy(msg.data.queue.label, label);
    if (send(fd, &msg, sizeof(msg), 0) < sizeof(msg)) {
        fprintf(stderr, "app_control_send_queue: incomplete send\n");
    }
}

void app_control_send_socket_info(int fd, socket_id_t id, uint8_t outq,
                                  int32_t mux_id)
{
    printf("app_control_send_socket_info\n");
    struct app_control_message msg;
    msg.type = APPCTRL_SOCKET_INFO;
    msg.data.socket_info.id = id;
    msg.data.socket_info.outq = outq;
    msg.data.socket_info.mux_id = mux_id;
    if (send(fd, &msg, sizeof(msg), 0) < sizeof(msg)) {
        fprintf(stderr, "app_control_send_socket_info: incomplete send\n");
    }
}
