#ifndef APP_CONTROL_H_
#define APP_CONTROL_H_

#include <stdbool.h>
#include <stdint.h>

typedef uint64_t app_id_t;
typedef uint64_t socket_id_t;

enum app_control_type {
    APPCTRL_WELCOME,            // Dragonet -> App
    APPCTRL_STATUS,             // Dragonet -> App
    APPCTRL_SOCKET_UDPLISTEN,   // App -> Dragonet
    APPCTRL_SOCKET_UDPFLOW,     // App -> Dragonet
    APPCTRL_SOCKET_CLOSE,       // App -> Dragonet
};

struct app_control_message {
    enum app_control_type type;
    union {
        struct {
            app_id_t id;
        } welcome;
        struct {
            bool success;
        } status;
        struct {
            socket_id_t id;
            uint32_t ip;
            uint16_t port;
        } socket_udplisten;
        struct {
            socket_id_t id;
            uint32_t s_ip;
            uint32_t d_ip;
            uint16_t s_port;
            uint16_t d_port;
        } socket_udpflow;
        struct {
            socket_id_t id;
        } socket_close;
    } data;
};

void app_control_init(
    const char *stackname,
    void (*new_application)(int),
    void (*stop_application)(int,bool),
    void (*socket_udplisten)(int,socket_id_t,uint32_t,uint16_t),
    void (*socket_udpflow)(int,socket_id_t,uint32_t,uint16_t,uint32_t,uint16_t),
    void (*socket_close)(int,socket_id_t));

void app_control_send_welcome(int fd, app_id_t id);
void app_control_send_status(int fd, bool success);


#endif // ndef APP_CONTROL_H_
