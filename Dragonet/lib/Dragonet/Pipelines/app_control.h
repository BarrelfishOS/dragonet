#ifndef APP_CONTROL_H_
#define APP_CONTROL_H_

#include <stdbool.h>
#include <stdint.h>

typedef uint64_t app_id_t;
typedef uint64_t socket_id_t;

#define MAX_APPLBL 32
#define MAX_QUEUELBL 32

enum app_control_type {
    APPCTRL_WELCOME,            // Dragonet -> App
    APPCTRL_OUTQUEUE,           // Dragonet -> App
    APPCTRL_INQUEUE,            // Dragonet -> App
    APPCTRL_STATUS,             // Dragonet -> App
    APPCTRL_SOCKET_INFO,        // Dragonet -> App
    APPCTRL_REGISTER,           // App -> Dragonet
    APPCTRL_SOCKET_UDPLISTEN,   // App -> Dragonet
    APPCTRL_SOCKET_UDPFLOW,     // App -> Dragonet
    APPCTRL_SOCKET_CLOSE,       // App -> Dragonet
};

struct app_control_message {
    enum app_control_type type;
    union {
        struct {
            app_id_t id;
            uint8_t  num_inq;
            uint8_t  num_outq;
        } welcome;
        struct {
            char label[MAX_QUEUELBL];
        } queue;
        struct {
            bool success;
        } status;
        struct {
            int32_t mux_id;
            uint8_t outq;
        } socket_info;
        struct {
            char label[MAX_APPLBL];
        } register_app;
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
    void (*register_app)(int,const char *),
    void (*stop_application)(int,bool),
    void (*socket_udplisten)(int,socket_id_t,uint32_t,uint16_t),
    void (*socket_udpflow)(int,socket_id_t,uint32_t,uint16_t,uint32_t,uint16_t),
    void (*socket_close)(int,socket_id_t));

void app_control_send_welcome(int fd, app_id_t id,
                              uint8_t num_inq, uint8_t num_outq);
void app_control_send_status(int fd, bool success);
void app_control_send_queue(int fd, bool out, const char *label);
void app_control_send_socket_info(int fd, uint8_t outq, int32_t mux_id);


#endif // ndef APP_CONTROL_H_
