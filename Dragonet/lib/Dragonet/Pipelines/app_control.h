#ifndef APP_CONTROL_H_
#define APP_CONTROL_H_

#include <stdbool.h>
#include <stdint.h>

#include <dyn_remote.h>

typedef uint64_t app_id_t;
typedef uint64_t socket_id_t;
typedef uint32_t app_flags_t;

#define MAX_APPLBL 32
#define MAX_QUEUELBL 32

enum app_control_type {
    APPCTRL_WELCOME,            // Dragonet -> App
    APPCTRL_STATUS,             // Dragonet -> App
    APPCTRL_SOCKET_INFO,        // Dragonet -> App
    APPCTRL_GRAPH_CMD,          // Dragonet -> App
    APPCTRL_REGISTER,           // App -> Dragonet
    APPCTRL_SOCKET_UDPBIND,     // App -> Dragonet
    APPCTRL_SOCKET_UDPFLOW,     // App -> Dragonet
    APPCTRL_SOCKET_SPAN,        // App -> Dragonet
    APPCTRL_SOCKET_CLOSE,       // App -> Dragonet
    APPCTRL_APPQ_NOP,           // App -> Dragonet
};

// flags:
//  MORE: more control messages comming (advise to stack)
#define APPCTRL_MSG_FLAGS_MORE  (0x1)

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
        } socket_info;
        struct {
            struct dynr_action act;
        } graph_cmd;
        struct {
            char label[MAX_APPLBL];
        } register_app;
        // local/remote UDP endpoints
        struct {
            uint32_t l_ip;
            uint32_t r_ip;
            uint16_t l_port;
            uint16_t r_port;
        } socket_udpbind;
        // register flow: socket id /local/remote UDP endpoints
        struct {
            socket_id_t sid;
            uint32_t l_ip;
            uint32_t r_ip;
            uint16_t l_port;
            uint16_t r_port;
        } socket_udpflow;
        struct {
            socket_id_t id;
        } socket_span;
        struct {
            socket_id_t id;
        } socket_close;
    } data;
    app_flags_t flags;
};

void app_control_init(
    const char *stackname,
    void (*new_application)(int,struct dynr_client *),
    void (*register_app)(int,const char *,app_flags_t),
    void (*stop_application)(int,bool),
    void (*socket_udpbind)(int,uint32_t, uint16_t, uint32_t, uint16_t,app_flags_t),
    void (*socket_udpflow)(int,socket_id_t,uint32_t,uint16_t,uint32_t,uint16_t,app_flags_t),
    void (*socket_span)(int,socket_id_t,app_flags_t),
    void (*socket_close)(int,socket_id_t,app_flags_t),
    void (*nop)(app_flags_t));

void app_control_send_welcome(int fd, app_id_t id);
void app_control_send_status(int fd, bool success);
void app_control_send_socket_info(int fd, socket_id_t id);
void app_control_send_graph_cmd(int fd, struct dynr_action *action);


#endif // ndef APP_CONTROL_H_
