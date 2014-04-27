#ifndef HELPERS_H_
#define HELPERS_H_

#include <implementation.h>

typedef void (*event_handler_fun_ptr)(const int fd, const short which, void *arg);

void stack_init(const char *name, const char *inq, const char *outq);
struct state *stack_get_state(void);
struct input *stack_get_packet(void);
void stack_send_udp_packet(struct input *in);


int register_callback_dn(event_handler_fun_ptr fun, int fd, short which,
        void *arg);

void event_handle_loop_dn(void);

int recvfrom_dn(uint8_t *buff, int bufsize);
int send_dn(uint8_t *buff, int bufsize);

#endif // ndef HELPERS_H_

