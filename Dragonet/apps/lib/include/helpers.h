#ifndef HELPERS_H_
#define HELPERS_H_

#include <implementation.h>

void stack_init(const char *name, const char *inq, const char *outq);
struct input *stack_get_packet(void);
void stack_send_udp_packet(struct input *in);

#endif // ndef HELPERS_H_

