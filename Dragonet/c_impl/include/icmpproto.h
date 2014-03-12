#ifndef ICMP_PROTOCOL_H_
#define ICMP_PROTOCOL_H_

#include <stddef.h>
#include <stdio.h>

#include <implementation.h>
#include <packet_access.h>


#define icmpTypeEchoRequest         (0x08)
#define icmpTypeEchoReply           (0x00)

// ICMP header length
#define icmp_header_len     (8)

static inline size_t l4Offset(struct input *pkt)
{
    return (pkt->attrs[L4Offset]);
}

static inline size_t icmp_header_offset(struct input *pkt)
{
    return (l4Offset(pkt));
}

static inline size_t icmp_header_field_offset(struct input *pkt,
            size_t field_offset)
{
    return (icmp_header_offset(pkt) + field_offset);
}

// ----------------------- ICMP field PAYLOAD -----------

static inline size_t icmp_payload_offset(struct input *pkt)
{
    return icmp_header_field_offset(pkt, 8);
}

static inline size_t icmp_payload_length(struct input *pkt)
{
    return (pkt->len - icmp_payload_offset(pkt));
}


// ----------------------- ICMP field accessing functions -----------
uint8_t icmp_hdr_type_read(struct input *pkt);
void icmp_hdr_type_write(struct input *pkt, uint8_t val);
uint8_t icmp_hdr_code_read(struct input *pkt);
void icmp_hdr_code_write(struct input *pkt, uint8_t val);
uint16_t icmp_hdr_checksum_read(struct input *pkt);
void icmp_hdr_checksum_write(struct input *pkt, uint16_t val);
uint32_t icmp_hdr_misc_read(struct input *pkt);
void icmp_hdr_misc_write(struct input *pkt, uint32_t val);

#endif // ICMP_PROTOCOL_H_
