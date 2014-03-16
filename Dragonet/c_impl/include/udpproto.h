#ifndef UDP_PROTOCOL_H_
#define UDP_PROTOCOL_H_

#include <stddef.h>
#include <stdio.h>

#include <implementation.h>
#include <packet_access.h>

// ---------------------------- --------------------

// UDP header length
#define udp_header_len     (8)


static inline pktoff_t l4Offset(struct input *pkt)
{
    return pkt->offset_l4;
}


static inline pktoff_t udp_header_offset(struct input *pkt)
{
    return (l4Offset(pkt));
}

static inline pktoff_t udp_header_field_offset(struct input *pkt,
            pktoff_t field_offset)
{
    return (udp_header_offset(pkt) + field_offset);
}

// ----------------------- udp field PAYLOAD -----------

static inline pktoff_t udp_payload_offset(struct input *pkt)
{
    return udp_header_field_offset(pkt, 8);
}

static inline pktoff_t udp_payload_length(struct input *pkt)
{
    return (pkt->len - udp_payload_offset(pkt));
}

// NOTE: It is assumed that destination is atleast a buffer size
static inline int udp_copy_payload(struct input *pkt, void *dst,
        pktoff_t bufsize)
{
    pktoff_t payloadsize = udp_payload_length(pkt);
    if (bufsize < payloadsize) {
        panic("too small buffer to copy data\n");
        return -1;
    }

    pktoff_t off = udp_payload_offset(pkt);
    pkt_read(pkt, off, payloadsize, dst);
    return payloadsize;
}


// ----------------------- UDP field accessing functions -----------


// ----------------------- UDP source port field -----------

static inline pktoff_t udp_hdr_sport_offset(struct input *pkt)
{
    return udp_header_field_offset(pkt, 0);
}

static inline portno_t udp_hdr_sport_read(struct input *pkt)
{
    return pkt_read16be(pkt, udp_hdr_sport_offset(pkt));
}

static inline void udp_hdr_sport_write(struct input *pkt, portno_t val)
{
    return pkt_write16be(pkt, udp_hdr_sport_offset(pkt), val);
}

// ----------------------- UDP destination port field -----------

static inline pktoff_t udp_hdr_dport_offset(struct input *pkt)
{
    return udp_header_field_offset(pkt, 2);
}

static inline portno_t udp_hdr_dport_read(struct input *pkt)
{
    return pkt_read16be(pkt, udp_hdr_dport_offset(pkt));
}

static inline void udp_hdr_dport_write(struct input *pkt, portno_t val)
{
    return pkt_write16be(pkt, udp_hdr_dport_offset(pkt), val);
}

// ----------------------- UDP length field -----------

static inline pktoff_t udp_hdr_pkt_length_offset(struct input *pkt)
{
    return udp_header_field_offset(pkt, 4);
}

static inline uint16_t udp_hdr_pkt_length_read(struct input *pkt)
{
    return pkt_read16be(pkt, udp_hdr_pkt_length_offset(pkt));
}

static inline void udp_hdr_pkt_length_write(struct input *pkt, uint16_t val)
{
    return pkt_write16be(pkt, udp_hdr_pkt_length_offset(pkt), val);
}

// ----------------------- UDP checksum field -----------

static inline pktoff_t udp_hdr_checksum_offset(struct input *pkt)
{
    return udp_header_field_offset(pkt, 6);
}

static inline uint16_t udp_hdr_checksum_read(struct input *pkt)
{
    return pkt_read16be(pkt, udp_hdr_checksum_offset(pkt));
}

static inline void udp_hdr_checksum_write(struct input *pkt, uint16_t val)
{
    return pkt_write16be(pkt, udp_hdr_checksum_offset(pkt), val);
}

#endif // UDP_PROTOCOL_H_
