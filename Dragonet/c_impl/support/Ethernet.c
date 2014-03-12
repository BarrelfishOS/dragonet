#include <ethernetproto.h>

// Ethernet header length
#define ethernet_header_len     (14)

#define ethernet_header_len     (14)


#define l2Offset                (0)

#define eth_header_field_offset(x)      (l2Offset + (x))

// ---------------------------- DST MAC --------------------

#define eth_dst_mac_offset              (eth_header_field_offset(0))

mac_t eth_dst_mac_read(struct input *pkt)
{
    return pkt_read48be(pkt, eth_dst_mac_offset);
}

void eth_dst_mac_write(struct input *pkt, mac_t val)
{
    pkt_write48(pkt, eth_dst_mac_offset, val);
}

// ---------------------------- SRC MAC --------------------

#define eth_src_mac_offset              (eth_header_field_offset(6))

mac_t eth_src_mac_read(struct input *pkt)
{
    return pkt_read48be(pkt, eth_src_mac_offset);
}

void eth_src_mac_write(struct input *pkt, mac_t val)
{
    pkt_write48(pkt, eth_src_mac_offset, val);
}



// ---------------------------- ETH TYPE --------------------

#define eth_type_offset                 (eth_header_field_offset(12))

uint16_t eth_type_read(struct input *pkt)
{
    return pkt_read16be(pkt, eth_type_offset);
}

void eth_type_write(struct input *pkt, uint16_t val)
{
    pkt_write16be(pkt, eth_type_offset, val);
}

// ---------------------------- ETH checksum --------------------

size_t eth_checksum_offset(struct input *pkt)
{
    return (pkt->len - 4);
}

uint32_t eth_checksum_read(struct input *pkt)
{
    return pkt_read32be(pkt, eth_checksum_offset(pkt));
}

void eth_checksum_write(struct input *pkt, uint32_t val)
{
    pkt_write32be(pkt, eth_checksum_offset(pkt), val);
}




