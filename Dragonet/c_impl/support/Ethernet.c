#include <ethernetproto.h>


#define eth_header_field_offset(x,y)      (l2Offset(x) + (y))

// ---------------------------- DST MAC --------------------

#define eth_dst_mac_offset(x)              (eth_header_field_offset((x), (0)))

mac_t eth_dst_mac_read(struct input *pkt)
{
    return pkt_read48(pkt, eth_dst_mac_offset(pkt));
}

void eth_dst_mac_write(struct input *pkt, mac_t val)
{
    pkt_write48(pkt, eth_dst_mac_offset(pkt), val);
}

// ---------------------------- SRC MAC --------------------

#define eth_src_mac_offset(x)             (eth_header_field_offset((x), (6)))

mac_t eth_src_mac_read(struct input *pkt)
{
    return pkt_read48(pkt, eth_src_mac_offset(pkt));
}

void eth_src_mac_write(struct input *pkt, mac_t val)
{
    pkt_write48(pkt, eth_src_mac_offset(pkt), val);
}



// ---------------------------- ETH TYPE --------------------

#define eth_type_offset(x)                (eth_header_field_offset((x), 12))

uint16_t eth_type_read(struct input *pkt)
{
    return pkt_read16be(pkt, eth_type_offset(pkt));
}

void eth_type_write(struct input *pkt, uint16_t val)
{
    pkt_write16be(pkt, eth_type_offset(pkt), val);
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




