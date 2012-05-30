// RXPath.click
/*
This configuration refers to 
*/


new_packet :: Arrival();      // Incoming packets on RX PHY
drop_packet :: Discard();  // Dropped packets (for any reasons)

// ********** Validate packet *****************
mac_filter :: ExactUnicast(validMac, promiscuous);
multicast_filter :: ExactMulticast(ValidMulticast, promiscous);
new_packet -> mac_filter;
new_packet -> multicast_filter;

// When filter fails
mac_filter[1] -> drop_packet; // no MAC address matched
multicast_filter[1] -> drop_packet; // No multicast group matched

// Error Checking
crc_error_check :: CalculateCRC();
length_error_check :: VerifyLengh();
classifier_ipv4 :: Classifier(IPv4, IPv6);
classifier_tcp_udp :: Classifier(tcp, udp);
ipv4_checksum :: IPv4Validate();
TCP_checksum :: TCPValidate();
UDP_checksum :: UDPValidate();
valid_packet :: ValidPacketStats();

mac_filter -> crc_error_check;
multicast_filter -> crc_error_check;

crc_error_check -> length_error_check -> classifier_ipv4;
classifier_ipv4[0] -> ipv4_checksum  -> classifier_tcp_udp;
classifier_ipv4[1] -> classifier_tcp_udp;
classifier_tcp_udp[2] -> UDP_checksum -> valid_packet; // valid UDP packet
classifier_tcp_udp[1] -> TCP_checksum -> valid_packet; // valid TCP packet
classifier_tcp_udp[0] -> valid_packet; // valid other packet

crc_error_check[1] -> drop_packet;  // CRC checksum failed
length_error_check[1] -> drop_packet;  // Pkt len does not match actual len
ipv4_checksum[1] ->drop_packet; // IPv4 checksum failed
UDP_checksum[1] ->drop_packet; // UDP checksum failed
TCP_checksum[1] ->drop_packet; // TCP checksum failed

// ********** Classify/Filter packet *****************

// Apply filters, and lookup redirection table
classifier_ip :: Classifier(IPv4, IPv6);
redirection_table :: TableLookup(128);

valid_packet -> classifier_ip;

// IPv4 classification
classifier_L4 :: Classifier(tcp, udp);
hash_ipv4_tcp :: CalculateHash(ipv4, tcp);
hash_ipv4_udp :: CalculateHash(ipv4, udp);
hash_ipv4 :: CalculateHash(ipv4);

classifier_ip[0] -> classifier_L4;
classifier_L4[0] -> hash_ipv4_tcp -> redirection_table;
classifier_L4[1] -> hash_ipv4_udp -> redirection_table;
classifier_L4[2] -> hash_ipv4 -> redirection_table;

// IPv6 classification
classifier_L4_ipv6 :: Classifier(tcp, udp, ipv6);
hash_ipv6_tcp :: CalculateHash(ipv6, tcp);
hash_ipv6_udp :: CalculateHash(ipv6, udp);
hash_ipv6 :: CalculateHash(ipv6);

classifier_ip[1] -> classifier_L4_ipv6;
classifier_L4_ipv6[0] -> hash_ipv6_tcp -> redirection_table;
classifier_L4_ipv6[1] -> hash_ipv6_udp -> redirection_table;
classifier_L4_ipv6[2] -> hash_ipv6 -> redirection_table;

// ********** DMA packet *****************

// Packet Demultiplexer
queue_selector :: SelectQueueIndex(2);
get_buf_descriptor_0 :: GetDescriptor(qid);
get_buf_descriptor_1 :: GetDescriptor(qid);
packet_dma_0 :: PacketDMA(0);
packet_dma_1 :: PacketDMA(1);
Q0 :: RXQueue(0);
Q1 :: RXQueue(1);

redirection_table -> queue_selector;
queue_selector[0] -> get_buf_descriptor_0 -> packet_dma_0 -> Q0;
queue_selector[1] -> get_buf_descriptor_1 -> packet_dma_1 -> Q1;
get_buf_descriptor_0[1] -> drop_packet;  // ERROR: no free descriptors in Q0
get_buf_descriptor_1[1] -> drop_packet;  // ERROR: no free descriptors in Q1


// update head index after receiving packet
update_head_idx_0 :: SetQueueHead(0);
update_head_idx_1 :: SetQueueHead(1);

packet_dma_0 -> update_head_idx_0;
packet_dma_1 -> update_head_idx_1;

// ********** Packet arrival notification ****************

// Interrupt Notification Generation
notification_generation :: NotificationManager();
redirection_table -> [0] notification_generation; // for cpu_id
update_head_idx_0 -> [1] notification_generation;
update_head_idx_1 -> [1] notification_generation;

interrupt_throttler :: InterruptThrottling(interrupt_rate);
pcie_interface :: PCIExpressInterface();
notification_generation -> interrupt_throttler -> pcie_interface;

// *****************************************