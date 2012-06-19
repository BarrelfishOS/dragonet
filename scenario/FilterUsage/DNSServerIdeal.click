// RXPath.click
/*
This configuration refers to the ideal configuration for DNS server 
*/


new_packet :: Arrival();      // Incoming packets on RX 
send_packets :: Discard();  
// ********** Validate packet *****************
app_selection_filter :: Filter_protocol_dstIP_dstPort(protocol, dstIP, dstPort);
inapp_packet_distributer :: Hash_srcIP_srcPort(srcIP, srcPort);
queue_selector :: SelectQueueIndex(hash);
other_apps_queue :: RXQueue;
Q1 :: RXQueue;
Q2 :: RXQueue;
Q3 :: RXQueue;
Qn :: RXQueue;

core_1 :: Core;
core_2 :: Core;
core_3 :: Core;
core_n :: Core;

app_thread_1 :: Thread;
app_thread_2 :: Thread;
app_thread_3 :: Thread;
app_thread_n :: Thread;

TXQ1 :: TXQueue;
TXQ2 :: TXQueue;
TXQ3 :: TXQueue;
TXQn :: TXQueue;




new_packet -> app_selection_filter;
app_selection_filter[0] -> inapp_packet_distributer;
app_selection_filter[1] -> other_apps_queue;
inapp_packet_distributer -> queue_selector;

queue_selector -> Q1 -> core_1 -> app_thread_1 -> TXQ1 -> send_packets;
queue_selector -> Q2 -> core_2 -> app_thread_2 -> TXQ2 -> send_packets;
queue_selector -> Q3 -> core_3 -> app_thread_3 -> TXQ3 -> send_packets;
queue_selector -> Qn -> core_n -> app_thread_n -> TXQn -> send_packets;

/*
Q1 -> core_1;
Q2 -> core_2;
Q3 -> core_3;
Qn -> core_n;

core_1 -> app_thread_1 -> TXQ1 -> send_packets;
core_2 -> app_thread_2 -> TXQ2 -> send_packets;
core_3 -> app_thread_3 -> TXQ3 -> send_packets;
core_n -> app_thread_n -> TXQ4 -> send_packets;
*/

