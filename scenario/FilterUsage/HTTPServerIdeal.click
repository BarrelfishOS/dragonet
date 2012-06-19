// HTTPServerIdeal.click
/*
This configuration refers to the ideal configuration for HTTP server 
*/


new_packet :: Arrival();      // Incoming packets on RX PHY
send_packets :: Discard();  

app_selection_filter :: Filter_protocol_dstIP_dstPort(protocol, dstIP, dstPort);
inapp_load_balancer :: Round_robin_schedule();
inapp_flow_director :: Hash_srcIP_srcPort(srcIP, srcPort);
inapp_redirection_table :: TableLookup(hash);
inapp_syn_filter :: Filter_syn_packets();

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


// The actual packet flow
new_packet -> app_selection_filter;
app_selection_filter[0] -> other_apps_queue;

app_selection_filter[1] -> inapp_syn_filter;
inapp_syn_filter[0] -> inapp_load_balancer;
inapp_syn_filter[1] -> inapp_flow_director;
inapp_flow_director -> queue_selector;
inapp_load_balancer -> inapp_redirection_table;
inapp_redirection_table -> queue_selector;


queue_selector -> Q1 -> core_1 -> app_thread_1 -> TXQ1 -> send_packets;
queue_selector -> Q2 -> core_2 -> app_thread_2 -> TXQ2 -> send_packets;
queue_selector -> Q3 -> core_3 -> app_thread_3 -> TXQ3 -> send_packets;
queue_selector -> Qn -> core_n -> app_thread_n -> TXQn -> send_packets;
