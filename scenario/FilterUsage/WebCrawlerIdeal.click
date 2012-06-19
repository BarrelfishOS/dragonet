// WebCrawlerIdeal.click
/*
This configuration refers to the ideal configuration for DNS server 
*/


new_packet :: Arrival();      // Incoming packets on RX PHY
send_packets :: Send();

app_selection_filter :: Filter_protocol_dstIP_dstPort(protocol, dstIP, dstPort);
inapp_flow_director :: Hash_srcIP_srcPort(srcIP, srcPort);

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

connect_1 :: Connect;
connect_2 :: Connect;
connect_3 :: Connect;
connect_n :: Connect;


TXQ1 :: TXQueue;
TXQ2 :: TXQueue;
TXQ3 :: TXQueue;
TXQn :: TXQueue;

/*
send_packets :: Discard();
app_thread_1 :: Thread;
connect_1 :: Connect;
TXQ1 :: TXQueue;
*/

inapp_flow_table :: FlowTable;

network:: Network;

app_thread_1 -> connect_1 -> TXQ1 -> send_packets;
app_thread_2 -> connect_2 -> TXQ2 -> send_packets;
app_thread_3 -> connect_3 -> TXQ3 -> send_packets;
app_thread_n -> connect_n -> TXQn -> send_packets;

connect_1 -> inapp_flow_table;
connect_2 -> inapp_flow_table;
connect_3 -> inapp_flow_table;
connect_n -> inapp_flow_table;

inapp_flow_table -> [1]queue_selector;

send_packets -> network;

network -> new_packet;

// The actual packet flow
new_packet -> app_selection_filter;
app_selection_filter[0] -> other_apps_queue;

app_selection_filter[1] -> inapp_flow_director;
inapp_flow_director -> [0]queue_selector;

queue_selector -> Q1 -> core_1;
queue_selector -> Q2 -> core_2;
queue_selector -> Q3 -> core_3;
queue_selector -> Qn -> core_n;


/*
queue_selector -> Q1 -> core_1 -> app_thread_1 -> TXQ1 -> send_packets;
queue_selector -> Q2 -> core_2 -> app_thread_2 -> TXQ2 -> send_packets;
queue_selector -> Q3 -> core_3 -> app_thread_3 -> TXQ3 -> send_packets;
queue_selector -> Qn -> core_n -> app_thread_n -> TXQn -> send_packets;


*/