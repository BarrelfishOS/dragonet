// RXPath.click
/*
This configuration refers to the ideal configuration for DNS server 
*/


new_packet :: Arrival();      // Incoming packets on RX PHY
//drop_packet :: Discard();  // Dropped packets (for any reasons)


// ********** Validate packet *****************
app_selection_filter :: Filter_protocol_dstIP_dstPort(protocol, dstIP, dstPort);
inapp_load_balancer :: Hash_srcIP_srcPort(srcIP, srcPort);
inapp_redirection_table :: TableLookup(hash);
//queue_selector :: SelectQueueIndex(hash);
other_apps_queue :: RXQueue;
Q1 :: RXQueue;
Q2 :: RXQueue;
Q3 :: RXQueue;
Qn :: RXQueue;

core_1 :: Core;
core_2 :: Core;
core_3 :: Core;
core_n :: Core;

new_packet -> app_selection_filter;
app_selection_filter -> inapp_load_balancer;
app_selection_filter -> other_apps_queue;
inapp_load_balancer -> inapp_redirection_table; 
inapp_redirection_table -> Q1;
inapp_redirection_table -> Q2;
inapp_redirection_table -> Q3;
inapp_redirection_table -> Qn;

/*
notification_generator :: NotificationGenerator(coreID);
inapp_redirection_table -> notification_generator;
notification_generator -> core_1;
notification_generator -> core_2;
notification_generator -> core_3;
notification_generator -> core_n;
*/

Q1 -> core_1;
Q2 -> core_2;
Q3 -> core_3;
Qn -> core_n;


/*
core_1 -> Q1;
core_2 -> Q2;
core_3 -> Q3;
core_n -> Qn;
*/