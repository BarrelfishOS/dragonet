#!/bin/bash
#./plot_scale_data.sh ../nsdi15Results/loadBalacingP1024/ ../nsdi15Results/loadBalacingP1024_Plots 1024 "load balancing in echoServer"

./plot_scale_data.sh ./nsdi_data/Test_udp_rr/CONCUR_1/PKT_1024/NIC_Intel/ ./nsdi_data_graphs/Test_udp_rr/CONCUR_1/PKT_1024/NIC_Intel/ 1024 "test graph for Intel with Echo Server"

./plot_scale_data.sh ./nsdi_data/Test_udp_rr/CONCUR_1/PKT_64/NIC_Intel/ ./nsdi_data_graphs/Test_udp_rr/CONCUR_1/PKT_64/NIC_Intel/ 64 "test graph for Intel with Echo Server"
