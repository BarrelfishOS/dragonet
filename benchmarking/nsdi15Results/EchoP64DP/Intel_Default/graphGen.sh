./netperf-wrapper -p bbox -o ../netperfScaleResults/Linux/rP64//Intel_S//fancyEchoLinux/udp/P_64/HWQ_1/2/udp_rr//TP_MAX/scalability-TPS-Intel_S-fancyEchoLinux-udp.png -i `find ../netperfScaleResults/Linux/rP64//Intel_S//fancyEchoLinux/udp/P_64/HWQ_1/2/udp_rr//TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
./netperf-wrapper -p bboxNet -o ../netperfScaleResults/Linux/rP64//Intel_S//fancyEchoLinux/udp/P_64/HWQ_1/2/udp_rr//TP_MAX/scalability-BW-Intel_S-fancyEchoLinux-udp.png -i `find ../netperfScaleResults/Linux/rP64//Intel_S//fancyEchoLinux/udp/P_64/HWQ_1/2/udp_rr//TP_MAX/ -name '*.json*' | grep -i 'best' | sort`
