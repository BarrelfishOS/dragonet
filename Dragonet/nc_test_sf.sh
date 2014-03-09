#!/bin/bash
nc.traditional -n -vv -s 10.113.4.38 -u -p 5555 10.113.4.71 5556

# ping -I eth3 10.113.4.71
# # to find the proper interface
# ifconfig -a | grep "00:1b:21:8f:18:65"

