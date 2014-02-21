#!/bin/bash
RANDOMPORT=51099
# Echo on DNS port works
DNSPORT=51098

# Echo on echo port does not works
ECHOPORT=5556

#nc.traditional -n -vv -u -p 5555 192.168.123.1 "${ECHOPORT}"
nc.traditional -n -vv -u -p 5555 192.168.123.1 "${DNSPORT}"

