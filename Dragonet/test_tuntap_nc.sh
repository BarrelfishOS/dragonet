#!/bin/bash
RANDOMPORT=51099
DNSPORT=51098
ECHOPORT=5556
nc.traditional -n -vv -u -p 5555 192.168.123.1 "${ECHOPORT}"
#nc.traditional -n -vv -u -p 5555 192.168.123.1 "${DNSPORT}"

