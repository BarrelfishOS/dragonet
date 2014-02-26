#!/bin/bash


SERVERPORT=1234
#IPADDR="127.0.0.1"  # for localhost
IPADDR="192.168.123.1"

CLIENTPORT=1234

INFILE="./test_nc_msg.txt"

#nc.traditional -n -vv -t -p 4567 192.168.123.1 "${SERVERPORT}"
nc.traditional -n -vv -i 1 -q 1 -t -p ${CLIENTPORT} ${IPADDR} ${SERVERPORT} < ${INFILE}

# On server side, in case you want to test
# nc.traditional -n -vv -t -l -p ${SERVERPORT}

