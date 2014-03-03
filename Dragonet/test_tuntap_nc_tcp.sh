#!/bin/bash


SERVERPORT=1234
#IPADDR="127.0.0.1"  # for localhost
IPADDR="192.168.123.1"

CLIENTPORT=2231

INFILE="./test_nc_msg.txt"
OUTFILE="./test_nc_answer.txt"

#nc.traditional -n -vv -t -p 4567 192.168.123.1 "${SERVERPORT}"
nc.traditional -n -vv -i 1 -q 1 -t -p ${CLIENTPORT} ${IPADDR} ${SERVERPORT} < ${INFILE} &> ${OUTFILE}

cat ${OUTFILE}

# On server side, in case you want to test
# nc.traditional -n -vv -t -l -p ${SERVERPORT}

