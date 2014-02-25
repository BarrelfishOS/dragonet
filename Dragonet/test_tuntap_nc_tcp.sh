#!/bin/bash

#ECHOPORT=5556
ECHOPORT=1234
ECHOPORT2=1235

nc.traditional -n -vv -t -p 4567 192.168.123.1 "${ECHOPORT}"

