#!/bin/bash

function setDHCPIPaddr()
{
    MAC=$1
    IP=$2
    HNAME=$3
    IFACE=`ifconfig -a | grep -i $MAC | cut -d' ' -f1`
    if [ ! -z "${IFACE}" ]
    then
        echo "MAC $MAC found in interface $IFACE"
        is_assigned=`ifconfig $IFACE | grep "${IP}"`
        if [ -z "${is_assigned}" ] ; then
            sudo ifconfig $IFACE up
            sudo dhclient $IFACE
        fi
        is_named=`hostname | grep "${HNAME}"`
        if [ -z "${is_named}" ] ; then
            sudo hostname $HNAME
        fi
    fi
    echo "$IP $HNAME # $HNAME, $MAC, $IP, $HNAME" >> ${HOSTS_FILE}
}

function setIPaddr()
{
    MAC=$1
    IP=$2
    HNAME=$3
    NICNAME=$4
    CTYPE=$5
    DEST=$6
    PCI_ADDR=$7
    CABLE=$8
    CONNECTNAME="${HNAME}-${NICNAME}-${CTYPE}"
    IFACE=`ifconfig -a | grep -i $MAC | cut -d' ' -f1`
    if [ ! -z "${IFACE}" ]
    then
        echo "MAC $MAC found in interface $IFACE"
        if [ "${CTYPE}" != "no" ] ; then

            is_assigned=`ifconfig $IFACE | grep "${IP}"`
            if [ -z "${is_assigned}" ]; then
                sudo ifconfig $IFACE up
                sudo ifconfig $IFACE $IP netmask 255.255.255.0
                ifconfig $IFACE
            fi
            echo "$IFACE, ${NICNAME}, ${MAC}, ${IP}, ${CTYPE}, ${DEST}, ${CONNECTNAME}" >> ${IFACE_NAME_FILE}
        else
            echo "Egnoring it as it is not connected"
        fi
    fi
    echo "${IP} ${CONNECTNAME} # ${HNAME}, ${MAC}, ${IP}, ${NICNAME}, ${CTYPE}, ${DEST}" >> ${HOSTS_FILE}
}


IFACE_NAME_FILE="./minfo/used_if.log"
HOSTS_FILE="./minfo/hosts.txt"
mkdir -p `dirname $IFACE_NAME_FILE`
echo "" > ${IFACE_NAME_FILE}
echo "" > ${HOSTS_FILE}

echo "for sbrinz1"
setDHCPIPaddr "00:30:48:d0:d0:ce" "10.110.4.26" "sbrinz1"
setIPaddr "00:1b:21:8f:1a:c0" "10.113.4.26" "sbrinz1" "intel1" "switch" "12" "" "cable-nc10-m1"
setIPaddr "00:1b:21:8f:1a:c0" "10.44.4.26" "sbrinz1" "intel1" "no" "no" "" ""

echo "for sbrinz2"
setDHCPIPaddr "00:30:48:d7:99:58" "10.110.4.29" "sbrinz2"
setIPaddr "90:e2:ba:3a:6d:20" "10.22.4.29" "sbrinz2" "intel1" "direct" "gruyere-intel1" "0000:85:00.0" "cable-nc10-m3"
setIPaddr "90:e2:ba:3a:6d:21" "10.113.4.29" "sbrinz2" "intel2" "switch" "18" "0000:85:00.1" "cable-short-thin"

echo "for ziger1"
setDHCPIPaddr "00:21:28:6B:98:ba" "10.110.4.51" "ziger1"
setIPaddr "00:1b:21:8f:18:64" "10.22.4.51" "ziger1" "intel1" "direct" "appenzeller-sf1" "" "cable-144"
setIPaddr "00:1b:21:8f:18:65" "10.113.4.51" "ziger1" "intel2" "switch" "17" "" "cable-2"

echo "for ziger2"
setDHCPIPaddr "00:21:28:6B:98:a6" "10.110.4.57" "ziger2"
setIPaddr "00:0f:53:07:48:d4" "10.113.4.57" "ziger2" "sf1" "switch" "19" "" "cable-3"
setIPaddr "00:0f:53:07:48:d5" "10.44.4.57" "ziger2" "sf2" "no" "no" "" "cable-red-tag"

echo "for gottardo"
setDHCPIPaddr "00:30:48:fe:58:1e"  "10.110.4.67" "gottardo"
setIPaddr "00:1b:21:8f:1b:20" "10.113.4.67" "gottardo" "intel1" "switch" "9" "" "cable-7"
setIPaddr "00:1b:21:8f:1b:21" "10.44.4.67" "gottardo" "intel2" "no" "no" "" ""

echo "for appenzeller"
setDHCPIPaddr "00:25:64:fc:61:dd" "10.110.4.64" "appenzeller"
setIPaddr "00:0f:53:07:51:48" "10.22.4.64" "appenzeller" "sf1" "direct" "ziger1-intel1" "" "cable-144"
setIPaddr "00:0f:53:07:51:49" "10.113.4.71" "appenzeller" "sf2" "switch" "8" "" "cable-6"

echo "for gruyere"
setDHCPIPaddr "00:e0:81:b1:ca:20" "10.110.4.20" "gruyere"
setIPaddr "90:e2:ba:3a:ae:9c" "10.22.4.20" "gruyere" "intel1" "direct" "sbrinz2-intel1" "" "nc-10"
setIPaddr "90:e2:ba:3a:ae:9d" "10.113.4.20" "gruyere" "intel2" "switch" "24" "" "cable-noname"

echo "for babybel"
setDHCPIPaddr "00:1e:67:92:53:17" "10.110.4.92" "babybel"
setIPaddr "00:1e:67:9f:0c:ca" "10.113.4.82" "babybel" "intel1" "no" "no" "" ""
setIPaddr "de:f2:f8:2f:80:f0" "10.113.4.81" "babybel" "intel2" "switch" "15" "" "cable-pat206310"

echo "for asiago"
setDHCPIPaddr "00:1e:67:92:48:4a" "10.110.4.95" "asiago"
setIPaddr "00:1e:67:9f:45:06" "10.22.4.95" "asiago" "intel1" "direct" "burrata-intel2" "0000:04:00.0" ""
setIPaddr "00:1e:67:9f:45:07" "10.113.4.95" "asiago" "intel2" "switch" "21" "0000:04:00.1" "cable-pat206309"
setIPaddr "00:0f:53:07:4d:64" "10.113.4.195" "asiago" "sf1" "switch" "22" "" "cable-red"
setIPaddr "00:0f:53:07:4d:65" "10.23.4.195" "asiago" "sf2" "direct" "burrata-intel1" "" ""

echo "for burrata"
setDHCPIPaddr "00:1e:67:92:52:d1" "10.110.4.96" "burrata"
setIPaddr "00:1e:67:9f:4f:de" "10.22.4.96" "burrata" "intel1" "direct" "asiago-sf2" ""
setIPaddr "00:1e:67:9f:4f:df" "10.22.4.196" "burrata" "intel2" "direct" "asiago-intel1" ""

setDHCPIPaddr "00:21:28:3b:3f:9e" "10.110.4.41" "tomme1"
setIPaddr "90:e2:ba:3a:a8:40" "10.44.4.41" "tomme1" "intel1" "no" "no" "" ""
#setIPaddr "90:e2:ba:3a:a8:40" "10.44.4.41" "tomme1" "intel1" "no" "no" "" ""

