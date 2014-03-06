#!/bin/bash

function setemmentalerASrouter()
{
    gwip=$1
    /sbin/route del default
    /sbin/route add default gw ${gwip}
    /sbin/route -n
}


function setDHCPIPaddr()
{
    MAC=$1
    IP=$2
    HNAME=$3
    SNAME=$4
    IFACE=`ifconfig -a | grep -i $MAC | cut -d' ' -f1`
    if [ ! -z $IFACE ]
    then
        echo "MAC $MAC found in interface $IFACE"
        ifconfig $IFACE up
        dhclient $IFACE
        hostname $HNAME
        echo "use $IP $SNAME for ssh"
    fi
    echo "$IP $SNAME # $HNAME $MAC $IP $SNAME" >> hosts.txt_new

    echo "" >> ssh_config.txt_new
    echo "HOST $SNAME*" >> ssh_config.txt_new
    echo "  User ubuntu" >> ssh_config.txt_new
}

function setIPaddr()
{
    MAC=$1
    IP=$2
    HNAME=$3
    SNAME=$4
    USED=$5
    IFACE=`ifconfig -a | grep -i $MAC | cut -d' ' -f1`
    if [ ! -z $IFACE ]
    then
        echo "MAC $MAC found in interface $IFACE"
        ifconfig $IFACE up
        ifconfig $IFACE $IP netmask 255.255.255.0
        ifconfig $IFACE
        if [ "$USED" == 1 ]; then
            echo "$IFACE" > ${IFACE_NAME_FILE}
        fi
    fi
    echo "$IP $SNAME # $HNAME $MAC $IP $SNAME" >> hosts.txt_new

}



check_file_exist1() {
    FNAME=$1
    if [ ! -f ${FNAME} ] ;  then
        echo "ERROR: file ${FNAME} does not exist!"
        exit 1
    fi
}


#source ./common.h

if [ -f "./common.h" ] ;  then
    source ./common.h
    IFACE_NAME_FILE="../minfo/used_if.log"
    mkdir -p `dirname $IFACE_NAME_FILE`
    cp ssh_config.txt ssh_config.txt_new
    cp hosts.txt hosts.txt_new
else
    IFACE_NAME_FILE="./minfo/used_if.log"
    mkdir -p `dirname $IFACE_NAME_FILE`
    echo "running in stand-alone mode!!"
    echo "and storing useful information in ${IFACE_NAME_FILE}!!"
fi

echo "for gottardo"
setDHCPIPaddr "00:30:48:fe:58:1e"  "10.110.4.67" "gottardo" "gottardo"
setIPaddr "00:1b:21:8f:1b:20" "10.113.4.49" "gottardo" "g1" 1
setIPaddr "00:1b:21:8f:1b:21" "10.112.4.72" "gottardo" "gd"
#setIPaddr "00:0f:53:07:4d:64" "10.113.4.44" "gottardo" "gs3"  # SF
#setIPaddr "00:0f:53:07:4d:65" "10.113.4.48" "gottardo" "gs4"  # SF

echo "for sbrinz1"
setDHCPIPaddr "00:30:48:d0:d0:ce" "10.110.4.26" "sbrinz1" "sbrinz1"
setIPaddr "00:1b:21:8f:1a:c0" "10.113.4.35" "sbrinz1" "sb11" 1
setIPaddr "00:1b:21:8f:1a:c1" "10.112.4.36" "sbrinz1" "sb1d"


echo "for ziger1"
setDHCPIPaddr "00:21:28:6B:98:ba" "10.110.4.51" "ziger1" "ziger1"
setIPaddr "00:1b:21:8f:18:64" "10.113.5.37" "ziger1" "z11d"  # not connected
setIPaddr "00:1b:21:8f:18:65" "10.113.4.38" "ziger1" "z12" 1


echo "for ziger2"
setDHCPIPaddr "00:21:28:6B:98:a6" "10.110.4.57" "ziger2" "ziger2"
setIPaddr "00:0f:53:07:48:d4" "10.113.4.39" "ziger2" "z2s1" # SF
setIPaddr "00:0f:53:07:48:d5" "10.113.5.43" "ziger2" "z2s2d" # SF # not connected

echo "for appenzeller"
setDHCPIPaddr "00:25:64:fc:61:dd" "10.110.4.64" "appenzeller" "appenzeller"
setIPaddr "00:0f:53:07:51:49" "10.113.4.71" "appenzeller" "a1" # SF

ROUTER="10.110.4.4"
#echo "setting ${ROUTER} as router"
#setemmentalerASrouter ${ROUTER}

# following steps are done in setTools.sh script

#cat hosts.txt_new >> /etc/hosts

#mkdir -p /root/.ssh/
#mkdir -p ~/.ssh/
#cat config.txt_new >> ~/.ssh/config
#cat config.txt_new >> /root/.ssh/config

#rm -f hosts.txt_new
#rm -f config.txt_new

