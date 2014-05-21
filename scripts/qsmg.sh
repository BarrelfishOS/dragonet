#!/bin/bash

#set -x
set -e

############################################3
## RX

function xdo() {
	cmd=$*
	echo " " $cmd
	eval $cmd
}

function check_rps_qs() {
	f0="/sys/class/net/$iface/queues/rx-0/rps_cpus"
	if [ ! -f  $f0 ]; then
		echo "$f0 does not exist: bailing out"
		exit 1
	fi
}

function max_rxq() {
	maxq=0
	for f in /sys/class/net/$iface/queues/rx-*/rps_cpus; do
	    q=$(echo $f | sed -e 's/.*rx-\([[:digit:]]\+\).*/\1/')
	    if [ $q -gt $maxq ]; then
	        maxq=$q
	    fi
	done
	echo $maxq
}


function rps_rr() {

	check_rps_qs
	maxq=$(max_rxq)

	nqueues=$((maxq+1))
	qentries=$(( $rps_entries / $nqueues ))

	xdo $(printf "echo %s > %s\n" $rps_entries "/proc/sys/net/core/rps_sock_flow_entries")
	for q in $(seq 0 $maxq); do
	    c=$(( $q % $ncpus))
	    f="/sys/class/net/$iface/queues/rx-$q/rps_cpus"
	    echo "# Assigning RX Queue $q to cpu $c"
	    xdo $(printf "echo %x > %s\n" $(( 1<<$c )) $f)
	    xdo "echo $qentries > /sys/class/net/$iface/queues/rx-$q/rps_flow_cnt"
	done
	echo
}

function rps_reset() {

	check_rps_qs
	maxq=$(max_rxq)

	nqueues=$((maxq+1))
	qentries=$(( $rps_entries / $nqueues ))

	xdo "echo 0 > /proc/sys/net/core/rps_sock_flow_entries"
	for q in $(seq 0 $maxq); do
	    c=$(( $q % $ncpus))
	    f="/sys/class/net/$iface/queues/rx-$q/rps_cpus"
	    xdo "echo 0 > $f"
	    xdo "echo 0 > /sys/class/net/$iface/queues/rx-$q/rps_flow_cnt"
	done
	echo
}

function rps_pr() {
	check_rps_qs
	maxq=$(max_rxq)

	nqueues=$((maxq+1))
	qentries=$(( $rps_entries / $nqueues ))

	echo "/sys/class/net/$iface/queues/rx-XX/rps_cpus:"
	for q in $(seq 0 $maxq); do
		c=$(( $q % $ncpus))
		f="/sys/class/net/$iface/queues/rx-$q/rps_cpus"
		printf "%7s :: %s\n" "rx-$q" $(cat $f)
	done
}

############################################3
## TX

function check_xps_qs() {
	f0="/sys/class/net/$iface/queues/tx-0/xps_cpus"
	if [ ! -f  $f0 ]; then
		echo "$f0 does not exist: bailing out"
		exit 1
	fi
}

function max_txq() {
	maxq=0
	for f in /sys/class/net/$iface/queues/tx-*/xps_cpus; do
	    q=$(echo $f | sed -e 's/.*tx-\([[:digit:]]\+\).*/\1/')
	    if [ $q -gt $maxq ]; then
	        maxq=$q
	    fi
	done
	echo $maxq
}

function xps_rr() {

	check_xps_qs
	maxq=$(max_txq)

	nqueues=$((maxq+1))
	for q in $(seq 0 $maxq); do
	    c=$(( $q % $ncpus))
	    f="/sys/class/net/$iface/queues/tx-$q/xps_cpus"
	    echo "# Assigning TX Queue $q to cpu $c"
	    xdo $(printf "echo %x > %s\n" $(( 1<<$c )) $f)
	done
	echo
}

function xps_reset() {

	check_xps_qs
	maxq=$(max_txq)

	nqueues=$((maxq+1))
	for q in $(seq 0 $maxq); do
	    c=$(( $q % $ncpus))
	    f="/sys/class/net/$iface/queues/tx-$q/xps_cpus"
	    xdo "echo 0 > $f"
	done
	echo
}

function xps_pr() {
	check_xps_qs
	maxq=$(max_txq)

	nqueues=$((maxq+1))
	qentries=$(( $rps_entries / $nqueues ))

	echo "/sys/class/net/$iface/queues/tx-XX/xps_cpus:"
	for q in $(seq 0 $maxq); do
		c=$(( $q % $ncpus))
		f="/sys/class/net/$iface/queues/tx-$q/xps_cpus"
		printf "%7s :: %s\n" "tx-$q" $(cat $f)
	done
}

############################################3
## Flows

function flows_reset() {
	 for fid  in $(/sbin/ethtool -n $iface | sed -n 's/Filter: \([[:digit:]]\+\)/\1/p')
	 do
		echo /sbin/ethtool -N $iface delete $fid
	 done
}

function udp_flows_src_port() {
	srcp0=$1
	nqs=$2
	q0=0;

	echo $(($q0 + $nqs - 1))
	for i in $(seq 0 $(($nqs - 1)))
	do
		q=$(( $q0 + $i))
		srcp=$(( $srcp0 + $i))
		xdo "/sbin/ethtool -N $iface flow-type udp4 src-port $srcp action $q"
	done
}

function udp_flows_dst_port() {
	dstp0=$1
	nqs=$2
	q0=0;

	echo $(($q0 + $nqs - 1))
	for i in $(seq 0 $(($nqs - 1)))
	do
		q=$(( $q0 + $i))
		dstp=$(( $dstp0 + $i))
		xdo "/sbin/ethtool -N $iface flow-type udp4 dst-port $dstp action $q"
	done
}

if [ -z "$2" ]; then
    echo "Usage: $0 [iface] [commands...]"
    echo "Commands: rps_rr"
    echo "          xps_rr"
    echo "          rps_pr"
    echo "          xps_pr"
    echo "          rps_reset"
    echo "          xps_reset"
    echo "          fl_reset"
    echo "          udpfl_srcp <src_port0> <nqueues>"
    echo "          udpfl_dstp <dst_port0> <nqueues>"
    echo "          rps_pr"
    echo "          xps_pr"
    exit 1
fi

iface=$1
shift

ncpus=$(nproc)
rps_entries=32768


while [ "$#" -ne 0 ]
do
	cmd=$1
	shift
	case $cmd in

		rps_rr) rps_rr;;
		rps_pr) rps_pr;;
		rps_reset) rps_reset;;
		xps_rr) xps_rr;;
		xps_pr) xps_pr;;
		xps_reset) xps_reset;;

		fl_reset) flows_reset;;

		udpfl_srcp)
			if [ -z "$2" ]; then
				echo: "udpfl_srcp <src_port0> <nqueues>"
				exit 1
			fi;
			udp_flows_src_port $1 $2;
			shift 2
			;;

		udpfl_dstp)
			if [ -z "$2" ]; then
				echo: "udpfl_dstp <dst_port0> <nqueues>"
				exit 1
			fi;
			udp_flows_dst_port $1 $2;
			shift 2
			;;
		*)
			echo "Unknown command: $cmd"; exit 1;;
	esac
done

