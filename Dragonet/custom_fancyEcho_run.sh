#!/bin/bash

### for memcached
#sudo ../benchmarking/memcached/memcached -N f[${SERVERIP}:7777/10.113.4.71:0]F[${SERVERIP}:7777/10.113.4.29:0]F[${SERVERIP}:7777/10.113.4.51:0]F[${SERVERIP}:7777/10.113.4.57:0] -c 64000 -m 64000 -u root -p 0 -U  7777 -t 8 -l ${SERVERIP}

#sudo ../benchmarking/memcached/memcached -N p[7777] -c 64000 -m 64000 -u root -p 0 -U  7777 -t 1 -l ${SERVERIP}
#sudo ../benchmarking/memcached/memcached -N T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 2 -l ${SERVERIP}

run_memcached_big() {
sudo ../benchmarking/memcached/memcached -N \
T0.p[7777]F[10.113.4.26:7777/10.113.4.51:8000]F[10.113.4.26:7777/10.113.4.51:8001]F[10.113.4.26:7777/10.113.4.51:8002]F[10.113.4.26:7777/10.113.4.51:8003]F[10.113.4.26:7777/10.113.4.57:8000]F[10.113.4.26:7777/10.113.4.57:8001]F[10.113.4.26:7777/10.113.4.57:8002]F[10.113.4.26:7777/10.113.4.57:8003]F[10.113.4.26:7777/10.113.4.29:8000]F[10.113.4.26:7777/10.113.4.29:8001]F[10.113.4.26:7777/10.113.4.29:8002]F[10.113.4.26:7777/10.113.4.29:8003]F[10.113.4.26:7777/10.113.4.51:8004]F[10.113.4.26:7777/10.113.4.51:8005]F[10.113.4.26:7777/10.113.4.51:8006]F[10.113.4.26:7777/10.113.4.51:8007]F[10.113.4.26:7777/10.113.4.57:8004]F[10.113.4.26:7777/10.113.4.57:8005]F[10.113.4.26:7777/10.113.4.57:8006]F[10.113.4.26:7777/10.113.4.57:8007]F[10.113.4.26:7777/10.113.4.29:8004]F[10.113.4.26:7777/10.113.4.29:8005]F[10.113.4.26:7777/10.113.4.29:8006]F[10.113.4.26:7777/10.113.4.29:8007]F[10.113.4.26:7777/10.113.4.51:8008]F[10.113.4.26:7777/10.113.4.51:8009]F[10.113.4.26:7777/10.113.4.51:8010]F[10.113.4.26:7777/10.113.4.51:8011]F[10.113.4.26:7777/10.113.4.57:8008]F[10.113.4.26:7777/10.113.4.57:8009]F[10.113.4.26:7777/10.113.4.57:8010]F[10.113.4.26:7777/10.113.4.57:8011]F[10.113.4.26:7777/10.113.4.29:8008]F[10.113.4.26:7777/10.113.4.29:8009]F[10.113.4.26:7777/10.113.4.29:8010]F[10.113.4.26:7777/10.113.4.29:8011]F[10.113.4.26:7777/10.113.4.51:8012]F[10.113.4.26:7777/10.113.4.51:8013]F[10.113.4.26:7777/10.113.4.51:8014]F[10.113.4.26:7777/10.113.4.51:8015]F[10.113.4.26:7777/10.113.4.57:8012]F[10.113.4.26:7777/10.113.4.57:8013]F[10.113.4.26:7777/10.113.4.57:8014]F[10.113.4.26:7777/10.113.4.57:8015]F[10.113.4.26:7777/10.113.4.29:8012]F[10.113.4.26:7777/10.113.4.29:8013]F[10.113.4.26:7777/10.113.4.29:8014]F[10.113.4.26:7777/10.113.4.29:8015]F[10.113.4.26:7777/10.113.4.51:8016]F[10.113.4.26:7777/10.113.4.51:8017]F[10.113.4.26:7777/10.113.4.51:8018]F[10.113.4.26:7777/10.113.4.51:8019]F[10.113.4.26:7777/10.113.4.57:8016]F[10.113.4.26:7777/10.113.4.57:8017]F[10.113.4.26:7777/10.113.4.57:8018]F[10.113.4.26:7777/10.113.4.57:8019]F[10.113.4.26:7777/10.113.4.29:8016]F[10.113.4.26:7777/10.113.4.29:8017]F[10.113.4.26:7777/10.113.4.29:8018]F[10.113.4.26:7777/10.113.4.29:8019]F[10.113.4.26:7777/10.113.4.51:8020]F[10.113.4.26:7777/10.113.4.51:8021]F[10.113.4.26:7777/10.113.4.51:8022]F[10.113.4.26:7777/10.113.4.51:8023]F[10.113.4.26:7777/10.113.4.57:8020]F[10.113.4.26:7777/10.113.4.57:8021]F[10.113.4.26:7777/10.113.4.57:8022]F[10.113.4.26:7777/10.113.4.57:8023]F[10.113.4.26:7777/10.113.4.29:8020]F[10.113.4.26:7777/10.113.4.29:8021]F[10.113.4.26:7777/10.113.4.29:8022]F[10.113.4.26:7777/10.113.4.29:8023]F[10.113.4.26:7777/10.113.4.51:8024]F[10.113.4.26:7777/10.113.4.51:8025]F[10.113.4.26:7777/10.113.4.51:8026]F[10.113.4.26:7777/10.113.4.51:8027]F[10.113.4.26:7777/10.113.4.57:8024]F[10.113.4.26:7777/10.113.4.57:8025]F[10.113.4.26:7777/10.113.4.57:8026]F[10.113.4.26:7777/10.113.4.57:8027]F[10.113.4.26:7777/10.113.4.29:8024]F[10.113.4.26:7777/10.113.4.29:8025]F[10.113.4.26:7777/10.113.4.29:8026]F[10.113.4.26:7777/10.113.4.29:8027]F[10.113.4.26:7777/10.113.4.51:8028]F[10.113.4.26:7777/10.113.4.51:8029]F[10.113.4.26:7777/10.113.4.51:8030]F[10.113.4.26:7777/10.113.4.51:8031]F[10.113.4.26:7777/10.113.4.57:8028]F[10.113.4.26:7777/10.113.4.57:8029]F[10.113.4.26:7777/10.113.4.57:8030]F[10.113.4.26:7777/10.113.4.57:8031]F[10.113.4.26:7777/10.113.4.29:8028]F[10.113.4.26:7777/10.113.4.29:8029]F[10.113.4.26:7777/10.113.4.29:8030]F[10.113.4.26:7777/10.113.4.29:8031]F[10.113.4.26:7777/10.113.4.51:8032]F[10.113.4.26:7777/10.113.4.51:8033]F[10.113.4.26:7777/10.113.4.51:8034]F[10.113.4.26:7777/10.113.4.51:8035]F[10.113.4.26:7777/10.113.4.57:8032]F[10.113.4.26:7777/10.113.4.57:8033]F[10.113.4.26:7777/10.113.4.57:8034]F[10.113.4.26:7777/10.113.4.57:8035]F[10.113.4.26:7777/10.113.4.29:8032]F[10.113.4.26:7777/10.113.4.29:8033]F[10.113.4.26:7777/10.113.4.29:8034]F[10.113.4.26:7777/10.113.4.29:8035]F[10.113.4.26:7777/10.113.4.51:8036]F[10.113.4.26:7777/10.113.4.51:8037]F[10.113.4.26:7777/10.113.4.51:8038]F[10.113.4.26:7777/10.113.4.51:8039]F[10.113.4.26:7777/10.113.4.57:8036]F[10.113.4.26:7777/10.113.4.57:8037]F[10.113.4.26:7777/10.113.4.57:8038]F[10.113.4.26:7777/10.113.4.57:8039]F[10.113.4.26:7777/10.113.4.29:8036]F[10.113.4.26:7777/10.113.4.29:8037]F[10.113.4.26:7777/10.113.4.29:8038]F[10.113.4.26:7777/10.113.4.29:8039]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 4 -l 10.113.4.26
}



run_memcached_for_1_cores() {
sudo ../benchmarking/memcached/memcached -N T0.p[7777]F[${SERVERIP}:7777/10.113.4.71:8000]F[${SERVERIP}:7777/10.113.4.29:8000]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.51:8001]F[${SERVERIP}:7777/10.113.4.57:8001]F[${SERVERIP}:7777/10.113.4.71:8001]F[${SERVERIP}:7777/10.113.4.29:8001] -c 64000 -m 64000 -u root -p 0 -U  7777 -t 1 -l ${SERVERIP}
}

run_memcached_4_clients_2_load() {
sudo ../benchmarking/memcached/memcached -N \
T0.p[7777]F[10.113.4.95:7777/10.113.4.51:8000]F[10.113.4.95:7777/10.113.4.51:8001]F[10.113.4.95:7777/10.113.4.57:8000]F[10.113.4.95:7777/10.113.4.57:8001]F[10.113.4.95:7777/10.113.4.26:8000]F[10.113.4.95:7777/10.113.4.26:8001]F[10.113.4.95:7777/10.113.4.29:8000]F[10.113.4.95:7777/10.113.4.29:8001]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 5 -l 10.113.4.95
}

run_memcached_2_clients_2_load() {
sudo ../benchmarking/memcached/memcached -N \
T0.p[7777]F[10.113.4.95:7777/10.113.4.51:8000]F[10.113.4.95:7777/10.113.4.51:8001]F[10.113.4.95:7777/10.113.4.26:8000]F[10.113.4.95:7777/10.113.4.26:8001]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 5 -l 10.113.4.95
}

run_memcached_2_clients_3_load() {
sudo ../benchmarking/memcached/memcached -N \
T0.p[7777]F[10.113.4.95:7777/10.113.4.51:8000]F[10.113.4.95:7777/10.113.4.51:8001]F[10.113.4.95:7777/10.113.4.51:8002]F[10.113.4.95:7777/10.113.4.26:8000]F[10.113.4.95:7777/10.113.4.26:8001]F[10.113.4.95:7777/10.113.4.26:8002]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 5 -l 10.113.4.95
}


run_memcached_broken_case() {
#sudo   \
#taskset -c 0,1,2,3   \
#./scripts/pravin/runBetterBg.sh 2 ./ ./memcached-out.log   \
sudo ../benchmarking/memcached/memcached \
-N T0.p[7777]F[10.113.4.26:7777/10.113.4.51:8000]F[10.113.4.26:7777/10.113.4.51:8001]F[10.113.4.26:7777/10.113.4.51:8002]F[10.113.4.26:7777/10.113.4.51:8003]F[10.113.4.26:7777/10.113.4.57:8000]F[10.113.4.26:7777/10.113.4.57:8001]F[10.113.4.26:7777/10.113.4.57:8002]F[10.113.4.26:7777/10.113.4.57:8003]F[10.113.4.26:7777/10.113.4.20:8000]F[10.113.4.26:7777/10.113.4.20:8001]F[10.113.4.26:7777/10.113.4.20:8002]F[10.113.4.26:7777/10.113.4.20:8003]F[10.113.4.26:7777/10.113.4.51:8004]F[10.113.4.26:7777/10.113.4.51:8005]F[10.113.4.26:7777/10.113.4.51:8006]F[10.113.4.26:7777/10.113.4.51:8007]  \
-c 64000 -m 64000 -u root -p 0 -U  7777 -t 4 -l 10.113.4.26
}


run_memcached_single_core() {
sudo ../benchmarking/memcached/memcached -N p[7777] -c 64000 -m 64000 -u root -p 0 -U  7777 -t 1 -l ${SERVERIP}
}

run_memcached_single_core_specific_one_flow() {
sudo ../benchmarking/memcached/memcached -N  \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.57:8000] \
 -c 64000 -m 64000 -u root -p 0 -U  7777 -t 1 -l ${SERVERIP}
}

run_memcached_single_core_specific_two_flows() {
sudo ../benchmarking/memcached/memcached -N  \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001] \
 -c 64000 -m 64000 -u root -p 0 -U  7777 -t 1 -l ${SERVERIP}
}

run_memcached_2_cores_specific() {
sudo ../benchmarking/memcached/memcached -N  \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001] \
 -c 64000 -m 64000 -u root -p 0 -U  7777 -t 2 -l ${SERVERIP}
# use following command from appenzeller to test the flows (10.113.4.51 is ziger1 10G interface)
# ssh ziger1 'cd dragonet/benchmarking/libmemcached-1.0.18 ; memaslap -s ${SERVERIP}:7777 --udp -x100  --cfg_cmd=./bmKey_64_val_1024.conf -T1 -c 2 -b -S10m -z 8000'
}


run_memcached_single_core_linux() {
sudo ../benchmarking/memcached/memcached -c 64000 -m 64000 -u root -p 0 -U  7777 -t 2 -l 10.110.4.96
}

run_memcached_1conc_4clients() {
sudo ../benchmarking/memcached/memcached -N  \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.29:8000]F[${SERVERIP}:7777/10.113.4.20:8000] \
 -c 64000 -m 64000 -u root -p 0 -U  7777 -t 4 -l ${SERVERIP}
}

run_memcached_4conc_4clients() {
sudo ../benchmarking/memcached/memcached -N  \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001]F[${SERVERIP}:7777/10.113.4.51:8002]F[${SERVERIP}:7777/10.113.4.51:8003]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.57:8001]F[${SERVERIP}:7777/10.113.4.57:8002]F[${SERVERIP}:7777/10.113.4.57:8003]F[${SERVERIP}:7777/10.113.4.29:8000]F[${SERVERIP}:7777/10.113.4.29:8001]F[${SERVERIP}:7777/10.113.4.29:8002]F[${SERVERIP}:7777/10.113.4.29:8003]F[${SERVERIP}:7777/10.113.4.20:8000]F[${SERVERIP}:7777/10.113.4.20:8001]F[${SERVERIP}:7777/10.113.4.20:8002]F[${SERVERIP}:7777/10.113.4.20:8003] \
 -c 64000 -m 64000 -u root -p 0 -U  7777 -t 4 -l ${SERVERIP}
}


run_memcached_cl4_flows4() {
sudo ../benchmarking/memcached/memcached -N  \
T0.p[7777]F[10.113.4.26:7777/10.113.4.51:8000]F[10.113.4.26:7777/10.113.4.51:8001]F[10.113.4.26:7777/10.113.4.51:8002]F[10.113.4.26:7777/10.113.4.51:8003]F[10.113.4.26:7777/10.113.4.57:8000]F[10.113.4.26:7777/10.113.4.57:8001]F[10.113.4.26:7777/10.113.4.57:8002]F[10.113.4.26:7777/10.113.4.57:8003]F[10.113.4.26:7777/10.113.4.20:8000]F[10.113.4.26:7777/10.113.4.20:8001]F[10.113.4.26:7777/10.113.4.20:8002]F[10.113.4.26:7777/10.113.4.20:8003]F[10.113.4.26:7777/10.113.4.51:8004]F[10.113.4.26:7777/10.113.4.51:8005]F[10.113.4.26:7777/10.113.4.51:8006]F[10.113.4.26:7777/10.113.4.51:8007]  -c 64000 -m 64000 -u root -p 0 -U  7777 -t 4 -l 10.113.4.26
}

run_memcached_cl20_flows4_6clients() {
sudo ../benchmarking/memcached/memcached -N  \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.51:8001]F[${SERVERIP}:7777/10.113.4.51:8002]F[${SERVERIP}:7777/10.113.4.51:8003]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.57:8001]F[${SERVERIP}:7777/10.113.4.57:8002]F[${SERVERIP}:7777/10.113.4.57:8003]F[${SERVERIP}:7777/10.113.4.26:8000]F[${SERVERIP}:7777/10.113.4.26:8001]F[${SERVERIP}:7777/10.113.4.26:8002]F[${SERVERIP}:7777/10.113.4.26:8003]F[${SERVERIP}:7777/10.113.4.29:8000]F[${SERVERIP}:7777/10.113.4.29:8001]F[${SERVERIP}:7777/10.113.4.29:8002]F[${SERVERIP}:7777/10.113.4.29:8003]F[${SERVERIP}:7777/10.113.4.20:8000]F[${SERVERIP}:7777/10.113.4.20:8001]F[${SERVERIP}:7777/10.113.4.20:8002]F[${SERVERIP}:7777/10.113.4.20:8003]F[${SERVERIP}:7777/10.113.4.71:8000]F[${SERVERIP}:7777/10.113.4.71:8001]F[${SERVERIP}:7777/10.113.4.71:8002]F[${SERVERIP}:7777/10.113.4.71:8003]F[${SERVERIP}:7777/10.113.4.51:8004]F[${SERVERIP}:7777/10.113.4.51:8005]F[${SERVERIP}:7777/10.113.4.51:8006]F[${SERVERIP}:7777/10.113.4.51:8007]F[${SERVERIP}:7777/10.113.4.57:8004]F[${SERVERIP}:7777/10.113.4.57:8005]F[${SERVERIP}:7777/10.113.4.57:8006]F[${SERVERIP}:7777/10.113.4.57:8007]F[${SERVERIP}:7777/10.113.4.26:8004]F[${SERVERIP}:7777/10.113.4.26:8005]F[${SERVERIP}:7777/10.113.4.26:8006]F[${SERVERIP}:7777/10.113.4.26:8007]F[${SERVERIP}:7777/10.113.4.29:8004]F[${SERVERIP}:7777/10.113.4.29:8005]F[${SERVERIP}:7777/10.113.4.29:8006]F[${SERVERIP}:7777/10.113.4.29:8007]F[${SERVERIP}:7777/10.113.4.20:8004]F[${SERVERIP}:7777/10.113.4.20:8005]F[${SERVERIP}:7777/10.113.4.20:8006]F[${SERVERIP}:7777/10.113.4.20:8007]F[${SERVERIP}:7777/10.113.4.71:8004]F[${SERVERIP}:7777/10.113.4.71:8005]F[${SERVERIP}:7777/10.113.4.71:8006]F[${SERVERIP}:7777/10.113.4.71:8007]F[${SERVERIP}:7777/10.113.4.51:8008]F[${SERVERIP}:7777/10.113.4.51:8009]F[${SERVERIP}:7777/10.113.4.51:8010]F[${SERVERIP}:7777/10.113.4.51:8011]F[${SERVERIP}:7777/10.113.4.57:8008]F[${SERVERIP}:7777/10.113.4.57:8009]F[${SERVERIP}:7777/10.113.4.57:8010]F[${SERVERIP}:7777/10.113.4.57:8011]F[${SERVERIP}:7777/10.113.4.26:8008]F[${SERVERIP}:7777/10.113.4.26:8009]F[${SERVERIP}:7777/10.113.4.26:8010]F[${SERVERIP}:7777/10.113.4.26:8011]F[${SERVERIP}:7777/10.113.4.29:8008]F[${SERVERIP}:7777/10.113.4.29:8009]F[${SERVERIP}:7777/10.113.4.29:8010]F[${SERVERIP}:7777/10.113.4.29:8011]F[${SERVERIP}:7777/10.113.4.20:8008]F[${SERVERIP}:7777/10.113.4.20:8009]F[${SERVERIP}:7777/10.113.4.20:8010]F[${SERVERIP}:7777/10.113.4.20:8011]F[${SERVERIP}:7777/10.113.4.71:8008]F[${SERVERIP}:7777/10.113.4.71:8009]F[${SERVERIP}:7777/10.113.4.71:8010]F[${SERVERIP}:7777/10.113.4.71:8011]F[${SERVERIP}:7777/10.113.4.51:8012]F[${SERVERIP}:7777/10.113.4.51:8013]F[${SERVERIP}:7777/10.113.4.51:8014]F[${SERVERIP}:7777/10.113.4.51:8015]F[${SERVERIP}:7777/10.113.4.57:8012]F[${SERVERIP}:7777/10.113.4.57:8013]F[${SERVERIP}:7777/10.113.4.57:8014]F[${SERVERIP}:7777/10.113.4.57:8015] \
 -c 64000 -m 64000 -u root -p 0 -U  7777 -t 10 -l ${SERVERIP}
}


run_memcached_client_manual_debug_8flows_8unique_clients() {
sudo ../benchmarking/memcached/memcached -N  \
T0.p[7777]F[${SERVERIP}:7777/10.113.4.51:8000]F[${SERVERIP}:7777/10.113.4.57:8000]F[${SERVERIP}:7777/10.113.4.96:8000]F[${SERVERIP}:7777/10.113.4.67:8000]F[${SERVERIP}:7777/10.113.4.71:8000]F[${SERVERIP}:7777/10.113.4.26:8000]F[${SERVERIP}:7777/10.113.4.20:8000]F[${SERVERIP}:7777/10.113.4.29:8000] \
 -c 64000 -m 64000 -u root -p 0 -U  7777 -t 8 -l ${SERVERIP}
}



run_echobench_client_manual_debug() {

sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
    -a t0  -f ${SERVERIP}:888/10.113.4.71:9000  \
    -f ${SERVERIP}:888/10.113.4.71:9001  -f ${SERVERIP}:888/10.113.4.71:9002  \
    -f ${SERVERIP}:888/10.113.4.71:9003  -a t1  -f ${SERVERIP}:888/10.113.4.96:9000 \
    -f ${SERVERIP}:888/10.113.4.96:9001  -f ${SERVERIP}:888/10.113.4.96:9002  \
    -f ${SERVERIP}:888/10.113.4.96:9003  -a t2  -f ${SERVERIP}:888/10.113.4.96:9000  \
    -f ${SERVERIP}:888/10.113.4.96:9001  -f ${SERVERIP}:888/10.113.4.96:9002  -f ${SERVERIP}:888/10.113.4.96:9003  -a t3  -f ${SERVERIP}:888/10.113.4.67:9000  -f ${SERVERIP}:888/10.113.4.67:9001  -f ${SERVERIP}:888/10.113.4.67:9002  -f ${SERVERIP}:888/10.113.4.67:9003  -a t4  -f ${SERVERIP}:888/10.113.4.20:9000  -f ${SERVERIP}:888/10.113.4.20:9001  -f ${SERVERIP}:888/10.113.4.20:9002  -f ${SERVERIP}:888/10.113.4.20:9003  -a t5  -f ${SERVERIP}:888/10.113.4.26:9000  -f ${SERVERIP}:888/10.113.4.26:9001  -f ${SERVERIP}:888/10.113.4.26:9002  -f ${SERVERIP}:888/10.113.4.26:9003  -a t6  -f ${SERVERIP}:888/10.113.4.29:9000  -f ${SERVERIP}:888/10.113.4.29:9001  -f ${SERVERIP}:888/10.113.4.29:9002  -f ${SERVERIP}:888/10.113.4.29:9003  -a t7  -f ${SERVERIP}:888/10.113.4.29:9000  -f ${SERVERIP}:888/10.113.4.29:9001  -f ${SERVERIP}:888/10.113.4.29:9002  -f ${SERVERIP}:888/10.113.4.29:9003  -a t8  -f ${SERVERIP}:888/10.113.4.51:9000  -f ${SERVERIP}:888/10.113.4.51:9001  -f ${SERVERIP}:888/10.113.4.51:9002  -f ${SERVERIP}:888/10.113.4.51:9003  -a t9  -f ${SERVERIP}:888/10.113.4.57:9000  -f ${SERVERIP}:888/10.113.4.57:9001  -f ${SERVERIP}:888/10.113.4.57:9002  -f ${SERVERIP}:888/10.113.4.57:9003  -t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7  -t -q t8  -t -q t9

}


run_for_16_cores_echo_like_memcached() {
    echo fancyEcho running on ${SERVERIP} with 16 application thread and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:7777/10.113.4.51:8000  -a t1  -f ${SERVERIP}:7777/10.113.4.57:8000  -a t8   -f ${SERVERIP}:7777/10.113.4.51:8002 -a t12  -f ${SERVERIP}:7777/10.113.4.51:8003 \
-a t2  -f ${SERVERIP}:7777/10.113.4.71:8000  -a t3  -f ${SERVERIP}:7777/10.113.4.71:8001  -a t9   -f ${SERVERIP}:7777/10.113.4.71:8002 -a t13  -f ${SERVERIP}:7777/10.113.4.71:8003 \
-a t4  -f ${SERVERIP}:7777/10.113.4.51:8001  -a t5  -f ${SERVERIP}:7777/10.113.4.57:8001  -a t10  -f ${SERVERIP}:7777/10.113.4.57:8002 -a t14  -f ${SERVERIP}:7777/10.113.4.57:8003 \
-a t6  -f ${SERVERIP}:7777/10.113.4.29:8000  -a t7  -f ${SERVERIP}:7777/10.113.4.29:8001  -a t11  -f ${SERVERIP}:7777/10.113.4.29:8002 -a t15  -f ${SERVERIP}:7777/10.113.4.29:8003 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7 \
-t -q t8 -t -q t9 -t -q t10 -t -q t11 -t -q t12 -t -q t13  -t -q t14 -t -q t15
}


run_for_40_cores_10threads_debug_echoserver() {
    echo fancyEcho running on ${SERVERIP} with 10 application thread and 10 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f 10.113.4.195:888/10.113.4.29:8004  -f 10.113.4.195:888/10.113.4.20:8004  -f 10.113.4.195:888/10.113.4.26:8004  -f 10.113.4.195:888/10.113.4.71:8004  -a t1  -f 10.113.4.195:888/10.113.4.67:8004  -f 10.113.4.195:888/10.113.4.96:8004  -f 10.113.4.195:888/10.113.4.57:8004  -f 10.113.4.195:888/10.113.4.51:8004  -a t2  -f 10.113.4.195:888/10.113.4.29:8003  -f 10.113.4.195:888/10.113.4.20:8003  -f 10.113.4.195:888/10.113.4.26:8003  -f 10.113.4.195:888/10.113.4.71:8003  -a t3  -f 10.113.4.195:888/10.113.4.67:8003  -f 10.113.4.195:888/10.113.4.96:8003  -f 10.113.4.195:888/10.113.4.57:8003  -f 10.113.4.195:888/10.113.4.51:8003  -a t4  -f 10.113.4.195:888/10.113.4.29:8002  -f 10.113.4.195:888/10.113.4.20:8002  -f 10.113.4.195:888/10.113.4.26:8002  -f 10.113.4.195:888/10.113.4.71:8002  -a t5  -f 10.113.4.195:888/10.113.4.67:8002  -f 10.113.4.195:888/10.113.4.96:8002  -f 10.113.4.195:888/10.113.4.57:8002  -f 10.113.4.195:888/10.113.4.51:8002  -a t6  -f 10.113.4.195:888/10.113.4.29:8001  -f 10.113.4.195:888/10.113.4.20:8001  -f 10.113.4.195:888/10.113.4.26:8001  -f 10.113.4.195:888/10.113.4.71:8001  -a t7  -f 10.113.4.195:888/10.113.4.67:8001  -f 10.113.4.195:888/10.113.4.96:8001  -f 10.113.4.195:888/10.113.4.57:8001  -f 10.113.4.195:888/10.113.4.51:8001  -a t8  -f 10.113.4.195:888/10.113.4.29:8000  -f 10.113.4.195:888/10.113.4.20:8000  -f 10.113.4.195:888/10.113.4.26:8000  -f 10.113.4.195:888/10.113.4.71:8000  -a t9  -f 10.113.4.195:888/10.113.4.67:8000  -f 10.113.4.195:888/10.113.4.96:8000  -f 10.113.4.195:888/10.113.4.57:8000  -f 10.113.4.195:888/10.113.4.51:8000  -t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7  -t -q t8  -t -q t9
}

run_for_16_cores() {
    echo fancyEcho running on ${SERVERIP} with 16 application thread and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000  -a t1  -f ${SERVERIP}:888/10.113.4.51:9001  -a t8   -f ${SERVERIP}:888/10.113.4.51:9002 -a t12  -f ${SERVERIP}:888/10.113.4.51:9003 \
-a t2  -f ${SERVERIP}:888/10.113.4.71:9000  -a t3  -f ${SERVERIP}:888/10.113.4.71:9001  -a t9   -f ${SERVERIP}:888/10.113.4.71:9002 -a t13  -f ${SERVERIP}:888/10.113.4.71:9003 \
-a t4  -f ${SERVERIP}:888/10.113.4.57:9000  -a t5  -f ${SERVERIP}:888/10.113.4.57:9001  -a t10  -f ${SERVERIP}:888/10.113.4.57:9002 -a t14  -f ${SERVERIP}:888/10.113.4.57:9003 \
-a t6  -f ${SERVERIP}:888/10.113.4.29:9000  -a t7  -f ${SERVERIP}:888/10.113.4.29:9001  -a t11  -f ${SERVERIP}:888/10.113.4.29:9002 -a t15  -f ${SERVERIP}:888/10.113.4.29:9003 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7 \
-t -q t8 -t -q t9 -t -q t10 -t -q t11 -t -q t12 -t -q t13  -t -q t14 -t -q t15
}

run_echoserver_single_core_specific_two_flows() {
sudo ./dist/build/bench-fancyecho/bench-fancyecho \
-a T0 -f ${SERVERIP}:7777/10.113.4.57:8000 -f ${SERVERIP}:7777/10.113.4.29:8000 \
-t -q T0
}



run_for_12_cores() {
    echo fancyEcho running on ${SERVERIP} with 12 application thread and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000  -a t1  -f ${SERVERIP}:888/10.113.4.51:9001  -a t8   -f ${SERVERIP}:888/10.113.4.51:9002 \
-a t2  -f ${SERVERIP}:888/10.113.4.71:9000  -a t3  -f ${SERVERIP}:888/10.113.4.71:9001  -a t9   -f ${SERVERIP}:888/10.113.4.71:9002 \
-a t4  -f ${SERVERIP}:888/10.113.4.57:9000  -a t5  -f ${SERVERIP}:888/10.113.4.57:9001  -a t10  -f ${SERVERIP}:888/10.113.4.57:9002 \
-a t6  -f ${SERVERIP}:888/10.113.4.29:9000  -a t7  -f ${SERVERIP}:888/10.113.4.29:9001  -a t11  -f ${SERVERIP}:888/10.113.4.29:9002 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7 \
-t -q t8 -t -q t9 -t -q t10 -t -q t11
}

run_for_10_cores() {
    echo fancyEcho running on ${SERVERIP} with 10 application threads
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000  -a t1  -f ${SERVERIP}:888/10.113.4.51:9001 \
-a t2  -f ${SERVERIP}:888/10.113.4.57:9000  -a t3  -f ${SERVERIP}:888/10.113.4.57:9001 \
-a t4  -f ${SERVERIP}:888/10.113.4.71:9000  -a t5  -f ${SERVERIP}:888/10.113.4.71:9001 \
-a t6  -f ${SERVERIP}:888/10.113.4.29:9000  -a t7  -f ${SERVERIP}:888/10.113.4.29:9001 \
-a t8  -f ${SERVERIP}:888/10.113.4.51:9002  -a t9  -f ${SERVERIP}:888/10.113.4.57:9002 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7 -t -q t8  -t -q t9
}


run_for_8_cores() {
    echo fancyEcho running on ${SERVERIP} with 8 application thread and 4 HW queues
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000  -a t1  -f ${SERVERIP}:888/10.113.4.51:9001 \
-a t2  -f ${SERVERIP}:888/10.113.4.71:9000  -a t3  -f ${SERVERIP}:888/10.113.4.71:9001 \
-a t4  -f ${SERVERIP}:888/10.113.4.57:9000  -a t5  -f ${SERVERIP}:888/10.113.4.57:9001 \
-a t6  -f ${SERVERIP}:888/10.113.4.29:9000  -a t7  -f ${SERVERIP}:888/10.113.4.29:9001 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5  -t -q t6  -t -q t7
}

run_for_4_cores() {
    echo fancyEcho running on ${SERVERIP} with 4 application threads
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.51:9000 \
-a t1  -f ${SERVERIP}:888/10.113.4.71:9000 \
-a t2  -f ${SERVERIP}:888/10.113.4.57:9000 \
-a t3  -f ${SERVERIP}:888/10.113.4.29:9000 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3
}

run_for_4_cores_appenzeller() {
    echo fancyEcho running on ${SERVERIP} with 4 application threads
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0  -f ${SERVERIP}:888/10.113.4.71:9000 \
-a t1  -f ${SERVERIP}:889/10.113.4.71:9000 \
-a t2  -f ${SERVERIP}:890/10.113.4.71:9000 \
-a t3  -f ${SERVERIP}:891/10.113.4.71:9000 \
-t -q t0  -t -q t1  -t -q t2  -t -q t3
}


run_for_4_cores_with_wildcards() {
# old way, with wildcards
    echo fancyEcho running on ${SERVERIP} with 4 application threads and wildcards
sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:888/10.113.4.51:0 \
    -a t1  -f ${SERVERIP}:888/10.113.4.71:0 \
    -a t2  -f ${SERVERIP}:888/10.113.4.57:0 \
    -a t3  -f ${SERVERIP}:888/10.113.4.29:0 \
    -t -q t0  -t -q t1 -t -q t2 -t -q t3
}

run_for_2_cores_SP() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:888/10.113.4.51:9000 -f ${SERVERIP}:888/10.113.4.71:9000 \
    -a t1  -f ${SERVERIP}:888/10.113.4.57:9000 -f ${SERVERIP}:888/10.113.4.29:9000 \
    -t -q t0 -t -q t1
}


run_for_2_cores_full_filters() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:888/10.113.4.51:9000 \
    -f ${SERVERIP}:888/10.113.4.29:9000 \
    -a t1 -p 888 \
    -t -q t0 -t -q t1
}

run_for_1_cores_full_filters() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:7777/10.113.4.71:9000 \
    -t -q t0
}

run_for_2_cores_full_filters_test() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:888/10.113.4.51:8000 \
    -a t1  -f ${SERVERIP}:888/10.113.4.51:8001 \
    -t -q t0 -t -q t1
}

run_for_6_cores_6_flows() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:888/10.113.4.29:8000  \
    -a t1  -f ${SERVERIP}:888/10.113.4.26:8000  \
    -a t2  -f ${SERVERIP}:888/10.113.4.71:8000  \
    -a t3  -f ${SERVERIP}:888/10.113.4.96:8000  \
    -a t4  -f ${SERVERIP}:888/10.113.4.57:8000  \
    -a t5  -f ${SERVERIP}:888/10.113.4.51:8000  \
    -t -q t0  -t -q t1  -t -q t2  -t -q t3  -t -q t4  -t -q t5

}

dpdk_debug_2_cores_test() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0  -f ${SERVERIP}:888/10.113.4.26:8000  \
    -a t1  -f ${SERVERIP}:888/10.113.4.51:8000  \
    -t -q t0  -t -q t1
}

run_for_1_cores() {
    echo fancyEcho running on ${SERVERIP} with single application thread and single socket
    sudo ./dist/build/bench-fancyecho/bench-fancyecho -a t0  -p 888 -t -q t0
}

run_for_1_cores_null() {
    echo fancyEcho running on ${SERVERIP} with single application thread and two sockets for null testing
    sudo ./dist/build/bench-fancyecho/bench-fancyecho \
    -a t0 -p 222 \
    -a t1 -p 111 \
    -a t2 -p 333 \
    -a t3 -p 444 \
    -t -q t0 -q t1 -q t2 -q t3
}


set -x
set -e

if [ -z $1 ] ; then
    echo "ERROR: provide the stack name"
    echo "USAGE: $0 <stackname>"
    echo "EXAMPLE: $0 tap"
    echo "EXAMPLE: $0 sf"
    echo "EXAMPLE: $0 e10k"
    echo "EXAMPLE: $0 null"
    exit 1
fi


STACKNAME=stack-"${1}"

# For Solarflare
if [ "$STACKNAME" ==  "stack-sf" ] ; then
    SERVERIP="10.113.4.195"  # for asiago as server on solarflare
else

    # For intel
    if [ "$STACKNAME" ==  "stack-e10k" ] ; then
        SERVERIP="10.113.4.96"  # For burrata as server
        SERVERIP="10.113.4.95"  # For asiago as server
        SERVERIP="10.113.4.26"  # For sbrinz1 as server
        echo "NOTE: using ${SERVERIP} as server-IP address"
    else

        if [ "$STACKNAME" ==  "stack-dpdk" ] ; then
            SERVERIP="10.113.4.96"  # For burrata as server
            SERVERIP="10.113.4.95"  # For asiago as server
            SERVERIP="10.113.4.26"  # For sbrinz1 as server
            echo "NOTE: using ${SERVERIP} as server-IP address"
        else

            if [ "$STACKNAME" ==  "stack-tap" ] ; then
                SERVERIP="192.168.123.1"
            else

                if [ "$STACKNAME" ==  "stack-null" ] ; then
                    SERVERIP="127.0.0.1"
                else
                    print "Invalid stack type $STACKNAME"
                    exit 1
                fi
            fi
        fi
    fi
fi

run_for_4_cores_ziger1() {
    echo fancyEcho running on ${SERVERIP} with 4 application threads
    lport=9000
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-W -a t0 -p 9000  \
-F ${SERVERIP}:${lport}/10.113.4.51:8000 \
-F ${SERVERIP}:${lport}/10.113.4.51:8001 \
-F ${SERVERIP}:${lport}/10.113.4.51:8002 \
-F ${SERVERIP}:${lport}/10.113.4.51:8003 \
-t -q t0
}

run_for_4_cores_ziger1_working() {
    echo fancyEcho running on ${SERVERIP} with 4 application threads
    lport=9000
sudo ./dist/build/bench-fancyecho/bench-fancyecho  \
-a t0 \
-f ${SERVERIP}:${lport}/10.113.4.51:8000 \
-f ${SERVERIP}:${lport}/10.113.4.51:8001 \
-f ${SERVERIP}:${lport}/10.113.4.51:8002 \
-f ${SERVERIP}:${lport}/10.113.4.51:8003 \
-t -q t0
}


##################################################################
        ## Main ##
##################################################################

./scripts/pravin/wait_for_dragonet.sh 4 ${STACKNAME}
#run_memcached_big
run_memcached_2_clients_3_load
#run_memcached_4_clients_2_load
#run_memcached_for_1_cores
#run_memcached_single_core_specific_two_flows
#run_memcached_2_cores_specific
#run_memcached_cl4_flows4
#run_memcached_cl20_flows4_6clients

#run_for_4_cores_ziger1
#run_memcached_4conc_4clients
#run_memcached_4conc_4clients
#run_memcached_1conc_4clients
#run_for_2_cores_full_filters_test
#run_for_4_cores_ziger1_working
#run_for_4_cores_ziger1
#run_memcached_1conc_6clients
#run_memcached_cl20_flows4_6clients
#run_for_6_cores_6_flows
#run_for_1_cores
#dpdk_debug_2_cores_test
sudo killall ${STACKNAME}
exit 0
##################################################################


#run_for_4_cores_appenzeller
#run_for_2_cores_full_filters_test
#run_for_1_cores_full_filters
##################################################################


#run_for_40_cores_10threads_debug_echoserver

#run_memcached_client_manual_debug_40flows_8unique_clients
#run_memcached_client_manual_debug_8flows_8unique_clients
#run_for_8_cores

#run_echoserver_single_core_specific_two_flows
#run_memcached_single_core_specific_one_flow
#run_memcached_single_core_specific_two_flows
#run_memcached_for_8_cores
#run_memcached_client_manual_debug_40flows_8unique_clients
#run_for_16_cores_echo_like_memcached

#run_memcached_client_manual_debug
# for quick cleanup

#run_memcached_single_core
#run_echobench_client_manual_debug

#run_for_8_cores
#run_for_4_cores
#run_for_1_cores

#run_for_2_cores_SP

#run_for_10_cores
#run_for_8_cores
#run_for_1_cores_null

#run_for_4_cores_with_wildcards

#run_for_2_cores_SP

#run_for_4_cores

#run_for_2_cores_full_filters

#run_for_16_cores
#run_for_16_cores
#run_for_1_cores
#run_for_12_cores
#run_for_8_cores
#run_for_2_cores

exit 0

## NOTE:
# This code was used for deploying fancyecho manually for initial results with
#       new Dragonet stack.

# 10.113.4.51  # Burrata # 175178848 -- old
# 10.113.4.96  # Burrata #  Also works as server

# 10.113.4.20  # gruyere # 175178772
# 10.113.4.57  # ziger2  # 175178809
# 10.113.4.29  # sbrinz2 # 175178781
# 10.113.4.95  # Asiago  # 175178847  # Intel
# 10.113.4.195 # Asiago  # 175178947  # solarflare

# appenzeller-e1000 : 10.113.4.71
# ziger1 : 10.113.4.51
# ziger2 : 10.113.4.57
# sbrinz2 : 10.113.4.29



#sudo ./dist/build/bench-fancyecho/bench-fancyecho -W -a t0 -p 4444 -a t1 -p 4444 -F 10.113.4.51:4444/10.113.4.26:1 -F 10.113.4.51:4444/10.113.4.26:2 -t -q t0 -t -q t1

