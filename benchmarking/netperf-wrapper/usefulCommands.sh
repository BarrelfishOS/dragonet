cat debut.py | egrep '^(aggregator.py|formatters.py|runners.py|transformers.py|settings.py|util.py|metadata.py|resultset.py|build_info.py)'
./netperf-wrapper -l 1 -H localhost udp_stream -L ./logout.txt
../netperf-2.6.0/src/netperf -P 0 -l 5 -t UDP_RR -fg -- -r 8000 -k all

./netperf-wrapper -l 1 -H localhost udp_stream -L ./logout.txt --extended-metadata
cat tout.json | python -mjson.tool
gunzip -d udp_demo-2014-03-31T123917.623880.json.gz
./netperf-wrapper -l 1 -H localhost udp_demo -L ../out-nf/r2.logs

./netperf-wrapper -i udp_localhost_stream-2014-*.json.gz -p idlecpu -o p_cpu_util.png
./netperf-wrapper -l 1 -H asiago -C burrata -T 10.22.4.11 udp_localhost_stream -L ../out-nf/r4.logs
./netperf-wrapper -i ./udp_localhost_stream-2014-04-02T170*json -p serverbw -o p_sb_2.png