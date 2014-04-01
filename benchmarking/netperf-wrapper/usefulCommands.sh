cat debut.py | egrep '^(aggregator.py|formatters.py|runners.py|transformers.py|settings.py|util.py|metadata.py|resultset.py|build_info.py)'
./netperf-wrapper -l 1 -H localhost udp_stream -L ./logout.txt
../netperf-2.6.0/src/netperf -P 0 -l 5 -t UDP_RR -fg -- -r 8000 -k all

./netperf-wrapper -l 1 -H localhost udp_stream -L ./logout.txt --extended-metadata
cat tout.json | python -mjson.tool
gunzip -d udp_demo-2014-03-31T123917.623880.json.gz 
./netperf-wrapper -l 1 -H localhost udp_demo -L ../out-nf/r2.logs 
