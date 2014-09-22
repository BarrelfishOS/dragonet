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
./netperf-wrapper -p userbw2 -o test.png   -i `find ../myplots/myplots/  -name "*json*" | egrep "*_rr*" `

./netperf-wrapper -i ../myplots/IntelD_exp//CImplDpdk/LATENCY/udp_rr-2014-04-08T120436.862794.CImplDpdk_IntelD_exp_PKT_1400_B_64.json.gz
./netperf-wrapper -p bbox -o t.png -i `find ../myplots/results2/ -name "*.json*" | grep -i "best"`
./netperf-wrapper  -i `find ../myplots/results2/ -name "*.json*" | grep -i "64_B_1"`
./netperf-wrapper -c memcached -C ziger2 -C ziger2 -T 10.110.4.95  -H asiago -l 10 -L mylog1.log memcached_rr
./netperf-wrapper -l 10 -c memcached -C ziger2 -C sbrinz2 -C gruyere  -T 10.113.4.95 -Q 2 -q 18  -H asiago -b 100 -L mylog3.log memcached_rr
./netperf-wrapper -I 1 -l 10 -c memcached -H asiago -C ziger2 -C sbrinz2 -C ziger2 -C sbrinz2  --serverInstances 4 --servercores 1 --clientcores 2 -T 10.113.4.95 memcached_rr --concurrency 64 -L deleteme_mylog6.log 
./netperf-wrapper  -c fancyEchoLinux -l 5  -T 10.113.4.95 -H asiago  -C ziger2 -C sbrinz2 -C gruyere --packet 1024 --udp  -L myRSSlog3.log udp_rr
./plot_scale_data.sh ../echoServerResults/Dragonet_E10k/ ../echoServerResults/plots/ 1024 "Dragonet on E10k with 10 queues"
