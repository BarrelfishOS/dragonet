BINARIES=graph plos13 impl confgen incremental impl_th
#BINARIES=graph plos13 impl tuntap confgen incremental impl_th
#OSUF=p_o
#GHCFLAGS=-fllvm -Wall -O2 -XRankNTypes -rtsopts -prof -fprof-auto -osuf p_o
OSUF=o
#GHCFLAGS=-fllvm -Wall -O2 -XRankNTypes -rtsopts
#GHCFLAGS=-Wall -O2 -XRankNTypes -rtsopts
GHCFLAGS=-O2 -XRankNTypes -rtsopts
GHC=ghc
CC=clang
# Debuging for DSL -> Haskell process
# GHCFLAGS+=-ddump-splices

# Setting the default goal to simplify compiling
#.DEFAULT_GOAL := useful
#.DEFAULT_GOAL := graph
.DEFAULT_GOAL := tuntap
#.DEFAULT_GOAL := dpdk
#.DEFAULT_GOAL := tcpGraph_run


useful: graph_run tcpGraph_run tcpGraph2_run confgen tuntap #dpdk #impl
	echo "Removing all png except the one which I want"
	rm `find *.png | grep -v "lpgImpl" | grep -v "tcp*"`



incrementalSim: base
	$(GHC) $(GHCFLAGS) -threaded testIncremental.hs -o incrementalSim

incrementalSim_run: incrementalSim
	./incrementalSim

incremental: base Util/Dpdk.$(OSUF)
	$(GHC) $(GHCFLAGS) -threaded testIncremental.hs -o incremental  -L../dpdk-1.5.0r1/build/lib/ -lintel_dpdk

incremental_run: incremental
	export LD_LIBRARY_PATH=/home/ubuntu/dragonet/dpdk-1.5.0r1/build/lib/ ;   ./incremental

all: models

# generates graphs for tcp protocol SEnd side
tcpGraph2: base lpgImpl.unicorn
	$(GHC) $(GHCFLAGS) tcpGraph2.hs -o tcpGraph2

tcpGraph2_run: tcpGraph2
	./tcpGraph2
	cd graphsGen ; ../convertDot.sh

# generates graphs for tcp protocol processing
tcpGraph: base lpgImpl.unicorn
	$(GHC) $(GHCFLAGS) tcpGraph.hs -o tcpGraph

tcpGraph_run: tcpGraph
	./tcpGraph
	cd graphsGen ; ../convertDot.sh

# generates graphs (lpg, prg, prg_conf, embedded, constrained)
graph: base lpgImpl.unicorn
	$(GHC) $(GHCFLAGS) testGraph.hs -o graph
graph_run: graph
	mkdir -p graphsGen/
	./graph
	cd graphsGen ; ../convertDot.sh

confgen: base
	$(GHC) $(GHCFLAGS) testConfGen.hs -o confgen
confgen_run: confgen
	./confgen
	cd graphsGen ; ../convertConfGen.sh

# generates graphs for plos paper (lpg, prg, prgUnconf, embedded, embeddedFull, clustered)
plos13: base
	$(GHC) $(GHCFLAGS) testPLOS13.hs -o plos13
plos13_run: plos13
	./plos13

# checks a packet by executing it (dns packet)
impl: base
	$(GHC) $(GHCFLAGS) testImpl.hs -o impl
impl_run: impl
	./impl

impl_th: base
	$(GHC) $(GHCFLAGS) testImplTH.hs -o impl_th
impl_th_run: impl_th
	./impl_th

kk-test: base
	$(GHC) $(GHCFLAGS) kk-test.hs -o kk-test

# Runs the simulator against tuntap interface
tuntap : base Util/Tap.$(OSUF)
	$(GHC) $(GHCFLAGS) -threaded tuntapTests.hs -o tuntap

tuntap_run: tuntap
	sudo ./tuntap


# Runs the simulator against tuntap interface
tuntap_icmp : base Util/Tap.$(OSUF)
	$(GHC) $(GHCFLAGS) -threaded tuntapPing.hs -o tuntapPing

tuntap_icmp_run: tuntap_icmp
	sudo ./tuntapPing


## Util/Tap

Util/tap.$(OSUF): Util/tap.c
	$(CC) $(CFLAGS) -c $< -o $@

Util/Tap_.$(OSUF): Util/Tap.hs
	$(GHC) $(GHCFLAGS) -c $< -o $@

Util/Tap.$(OSUF): Util/Tap_.$(OSUF) Util/tap.$(OSUF)
	echo ld -i $^ -o $@
	ld -i $^ -o $@

# Runs the simulator against dpdk userspace network
dpdk: base Util/Dpdk.$(OSUF) #intel_dpdk
	$(GHC) $(GHCFLAGS) -threaded dpdkTests.hs -o dpdkDragonet -L../dpdk-1.5.0r1/build/lib/ -lintel_dpdk

dpdk_run: dpdk
	export LD_LIBRARY_PATH=/home/ubuntu/dragonet/dpdk-1.5.0r1/build/lib/ ; ./dpdkDragonet

## Util/Dpdk

RTE_TARGET ?= x86_64-default-linuxapp-gcc
RTE_SDK ?= ../dpdk-1.5.0r1/


Util/dpdk.$(OSUF): ../dpdk-1.5.0r1/examples/uDragonet/dpdk.c intel_dpdk
	echo "##########################"
	cd ../dpdk-1.5.0r1/examples/uDragonet  && $(MAKE)
	cp ../dpdk-1.5.0r1/examples/uDragonet/build/dpdk.$(OSUF) Util/

objfiles: ../dpdk-1.5.0r1/app/test-pmd/dpdk.c #intel_dpdk
	echo "##########################"
	cd ../dpdk-1.5.0r1/app/dpdkDriver/ && ./myMake.sh
	cp ../dpdk-1.5.0r1/app/dpdkDriver/build/*.$(OSUF) Util/


intel_dpdk: # ../dpdk-1.5.0r1/build/lib/libintel_dpdk.so
	cd ../dpdk-1.5.0r1/ && ./doConfig.sh && $(MAKE)

Util/Dpdk_.$(OSUF): Util/Dpdk.hs
	echo $(GHC) $(GHCFLAGS) -c $< -o $@
	$(GHC) $(GHCFLAGS) -c $< -o $@

#Util/Dpdk.$(OSUF): Util/Dpdk_.$(OSUF) Util/dpdk.$(OSUF)
Util/Dpdk.$(OSUF): Util/Dpdk_.$(OSUF) objfiles
	ld -i  Util/Dpdk_.$(OSUF) Util/cmdline.o Util/dpdkData.o Util/config.o Util/csumonly.o Util/iofwd.o Util/macfwd.o Util/parameters.o Util/rxonly.o Util/dpdkControl.o Util/txonly.o -o $@

Util/tap.bc: Util/tap.c
	clang -O2 -emit-llvm -c $< -o $@

llvm-helpers.bc: Util/tap.bc c_impl/llvm_pg_impl.bc
	llvm-link-3.5 $^ -o $@

# Converts graphs into pdfs
models: graph_run
	cd graphsGen ; ../convertDot.sh

base: Makefile *.hs */*.hs */*/*.hs *.unicorn Dragonet/Implementation/*.hs

clean:
	rm -f *.hi *.o */*.hi */*.o */*/*.hi */*/*.o $(BINARIES)
	rm -f *.pdf *.dot *.png *.svg
	cd graphsGen ; rm -f *.pdf *.dot *.png *.svg

