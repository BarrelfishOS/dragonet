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

all: models

# generates graphs (lpg, prg, prg_conf, embedded, constrained)
graph: base
	$(GHC) $(GHCFLAGS) testGraph.hs -o graph
graph_run: graph
	./graph

confgen: base
	$(GHC) $(GHCFLAGS) testConfGen.hs -o confgen
confgen_run: confgen
	./confgen
	./convertConfGen.sh

incremental: base
	$(GHC) $(GHCFLAGS) testIncremental.hs -o incremental
incremental_run: incremental
	./incremental


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

# Runs the simulator against tuntap interface
tuntap : base Util/Tap.$(OSUF)
	$(GHC) $(GHCFLAGS) -threaded tuntapTests.hs -o tuntap

tuntap_run: tuntap
	./tuntap



## Util/Tap

Util/tap.$(OSUF): Util/tap.c
	$(CC) $(CFLAGS) -c $< -o $@

Util/Tap_.$(OSUF): Util/Tap.hs
	$(GHC) $(GHCFLAGS) -c $< -o $@

Util/Tap.$(OSUF): Util/Tap_.$(OSUF) Util/tap.$(OSUF)
	echo ld -i $^ -o $@
	ld -i $^ -o $@


# Runs the simulator against dpdk userspace network
dpdk: base Util/Dpdk.$(OSUF)
	echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
#	cp `find  ../dpdk-1.5.0r1/x86_64-default-linuxapp-gcc/build/lib/librte_eal/linuxapp/eal/ -name "*.o"` ofiles/  2> /dev/null || pwd
#	rm -f ofiles/built-in.o
	cp `cat  ../dpdk-1.5.0r1/examples/uDragonet/mydepsFiltered.txt | sort | uniq` ofiles/
	#cp `cat  ../dpdk-1.5.0r1/examples/uDragonet/mydeps.txt` ofiles/
	#$(GHC) $(GHCFLAGS) -threaded dpdkTests.hs ofiles/*.o -o dpdkDragonet #-L../dpdk-1.5.0r1/x86_64-default-linuxapp-gcc/lib/ -Lofiles/

	$(GHC) $(GHCFLAGS) -threaded dpdkTests.hs -o dpdkDragonet -L../dpdk-1.5.0r1/x86_64-default-linuxapp-gcc/lib/
#	$(GHC) $(GHCFLAGS) -threaded dpdkTests.hs -o dpdkDragonet -L  $(wildcard ../dpdk-1.5.0r1/x86_64-default-linuxapp-gcc/lib/*.a)

dpdk_run: dpdk
	./dpdkDragonet

## Util/Dpdk

RTE_TARGET ?= x86_64-default-linuxapp-gcc
RTE_SDK ?= ../dpdk-1.5.0r1/


#Util/dpdk.$(OSUF): Util/dpdk.c
#../dpdk-1.5.0r1/examples/uDragonet/build/dpdk.$(OSUF): ../dpdk-1.5.0r1/examples/uDragonet/dpdk.c
Util/dpdk.$(OSUF): ../dpdk-1.5.0r1/examples/uDragonet/dpdk.c
	echo "################################################"
#	cd ../dpdk-1.5.0r1/examples/uDragonet  && $(MAKE)
	cp ../dpdk-1.5.0r1/examples/uDragonet/build/dpdk.$(OSUF) Util/

#	$(CC) $(CFLAGS) -c $< -o $@

Util/Dpdk_.$(OSUF): Util/Dpdk.hs
	echo $(GHC) $(GHCFLAGS) -c $< -o $@
	$(GHC) $(GHCFLAGS) -c $< -o $@

Util/Dpdk.$(OSUF): Util/Dpdk_.$(OSUF) Util/dpdk.$(OSUF)
	echo "################################################"
	#ld -i $(wildcard ../dpdk-1.5.0r1/x86_64-default-linuxapp-gcc/lib/*.a) $^ -o $@
	ld -i $(wildcard ofiles/*.o) $^ -o $@
	#ld -i  $^ -o $@


# Converts graphs into pdfs
models: graph_run
	./convertDot.sh

base: Makefile *.hs */*.hs

clean:
	rm -f *.hi *.o */*.hi */*.o */*/*.hi */*/*.o $(BINARIES)
	rm -f *.pdf *.dot *.png

