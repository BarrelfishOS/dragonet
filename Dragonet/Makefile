BINARIES=graph plos13 impl tuntap confgen
GHCFLAGS=-Wall -O2 -XRankNTypes
# Debuging for DSL -> Haskell process
# GHCFLAGS+=-ddump-splices

all: models

graph: base
	ghc $(GHCFLAGS) testGraph.hs -o graph
graph_run: graph
	./graph

confgen: base
	ghc $(GHCFLAGS) testConfGen.hs -o confgen
confgen_run: confgen
	./confgen
	./convertConfGen.sh

incremental: base
	ghc $(GHCFLAGS) testIncremental.hs -o incremental
incremental_run: incremental
	./incremental


plos13: base
	ghc $(GHCFLAGS) testPLOS13.hs -o plos13
plos13_run: plos13
	./plos13

impl: base
	ghc $(GHCFLAGS) testImpl.hs -o impl

impl_run: impl
	./impl

tuntap : base Util/Tap.o
	ghc $(GHCFLAGS) -threaded tuntapTests.hs -o tuntap

tuntap_run: tuntap
	./tuntap

## Util/Tap

Util/tap.o: Util/tap.c
	gcc $(CFLAGS) -c $< -o $@

Util/Tap_.o: Util/Tap.hs
	ghc $(GHCFLAGS) -c $< -o $@

Util/Tap.o: Util/Tap_.o Util/tap.o
	ld -i $^ -o $@

models: graph_run
	./convertDot.sh

base: Makefile *.hs */*.hs

clean:
	rm -f *.hi *.o */*.hi */*.o */*/*.hi */*/*.o $(BINARIES)
	rm -f *.pdf *.dot *.png

