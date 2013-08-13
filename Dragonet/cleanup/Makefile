BINARIES=graph plos13
GHCFLAGS=-Wall -O2

#all: dragonet
all: models

graph: base
	ghc $(GHCFLAGS) testGraph.hs -o graph
graph_run: graph
	./graph

plos13: base
	ghc $(GHCFLAGS) testPLOS13.hs -o plos13
plos13_run: plos13
	./plos13


models: graph_run
	../convertDot.sh

base: Makefile *.hs */*.hs

clean:
	rm -f *.hi *.o */*.hi */*.o $(BINARIES)
	rm -f *.pdf *.dot *.png

