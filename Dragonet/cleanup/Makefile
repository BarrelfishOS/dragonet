BINARIES=graph
GHCFLAGS=-Wall -O2

#all: dragonet
all: models

graph: base
	ghc $(GHCFLAGS) testGraph.hs -o graph
graph_run: graph
	./graph

models: graph_run
	../convertDot.sh

base: Makefile *.hs */*.hs

clean:
	rm -f *.hi *.o */*.hi */*.o $(BINARIES)
	rm -f *.pdf *.dot *.png

