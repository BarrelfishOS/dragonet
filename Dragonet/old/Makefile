BINARIES=constraints plos13
GHCFLAGS=-Wall -O2

#all: dragonet
all: models

constraints: base
	ghc $(GHCFLAGS) testConstraints.hs -o constraints
constraints_run: constraints
	./constraints

plos13:
	ghc $(GHCFLAGS) testPLOS13.hs -o plos13
plos13_run: plos13
	./plos13

models: constraints_run
	./convertDot.sh

base: Makefile *.hs *.unicorn */*.hs

clean:
	rm -f *.hi *.o */*.hi */*.o $(BINARIES)
	rm -f *.pdf *.dot *.png

