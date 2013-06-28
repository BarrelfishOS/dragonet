
all: dragonet

dragonet_compile: Dragonet.hs base
	ghc -rtsopts Dragonet.hs -o Dragonet -Wall # -Werror

dragonet: dragonet_compile
	./Dragonet

models: dragonet
	./convertDot.sh

#base: Computations.hs Configurations.hs E1k.hs LPG.hs MyGraph.hs NetworkProcessing.hs Makefile
base: NetBasics.hs Operations.hs Dragonet.hs Makefile

clean:
	rm -f *.hi *.o Dragonet
	rm -f *.pdf *.dot *.png

