
all: models

dragonet_compile: Dragonet.hs base
	ghc -rtsopts Dragonet.hs -o Dragonet -Wall # -Werror

dragonet: dragonet_compile
	./Dragonet

models: dragonet
	./convertDot.sh

base: Makefile NetBasics.hs Operations.hs NetworkProcessing.hs E10kPRG.hs Dragonet.hs DotGenerator.hs

clean:
	rm -f *.hi *.o Dragonet
	rm -f *.pdf *.dot *.png

