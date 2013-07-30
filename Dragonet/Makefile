
#all: dragonet
all: models

dragonet_compile: base
#	ghc -XQuasiQuotes -Wall testConstraints.hs -o Dragonet
	ghc -XQuasiQuotes testConstraints.hs -o Dragonet

dragonet: dragonet_compile
	./Dragonet

models: dragonet
	./convertDot.sh

base: Makefile *.hs *.unicorn

clean:
	rm -f *.hi *.o Dragonet
	rm -f *.pdf *.dot *.png

