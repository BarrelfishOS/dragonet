
#all: dragonet
all: models

dragonet_compile: base
	ghc -XQuasiQuotes -ddump-splices testConstraints.hs -o Dragonet
#	ghc -rtsopts Dragonet.hs -o Dragonet -Wall -fno-warn-unused-binds # -Werror

dragonet: dragonet_compile
	./Dragonet

models: dragonet
	./convertDot.sh

base: Makefile *.hs *.unicorn

clean:
	rm -f *.hi *.o Dragonet
	rm -f *.pdf *.dot *.png

