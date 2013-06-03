
all: Dragonet

Dragonet_compile: Dragonet.hs Base
	ghc Dragonet.hs -o Dragonet -Wall # -Werror

Dragonet: Dragonet_compile
	./Dragonet
	dot -Tpng -o LPG.png LPG.dot
	dot -Tpdf -o LPG.pdf LPG.dot
	dot -Tpng -o NetDep.png NetDep.dot
	dot -Tpdf -o NetDep.pdf NetDep.dot
	dot -Tpng -o E1kPRG.png E1kPRG.dot
	dot -Tpdf -o E1kPRG.pdf E1kPRG.dot

Base: Computations.hs E1k.hs LPG.hs MyGraph.hs NetworkProcessing.hs Makefile

clean:
	rm -f *.hi *.o Dragonet
	rm -f *.pdf *.dot *.png

