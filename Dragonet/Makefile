
all: dragonet

dragonet_compile: Dragonet.hs base
	ghc -rtsopts Dragonet.hs -o Dragonet -Wall # -Werror

dragonet: dragonet_compile
	./Dragonet

models: dragonet
	dot -Tpng -o E1kPRGConf.png E1kPRGConf.dot
	dot -Tpdf -o E1kPRGConf.pdf E1kPRGConf.dot
	dot -Tpng -o LPG.png LPG.dot
	dot -Tpdf -o LPG.pdf LPG.dot
	dot -Tpng -o NetDep.png NetDep.dot
	dot -Tpdf -o NetDep.pdf NetDep.dot
	dot -Tpng -o E1kPRG.png E1kPRG.dot
	dot -Tpdf -o E1kPRG.pdf E1kPRG.dot
	dot -Tpng -o Embedded.png Embedded.dot
	dot -Tpdf -o Embedded.pdf Embedded.dot
	dot -Tpng -o EmbeddedV2.png EmbeddedV2.dot
	dot -Tpdf -o EmbeddedV2.pdf EmbeddedV2.dot



base: Computations.hs Configurations.hs E1k.hs LPG.hs MyGraph.hs NetworkProcessing.hs Makefile

clean:
	rm -f *.hi *.o Dragonet
	rm -f *.pdf *.dot *.png

