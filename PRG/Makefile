
all: e10k e1k tulip

e1k:	base e1k.hs
	ghc e1k.hs -o e1k -fwarn-incomplete-patterns
	./e1k | grep -i -v dropped > e1k.dot
	dot -Tpdf -o e1k.pdf e1k.dot
#	evince e1k.pdf

e10k:	base e10k.hs
	ghc e10k.hs -o e10k -fwarn-incomplete-patterns
	./e10k | grep -i -v dropped > e10k.dot
	dot -Tpdf -o e10k.pdf e10k.dot
#	evince e10k.pdf

tulip:	base tulip.hs
	ghc tulip.hs -o tulip -fwarn-incomplete-patterns
	./tulip | grep -i -v dropped > tulip.dot
	dot -Tpdf -o tulip.pdf tulip.dot
#	evince tulip.pdf

base: ConvertDecision.hs DecisionTree.hs Makefile

clean:
	rm -f *.hi *.o
	rm -f e1k e1k.dot e1k.pdf
	rm -f e10k e10k.dot e10k.pdf
	rm -f tulip tulip.dot tulip.pdf

