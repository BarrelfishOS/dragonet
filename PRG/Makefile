
all: e10k e1k tulip BCM5704

e1k:	base e1k.hs
	ghc e1k.hs -o e1k -fwarn-incomplete-patterns
	./e1k | grep -i -v dropped > e1k.dot
	dot -Tpdf -o e1k.pdf e1k.dot

e1k_clean: base_clean
	rm -f e1k e1k.dot e1k.pdf

e10k:	base e10k.hs
	ghc e10k.hs -o e10k -fwarn-incomplete-patterns
	./e10k | grep -i -v dropped > e10k.dot
	dot -Tpdf -o e10k.pdf e10k.dot

e10k_clean: base_clean
	rm -f e10k e10k.dot e10k.pdf

tulip:	base tulip.hs
	ghc tulip.hs -o tulip -fwarn-incomplete-patterns
	./tulip | grep -i -v dropped > tulip.dot
	dot -Tpdf -o tulip.pdf tulip.dot

tulip_clean: base_clean
	rm -f tulip tulip.dot tulip.pdf

BCM5704: base BCM5704.hs
	ghc BCM5704.hs -o BCM5704 -fwarn-incomplete-patterns
	./BCM5704 | grep -i -v dropped > BCM5704.dot
	./BCM5704 > BCM5704.dot
	dot -Tpdf -o BCM5704.pdf BCM5704.dot

BCM5704_clean: base_clean
	rm -f BCM5704 BCM5704.dot BCM5704.pdf

base: ConvertDecision.hs DecisionTree.hs Makefile

base_clean:
	rm -f *.hi *.o

clean: e1k_clean e10k_clean tulip_clean BCM5704_clean

