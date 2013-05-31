
both: runEX

e10k:	*.hs
	ghc e10k.hs -o e10k -fwarn-incomplete-patterns

runEX: e10k
	./e10k > e10k.dot
	./e10k | grep -i -v dropped > e10k.dot
	dot -Tpdf -o e10k.pdf e10k.dot
#	evince e10k.pdf

clean:
	rm -f *.hi *.o e10k
	rm -f e10k.click e10k.pdf
#	rm -f LPG.click LPG.pdf

