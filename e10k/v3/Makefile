
e1kExample:	*.hs
	ghc e10k.hs -o e10k -fwarn-incomplete-patterns

runEX: e1kExample
	./e10k | tee e10k.click
	clicky -f e10k.click --pdf=e10k.pdf
	xpdf e10k.pdf &

run: LPG
	./ExampleLPG | tee LPG.click
	clicky -f LPG.click --pdf=LPG.pdf
#	evince LPG.pdf &

clean:
	rm -f *.hi *.o e10k
	rm -f e10k.click e10k.pdf

