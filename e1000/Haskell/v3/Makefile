
both: e1kExample LPGexpanded

e1kExample:	*.hs
	ghc e1kExample.hs -o e1kExample -fwarn-incomplete-patterns

LPGexpanded:	*.hs
	ghc ExampleLPG.hs -o ExampleLPG -fwarn-incomplete-patterns

runBoth: runEX runExpanded

runEX: e1kExample
	./e1kExample > e1k.dot
	dot -Tpdf -o e1kExample.pdf e1k.dot

runExpanded: LPGexpanded
	./ExampleLPG | grep -i -v dropped > LPGexpanded.dot
	dot -Tpdf -o LPGexpanded.pdf LPGexpanded.dot

clean:
	rm -f *.hi *.o e1kExample
	rm -f e1kExample.click e1kExample.pdf
	rm -f LPG.click LPG.pdf

