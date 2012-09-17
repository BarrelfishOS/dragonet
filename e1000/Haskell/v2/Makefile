
e1kExample:	*.hs
	ghc e1kExample.hs -o e1kExample -fwarn-incomplete-patterns

LPG:	*.hs
	ghc ExampleLPG.hs -o ExampleLPG -fwarn-incomplete-patterns

runEX: e1kExample
	./e1kExample | tee e1kExample.click
	clicky -f e1kExample.click --pdf=e1kExample.pdf
#	evince e1kExample.pdf &

run: LPG
	./ExampleLPG | tee LPG.click
	clicky -f LPG.click --pdf=LPG.pdf
#	evince LPG.pdf &

clean:
	rm -f *.hi *.o e1kExample
	rm -f config.click config.pdf
	rm -f LPG.click LPG.pdf

