
e1kExample:	*.hs
	ghc e1kExample.hs -o e1kExample -fwarn-incomplete-patterns

LPG:	*.hs
	ghc ExampleLPG.hs -o ExampleLPG -fwarn-incomplete-patterns

runEX: e1kExample
	./e1kExample | tee config.click
	clicky -f config.click --pdf=config.pdf
#	evince config.pdf &

run: LPG
	./ExampleLPG | tee LPG.click
	clicky -f LPG.click --pdf=LPG.pdf
#	evince LPG.pdf &

clean:
	rm -f *.hi *.o e1kExample
	rm -f config.click config.pdf
	rm -f LPG.click LPG.pdf

