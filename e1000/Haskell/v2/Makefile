

e1kExample:	*.hs
	ghc e1kExample.hs -o e1kExample -fwarn-incomplete-patterns

run: e1kExample
	./e1kExample | tee config.click
	clicky -f config.click --pdf=config.pdf
clean:
	rm -f *.hi *.o e1kExample
	rm -f config.click config.pdf

