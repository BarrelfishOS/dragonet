

RXPath:	*.hs
	ghc RXPath.hs -o RXPath -fwarn-incomplete-patterns

run: RXPath
	./RXPath | tee config.click
	clicky -f config.click --pdf=config.pdf
clean:
	rm -f *.hi *.o RXPath
	rm -f config.click config.pdf

