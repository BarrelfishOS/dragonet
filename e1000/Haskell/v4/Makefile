
GraphGenerator:	*.hs
	ghc GraphGenerator.hs -o GraphGenerator -fwarn-incomplete-patterns

run:	GraphGenerator
	./GraphGenerator

clean:
	rm -f *.hi *.o GraphGenerator

