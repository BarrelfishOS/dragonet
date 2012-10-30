
GraphGenerator:	*.hs
	ghc GraphGenerator.hs -o GraphGenerator -fwarn-incomplete-uni-patterns -Wall -Werror

run:	GraphGenerator
	./GraphGenerator

clean:
	rm -f *.hi *.o GraphGenerator

