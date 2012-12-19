
GraphGenerator:	*.hs
	ghc -c -O DecisionTree.hs -Wall -Werror
	ghc -c -O Elements.hs -Wall -Werror
	ghc -c -O GraphGenerator.hs -Wall -Werror
	ghc -o GraphGenerator DecisionTree.o GraphGenerator.o Elements.o  -Wall -Werror
#	ghc GraphGenerator.hs -o GraphGenerator -fwarn-incomplete-uni-patterns -Wall -Werror

run:	GraphGenerator
	./GraphGenerator

clean:
	rm -f *.hi *.o GraphGenerator

