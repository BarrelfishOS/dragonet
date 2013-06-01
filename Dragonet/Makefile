
compile: Conditions.hs
	ghc Conditions.hs -o test1 -Wall # -Werror

run:	compile
	./test1 | tee netConditions.dot
	dot -Tpng -o netConditions.png netConditions.dot
	dot -Tpdf -o netConditions.pdf netConditions.dot


compileG: Graph.hs
	ghc Graph.hs -o GraphTest -Wall # -Werror

runG:	compileG
	./GraphTest > testG.dot
	dot -Tpng -o testG.png testG.dot
	cat testG.dot

clean:
	rm -f *.hi *.o test1 GraphTest  testG.dot netConditions.dot netConditions.png

