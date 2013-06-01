
all: E1k_run NetworkProcessing_run

E1k_compile: E1k.hs Base
	ghc E1k.hs -o E1k -Wall # -Werror

E1k_run: E1k_compile
	./E1k | tee E1k.dot
	dot -Tpng -o E1k.png E1k.dot
	dot -Tpdf -o E1k.pdf E1k.dot

E1k_clean:
	rm -f E1k.hi E1k.o E1k E1k.dot E1k.png E1k.pdf


NetworkProcessing_compile: NetworkProcessing.hs Base
	ghc NetworkProcessing.hs -o NetworkProcessing -Wall # -Werror

NetworkProcessing_run: NetworkProcessing_compile
	./NetworkProcessing | tee NetworkProcessing.dot
	dot -Tpng -o NetworkProcessing.png NetworkProcessing.dot
	dot -Tpdf -o NetworkProcessing.pdf NetworkProcessing.dot

NetworkProcessing_clean:
	rm -f NetworkProcessing.hi NetworkProcessing.o NetworkProcessing
	rm -f NetworkProcessing.dot NetworkProcessing.png NetworkProcessing.pdf

Base: Computations.hs

clean: E1k_clean NetworkProcessing_clean
	rm -f *.hi *.o

