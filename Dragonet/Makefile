
all: E1k_run NetworkProcessing_run LPG_run

E1k_compile: E1k.hs Base
	ghc E1k.hs -o E1k -Wall # -Werror

E1k_run: E1k_compile
	./E1k > E1k.dot
	# ./E1k | tee E1k.dot
	dot -Tpng -o E1k.png E1k.dot
	dot -Tpdf -o E1k.pdf E1k.dot

E1k_clean:
	rm -f E1k.hi E1k.o E1k E1k.dot E1k.png E1k.pdf


LPG_compile: LPG.hs Base
	ghc LPG.hs -o LPG -Wall # -Werror

LPG_run: LPG_compile
	./LPG > LPG.dot
	# ./LPG | tee LPG.dot
	dot -Tpng -o LPG.png LPG.dot
	dot -Tpdf -o LPG.pdf LPG.dot

LPG_clean:
	rm -f LPG.hi LPG.o LPG LPG.dot LPG.png LPG.pdf

NetworkProcessing_compile: NetworkProcessing.hs Base
	ghc NetworkProcessing.hs -o NetworkProcessing -Wall # -Werror

NetworkProcessing_run: NetworkProcessing_compile
	./NetworkProcessing > NetworkProcessing.dot
	# ./NetworkProcessing | tee NetworkProcessing.dot
	dot -Tpng -o NetworkProcessing.png NetworkProcessing.dot
	dot -Tpdf -o NetworkProcessing.pdf NetworkProcessing.dot

NetworkProcessing_clean:
	rm -f NetworkProcessing.hi NetworkProcessing.o NetworkProcessing
	rm -f NetworkProcessing.dot NetworkProcessing.png NetworkProcessing.pdf

Base: Computations.hs Makefile

clean: E1k_clean NetworkProcessing_clean
	rm -f *.hi *.o

