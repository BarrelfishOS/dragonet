.PHONY: all

SRCFILES= $(wildcard *.c)
SRCFILES+= $(wildcard support/*.c)
OBJFILES= $(addsuffix .o,$(basename $(SRCFILES)))
CFLAGS+= -Iinclude -Wall -O2

BCSRCFILES = $(filter-out test.c,$(SRCFILES))
BCFILES = $(addsuffix .bc,$(basename $(BCSRCFILES))) # llvm bytecode

all: test llvm_pg_impl.bc

test: $(OBJFILES)
	gcc -o $@ $^

run: test
	./test

%.bc : %.c
	clang $(CFLAGS) -c -emit-llvm $^ -o $@

llvm_pg_impl.bc:  $(BCFILES)
	llvm-link-3.5 $^ -o $@

clean:
	rm -f test llvm_pg_impl.bc $(OBJFILES) $(BCFILES)
