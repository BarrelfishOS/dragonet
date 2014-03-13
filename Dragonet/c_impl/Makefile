.PHONY: all

SRCFILES= $(wildcard *.c)
SRCFILES+= $(wildcard support/*.c)
OBJFILES= $(addsuffix .o,$(basename $(SRCFILES)))
CFLAGS+= -Iinclude -Wall -O3 -flto
LDFLAGS= -O3 -flto

BCSRCFILES = $(filter-out test.c,$(SRCFILES))
BCFILES = $(addsuffix .bc,$(basename $(BCSRCFILES))) # llvm bytecode

all: test llvm_pg_impl.bc

test: $(OBJFILES)
	$(CC) $(LDFLAGS) -o $@ $^

run: test
	./test

%.bc : %.c
	clang $(CFLAGS) -c -emit-llvm $^ -o $@

llvm_pg_impl.bc:  $(BCFILES)
	llvm-link-3.5 $^ -o $@

clean:
	rm -f test llvm_pg_impl.bc $(OBJFILES) $(BCFILES)
