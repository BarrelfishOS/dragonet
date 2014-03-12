SRCFILES= $(wildcard *.c)
SRCFILES+= $(wildcard support/*.c)
OBJFILES= $(addsuffix .o,$(basename $(SRCFILES)))
CFLAGS+= -Iinclude -Wall -O2

test: $(OBJFILES)
	gcc -o $@ $^

run: test
	./test

clean:
	rm -f test $(OBJFILES)
