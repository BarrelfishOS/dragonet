SRCFILES= $(wildcard *.c)
OBJFILES= $(addsuffix .o,$(basename $(SRCFILES)))
CFLAGS+= -Iinclude -Wall -O2

test: $(OBJFILES)
	gcc -o $@ $^

clean:
	rm -f test $(OBJFILES)
