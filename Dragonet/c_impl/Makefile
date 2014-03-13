SRCFILES= $(wildcard *.c)
SRCFILES+= $(wildcard support/*.c)
OBJFILES= $(addsuffix .o,$(basename $(SRCFILES)))
CFLAGS+= -Iinclude -Wall -O2


TTSRCFILES= $(wildcard tuntapDrv/*.c)
TTSRCFILES+= $(SRCFILES)
TTOBJFILES= $(addsuffix .o,$(basename $(TTSRCFILES)))

tuntap: $(TTOBJFILES)
	gcc -o $@ $^

tuntap_run: tuntap
	sudo ./tuntap

clean:
	rm -f tuntap $(OBJFILES) $(TTOBJFILES)

