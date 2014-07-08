OBJECTS = fancyEchoLinux.o

default: fancyEchoLinux

%.o: %.c
	gcc -g -Wall -c $< -o $@

fancyEchoLinux: $(OBJECTS)
	gcc -g -Wall $(OBJECTS) -o $@ -lpthread

clean:
	-rm -f $(OBJECTS)
	-rm -f fancyEchoLinux
