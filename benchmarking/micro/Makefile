OBJECTS = fancyEchoLinux.o
BINARIES= fancyEchoLinux udp_rttbench udp_tpbench

default: $(BINARIES)

%.o: %.c
	gcc -g -Wall -c $< -o $@

fancyEchoLinux: $(OBJECTS)
	gcc -g -Wall $(OBJECTS) -o $@ -lpthread

udp_rttbench: udp_rttbench.c
	gcc -O3 -o udp_rttbench udp_rttbench.c -lrt

udp_tpbench: udp_tpbench.c
	gcc -O3 -o udp_tpbench udp_tpbench.c -lrt
clean:
	-rm -f $(OBJECTS)
	-rm -f $(BINARIES)
