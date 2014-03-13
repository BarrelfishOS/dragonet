SRCFILES= $(wildcard *.c)
SRCFILES+= $(wildcard support/*.c)
OBJFILES= $(addsuffix .o,$(basename $(SRCFILES)))
CFLAGS+= -Iinclude -Wall -O2

#.DEFAULT_GOAL := tuntap
.DEFAULT_GOAL := sfOnload

TTSRCFILES= $(wildcard tuntapDrv/*.c)
TTSRCFILES+= $(SRCFILES)
TTOBJFILES= $(addsuffix .o,$(basename $(TTSRCFILES)))

SFSRCFILES= $(wildcard onloadDrv/*.c)
SFSRCFILES+= $(SRCFILES)
SFOBJFILES= $(addsuffix .o,$(basename $(SFSRCFILES)))

tuntap: $(TTOBJFILES)
	gcc -o $@ $^

tuntap_run: tuntap
	sudo ./tuntap


sfOnload: $(SFOBJFILES) onloadDrv/efdragonet.o onloadDrv/efvi_sfw.o
	 gcc -o $@ $^ -L../../openonload-201310-u2/build/gnu_x86_64/lib/ciul/ -lciul -lpthread

sfOnload_run: sfOnload
	export LD_LIBRARY_PATH=/home/ubuntu/dragonet/openonload-201310-u2/build/gnu_x86_64/lib/ciul/ ; ./sfOnload

onloadDrv/efdragonet.o: sf_onloadLibs
	cp ../../openonload-201310-u2/build/gnu_x86_64/tests/ef_vi/efdragonet.o  onloadDrv/

onloadDrv/efvi_sfw.o: sf_onloadLibs
	cp ../../openonload-201310-u2/build/gnu_x86_64/tests/ef_vi/efvi_sfw.o onloadDrv/

sf_onloadLibs:
	#cd ../openonload-201310-u2/ && ./scripts/onload_build --user

clean:
	rm -f tuntap $(OBJFILES) $(TTOBJFILES) $(SFOBJFILES)

