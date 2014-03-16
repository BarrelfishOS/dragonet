.PHONY: all

SRCFILES= $(wildcard *.c)
SRCFILES+= $(wildcard support/*.c)
OBJFILES= $(addsuffix .o,$(basename $(SRCFILES)))
CFLAGS+= -Iinclude -Wall -O2 -flto
LDFLAGS+= -O2 -flto   # having both these flags causes incorrect calculation of checksum for UDP
#LDFLAGS+= -flto

BCSRCFILES = $(filter-out test.c,$(SRCFILES))
BCFILES = $(addsuffix .bc,$(basename $(BCSRCFILES))) # llvm bytecode

all: test llvm_pg_impl.bc

.DEFAULT_GOAL := tuntap
#.DEFAULT_GOAL := sfOnload

TTSRCFILES= $(wildcard tuntapDrv/*.c)
TTSRCFILES+= $(SRCFILES)
TTOBJFILES= $(addsuffix .o,$(basename $(TTSRCFILES)))

SFSRCFILES= $(wildcard onloadDrv/*.c)
SFSRCFILES+= $(SRCFILES)
SFOBJFILES= $(addsuffix .o,$(basename $(SFSRCFILES)))

tuntap: $(TTOBJFILES)
	$(CC) $(LDFLAGS) -o $@ $^

tuntap_run: tuntap
	sudo ./tuntap


sfOnload: $(SFOBJFILES) onloadDrv/efdragonet.o onloadDrv/efvi_sfw.o
	$(CC) $(LDFLAGS) -o $@ $^ -L../../openonload-201310-u2/build/gnu_x86_64/lib/ciul/ -lciul -lpthread

sfOnload_run: sfOnload
	export LD_LIBRARY_PATH=/home/ubuntu/dragonet/openonload-201310-u2/build/gnu_x86_64/lib/ciul/ ; ./sfOnload

onloadDrv/efdragonet.o: sf_onloadLibs
	cp ../../openonload-201310-u2/build/gnu_x86_64/tests/ef_vi/efdragonet.o  onloadDrv/

onloadDrv/efvi_sfw.o: sf_onloadLibs
	cp ../../openonload-201310-u2/build/gnu_x86_64/tests/ef_vi/efvi_sfw.o onloadDrv/

sf_onloadLibs:
	#cd ../openonload-201310-u2/ && ./scripts/onload_build --user

%.bc : %.c
	clang $(CFLAGS) -c -emit-llvm $^ -o $@

llvm_pg_impl.bc:  $(BCFILES)
	llvm-link-3.5 $^ -o $@

clean:
	rm -f tuntap sfOnload llvm_pg_impl.bc $(OBJFILES) $(TTOBJFILES) $(SFOBJFILES) $(BCFILES)
