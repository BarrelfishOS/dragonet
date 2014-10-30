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

tuntap: $(TTOBJFILES)
	$(CC) $(LDFLAGS) -o $@ $^

tuntap_run: tuntap
	sudo ./tuntap

############################## SF Openonload compilation ###############

SFSRCFILES= $(wildcard onloadDrv/*.c)
SFSRCFILES+= $(SRCFILES)
SFOBJFILES= $(addsuffix .o,$(basename $(SFSRCFILES)))

sfOnload: $(SFOBJFILES) onloadDrv/efdragonet.o onloadDrv/efvi_sfw.o
	$(CC) $(LDFLAGS) -o $@ $^ -L ../../openonload-201310-u2/build/gnu_x86_64/lib/ciul/ -lciul -lpthread

sfOnload_run: sfOnload
	export LD_LIBRARY_PATH=/home/ubuntu/dragonet/openonload-201310-u2/build/gnu_x86_64/lib/ciul/ ; ./sfOnload

onloadDrv/efdragonet.o: sf_onloadLibs
	cp ../../openonload-201310-u2/build/gnu_x86_64/tests/ef_vi/efdragonet.o  onloadDrv/

onloadDrv/efvi_sfw.o: sf_onloadLibs
	cp ../../openonload-201310-u2/build/gnu_x86_64/tests/ef_vi/efvi_sfw.o onloadDrv/

sf_onloadLibs:
	# cd ../../openonload-201310-u2/ && ./scripts/onload_build --user


############################## DPDK compilation ###############

DPDKSRCFILES= $(wildcard dpdkDrv/*.c)
DPDKSRCFILES+= $(SRCFILES)
DPDKOBJFILES= $(addsuffix .o,$(basename $(DPDKSRCFILES)))

DPDKLIBOBJS=dpdkDrv/cmdline.o dpdkDrv/dpdkData.o dpdkDrv/config.o
DPDKLIBOBJS+=dpdkDrv/csumonly.o dpdkDrv/iofwd.o dpdkDrv/macfwd.o
DPDKLIBOBJS+=dpdkDrv/parameters.o dpdkDrv/rxonly.o dpdkDrv/dpdkControl.o
DPDKLIBOBJS+=dpdkDrv/txonly.o

cDpdk: $(DPDKOBJFILES) objfiles
	#DPDKLIBOBJS = $(wildcard dpdkDrv/*.o)
	$(CC) $(LDFLAGS) -o $@ $(DPDKOBJFILES) $(DPDKLIBOBJS) -L../../dpdk-1.5.0r1/build/lib/ -lintel_dpdk  -ldpdk_driver -lpthread -ldl -lrt
	# -lrt needed for clock_gettime
	# -ldl needed for dlopen and friends

cDpdk_run: cDpdk
	#../../dpdk-1.5.0r1/tools/pci_unbind.py -b igb_uio 81:00.0
	export LD_LIBRARY_PATH=/home/ubuntu/dragonet/dpdk-1.5.0r1/build/lib/ ;   ./cDpdk

## Util/Dpdk
OSUF=o
RTE_TARGET ?= x86_64-default-linuxapp-gcc
RTE_SDK ?= ../../dpdk-1.5.0r1/

objfiles: ../../dpdk-1.5.0r1/app/test-pmd/dpdk.c # intel_dpdk
	echo "##########################"
	cd ../../dpdk-1.5.0r1/app/dpdkDriver/ && ./myMake.sh
	cp ../../dpdk-1.5.0r1/app/dpdkDriver/build/*.$(OSUF) dpdkDrv/


intel_dpdk: #../../dpdk-1.5.0r1/build/lib/libintel_dpdk.so
	cd ../../dpdk-1.5.0r1/ && ./doConfig.sh && $(MAKE)



############################## LLVM related compilation ###############

%.bc : %.c
	clang $(CFLAGS) -c -emit-llvm $^ -o $@

llvm_pg_impl.bc:  $(BCFILES)
	llvm-link-3.5 $^ -o $@

############################## cleanup ###############

clean:
	rm -f tuntap sfOnload cDpdk llvm_pg_impl.bc
	rm -f dpdkDrv/*.o
	rm -f onloadDrv/*.o
	rm -f tuntapDrv/*.o
	rm -f $(OBJFILES) $(TTOBJFILES) $(SFOBJFILES) $(BCFILES) $(DPDKOBJFILES)
