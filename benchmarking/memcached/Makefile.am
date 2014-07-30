bin_PROGRAMS = memcached
pkginclude_HEADERS = protocol_binary.h
noinst_PROGRAMS = memcached-debug sizes testapp timedrun

BUILT_SOURCES=

testapp_SOURCES = testapp.c util.c util.h

timedrun_SOURCES = timedrun.c

memcached_SOURCES = memcached.c memcached.h \
                    hash.c hash.h \
                    slabs.c slabs.h \
                    items.c items.h \
                    assoc.c assoc.h \
                    thread.c daemon.c \
                    stats.c stats.h \
                    util.c util.h \
                    trace.h cache.h sasl_defs.h \
		    dnet_interface.c \
		    dnet_interface.h

if BUILD_CACHE
memcached_SOURCES += cache.c
testapp_SOURCES += cache.c
endif

if BUILD_SOLARIS_PRIVS
memcached_SOURCES += solaris_priv.c
endif

if ENABLE_SASL
memcached_SOURCES += sasl_defs.c
endif

BULKTRANSPORT_INCLUDES = "-I../../external/bulktransfer/include/"
BARRELFISH_INCLUDES = "-I../../external/barrelfish/include/"
BULKTRANSPORTLIB = $(shell find ../../external/bulktransfer/dist/ -name "libHSbulktransfer*" -exec dirname {}  \;)

BARRELFISHLIB = $(shell find ../../external/barrelfish/dist/ -name "libHSbarrelfish*" -exec dirname {} \;)

SHMCHANLIB = $(shell find ../../external/shmchan/dist/ -name "libHSshmchan*" -exec dirname {}  \;)

DRAGONETAPP_INCLUDES = -I../../Dragonet/c_impl/include/ \
		       -I../../Dragonet/lib/Dragonet/Pipelines/ \
		       -I../../Dragonet/apps/lib/include/
DRAGONETAPPLIB = $(shell find ../../Dragonet/apps/dist/ -name "libHSapps*" -exec dirname {} \;)

memcached_debug_SOURCES = $(memcached_SOURCES)
memcached_CPPFLAGS = -DNDEBUG -DENABLE_DRAGONET $(BULKTRANSPORT_INCLUDES) $(BARRELFISH_INCLUDES) $(DRAGONETAPP_INCLUDES)
memcached_debug_LDADD = @PROFILER_LDFLAGS@
memcached_debug_CFLAGS = @PROFILER_FLAGS@

# Note: the order in which you link the library is important.  The apps library
# needs to be first as it is using things from bulktransfer, shm and barrelfish
# libraries.
memcached_LDADD = -L$(DRAGONETAPPLIB) -lHSapps-0.1.0.0 \
		    -L$(BULKTRANSPORTLIB) -lHSbulktransfer-0.1.0.0 \
		    -L$(SHMCHANLIB) -lHSshmchan-0.1.0.0 \
		    -L$(BARRELFISHLIB) -lHSbarrelfish-0.1.0.0 \
		    -lrt

memcached_DEPENDENCIES =
memcached_debug_DEPENDENCIES =
CLEANFILES=

if BUILD_DTRACE
BUILT_SOURCES += memcached_dtrace.h
CLEANFILES += memcached_dtrace.h
endif

if DTRACE_INSTRUMENT_OBJ
memcached_LDADD += memcached_dtrace.o
memcached_DEPENDENCIES += memcached_dtrace.o
memcached_debug_LDADD += memcached_debug_dtrace.o
memcached_debug_DEPENDENCIES += memcached_debug_dtrace.o
CLEANFILES += memcached_dtrace.o memcached_debug_dtrace.o
endif

memcached_dtrace.h: memcached_dtrace.d
	${DTRACE} -h -s memcached_dtrace.d
	sed -e 's,void \*,const void \*,g' memcached_dtrace.h | \
            sed -e 's,char \*,const char \*,g' | tr '\t' ' ' > mmc_dtrace.tmp
	mv mmc_dtrace.tmp memcached_dtrace.h

memcached_dtrace.o: $(memcached_OBJECTS)
	$(DTRACE) $(DTRACEFLAGS) -G -o memcached_dtrace.o -s ${srcdir}/memcached_dtrace.d $(memcached_OBJECTS)

memcached_debug_dtrace.o: $(memcached_debug_OBJECTS)
	$(DTRACE) $(DTRACEFLAGS) -G -o memcached_debug_dtrace.o -s ${srcdir}/memcached_dtrace.d $(memcached_debug_OBJECTS)


SUBDIRS = doc
DIST_DIRS = scripts
EXTRA_DIST = doc scripts t memcached.spec memcached_dtrace.d version.m4 README.md

MOSTLYCLEANFILES = *.gcov *.gcno *.gcda *.tcov

test:	memcached-debug sizes testapp
	$(srcdir)/sizes
	$(srcdir)/testapp
	prove $(srcdir)/t
	@if test `basename $(PROFILER)` = "gcov"; then \
	  for file in memcached_debug-*.gc??; do \
	    mv -f $$file `echo $$file | sed 's/memcached_debug-//'`; \
	  done && \
	  for file in *.gcda; do \
	    srcfile=`echo $$file | sed 's/.gcda/.c/'`; \
	    if test -n "`echo $(memcached_debug_SOURCES) | grep $$srcfile`"; then \
	      echo `$(PROFILER) $$srcfile` | sed 's/'$$srcfile':.*//'; \
	    fi \
	  done \
	elif test `basename $(PROFILER)` = "tcov"; then \
	  files=`grep SRCFILE memcached-debug.profile/tcovd | sed 's/SRCFILE://' | sort | uniq` && \
	  $(PROFILER) -x memcached-debug.profile $$files 2>&1; \
	  for file in *.tcov; do \
	    srcfile=`echo $$file | sed 's/.tcov//'`; \
	    if test -n "`echo $(memcached_debug_SOURCES) | grep $$srcfile`"; then \
	      echo $$srcfile : `grep 'Percent of the file executed' $$file`; \
	    fi \
	  done \
	else :; fi

docs:
	(cat Doxyfile ; echo "PROJECT_NUMBER=`cat version.num`") | doxygen -

dist-hook:
	rm -f $(distdir)/*/*~ $(distdir)/t/lib/*~ $(distdir)/*~

maintainer-clean-local:
	-rm Makefile.in
	-rm aclocal.m4
	-rm config.guess
	-rm config.sub
	-rm depcomp
	-rm install-sh
	-rm ltmain.sh
	-rm missing
	-rm configure
	-rm config.log
	-rm config.status
