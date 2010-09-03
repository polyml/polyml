AUTOMAKE_OPTIONS=foreign

AM_CPPFLAGS = $(OSFLAG) -Wall
AM_CFLAGS = $(OSFLAG) -Wall -fno-strict-aliasing
AM_ASFLAGS = $(OSFLAG)
AM_CCASFLAGS = $(OSFLAG)

lib_LTLIBRARIES = libpolyml.la
libpolyml_la_LDFLAGS = -version-info 3:0:0

if NO_UNDEFINED
# Force all references to be defined to build the DLL.
libpolyml_la_LDFLAGS += -no-undefined
endif

# Select the architecture-specific modules
if ARCHI386
ARCHSOURCE = x86_dep.cpp x86asmtemp.S
else
if ARCHPPC
ARCHSOURCE = power_dep.cpp ppcasmtemp.s
else
if ARCHSPARC
ARCHSOURCE = sparc_dep.cpp sparc_assembly.S
else
if ARCHINTERPRET
ARCHSOURCE = interpret.cpp
else
if ARCHINTERPRET64
ARCHSOURCE = interpret.cpp
else
if ARCHX86_64
ARCHSOURCE = x86_dep.cpp x86asmtemp.S
else
endif
endif
endif
endif
endif
endif

# Select the object-format-specific modules
if EXPPECOFF
EXPORTSOURCE = pecoffexport.cpp
else
if EXPELF
EXPORTSOURCE = elfexport.cpp
else
if EXPMACHO
EXPORTSOURCE = machoexport.cpp
endif
endif
endif

if NATIVE_WINDOWS
OSSOURCE = Console.cpp windows_specific.cpp
else
OSSOURCE = unix_specific.cpp
endif

noinst_HEADERS = \
	arb.h \
	basicio.h \
	bitmap.h \
	check_objects.h \
	Console.h \
	diagnostics.h \
	elfexport.h \
	errors.h \
	exporter.h \
	foreign.h \
	gc.h \
	globals.h \
	int_opcodes.h \
	io_internal.h \
	locking.h \
	machine_dep.h \
	machoexport.h \
	memmgr.h \
	mpoly.h \
	network.h \
	noreturn.h \
	objsize.h \
	osmem.h \
	os_specific.h \
	pecoffexport.h \
	pexport.h \
	PolyControl.h \
	poly_specific.h \
	polystring.h \
	process_env.h \
	processes.h \
	profiling.h \
	proper_io.h \
	realconv.h \
	reals.h \
	rts_module.h \
	run_time.h \
	savestate.h \
	save_vec.h \
	scanaddrs.h \
	sharedata.h \
	sighandler.h \
	sys.h \
	timing.h \
	version.h \
	xcall_numbers.h \
	xwindows.h

libpolyml_la_SOURCES = mpoly.cpp gc.cpp diagnostics.cpp \
    arb.cpp reals.cpp timing.cpp processes.cpp profiling.cpp \
    realconv.cpp objsize.cpp proper_io.cpp foreign.cpp \
    process_env.cpp basicio.cpp network.cpp \
    sighandler.cpp xwindows.cpp check_objects.cpp scanaddrs.cpp \
    polystring.cpp run_time.cpp osmem.cpp save_vec.cpp \
    rts_module.cpp bitmap.cpp poly_specific.cpp \
    exporter.cpp sharedata.cpp memmgr.cpp pexport.cpp \
    locking.cpp savestate.cpp \
    $(ARCHSOURCE) $(EXPORTSOURCE) $(OSSOURCE)

# Convert the portable format to a native binary
polyexport.o: polyimport exportPoly.sml
	(cd ..; driver/polyimport driver/$(POLYIMPORT) < driver/exportPoly.sml > /dev/null)

# Special rule for x86 assembly code.
x86asmtemp.S: x86asm.asm
	sed -f $(srcdir)/masm2gas <  x86asm.asm > $@ || { rm -f $@; false; }

# Special rule for Power PC.  This is needed because of pecularities with MAC OS X.
ppcasmtemp.s: power_assembly.S
	$(CPP) $(ARCHFLAG) $(OSFLAG) -DGCC $< | tr ';' '\012' > ppcasmtemp.s

clean-local:
	rm -f x86asmtemp.S ppcasmtemp.s
