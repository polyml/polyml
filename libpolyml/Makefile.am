AUTOMAKE_OPTIONS=foreign

AM_CPPFLAGS = $(OSFLAG) -Wall
AM_CFLAGS = $(OSFLAG) -Wall -fno-strict-aliasing
AM_ASFLAGS = $(OSFLAG)
AM_CCASFLAGS = $(OSFLAG)

lib_LTLIBRARIES = libpolyml.la
libpolyml_la_LDFLAGS = -version-info 1:0

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
	sed -f masm2gas < x86asm.asm > x86asmtemp.S

# Special rule for Power PC.  This is needed because of pecularities with MAC OS X.
ppcasmtemp.s: power_assembly.S
	$(CPP) $(ARCHFLAG) $(OSFLAG) -DGCC $< | tr ';' '\012' > ppcasmtemp.s

clean-local:
	rm -f x86asmtemp.S ppcasmtemp.s
