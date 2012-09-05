AUTOMAKE_OPTIONS=foreign
ACLOCAL_AMFLAGS= -I m4 --install

AM_CPPFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall
AM_CFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall -fno-strict-aliasing

POLYIMPORT_OPTIONS = -H 50
BOOTSTRAP_OPTIONS = -H 50

DIST_SUBDIRS = libpolyml libpolymain libffi

if INTERNAL_LIBFFI
SUBDIRS = libffi libpolyml libpolymain
else
SUBDIRS = libpolyml libpolymain
endif

bin_PROGRAMS = polyimport poly

man_MANS = poly.1 polyimport.1

# Select the architecture-specific modules
if ARCHI386
POLYIMPORT = $(srcdir)/imports/polymli386.txt
else
if ARCHINTERPRET
POLYIMPORT = $(srcdir)/imports/polymlint.txt
else
if ARCHINTERPRET64
POLYIMPORT = $(srcdir)/imports/polymlint64.txt
else
if ARCHX86_64
POLYIMPORT = $(srcdir)/imports/polymlx86_64.txt
else
endif
endif
endif
endif

noinst_HEADERS = polyexports.h

poly_LDFLAGS=
polyimport_LDFLAGS=
POLYRESOURCES=

if NATIVE_WINDOWS
if ARCHINTERPRET64
poly_LDFLAGS += -mwindows -Wl,-u,WinMain
else
if ARCHX86_64
poly_LDFLAGS += -mwindows -Wl,-u,WinMain
else
poly_LDFLAGS += -mwindows -Wl,-u,_WinMain@16
endif
endif
polyimport_LDFLAGS += -mwindows
POLYOBJECTFILE = polyexport.obj
POLYRESOURCES += polyresource.o
else
POLYOBJECTFILE = polyexport.o
endif

poly_SOURCES = 
poly_LDADD = $(POLYOBJECTFILE) $(POLYRESOURCES) libpolymain/libpolymain.la libpolyml/libpolyml.la 

polyimport_SOURCES = polyimport.c
polyimport_LDADD = $(POLYRESOURCES) libpolyml/libpolyml.la

EXTRA_DIST = \
	imports/polymli386.txt \
	imports/polymlint64.txt \
	imports/polymlint.txt \
	imports/polymlx86_64.txt \
	libpolyml/x86asm.asm \
	exportPoly.sml \
	poly.1 \
	polyimport.1

dist-hook:
	find {basis,mlsource}/ -type d -exec mkdir $(distdir)/{} \;
	find {basis,mlsource}/ -type f -not -name .svn \
	    -exec cp -pf {} $(distdir)/{} \;



# Unix.
polyexport.o: polyimport exportPoly.sml polytemp.txt
	./polyimport $(POLYIMPORT_OPTIONS) polytemp.txt -I $(srcdir) < $(srcdir)/exportPoly.sml

# Windows.  When building on Windows make sure that we provide both stdin and stdout to suppress the GUI.
polyexport.obj: polyimport exportPoly.sml polytemp.txt
	./polyimport $(POLYIMPORT_OPTIONS) polytemp.txt -I $(srcdir) < $(srcdir)/exportPoly.sml | cat

polyresource.o: PolyML.rc poly.ico
	$(WINDRES) -o polyresource.o $(srcdir)/PolyML.rc

polytemp.txt: $(POLYIMPORT)
	cp $(POLYIMPORT) polytemp.txt

# This builds the compiler but does not update the files in the imports directory.
# It then builds a version of poly containing the new compiler.
compiler: all
	./poly $(BOOTSTRAP_OPTIONS) --error-exit < mlsource/BuildExport.sml
	$(MAKE)

reboot: compiler
	cp polytemp.txt $(POLYIMPORT)

# Retain this target for backwards compatibility.
cvs: compiler

clean-local:
	rm -f *.obj polytemp.txt

# Run tests
tests: all
	echo "val () = use \"$(srcdir)/Tests/RunTests\"; val () = OS.Process.exit(if runTests \"$(srcdir)/Tests\" then OS.Process.success else OS.Process.failure):unit;" | ./poly

