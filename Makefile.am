AUTOMAKE_OPTIONS=foreign

AM_CPPFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall
AM_CFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall -fno-strict-aliasing

POLYIMPORT_OPTIONS = -H 10
BOOTSTRAP_OPTIONS = -H 10

SUBDIRS = libpolyml libpolymain

bin_PROGRAMS = polyimport poly

man_MANS = poly.1 polyimport.1

# Select the architecture-specific modules
if ARCHI386
POLYIMPORT = $(srcdir)/imports/polymli386.txt
else
if ARCHPPC
POLYIMPORT = $(srcdir)/imports/polymlppc.txt
else
if ARCHSPARC
POLYIMPORT = $(srcdir)/imports/polymlsparc.txt
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
endif
endif

poly_LDFLAGS=
polyimport_LDFLAGS=
POLYRESOURCES=

if NATIVE_WINDOWS
poly_LDFLAGS += -mwindows -Wl,-u,_WinMain@16
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

#
polyexport.o: polyimport exportPoly.sml polytemp.txt
	./polyimport $(POLYIMPORT_OPTIONS) polytemp.txt -I $(srcdir) < $(srcdir)/exportPoly.sml

polyexport.obj: polyimport exportPoly.sml polytemp.txt
	./polyimport $(POLYIMPORT_OPTIONS) polytemp.txt -I $(srcdir) < $(srcdir)/exportPoly.sml

polyresource.o: PolyML.rc poly.ico
	windres -o polyresource.o PolyML.rc

polytemp.txt: $(POLYIMPORT)
	cp $(POLYIMPORT) polytemp.txt

# This builds the compiler but does not update the files in the imports directory.
# It then builds a version of poly containing the new compiler.
compiler: all
	./poly $(BOOTSTRAP_OPTIONS) < mlsource/BuildExport.sml
	$(MAKE)

reboot: cvs
	cp polytemp.txt $(POLYIMPORT)

# Retain this target for backwards compatibility.
cvs: compiler

clean-local:
	rm -f *.obj polytemp.txt

