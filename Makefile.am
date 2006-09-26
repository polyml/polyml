AUTOMAKE_OPTIONS=foreign

AM_CPPFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall
AM_CFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall -fno-strict-aliasing

SUBDIRS = libpolyml libpolymain

bin_PROGRAMS = polyimport poly

man_MANS = poly.1 polyimport.1

# Select the architecture-specific modules
if ARCHI386
POLYIMPORT = imports/polymli386.txt
else
if ARCHPPC
POLYIMPORT = imports/polymlppc.txt
else
if ARCHSPARC
POLYIMPORT = imports/polymlsparc.txt
else
if ARCHINTERPRET
POLYIMPORT = imports/polymlint.txt
else
if ARCHINTERPRET64
POLYIMPORT = imports/polymlint64.txt
else
if ARCHX86_64
POLYIMPORT = imports/polymlx86_64.txt
else
endif
endif
endif
endif
endif
endif

poly_SOURCES = 
poly_LDADD = polyexport.o libpolymain/libpolymain.la libpolyml/libpolyml.la 

polyimport_SOURCES = polyimport.c
polyimport_LDADD = libpolyml/libpolyml.la

#
polyexport.o: polyimport exportPoly.sml $(POLYIMPORT)
	./polyimport $(POLYIMPORT) < exportPoly.sml > /dev/null

# Poly/ML command to compile the compile and build the export version.
# Run "make" to import it and build the poly executable with it.
bootstrap: poly
	./poly < mlsource/BuildExport.sml


