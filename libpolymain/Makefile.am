AUTOMAKE_OPTIONS=foreign

AM_CPPFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall
AM_CFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall -fno-strict-aliasing

lib_LTLIBRARIES = libpolymain.la
libpolymain_la_LDFLAGS = -static -version-info 4:9

libpolymain_la_SOURCES = polystub.c

