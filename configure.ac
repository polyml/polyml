#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([[Poly/ML]], [[5.0]], [[polyml AT polyml DOT org]])
AM_INIT_AUTOMAKE
AC_PREREQ(2.59)

ac_debug_mode="no"

AC_ARG_ENABLE([debug],
   [  --enable-debug          Compiles without optimisation for debugging ],
   [ac_debug_mode="yes"])

if test "$ac_debug_mode" != "yes"; then
	# Default to maximum optimisation. -O2 is not good enough.
	if test "$CFLAGS" = ""; then
	  CFLAGS="-g -O3"
	fi
	if test "$CXXFLAGS" = ""; then
	   CXXFLAGS="-g -O3"
fi
else
	if test "$CFLAGS" = ""; then
	  CFLAGS="-g"
	fi
	if test "$CXXFLAGS" = ""; then
	   CXXFLAGS="-g"
	fi
fi

#AC_LIBTOOL_WIN32_DLL # libpolyml can be a DLL but libpolymain can't.
AC_PROG_LIBTOOL
AM_PROG_LIBTOOL
AM_MAINTAINER_MODE


# Check we're in the right directory
AC_CONFIG_SRCDIR([polyexports.h])
AC_CONFIG_HEADER([config.h])


# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_CPP

AM_PROG_AS

# Checks for libraries.
AC_CHECK_LIB(dl, dlopen)
AC_CHECK_LIB(m, floor)
AC_CHECK_LIB([stdc++], [main])
# Solaris needs -lsocket and -lnsl
# Use AC_CHECK_FUNC first so that the library is only included if it's really needed
# and not just because it's present.
AC_CHECK_FUNC(gethostbyname, , AC_CHECK_LIB(nsl, gethostbyname))
AC_CHECK_FUNC(getsockopt, , AC_CHECK_LIB(socket, getsockopt))

# Check for X and Motif headers and libraries
AC_PATH_X

if test "$no_x" != "yes"; then
	
	if test "$x_includes" != "" ; then
	if test  "$x_includes" != "NONE" ; then
		CFLAGS="$CFLAGS -I$x_includes"
		CXXFLAGS="$CXXFLAGS -I$x_includes"
		CPPFLAGS="$CPPFLAGS -I$x_includes"
	fi
	fi
	if test "$x_libraries" != "" ; then
	if test "$x_libraries" != "NONE" ; then
		LIBS="-L$x_libraries $LIBS"
	fi
	fi
	AC_CHECK_LIB(X11, XCreateGC)
	AC_CHECK_LIB(Xt, XtMalloc)
	AC_CHECK_LIB(Xext, XextAddDisplay)
	
	if test "$xm_includes" != "" ; then
	if test "$xm_includes" != "NONE" ; then
		CFLAGS="$CFLAGS -I$xm_includes"
		CXXFLAGS="$CXXFLAGS -I$xm_includes"
		CPPFLAGS="$CPPFLAGS -I$xm_includes"
	fi
	fi
	if test "$xm_libraries" != "" ; then
	if test "$xm_libraries" != "NONE" ; then
		LIBS="-L$xm_libraries $LIBS"
	fi
	fi
	AC_CHECK_LIB(Xm, XmGetDestination)

fi

# TODO: May need AC_PATH_XTRA for Solaris

# Check for headers

AC_FUNC_ALLOCA
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([stdio.h time.h fcntl.h float.h limits.h locale.h malloc.h netdb.h netinet/in.h stddef.h])
AC_CHECK_HEADERS([stdlib.h string.h sys/file.h sys/ioctl.h sys/param.h sys/socket.h sys/systeminfo.h])
AC_CHECK_HEADERS([sys/time.h unistd.h values.h dlfcn.h signal.h ucontext.h])
AC_CHECK_HEADERS([assert.h ctype.h direct.h errno.h excpt.h fenv.h fpu_control.h grp.h])
AC_CHECK_HEADERS([ieeefp.h io.h math.h memory.h netinet/tcp.h poll.h pwd.h siginfo.h])
AC_CHECK_HEADERS([stdarg.h sys/errno.h sys/filio.h sys/mman.h sys/resource.h])
AC_CHECK_HEADERS([sys/signal.h sys/sockio.h sys/stat.h sys/termios.h sys/times.h])
AC_CHECK_HEADERS([sys/types.h sys/uio.h sys/un.h sys/utsname.h sys/select.h sys/sysctl.h])
AC_CHECK_HEADERS([sys/elf_SPARC.h sys/elf_386.h])
AC_CHECK_HEADERS([windows.h tchar.h])

# Suppress the check for the X headers if the user said --without-x.
# That will disable the x-windows module.
if test "$no_x" != "yes"; then
    AC_CHECK_HEADERS([X11/Xlib.h Xm/Xm.h])
fi


# Find out which type of object code exporter to use.
# If we have winnt use PECOFF.  This really only applies to cygwin here.
# If we have elf.h use ELF.
# If we have mach-o/reloc.h use Mach-O
# Otherwise use the C source code exporter.
AC_CHECK_TYPES([IMAGE_FILE_HEADER],
    [AC_DEFINE([HAVE_PECOFF], [], [Define to 1 if you have the PE/COFF types.])]
        [polyexport=pecoff],
	[AC_CHECK_HEADER([elf.h],
		[AC_DEFINE([HAVE_ELF_H], [], [Define to 1 if you have the <elf.h> header file.])]
			[polyexport=elf],
              [AC_CHECK_HEADER([mach-o/reloc.h],
                  [AC_DEFINE([HAVE_MACH_O_RELOC_H], [], [Define to 1 if you have the <mach-o/reloc.h> header file.])]
                          [polyexport=macho]
                )]
    )],
    [#include <windows.h>]
)

AM_CONDITIONAL([EXPPECOFF], [test "$polyexport" = pecoff])
AM_CONDITIONAL([EXPELF], [test "$polyexport" = elf])
AM_CONDITIONAL([EXPMACHO], [test "$polyexport" = macho])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_UID_T
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM
# Mac OS X, at any rate, needs signal.h to be included first.
AC_CHECK_TYPES([ucontext_t, x86_thread_state32_t, x86_thread_state64_t, ppc_thread_state_t], , ,[#include "signal.h"
#include "ucontext.h"])
AC_CHECK_TYPES([struct sigcontext, stack_t, sighandler_t, sig_t], , ,[#include "signal.h"])
AC_CHECK_TYPES([socklen_t],,,[#include "sys/types.h"
#include "sys/socket.h"])

AC_CHECK_SIZEOF(void*)

# Checks for library functions.
AC_FUNC_CHOWN
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_GETGROUPS
AC_FUNC_GETPGRP
AC_PROG_GCC_TRADITIONAL
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_REALLOC
AC_FUNC_SELECT_ARGTYPES
AC_FUNC_SETVBUF_REVERSED
AC_TYPE_SIGNAL
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([bzero dup2 floor ftruncate getcwd gethostbyaddr gethostname gettimeofday memmove])
AC_CHECK_FUNCS([memset mkdir mkfifo munmap pathconf pow realpath rmdir select setlocale socket sqrt strdup strerror])
AC_CHECK_FUNCS([uname dlopen getrusage strtod dtoa sigaction mkstemp getpagesize sigaltstack mmap mprotect])
AC_CHECK_FUNCS([getnetbyname getnetbyaddr sysctl])

# Where are the registers?
AC_CHECK_MEMBERS([mcontext_t.gregs, mcontext_t.regs, mcontext_t.mc_esp],,,[#include "ucontext.h"])

# Additional checks. AC_CANONICAL_HOST has already been called.
#AC_CANONICAL_HOST

# I
AC_ARG_WITH(portable,
    [  --with-portable         Build the portable interpreter version of Poly/ML instead of native], [
    ac_cv_with_portable=yes], [
    ac_cv_with_portable=no])

case $ac_cv_with_portable in
   yes )
       AC_SUBST([ARCHFLAG], [-DINTERPRETED])
       polyarch=interpret
       ;;
   * )
    case "${host_cpu}" in
          i[[3456]]86*)
                AC_SUBST([ARCHFLAG], [-Di386])
                polyarch=i386
                ;;
          sparc*)
                AC_SUBST([ARCHFLAG], [-DSPARC])
                polyarch=sparc
                ;;
          power* | ppc*)
                AC_SUBST([ARCHFLAG], [-DPOWER2])
                polyarch=ppc
                ;;
          x86_64)
                AC_SUBST([ARCHFLAG], [-DX86_64])
                polyarch=x86_64
                ;;
	    *) AC_MSG_ERROR([Poly/ML is not supported for this architecture]) ;;
    esac;;
esac

AM_CONDITIONAL([ARCHI386], [test "$polyarch" = i386])
AM_CONDITIONAL([ARCHPPC], [test "$polyarch" = ppc])
AM_CONDITIONAL([ARCHSPARC], [test "$polyarch" = sparc])
AM_CONDITIONAL([ARCHINTERPRET], [test "$polyarch" = interpret -a X"$ac_cv_sizeof_voidp" = X4])
AM_CONDITIONAL([ARCHINTERPRET64], [test "$polyarch" = interpret -a X"$ac_cv_sizeof_voidp" = X8])
AM_CONDITIONAL([ARCHX86_64], [test "$polyarch" = x86_64])


# Set the OS flag.  This should not really be required but is still used in
# a few places for OS-specific quirks that can't be handled by more specific tests.
case "${host_os}" in
    darwin*)
        AC_SUBST([OSFLAG], [-DMACOSX])
        ;;
    sunos* | solaris*)
        AC_SUBST([OSFLAG], [-DSOLARIS])
        ;;
    *)
        AC_SUBST([OSFLAG], [-DLINUX])
        ;;
esac

AH_TOP([
#ifndef CONF_H_INCLUDED
#define CONF_H_INCLUDED])
AH_BOTTOM([
#endif])

AC_CONFIG_FILES([Makefile libpolyml/Makefile libpolymain/Makefile])
AC_OUTPUT
