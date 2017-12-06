#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([Poly/ML],[5.7.1],[polyml AT polyml DOT org],[polyml])
AM_INIT_AUTOMAKE
AC_PREREQ(2.69)
# libtoolize recommends this line.
AC_CONFIG_MACRO_DIR([m4])

ac_debug_mode="no"

AC_ARG_ENABLE([debug],
   [  --enable-debug          Compiles without optimisation for debugging ],
   [ac_debug_mode="yes"])

if test "$ac_debug_mode" != "yes"; then
    # Default to maximum optimisation. -O2 is not good enough.
        # Set CCASFLAGS to empty so that it doesn't get set to CFLAGS.
        #   The -g option on assembler causes problems on Sparc/Solaris 10.
        # test X || Y  is equivalent to  if !X then Y
        test "${CFLAGS+set}" = set || CFLAGS="-O3"
        test "${CXXFLAGS+set}" = set || CXXFLAGS="-O3"
        test "${CCASFLAGS+set}" = set || CCASFLAGS=""
else
        test "${CFLAGS+set}" = set || CFLAGS="-g"
        test "${CXXFLAGS+set}" = set || CXXFLAGS="-g"
        test "${CCASFLAGS+set}" = set || CCASFLAGS=""
fi

AC_CANONICAL_HOST
# Check also for mingw in which case we want to build native Windows.
poly_native_windows=no
# If we are building on cygwin or mingw we need to use the pre-built
# compiler that uses Windows calling conventions rather than
# SysV on 64-bit.
poly_use_windowscc=no
poly_need_macosopt=no
case "${host_os}" in
    darwin*)
        poly_need_macosopt=yes
        ;;
    sunos* | solaris*)
        ;;
    *mingw*)
        AC_CHECK_TOOL(WINDRES, windres)
        poly_native_windows=yes
        poly_use_windowscc=yes
        ;;
    *cygwin*)
        poly_no_undefined=yes
        poly_use_windowscc=yes
        if test "$ac_cv_c_compiler_gnu" = "yes"; then
            if $CC -dumpmachine | grep -q mingw; then
                poly_native_windows=yes
            fi
        fi
        ;;
esac

# Enable/Disable the GUI in Windows.  Should this only be present if
# building on mingw?
AC_ARG_ENABLE([windows-gui],
        [AS_HELP_STRING([--enable-windows-gui],
            [create a GUI in Windows. If this is disabled use a Windows console. @<:@default=yes@:>@])],
        [case "${enableval}" in
           yes) enablegui=true ;;
           no)  enablegui=false ;;
           *) AC_MSG_ERROR([bad value ${enableval} for --enable-windows-gui]) ;;
        esac],
        [enablegui=true])
AM_CONDITIONAL([WINDOWSGUI], [test x$enablegui = xtrue])

LT_INIT([])
AM_MAINTAINER_MODE


# Check we're in the right directory
AC_CONFIG_SRCDIR([polyexports.h])
AC_CONFIG_HEADER([config.h])


# Checks for programs.
AC_PROG_CXX
# The following check was supposed to check that there was actually a
# C++ compiler but doesn't work properly if CXX is set by the user.
#AC_CHECK_PROG(check_cpp, $CXX, "yes", "no")
#if test "$check_cpp" != "yes"; then
#    AC_MSG_ERROR([No C++ compiler found.  Unable to build Poly/ML.])
#fi

AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_CPP

AM_PROG_AS

# Activate large file mode if needed
AC_SYS_LARGEFILE

# For some reason libtool doesn't pass up all the libraries
# that are used in libpolyml. -lstdc++ and -lm at least are missing.

AC_CHECK_LIB(gcc, main)
AC_CHECK_LIB(gcc_s, main)
AC_CHECK_LIB(stdc++, main)
# These can sometimes be in the standard libraries
AC_SEARCH_LIBS([dlopen], [dl dld])
AC_SEARCH_LIBS([floor], [m])

# The next two are only used with mingw
AC_CHECK_LIB(ws2_32, main)
AC_CHECK_LIB(gdi32, main)

AC_CHECK_SIZEOF(void*)

## External names on Win64.  They have no leading underscores as per
## the X64 ABI published by MS.  Earlier versions of GCC (anything
## prior to 4.5.0) were faulty.
LT_SYS_SYMBOL_USCORE
if test x$sys_symbol_underscore = xyes; then
    AC_DEFINE(SYMBOLS_REQUIRE_UNDERSCORE, [1], [Defined if external symbols are prefixed by underscores])
fi

# TODO: May need AC_PATH_XTRA for Solaris

# Check for headers.  These are needed for polyexports.h.
AC_CHECK_HEADERS([time.h stdint.h inttypes.h stddef.h windows.h])

PKG_PROG_PKG_CONFIG

AC_CONFIG_SUBDIRS([libpolyml])

# This option enables the native code generator.  More precisely it allows
# the byte code interpreter to be built on X86.

AC_ARG_ENABLE([native-codegeneration],
        [AS_HELP_STRING([--disable-native-codegeneration],
            [disable the native code generator and use the slow byte code interpreter instead.])],
        [case "${enableval}" in
           no)  with_portable=yes ;;
           yes) with_portable=no ;;
           *) AC_MSG_ERROR([bad value ${enableval} for --enable-native-codegeneration]) ;;
        esac],
        [with_portable=check])

# Check which CPU we're building for.  Can we use a native pre-built compiler
# or do we need to fall back to the interpreter?

case "${host_cpu}" in
      i[[3456]]86*)
            AC_DEFINE([HOSTARCHITECTURE_X86], [1], [Define if the host is an X86 (32-bit)])
            polyarch=i386
            ;;
      x86_64* | amd64*)
            if test X"$ac_cv_sizeof_voidp" = X8; then
                AC_DEFINE([HOSTARCHITECTURE_X86_64], [1], [Define if the host is an X86 (64-bit)])
                polyarch=x86_64
            else
                AC_DEFINE([HOSTARCHITECTURE_X32], [1], [Define if the host is an X86 (32-bit ABI, 64-bit processor)])
                polyarch=interpret
            fi
            ;;
      sparc64*)
            AC_DEFINE([HOSTARCHITECTURE_SPARC64], [1], [Define if the host is a Sparc (64-bit)])
            polyarch=interpret
            ;;
      sparc*)
            AC_DEFINE([HOSTARCHITECTURE_SPARC], [1], [Define if the host is a Sparc (32-bit)])
            polyarch=interpret
            ;;
      powerpc64* | ppc64*)
            AC_DEFINE([HOSTARCHITECTURE_PPC64], [1], [Define if the host is a PowerPC (64-bit)])
            polyarch=interpret
            ;;
      power* | ppc*)
            AC_DEFINE([HOSTARCHITECTURE_PPC], [1], [Define if the host is a PowerPC (32-bit)])
            polyarch=interpret
            ;;
      arm*)
            AC_DEFINE([HOSTARCHITECTURE_ARM], [1], [Define if the host is an ARM (32-bit)])
            polyarch=interpret
            ;;
      aarch64*)
            AC_DEFINE([HOSTARCHITECTURE_AARCH64], [1], [Define if the host is an ARM (64-bit)])
            polyarch=interpret
            ;;
      hppa*)
            AC_DEFINE([HOSTARCHITECTURE_HPPA], [1], [Define if the host is an HP PA-RISC (32-bit)])
            polyarch=interpret
            ;;
      ia64*)
            AC_DEFINE([HOSTARCHITECTURE_IA64], [1], [Define if the host is an Itanium])
            polyarch=interpret
            ;;
      m68k*)
            AC_DEFINE([HOSTARCHITECTURE_M68K], [1], [Define if the host is a Motorola 68000])
            polyarch=interpret
            ;;
      mips64*)
            AC_DEFINE([HOSTARCHITECTURE_MIPS64], [1], [Define if the host is a MIPS (64-bit)])
            polyarch=interpret
            ;;
      mips*)
            AC_DEFINE([HOSTARCHITECTURE_MIPS], [1], [Define if the host is a MIPS (32-bit)])
            polyarch=interpret
            ;;
      s390x*)
            AC_DEFINE([HOSTARCHITECTURE_S390X], [1], [Define if the host is an S/390 (64-bit)])
            polyarch=interpret
            ;;
      s390*)
            AC_DEFINE([HOSTARCHITECTURE_S390], [1], [Define if the host is an S/390 (32-bit)])
            polyarch=interpret
            ;;
      sh*)
            AC_DEFINE([HOSTARCHITECTURE_SH], [1], [Define if the host is a SuperH (32-bit)])
            polyarch=interpret
            ;;
      alpha*)
            AC_DEFINE([HOSTARCHITECTURE_ALPHA], [1], [Define if the host is an Alpha (64-bit)])
            polyarch=interpret
            # GCC defaults to non-conforming floating-point, and does not respect the rounding mode
            # in the floating-point control register, so we force it to conform to IEEE and use the
            # dynamic suffix on the floating-point instructions it produces.
            CFLAGS="$CFLAGS -mieee -mfp-rounding-mode=d"
            CXXFLAGS="$CXXFLAGS -mieee -mfp-rounding-mode=d"
            ;;
      *) AC_MSG_ERROR([Poly/ML is not supported for this architecture]) ;;
esac

# If we explicitly asked to use the interpreter set the architecture to interpreted.
if test "x$with_portable" = "xyes" ; then
    if test "x$polyarch" != "xinterpret" ; then
        AC_MSG_WARN(
            [*******You have disabled native code generation.  Are you really sure you want to do that?*******])
    fi
    polyarch=interpret
fi

# If we asked not to use the interpreter check we have native code support.
if test "x$with_portable" = "xno" ; then
    if test "x$polyarch" = "xinterpret" ; then
        AC_MSG_ERROR(
            [--enable-native-codegeneration was given but native code is not supported on this platform])
    fi
fi

# Put this test at the end where it's less likely to be missed.
# If we're compiling on Cygwin (and mingw?) and /usr/bin/file is not present
# the link step will produce some strange warning messages of the form:
# "Warning: linker path does not have real file for library -lXXX".  I think
# that's really a bug in autoconf but to explain what's happening to the user
# add a test here.
if test "$lt_cv_file_magic_cmd" = "func_win32_libid";
then
    if test \! -x /usr/bin/file;
    then
        echo ""
        echo "*** Warning: You are building Poly/ML on Cygwin/Mingw but '/usr/bin/file' cannot be found."
        echo "*** You can still go ahead and build Poly/ML but libpolyml will not be built as a"
        echo "*** shared library and you may get strange warning messages from the linker step."
        echo "*** Install the 'file' package to correct this problem."
        echo ""
    fi
fi

AM_CONDITIONAL([ARCHI386], [test "$polyarch" = i386])
AM_CONDITIONAL([ARCHX86_64], [test "$polyarch" = x86_64])
AM_CONDITIONAL([ARCHINTERPRET], [test "$polyarch" = interpret -a X"$ac_cv_sizeof_voidp" = X4])
AM_CONDITIONAL([ARCHINTERPRET64], [test "$polyarch" = interpret -a X"$ac_cv_sizeof_voidp" = X8])
AM_CONDITIONAL([WINDOWS64CALLS], [test "$poly_use_windowscc" = yes -a "$polyarch" = x86_64])

AM_CONDITIONAL([NATIVE_WINDOWS], [test "$poly_native_windows" = yes])

AM_CONDITIONAL([MACOSLDOPTS], [test "$poly_need_macosopt" = yes ])

# Control whether to build the basis library with arbitrary precision as the default int
AC_ARG_ENABLE([intinf-as-int],
    [AS_HELP_STRING([--enable-intinf-as-int], [set arbitrary precision as the default int type])],
    [case "${enableval}" in
       no) intisintinf=no ;;
       yes) intisintinf=yes ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-intinf-as-int]) ;;
        esac],
        [intisintinf=no])

AM_CONDITIONAL([INTINFISINT], [test "$intisintinf" = "yes"])

# These are needed for building in a separate build directory, as they are
# referenced from exportPoly.sml.
AC_CONFIG_COMMANDS([basis], [test -e basis || ln -sf ${ac_top_srcdir}/basis .])
AC_CONFIG_COMMANDS([mlsource], [test -e mlsource || ln -sf ${ac_top_srcdir}/mlsource .])

AC_CONFIG_FILES([Makefile libpolymain/Makefile modules/Makefile modules/IntInfAsInt/Makefile])

AC_OUTPUT
