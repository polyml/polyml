To build the appropriate code generator first create CodeCons.  This
must be either a copy of the appropriate CodeCons.xxx directory or,
on Unix, a symbolic link to it.

CodeCons.i386		I386 Architecture and derivatives
CodeCons.sparc		SPARC Architecture
CodeCons.power		Power PC
CodeCons.hppa		HP ?

The HPPA code-generator is out of date and is included in case
someone is interested in doing some work on it.

There is one exception to this: when rebuilding the interpreted
version instead of CodeCons it is necessary to create GCode as
a copy of or link to GCode.int.
