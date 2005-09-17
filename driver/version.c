/*
    Title:      Version.

	Copyright (c) 2000
		Cambridge University Technical Services Limited

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.
	
	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

/* This was previously generated from the Makefile. */
#ifdef INTERPRETED
#define ARCH "Portable-"
#elif WINDOWS_PC
#define ARCH "Windows-"
#elif defined(i386)
#define ARCH "I386-"
#elif defined(SPARC)
#define ARCH "Sparc-"
#elif defined(POWER2)
#define ARCH "PPC-"
#endif

char *poly_runtime_system_version =
ARCH "4.1.4 (" __TIME__ " " __DATE__ ")\nCopyright (c) 2002-5 CUTS and contributors.";
