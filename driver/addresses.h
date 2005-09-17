/*
    Title: 	Addresses header
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

#ifndef _ADDRESSES_H
#define _ADDRESSES_H

#include "sys.h" /* for PAGEWORDS, PAGEBYTES */
#include "mm.h"  /* for HEADER    */

#define NIL            Words(TAGGED(0))

/****************************************************************************
 *
 * VIRTUAL MEMORY ADDRESSES AND ALLOCATION FUNCTIONS 
 *
 * We need to choose values for the fixed address spaces
 * for the mutable and immutable data. These values have
 * to be sufficiently high to be above the code and sbrk data,
 * but lower than the stack and shared libraries. 
 * nil is chosen to be outside of the mutable, immutable and all other
 * areas so that it gives a segmentation fault/bus error when accessed.
 *
 ***************************************************************************/

/* These are now used to initialise a variable and can be overridden by an
   option when running the disc garbage collector. */
/* These are now the default sizes for the database and can be overridden with
   an option to discgarb.  */
#if (0)
/* Allow large databases. */
#define H_MSPACE_PAGES  ROUNDUP_UNITS(MBytes(11), PAGEBYTES)
#define H_ISPACE_PAGES  ROUNDUP_UNITS(MBytes(88), PAGEBYTES)
#else
/* Leave others as they are for the moment. */
#define H_MSPACE_PAGES  ROUNDUP_UNITS(MBytes(7), PAGEBYTES)
#define H_ISPACE_PAGES  ROUNDUP_UNITS(MBytes(56), PAGEBYTES)
/* That's 63MB (56 + 7 = 63), which leaves an ample 1MB for the headers. */
#endif 


/* UNIX - calculate these values properly. 
   The old values ONLY worked if the logical page size was at least 8K 
   - otherwise the immutable page table would have been too small. There
   was no documentation of this dependance anywhere. Horrible!
   
   We've only got 256MB for the Sun database area (at present), so
   limit each database (including headers) to 64MB. This should give
   us 3 levels of hierarchy, plus an extra address space for discgarb.
 */

/* Header size, in PAGES */
#define H_SIZE          ROUNDUP_UNITS(sizeof(HEADER), PAGEBYTES)

/* 
   Each page table contains one WORD for each PAGE 
   in the corresponding database area. 
*/


#if defined(SOLARIS2) /* Sun UNIX versions */

/* 
  Experiments show what addresses we can use:
      SunOS 4.1 / SPARC 2: 
        most significant hex digit must be 0, 1, e or f
 
      Solaris 2.4 / SPARC 20: 
        most significant hex digit must be 0 - e inclusive
   
  We've given up the idea of portability, so we just have the
  restriction:
       
        most significant hex digit must be 0 - e inclusive
        
   I *hate* this sort of hacking, but what else can one do?
   
   However, we can't use all of this address range on HAL/OS 5.4
   so let's move things around again.
   SPF 17/2/1998
*/
/* #if defined(BIGHEAP) */
/* Always use big heap now. DCJM 7/10/2000. */
#if (1)
/* 0x00000000 - 0x20000000 reserved for RTS executable and C heap */
/* A big local heap (2GB + 512MB) */
#define LOCAL_IBOTTOM   Words(0x20000000)
#define LOCAL_ITOP      Words(0xA0000000)
#define LOCAL_MBOTTOM   Words(0xA0000000)
#define LOCAL_MTOP      Words(0xC0000000)

#else
/* 0x00000000 - 0x40000000 reserved for RTS executable and C heap */
/* 1GB immutable local heap */
#define LOCAL_IBOTTOM   Words(0x40000000)
#define LOCAL_ITOP      Words(0x80000000)

/* 0x80000000 - 0xB0000000 reserved for mapping shared objects. */
/* 256MB mutable local heap */
#define LOCAL_MBOTTOM   Words(0xB0000000)
#define LOCAL_MTOP      Words(0xC0000000)
#endif


/* IO area - compiled into ML code, but can be relocated provided
   the database is then discgarb'ed to updated the addresses.
*/
#define IO_BOTTOM      Words(0xC0000000)
#define IO_TOP         Words(0xC0002000)
#define IO_SPACING     8

/* 
   Database area (256MB) - to move, just discgarb the database.
   The HAL machine has 16K pages, so move H_BOTTOM up a little to
   avoid clash with the I/O area. (I've actually allowed 256K here,
   just in case I decide to use larger logical pages. The important
   thing is that there's still enough room for 4 databases.)
*/
#define H_BOTTOM        Words(0xC0040000)
#define H_TOP           Words(0xD0000000)


/* 0xD0000000 - 0xE0000000 used by HAL/OS (for what?) */
/* 0xE0000000 - 0xF0000000 reserved for C stack (256MB - generous!) */
/* 0xF0000000 - 0x00000000 reserved for operating system (I think) */

#elif defined(LINUX) && defined(i386)
/* Original comment --- */
/*
Linux appears to divide the 4GB address space into 4 1GB segments:

   0 - Unused?
   1 - Dynamic libraries
   2 - Program code and data
   3 - O/S kernel

So we'll try to put Poly/ML into segment 0
*/

/* This doesn't seem to be true, at least not any longer. 
   The code and data are around 0x08000000 in older kernels
   and 0x10000000 in later ones.  It seems that mmap without a
   fixed address (and so malloc) maps at upwards of 0x40000000.
   These should be reasonable. DCJM 29/11/01. */
#define LOCAL_IBOTTOM   Words(0x50000000)
#define LOCAL_ITOP      Words(0x98000000)
#define LOCAL_MBOTTOM   Words(0x98000000)
#define LOCAL_MTOP      Words(0xa8000000)
/* 1.1 Gbytes for immutable, 256Mbytes for mutable. */
/* This leaves 0xb8... - 0xbf... for the C stack. (i.e. 128 Mbytes) */

/* Database - as FreeBSD, Mac OS X etc. */

#define H_BOTTOM Words(0x20000000)
#define H_TOP Words(0x3F000000)
#define IO_BOTTOM   H_TOP
#define IO_TOP     Words(0x3F002000) 
#define IO_SPACING 8 

#elif defined(LINUX) && defined(POWER2)
/* LinuxPPC. Libraries and malloc start at 0x30000000.
   The stack is at 0x7fXXXXXX
   DCJM 29/11/01. */
/* 768 Mbytes for Immutable and 128 Mbytes for mutable. */
#define LOCAL_IBOTTOM   Words(0x40000000)
#define LOCAL_ITOP      Words(0x70000000)
#define LOCAL_MBOTTOM   Words(0x70000000)
#define LOCAL_MTOP      Words(0x78000000)
/* This leaves 0x7... - 0x78... for the C stack. (i.e. 128 Mbytes) */

/* Database - The libaries and malloc at 0x30000000 limit the 
   database size.  Maybe investigate alternatives.
   DCJM 29/11/01. */
#define H_BOTTOM Words(0x20000000)
#define H_TOP Words(0x30000000)
#define IO_BOTTOM   Words(0x3F000000)
#define IO_TOP     Words(0x3F002000) 
#define IO_SPACING 8 

#else
/* FreeBSD, Mac OS X and Windows. */

#define IO_BOTTOM  Words(0x3F000000) 
#define IO_TOP     Words(0x3F002000) 
#define IO_SPACING 8 

/* 27 * 16MB immutable local heap */
#ifdef WINDOWS_PC
#define LOCAL_IBOTTOM   Words(0x04000000) 
#else
#define LOCAL_IBOTTOM   Words(0x01000000) 
#endif
#define LOCAL_ITOP      Words(0x1C000000)
/* 4 * 16MB mutable local heap */
#define LOCAL_MBOTTOM   Words(0x1C000000) 
#define LOCAL_MTOP      Words(0x20000000)

/* Database areas start above the local heap */
/* and extend as far as the IO area.         */
/* Which gives a total of 29 * 16MB          */
/* DCJM June 2000.  It would seem much more sensible to allow
   for fewer larger databases since very few people use child
   databases at all and those that do only use a small
   heirarchy.  I would guess that 4 is an absolute limit.  */
#define H_BOTTOM LOCAL_MTOP
#define H_TOP IO_BOTTOM
#endif


/* Maximum Immutable/Mutable heap sizes in words. They're written like
   this to avoid annoying "arithmetic overflow" warnings from GCC. */
#define I_ABSOLUTE_MAXIMUM ROUNDDOWN_UNITS((unsigned)LOCAL_ITOP - (unsigned)LOCAL_IBOTTOM, sizeof(word))
#define M_ABSOLUTE_MAXIMUM ROUNDDOWN_UNITS((unsigned)LOCAL_MTOP - (unsigned)LOCAL_MBOTTOM, sizeof(word))

#endif /* we haven't seen this header file before */
