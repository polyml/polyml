/*
    Title: 	Globals for the system.
    Author: 	Dave Matthews, Cambridge University Computer Laboratory

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

#ifndef _GLOBALS_H
#define _GLOBALS_H

typedef int word;
typedef unsigned char byte;
typedef void (*VoidFunc)() ;
typedef int  (*IntFunc)() ;

typedef struct { word length; char chars[1]; } STRING, *pstring;

typedef struct HeaderStruct *Header ;



/* A handle is the address of an element of save_vec  */
/* This element points at an element of the Poly heap */
/* The element is currently represented as a (word *) */
/* These types will become more abstract eventually.  */
/* typedef word ***Handle; */

/* make silly declaration to pick up type errors */
typedef word ******Handle;
#define DEREFHANDLE(H) (*(word ***)(H))
#define DEREFWORDHANDLE(H) (*(word **)(H))
#define DEREFBYTEHANDLE(H) (*(byte **)(H))
#define DEREFSTRINGHANDLE(H) (*(pstring *)(H))
#define DEREFLISTHANDLE(H)  (*(ML_Cons_Cell **)(H))
#define DEREFEXNHANDLE(H)   (*(poly_exn **)(H))

#define DEREFPROCHANDLE(x) (*((process_base **)(x)))
#define DEREFCHANHANDLE(x) (*((channel **)(x)))
#define DEREFSYNCHROHANDLE(x) (*((synchroniser **)(x)))


/* 
 * Stream tokens are pointers to UNTAGGED C integers 
 * (stored in byte objects) 
 */
#define DEREFSTREAMHANDLE(x) (*(int **)(x))

#define ALIGNED(p) (((unsigned)(p) & 3) == 0)

/**********************************************************************
 *
 * Handles for different 'objects' are of type Handle
 *
 **********************************************************************/
typedef Handle ProcessHandle; /* Handle to (process_base *) */
typedef Handle SynchroHandle; /* Handle to (synchroniser *) */
typedef Handle ChanHandle;    /* Handle to (word *) */
typedef Handle EventHandle;

#include "objects.h"

/***********************************************************************
 *
 * Used by Mmapping.
 *
 ***********************************************************************/

#define M_WRITABLE  0x1
#define M_PROFILED  0x2
#define M_DISCGARB1 0x40	/* Set on the source database when running discgarb. */
#define M_DISCGARB2 0x80	/* Set on the destination database when running discgarb. */

#define WRITABLE(f)  ((f) & M_WRITABLE)
#define PROFILED(f)  ((f) & M_PROFILED)
#define DISCGARB1(f) ((f) & M_DISCGARB1)
#define DISCGARB2(f) ((f) & M_DISCGARB2)

typedef enum { NoMoreChildren,CannotCreate,CreatedOk } CStatus ;

/**********************************************************************
 *
 * The size of a single page named <space granularity> OS specific.
 *
 * PHYS_PAGE_BYTES is the page size used by the operating system
 * (or possibly a small multiple of this figure). All mmapping
 * activities must be a multiple of this number.
 *
 * PAGEBYTES is the page size used for entries in the Poly/ML database.
 * This must be a multiple of PHYS_PAGE_BYTES. Currently there's a
 * problem that PAGEBYTES for the SPARCs is 8K, but we need to set
 * PHYS_PAGE_BYTES to 16K to accomodate the HAL machine. When mmap.c
 * is ready for this change, I'll fix this problem.
 *
 * CHUNKBYTES is the allocation unit for the local heap. This must also
 * be a multiple of PHYS_PAGE_BYTES. I'm don't think it needs to
 * be a multiple of PAGEBYTES, but I'm not quite sure yet. If this is
 * too small, we waste time growing and shrinking the heap too often
 * (especially on the HP). If it's too large, we waste space in the heap.
 *********************************************************************/

#if defined(WINDOWS_PC)
/*  PC version - 64K bytes */
#define PHYS_PAGE_BYTES 0x10000
#define PAGEBYTES PHYS_PAGE_BYTES
#define CHUNKBYTES PAGEBYTES

#else  /* unix */

#if defined(SPARC)
/* Note - PHYS_PAGE_BYTES changed on the SPARC (for the HAL version) */
#define PHYS_PAGE_BYTES (16*1024)
#define PAGEBYTES PHYS_PAGE_BYTES
#define CHUNKBYTES (PAGEBYTES * 16)
#else
#define PHYS_PAGE_BYTES (8*1024)
#define PAGEBYTES PHYS_PAGE_BYTES
#define CHUNKBYTES (PAGEBYTES * 32)
#endif

#endif /* unix */

/* The size of a single page in words */
#define PAGEWORDS  (PAGEBYTES/sizeof(word))
#define CHUNKWORDS (CHUNKBYTES/sizeof(word))

/* How many units of size n do we need to hold an object of size m? */
#define ROUNDUP_UNITS(m,n)   (((m) + (n) - 1) / (n))
#define ROUNDDOWN_UNITS(m,n) ((m) / (n))

/* Rounds m UP or DOWN to nearest multiple of n */
#define ROUNDUP(m,n)   (ROUNDUP_UNITS(m,n) * (n))
#define ROUNDDOWN(m,n) (ROUNDDOWN_UNITS(m,n) * (n))

#define MBytes(n) ((n)*1024*1024)


/**********************************************************************
 *
 * Low level definitions.
 *
 **********************************************************************/

#define Word(w) ((word) (w))
#define Long(w) ((long) (w))

#define Bytes(p) ((char *) (p))
#define Words(p) ((word *) (p))

/* bitmaps using arrays of word */

#define BITS_PER_WORD 32
#define BITS_PER_BYTE 8

#define BitN(n) (1 << ((n) & 31))

#define SetBit(bitmap,n)   ( (bitmap)[(n)/BITS_PER_WORD] |=  BitN(n) )
#define ClearBit(bitmap,n) ( (bitmap)[(n)/BITS_PER_WORD] &= ~BitN(n) )
#define TestBit(bitmap,n)  ( (bitmap)[(n)/BITS_PER_WORD] &   BitN(n) )

#define BITMAP_WORDS(n) (((n)+BITS_PER_WORD-1)/BITS_PER_WORD)
#define BITMAP_BYTES(n) (BITMAP_WORDS(n)*sizeof(word))

/* Macro to round a number of bytes up to a number of words. */
#define WORDS(s) ((s+sizeof(word)-1)/sizeof(word))

/**********************************************************************
 *
 * Type declarations of function pointers used in runtime.
 *
 **********************************************************************/

typedef void (*GCOpFunc)(word **pt,int weak) ;
typedef void (*GCMapFunc)(GCOpFunc op);
typedef void (*InterruptFunc)(int signum);
typedef word *(*CopyFunc)(word *old_addr);
typedef word *(*AllocFunc)(Header H,word length);
typedef void (*OpRootsFunc)(CopyFunc);

/**********************************************************************
 *
 * Representation of list.
 *
 **********************************************************************/

typedef struct ML_Cons_Cell_Tag ML_Cons_Cell;

struct ML_Cons_Cell_Tag
{
word         *h;
ML_Cons_Cell *t;
};

/**********************************************************************
 *
 * Representation of option type.
 *
 **********************************************************************/
#define NONE_VALUE		(TAGGED(0))
/* SOME x is represented by a single word cell containing x. */

#if (defined(LINUX) || defined(FREEBSD) || defined (MACOSX))
/* In these versions the %p format specification generates 0x. */
#define ZERO_X	""
#else
/* Windows doesn't.  I haven't tried anything else. */
#define ZERO_X	"0x"
#endif

#endif
