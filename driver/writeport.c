/*
    Title:     Convert a database to portable format.
    Author:    David Matthews.

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
/* This is derived from discgarb.c */
/*
This program is provided largely for reference.  The portable interpreted
version of Poly/ML contained in the database was compiled specially
after modifying all occurrences of calls to functions to obtain the
word length and endianism in the compiler and, particularly, the basis
library.  There are several case of structures containing code such as
   val wordSize = RunCall.run_call0 POLY_SYS_bytes_per_word
followed by functions which use this value.  This is not portable
because the declaration will be executed once when the structure is
compiled and will freeze the value used on the source machine.
DCJM 28/9/00.
*/
#include <stdio.h>
#if !defined(WINDOWS_PC) 
#include <unistd.h>
#endif
#include <string.h>
#include <errno.h>
#include <assert.h>

#include "globals.h"

#include "mmap.h"
#include "diagnostics.h"
#include "sys.h"

#ifdef _DEBUG
/* MS C defines _DEBUG for debug builds. */
#define DEBUG
#endif

#ifdef DEBUG
#define ASSERT(x) assert(x)
#else
#define ASSERT(x)
#endif


/* imports */

static Header A = 0;

#ifdef WINDOWS_PC
void SetResetCC(const int enable) /* OK */
{
}
#endif

typedef struct {
	word *addr;
} OBJECTMAP, *POBJECTMAP;

static POBJECTMAP pMap;
static int mapSize, startImmut;

static void printValue(word q); /* Forward declaration. */

/* Get the index corresponding to an address. */
static int getIndex(word *p)
{
	int i;
	double d;
	/* Work out a first guess assuming that all the objects are
	   the same size. */
	if (p >= A->m_space.bottom && p < A->m_space.pointer)
	{
		d = ((double)(p - A->m_space.bottom)) * (double)startImmut /
			((double)(A->m_space.pointer-A->m_space.bottom));
		i = (int)d;
	}
	else if (p >= A->i_space.bottom && p < A->i_space.pointer)
	{
		d = ((double)(p - A->i_space.bottom)) *
			((double)(mapSize-startImmut)) /
			((double)(A->i_space.pointer-A->i_space.bottom));
		i = (int)d + startImmut;
	}
	else { ASSERT(0); }
	if (p > pMap[i].addr)
	{
		while (p > pMap[i].addr && i < mapSize) i++;
	}
	else
	{
		while (p < pMap[i].addr && i > 0) i--;
	}
	if (p == pMap[i].addr) return i;
	ASSERT(0);
	return -1;
}

static void printCodeAddr(byte *q)
/* Address into code.  Either the pc field of a stack segment or
   a word + 2 format address. */
{
	word *pt;
	int a;
	ASSERT((word*)q >= A->i_space.bottom && (word*)q < A->i_space.pointer);
	OBJ_CODEPTR_TO_PTR(q, pt);
	a = getIndex(pt);
	printf("$%d+%d", a, q - (byte*)pt);
}

static void printValue(word q)
{
	if (IS_INT(q)) printf("%d", UNTAGGED(q));
	else if ((word*)q >= A->gc_const.io_bottom &&
			 (word*)q < A->gc_const.io_top)
	{
		int n = q - (int)A->gc_const.io_bottom;
		int offset = n % (A->gc_const.io_spacing*sizeof(word));
		n = n / (A->gc_const.io_spacing*sizeof(word));
		ASSERT(n >= 0 && n < POLY_SYS_vecsize);
		if (offset == 0) printf("I%d", n, offset); else printf("J%d+%d", n, offset);
	}
	else if (OBJ_IS_CODEPTR(q)) printCodeAddr((byte *)q);
	else printf("@%d", getIndex((word*)q));
}

static void printObject(word *p)
{
	word L = p[-1];
	int i;
	int length = OBJ_OBJECT_LENGTH(L);

	printf("%d:", getIndex(p));

	if (OBJ_IS_MUTABLE_OBJECT(L)) putchar('M');
	if (OBJ_IS_NEGATIVE(L)) putchar('N');

	if (OBJ_IS_BYTE_OBJECT(L))
	{
		/* May be a string, a long format arbitrary precision
		   number or a real number. */
		pstring ps = (pstring)p;
		/* See if the first word is a possible length.  The length
		   cannot be one because single character strings are
		   represented by the character. */
		/* This is not infallible but it seems to be good enough
		   to detect the strings. */
		if (ps->length > 1 &&
			(int)((ps->length + sizeof(word) -1) / sizeof(word)) == length-1)
		{
			/* Looks like a string. */
			printf("S%d|", ps->length);
			for (i = 0; i < ps->length; i++)
			{
				char ch = ps->chars[i];
				printf("%02x", ch);
			}
		}
		else
		{
			/* Not a string. May be an arbitrary precision integer.
			   If the source and destination word lengths differ we
			   could find that some long-format arbitrary precision
			   numbers could be represented in the tagged short form
			   or vice-versa.  The former case might give rise to
			   errors because when comparing two arbitrary precision
			   numbers for equality we assume that they are not equal
			   if they have different representation.  The latter
			   case could be a problem because we wouldn't know whether
			   to convert the tagged form to long form, which would be
			   correct if the value has type "int" or to truncate it
			   which would be correct for "word".
			   It could also be a real number but that doesn't matter
			   if we recompile everything on the new machine.
			*/
			byte *u = (byte*)p;
			putchar('B');
			printf("%d|", length*sizeof(word));
			for (i = 0; i < (int)(length*sizeof(word)); i++)
			{
				printf("%02x", u[i]);
			}
		}
	}
	else if (OBJ_IS_CODE_OBJECT(L))
	{
		int isOpt = 0, constCount, byteCount;
		word *cp;
		byte *u;
		ASSERT(! OBJ_IS_MUTABLE_OBJECT(L) );
		putchar('C');
		/* Work out the number of bytes in the code and the
		   number of constants. */
		constCount = p[length - 1];
		OBJ_PTR_TO_CONSTS_PTR(p,cp);
		/* The byte count is the length of the segment minus the
		   number of constant minus one if this is an optimised
		   closure minus one for the constant count, one for the
		   marker word, one for the byte count and one for the
		   profile count. */
		byteCount = (length - constCount - isOpt - 4) * sizeof(word);
		printf("%d,%d|", constCount, byteCount);
		/* First the code. */
		if (isOpt) u = (byte*)(p+1);
		else u = (byte*)p;
		for (i = 0; i < byteCount; i++)
		{
			printf("%02x", u[i]);
		}
		putchar('|');
		/* Now the constants. */
		for (i = 0; i < constCount; i++)
		{
			printValue(cp[i]);
			if (i < constCount-1) putchar(',');
		}
	}
	else if (OBJ_IS_STACK_OBJECT(L))
	{
		StackObject *s = (StackObject*)p;
		int nUnchecked, stackLength;
		word *q;
		ASSERT(! OBJ_IS_MUTABLE_OBJECT(L));
		printf("Q%d|", length);
		/* First the standard registers, space, pc, sp, hr. */
		printf("%d,", s->p_space);
		/* pc may be TAGGED(0) indicating a retry. */
		if (IS_INT(s->p_pc)) printf("%d", UNTAGGED(s->p_pc)); else printCodeAddr(s->p_pc);
		putchar(',');
		printf("+%d,", s->p_sp-p); /* Word offset of sp. */
		printf("+%d", s->p_hr-p); /* Word offset of hr. */
		/* Checked registers. */
		printf(" %d|", s->p_nreg);
		for (i = 0; i < s->p_nreg; i++)
		{
			word r = s->p_reg[i];
			if ((word*)r >= p && (word*)r < p + length)
				printf("+%d", (word*)r-p);
			else printValue(r);
			if (i < s->p_nreg-1) putchar(',');
		}
		/* Unchecked registers, just as numbers. */
		nUnchecked = s->p_reg[i++];
		printf(" %d|", nUnchecked);
		nUnchecked += i;
		for (; i < nUnchecked; i++)
		{
			printf("%d", s->p_reg[i]);
			if (i < nUnchecked-1) putchar(',');
		}
		/* Now the values on the stack. */
		stackLength = length - (s->p_sp-p);
		printf(" %d|", stackLength);
		q = s->p_sp;
		for (i = 0; i < stackLength; i++)
		{
			word r = q[i];
			/* A stack may contain a value which is an offset. */
			if ((word*)r >= p && (word*)r < p + length)
				printf("+%d", (word*)r-p);
			else printValue(r);
			if (i < stackLength-1) putchar(',');
		}
	}
	else /* Ordinary objects, essentially tuples. */
	{
		putchar('O');
		printf("%d|", length);
		for (i = 0; i < length; i++)
		{
			printValue(p[i]);
			if (i < length-1) putchar(',');
		}
	}
	printf("\n");
}

/******************************************************************************/
/*                                                                            */
/*      main                                                                  */
/*                                                                            */
/******************************************************************************/

int main(int argc, char **argv)
{
	char    *filename    = NULL;
	int		i;
	word	*p;

	if (argc >= 2) filename = argv[1];

	if (filename == NULL) Exit("Usage: writeport <dbase>");

	A = OpenMappedDatabase(filename, M_DISCGARB1, 0);

	/* How many objects? */
	mapSize = 0;
	for (p = A->i_space.bottom; p < A->i_space.pointer; )
	{
		int length = OBJ_OBJECT_LENGTH(*p);
		/* At the end of a page we may have zeros. */
		ASSERT(length >= 0);
		if (length != 0) mapSize++;
		p += length+1;
	}
	ASSERT(p == A->i_space.pointer);

	for (p = A->m_space.bottom; p < A->m_space.pointer; )
	{
		int length = OBJ_OBJECT_LENGTH(*p);
		ASSERT(length >= 0);
		if (length != 0) mapSize++;
		p += length+1;
	}
	ASSERT(p == A->m_space.pointer);

	/* Allocate the map. */
	pMap = (POBJECTMAP)calloc(mapSize, sizeof(OBJECTMAP));

	/* And set the entries. */
	i = 0;
	/* Mutable area first because it's lower. */
	for (p = A->m_space.bottom; p < A->m_space.pointer; )
	{
		int length = OBJ_OBJECT_LENGTH(*p);
		if (length != 0)
		{
			ASSERT(i < mapSize);
			pMap[i++].addr = p+1;
		}
		p += length+1;
	}
	startImmut = i;

	for (p = A->i_space.bottom; p < A->i_space.pointer; )
	{
		int length = OBJ_OBJECT_LENGTH(*p);
		if (length != 0)
		{
			ASSERT(i < mapSize);
			pMap[i++].addr = p+1;
		}
		p += length+1;
	}

	ASSERT(i == mapSize);

	/* Start writing the information. */
	printf("Objects\t%d\n", mapSize);
	printf("Root\t%d\n", getIndex((word*)A->root));

	i = 0;
	for (p = A->m_space.bottom; p < A->m_space.pointer; )
	{
		int length = OBJ_OBJECT_LENGTH(*p);
		if (length != 0)
		{
			ASSERT(pMap[i++].addr == p+1);
			printObject(p+1);
		}
		p += length+1;
	}

	for (p = A->i_space.bottom; p < A->i_space.pointer; )
	{
		int length = OBJ_OBJECT_LENGTH(*p);
		if (length != 0)
		{
			ASSERT(pMap[i++].addr == p+1);
			printObject(p+1);
		}
		p += length+1;
	}

	ASSERT(i == mapSize);

	return 0;
}

#if ! (defined(i386) || defined(INTERPRETED))
/* We have to define this to keep the linker happy. */
void MD_flush_instruction_cache(void *x , int y)
{
}
#endif
