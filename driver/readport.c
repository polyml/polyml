/*
    Title:      Read portable database format

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
#include <stdio.h>
#if !defined(WINDOWS_PC) 
#include <unistd.h>
#endif
#include <stddef.h>
#include <signal.h>
#include <assert.h>
#include <string.h>
#include <errno.h>

/* filemode stuff */
#include <sys/types.h>
#include <sys/stat.h>

#if defined(FREEBSD) || defined(MACOSX)
#include <stdlib.h>
#elif defined(WINDOWS_PC)
#include <malloc.h>
#include <io.h>
#include <fcntl.h>
/* Just use built-in alloca without declaration */
#else
#include <alloca.h>
#endif

#include "globals.h"

#include "mmap.h"
#include "alloc.h"

/* added SPF 26/10/93 */
#include "gc.h"
#include "copygc.h"
#include "version.h"
#include "improve.h"
#include "diagnostics.h"
#include "proper_io.h"
#include "addresses.h"


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
static Header B = 0;
static FILE	*f;
static int nObjects, nRoot;
static word	**objMap;

#ifdef WINDOWS_PC
void SetResetCC(const int enable) /* OK */
{
	/* N.B. This only works in Windows NT. */
	SetConsoleCtrlHandler(NULL, enable ? TRUE : FALSE);
}
#endif

static void CopyRoot(root_struct *(*copy)())
{
  SetRoot ( B,(* copy)(Root(A)) );
}

/* Read a value and store it at the specified word. */
static void readValue(word *p, int i)
{
	int ch;
	ch = getc(f);
	if (ch == '@')
	{
		/* Address of an object. */
		int obj;
		fscanf(f, "%d", &obj);
		ASSERT(obj >= 0 && obj < nObjects);
		p[i] = (word)objMap[obj];
	}
	else if (ch == '$')
	{
		/* Code address. */
		int obj, offset;
		word *q;
		fscanf(f, "%d+%d", &obj, &offset);
		ASSERT(obj >= 0 && obj < nObjects);
		q = objMap[obj];
		ASSERT(OBJ_IS_CODE_OBJECT(q[-1]));
		p[i] = (word)q + offset; /* The offset is in bytes. */
	}
	else if ((ch >= '0' && ch <= '9') || ch == '-')
	{
		/* Tagged integer. */
		int j;
		ungetc(ch, f);
		fscanf(f, "%d", &j);
		/* The assertion may be false if we are porting to a machine
		   with a shorter tagged representation. */
		ASSERT(j >= -MAXTAGGED-1 && j <= MAXTAGGED);
		p[i] = TAGGED(j);
	}
	else if (ch == '+')
	{
		/* Offset within the object.  Only in a stack. */
		int j;
		ASSERT(OBJ_IS_STACK_OBJECT(p[-1]));
		fscanf(f, "%d", &j);
		ASSERT(j >= 0 && j < OBJ_OBJECT_LENGTH(p[-1]));
		p[i] = (word)(p + j);
	}
	else if (ch == 'I')
	{
		/* IO entry number. */
		int j;
		fscanf(f, "%d", &j);
		ASSERT(j >= 0 && j < POLY_SYS_vecsize);
		p[i] = ((word)IO_BOTTOM) + j * IO_SPACING * sizeof(word);
	}
	else if (ch == 'J')
	{
		/* IO entry number with offset. */
		int j, offset;
		fscanf(f, "%d+%d", &j, &offset);
		ASSERT(j >= 0 && j < POLY_SYS_vecsize);
		p[i] = ((word)IO_BOTTOM) + j * IO_SPACING * sizeof(word) + offset;
	}
	else
	{
		ASSERT(0);
		Crash("Unexpected character in stream");
	}
}

int main(int argc, char **argv)
{
	char     extension[] = "~";
	char    *filename    = NULL;
	char	*portname = NULL;
	char	*tempfilename;
	word     total;
	GCConst *a;
	GCConst *b;
	GCSpace *old;
	GCSpace  new;
	int		ch, objNo;

    if (argc != 3) {
		fprintf(stderr, "Usage: %s portabledatabase newdatabase\n", argv[0]);
		exit(1);
	}

	filename = argv[2];
	portname = argv[1];
 
	tempfilename = (char *)alloca(strlen(filename) + strlen(extension) + 1);
	strcpy(tempfilename,filename);
	strcat(tempfilename,extension);

	CreateMappedDatabase(filename);

	A = OpenMappedDatabase(filename, M_DISCGARB1, 0);

	/* First pass. Create the objects. */
	f = fopen(portname, "r");
	if (f == 0)
	{
		fprintf(stderr, "Unable to open file: %s\n", filename);
		exit(1);
	}

	ch = getc(f);
	/* Skip the "Mapping" line. */
	if (ch == 'M') { while (getc(f) != '\n') ; ch = getc(f); }
	ASSERT(ch == 'O'); /* Number of objects. */
	while (getc(f) != '\t') ;
	fscanf(f, "%d", &nObjects);
	/* Create a mapping table. */
	objMap = (word**)calloc(nObjects, sizeof(word*));

	do
	{
		ch = getc(f);
	} while (ch == '\n');
	ASSERT(ch == 'R'); /* Root object number. */
	while (getc(f) != '\t') ;
	fscanf(f, "%d", &nRoot);

	/* Now the objects themselves. */
	while (1)
	{
		int		isMutable = 0, isOpt = 0;
		word	objBits = 0, *p;
		int		nWords, nBytes;
		do
		{
			ch = getc(f);
		} while (ch == '\r' || ch == '\n');
		if (ch == EOF) break;
		ungetc(ch, f);
		fscanf(f, "%d", &objNo);
		ch = getc(f);
		ASSERT(ch == ':');
		ASSERT(objNo < nObjects);

		/* Modifiers, M, N or L. */
		do
		{
			ch = getc(f);
			if (ch == 'M') { isMutable = 1; objBits |= OBJ_MUTABLE_BIT; }
			else if (ch == 'N') objBits |= OBJ_NEGATIVE_BIT;
			else if (ch == 'L') { isOpt = 1; objBits |= OBJ_FIRST_BIT; }
		} while (ch == 'M' || ch == 'N' || ch == 'L');

		/* Object type. */
		switch (ch)
		{
		case 'Q': /* Stack segment. */
			objBits |= OBJ_STACK_BIT;
		case 'O': /* Simple object. */
			fscanf(f, "%d", &nWords);
			break;

		case 'B': /* Byte segment. */
			objBits |= OBJ_BYTE_BIT;
			fscanf(f, "%d", &nBytes);
			/* Round up to appropriate number of words. */
			nWords = (nBytes + sizeof(word) -1) / sizeof(word);
			break;

		case 'S': /* String. */
			objBits |= OBJ_BYTE_BIT;
			/* The length is the number of characters. */
			fscanf(f, "%d", &nBytes);
			/* Round up to appropriate number of words.  Need to add
			   one word for the length word.  */
			nWords = (nBytes + sizeof(word) -1) / sizeof(word) + 1;
			break;

		case 'C': /* Code segment. */
			objBits |= OBJ_CODE_BIT;
			/* Read the number of bytes of code and the number of words
			   for constants. */
			fscanf(f, "%d,%d", &nWords, &nBytes);
			if (isOpt) nWords++; /* Add word for pointer. */
			nWords += 4; /* Add words for extras. */
			/* Add in the size of the code itself. */
			nWords += (nBytes + sizeof(word) -1) / sizeof(word);
			break;

		default:
			ASSERT(0);
			SysError("Invalid object type");
		}

		if (isMutable) p = NewMutable(A, nWords);
		else p = NewImmutable(A, nWords);
		objMap[objNo] = p;
		/* Put in length word and flag bits. */
		p[-1] = nWords | objBits;

		/* Skip the object contents. */
		while (getc(f) != '\n') ;
	}
	ASSERT(objNo == nObjects-1);

	/* Second pass - fill in the contents. */
	fseek(f, 0, SEEK_SET);
	/* Skip the information at the start. */
	ch = getc(f);
	if (ch == 'M') { while (getc(f) != '\n') ; ch = getc(f); }
	ASSERT(ch == 'O'); /* Number of objects. */
	while (getc(f) != '\n') ; ch = getc(f);
	ASSERT(ch == 'R'); /* Root object number. */
	while (getc(f) != '\n') ;

	while (1)
	{
		int		i, isOpt = 0;
		word	*p;
		int		nWords, nBytes;
		if (feof(f)) break;
		fscanf(f, "%d", &objNo);
		if (feof(f)) break;
		ch = getc(f);
		ASSERT(ch == ':');
		ASSERT(objNo < nObjects);
		p = objMap[objNo];

		/* Modifiers, M, N or L. */
		do
		{
			ch = getc(f);
			if (ch == 'L') isOpt = 1;
		} while (ch == 'M' || ch == 'N' || ch == 'L');

		/* Object type. */
		switch (ch)
		{
		case 'O': /* Simple object. */
			fscanf(f, "%d", &nWords);
			ch = getc(f);
			ASSERT(ch == '|');
			ASSERT(nWords == OBJ_OBJECT_LENGTH(p[-1]));

			for (i = 0; i < nWords; i++)
			{
				readValue(p, i);
				ch = getc(f);
				ASSERT((ch == ',' && i < nWords-1) ||
					   (ch == '\n' && i == nWords-1));
			}

			break;

		case 'B': /* Byte segment. */
			{
				byte *u = (byte*)p;
				fscanf(f, "%d", &nBytes);
				ch = getc(f); ASSERT(ch == '|');
				for (i = 0; i < nBytes; i++)
				{
					int n;
					fscanf(f, "%02x", &n);
					u[i] = n;
				}
				ch = getc(f);
				ASSERT(ch == '\n');
				break;
			}

		case 'S': /* String. */
			{
				pstring ps = (pstring)p;
				/* The length is the number of characters. */
				fscanf(f, "%d", &nBytes);
				ch = getc(f); ASSERT(ch == '|');
				ps->length = nBytes;
				for (i = 0; i < nBytes; i++)
				{
					int n;
					fscanf(f, "%02x", &n);
					ps->chars[i] = n;
				}
				ch = getc(f);
				ASSERT(ch == '\n');
				break;
			}

		case 'C': /* Code segment. */
			{
				byte *u = (byte*)p;
				word length = OBJ_OBJECT_LENGTH(p[-1]);
				/* Read the number of bytes of code and the number of words
				   for constants. */
				fscanf(f, "%d,%d", &nWords, &nBytes);
				/* Set the first word of an optimised closure and skip it. */
				if (isOpt) { p[0] = (word)p; u = (byte*)(p+1); }
				/* Read the code. */
				ch = getc(f); ASSERT(ch == '|');
				for (i = 0; i < nBytes; i++)
				{
					int n;
					fscanf(f, "%02x", &n);
					u[i] = n;
				}
				ch = getc(f);
				ASSERT(ch == '|');
				/* Set the constant count. */
				p[length-1] = nWords;
				p[length-1-nWords-1] = 0; /* Profile count. */
				p[length-1-nWords-3] = 0; /* Marker word. */
				p[length-1-nWords-2] = (length-1-nWords-2)*sizeof(word);
				/* Check - the code should end at the marker word. */
				ASSERT(nBytes == (int)((length-1-nWords-3)*sizeof(word)));
				/* Read in the constants. */
				p = p+length-nWords-1;
				for (i = 0; i < nWords; i++)
				{
					readValue(p, i);
					ch = getc(f);
					ASSERT((ch == ',' && i < nWords-1) ||
						   (ch == '\n' && i == nWords-1));
				}
				break;
			}

		case 'Q': /* Stack segment. */
			{
				StackObject *s = (StackObject*)p;
				int n;
				word length = OBJ_OBJECT_LENGTH(p[-1]);
				fscanf(f, "%d", &nWords);
				ch = getc(f); ASSERT(ch == '|');

				/* Standard fields: size, pc, sp, hr. */
				fscanf(f, "%d", &s->p_space);
				ch = getc(f); ASSERT(ch == ',');
				readValue(p, offsetof(StackObject, p_pc) / sizeof(word));
				ch = getc(f); ASSERT(ch == ',');
				readValue(p, offsetof(StackObject, p_sp) / sizeof(word));
				ch = getc(f); ASSERT(ch == ',');
				readValue(p, offsetof(StackObject, p_hr) / sizeof(word));

				/* Checked registers. */
				fscanf(f, "%d", &n);
				s->p_nreg = n;
				ch = getc(f); ASSERT(ch == '|');
				for (i = 0; i < n; i++)
				{
					readValue(p, offsetof(StackObject, p_reg[i]) / sizeof(word));
					ch = getc(f);
					ASSERT((ch == ',' && i < n-1) ||
						   (ch == ' ' && i == n-1));
				}
				/* Unchecked registers. */
				fscanf(f, "%d", &n);
				s->p_reg[i] = n;
				ch = getc(f); ASSERT(ch == '|');
				for (i = 0; i < n; i++)
				{
					int n;
					fscanf(f, "%d", &n);
					s->p_reg[s->p_nreg+i+1] = n;
					ch = getc(f);
					ASSERT((ch == ',' && i < n-1) ||
						   (ch == ' ' && i == n-1));
				}
				/* Stack values. */
				fscanf(f, "%d", &n);
				ASSERT(n == length - (s->p_sp-p));
				ch = getc(f); ASSERT(ch == '|');
				for (i = 0; i < n; i++)
				{
					readValue(p, length-n+i);
					ch = getc(f);
					ASSERT((ch == ',' && i < n-1) ||
						   (ch == '\n' && i == n-1));
				}

				break;
			}

		default:
			ASSERT(0);
			SysError("Invalid object type");
		}
	}
	ASSERT(objNo == nObjects-1);


	/* Now create the database. */
	A->root = (root_struct*)(objMap[nRoot]);
	A->gc_space.i_top    = A->i_space.pointer;
	A->gc_space.m_top    = A->m_space.pointer;
    
	/* Create the new database, with a temporary name */
	CreateNewCopyDatabase(tempfilename, A);

	a = &A->gc_const;

	/* Do we need these special flags? */
	B = OpenMappedDatabase(tempfilename,M_WRITABLE | M_DISCGARB2,0);
	b = &->gc_const;
  
	old = &A->gc_space;
	new = NewDataSpaces(B);
  
	ResetAllocator();
  
	/* Copy the reachable heap from old to new;
     don't bother to restore old afterwards. */
	total = CopyGC ((OpRootsFunc)CopyRoot, a, b, old->parent, old, &new,
		  B, NewMutable, NewImmutable, 0);

	/* Close the new database */
	CommitMappedDatabase(B);
  
	/* Rename it */
	proper_printf("Renaming %s to %s\n",tempfilename, filename);
#ifdef WINDOWS_PC
	/* In Windows 95 there doesn't seem to be a way of atomically
	   replacing an existing file.  rename and MoveFile will only work
	   if the destination file does not already exist.  MoveFileEx will
	   do what we want in NT and 98.
	   We delete the old file first and then do the rename. */
	/* Must unmap all the mapped pages before trying to delete it
	   since Windows has an internal handle on the file.  Should
       probably do this in the Unix versions as well but we'll leave
       it for the moment.  */
	UnmapSpace(A,&A->i_space);
	UnmapSpace(A,&A->m_space);
	if (unlink(filename))
	{
		SysError("Unable to delete %s", filename);
	}
#endif
	if (rename(tempfilename,filename))
	{
		SysError("Unable to rename %s to %s", tempfilename, filename);
	}

	return 0;
}

#if ! (defined(i386) || defined(INTERPRETED))
/* We have to define this to keep the linker happy. */
void MD_flush_instruction_cache(void *x, int y)
{
}
#endif

