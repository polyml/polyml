/*
    Title:      Object size

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

#include <sys/types.h>
/*#include <malloc.h>*/
#include <stdlib.h>
#include <string.h>

#include "globals.h"
#include "memory.h"
#include "arb.h"
#include "proper_io.h"
#include "run_time.h"
#include "machine_dep.h"

#define bzero(dest,size) memset(dest,0,size);

typedef struct BitmapStruct Bitmap;

struct BitmapStruct
{
  word   *bitmap;
  word   *bottom;
  word   *top;
  Bitmap *next;
};

static int      total_length = 0;
static int      show_size    = 0;
static GCConst *C            = 0;
static Bitmap  *bitmaps      = 0;

#define AlreadyVisited(bm,p)   TestBit (bm->bitmap,p - bm->bottom)
#define SetVisited(bm,p)       SetBit  (bm->bitmap,p - bm->bottom)

static Bitmap *FindBitmap(Bitmap *bm, word *p)
{
  if (bm == 0) return 0;
  
  if (p > bm->bottom && p <= bm->top) return bm;
  
  return FindBitmap (bm->next,p);
}

static void ShowBytes(word *start, int length, int mutable)
{
  word  n,i;
  word  bytes = length * sizeof(word);
  char *array = (char *) start;
  
  proper_putc('\n', stdout);
  
  if (mutable)
  {
     proper_printf("MUTABLE ");
  }
  
  proper_printf("BYTES:%p:%d\n", array, bytes);
  
  for (i = 0, n = 0; n < bytes; n++)
  {
    proper_printf("%02x ",array[n] & 0xff);
    i++;
    if (i == 16)
    { 
       proper_putc('\n', stdout);
       i = 0;
    }
  }
  
  if (i != 0)
  {
     proper_putc('\n', stdout);
  }
}

#define MAXNAME 500

static char *CodeName(word *start, int length)
{
  static char buffer[MAXNAME+1];
  
  int n;
  
  for (n = 0; n < length; n++)
  {
    if (start[n] == 0)
    {
      word *string = (word *) start[n+3];
      
      if (Word(string) == TAGGED(0))
      {
        return "<not-named>";
      }
      else
      {
        /* bug-fixed version (SPF 17/12/1997) */
        (void) Poly_string_to_C((pstring)string, buffer, sizeof(buffer));
        return buffer;
      }
    }
  }
  
  return "<name-not-found>";
}

static void ShowCode(word *start, int length, int mutable)
{
  int n;
  int i;
  
  proper_putc('\n', stdout);
  
  if (mutable)
  {
     proper_printf("MUTABLE ");
  }
  
  proper_printf("CODE:%p:%d %s\n",
                  start, length, CodeName(start,length));
  
  for (i = 0, n = 0; n < length; n++)
  {
    if (i != 0)
    {
      proper_putc('\t', stdout);
    }

    proper_printf("%08x ", start[n]);
    i++;
    if (i == 4)
    { 
       proper_putc('\n', stdout);
       i = 0;
    }
  }
  
  if (i != 0)
  {
     proper_putc('\n', stdout);
  }
}

static void ShowWords(word *start, int length, int mutable)
{
  int n;
  int i;
  
  proper_putc('\n', stdout);
  
  if (mutable)
  {
     proper_printf("MUTABLE ");
  }
  
  proper_printf("WORDS:%p:%d\n", start, length);
  
  for (i = 0, n = 0; n < length; n++)
  {
    if (i != 0)
    {
      proper_putc('\t', stdout);
    }

    proper_printf("%08x ", start[n]);
    i++;
    if (i == 4)
    { 
       proper_putc('\n', stdout);
       i = 0;
    }
  }
  
  if (i != 0)
  {
     proper_putc('\n', stdout);
  }
}

static void VisitConstants(word **constnt); /* Forward declaration. */


static void VisitAddresses(word *start, int length)
{
  int n;
  int interesting_length;
  
  VISITLOOP:
  
  /* Discard non-interesting words from the end of the range.
     This helps the tail-call optimisation work better.
     Compare this with how we mark objects in gc.c.
     SPF 4/3/1998
  */
  for (n = length; 0 < n; n--)
  {
     word w = start[n - 1];
   
     if (IS_INT(w)) continue; /* not a pointer */
 
     if ((unsigned)w >= (unsigned)Word(C->io_bottom) && 
	 (unsigned)w <  (unsigned)Word(C->io_top))
	continue; /* IO segment */
     
     if (w == Word(C->nil)) continue;
 
     if (w == 0) continue; /* C nil */
     
     {
	word *p;
        word   *entry = Words(w);
        Bitmap *bm    = FindBitmap (bitmaps, entry);
        
        if (bm == 0)
        {
	   proper_printf("Bad address "ZERO_X"%p found\n", entry);
	   continue;
        }
    
	if (OBJ_IS_CODEPTR(entry))
	{
	   /* find beginning of the code object */
	   OBJ_CODEPTR_TO_PTR(entry,p);
	}
	else
	{
	   p = entry;
	}
	
	/* Have we already visited this object? */
	if (AlreadyVisited(bm, p))
	{
	   continue;
	}
     }
     
     /* We've finally found something worth visiting. */ 
     break;
  }
  
  interesting_length = n;
  
  for (n = 0; n < interesting_length; n++)
  {
    word w = start[n];
  
    if (IS_INT(w)) continue; /* not a pointer */

    if ((unsigned)w >= (unsigned)Word(C->io_bottom) && 
        (unsigned)w <  (unsigned)Word(C->io_top))
       continue; /* IO segment */
    
    if (w == Word(C->nil)) continue;

    if (w == 0) continue; /* C nil */
    
    /* Process all the words in each referenced object */
    {
	word  obj_length;
	word  L;
	int   mutable;
	word *p;
        word   *entry = Words(w);
        Bitmap *bm    = FindBitmap (bitmaps, entry);
        
        if (bm == 0)
        {
	   proper_printf("Bad address "ZERO_X"%p found\n", entry);
	   continue;
        }
    
	if (OBJ_IS_CODEPTR(entry))
	{
	   /* find beginning of the code object */
	   OBJ_CODEPTR_TO_PTR(entry,p);
	}
	else
	{
	   p = entry;
	}
	
	/* have we already marked this object? */
	if (AlreadyVisited(bm, p))
	{
	   continue;
	}
	SetVisited(bm,p);
      
	L          = p[-1];
	mutable    = OBJ_IS_MUTABLE_OBJECT(L);
	obj_length = OBJ_OBJECT_LENGTH(L);
      
	total_length += obj_length + 1; /* total space needed for object */
	
	if (OBJ_IS_BYTE_OBJECT(L))
	{
	  if (show_size) ShowBytes(p,obj_length,mutable);
	  continue;
	}
	else if (OBJ_IS_STACK_OBJECT(L))
	{
	  continue;
	}
	else if (OBJ_IS_CODE_OBJECT(L))
	{
	  word *cp;
	  int const_count = p[obj_length - 1];
	  
	  if (show_size)
	  {
	     ShowCode(p,obj_length,mutable);
	  }
	  
	  /* visit constants (at end of code segment) */
	  OBJ_PTR_TO_CONSTS_PTR(p,cp);

	  MD_update_code_addresses((word**)p, (word**)p, L, VisitConstants);
	  
	  if (n == interesting_length - 1)
	  {
	     /* simulate tail recursion */
	     start  = cp;
	     length = const_count;
	     goto VISITLOOP;
	  }
	  else
	  {
	     VisitAddresses (cp,const_count);
	  }
	}
	else /* Word object */
	{
	  if (show_size) ShowWords(p,obj_length,mutable);
	  if (n == interesting_length - 1)
	  {
	     /* simulate tail recursion */
	     start  = p;
	     length = obj_length;
	     goto VISITLOOP;
	  }
	  else
	  {
	     VisitAddresses (p,obj_length);
	  }
	}
    }
  } /* for loop */
}

static void VisitConstants(word **constnt)
{
	VisitAddresses((word*)constnt, 1);
}


static word *MakeBitmap(word *bottom, word *top)
{
  unsigned words  = top - bottom;
  unsigned bytes  = BITMAP_BYTES(words);
  word    *bitmap = Words(malloc(bytes));
  
  bzero (Bytes(bitmap),bytes);
  
  return bitmap;
}

static Bitmap *MakeBitmaps(GCSpace *s)
{
  Bitmap *bm1,*bm2;
  
  if (s == 0) return 0;
  
  bm1 = (Bitmap *) malloc(sizeof(Bitmap));
  bm2 = (Bitmap *) malloc(sizeof(Bitmap));

  bm1->bitmap = MakeBitmap (s->m_bottom,s->m_top);
  bm1->bottom = s->m_bottom;
  bm1->top    = s->m_top;
  bm1->next   = bm2;

  bm2->bitmap = MakeBitmap (s->i_bottom,s->i_top);
  bm2->bottom = s->i_bottom;
  bm2->top    = s->i_top;
  bm2->next   = s->parent ? MakeBitmaps (& s->parent->gc_space) : 0;

  return bm1;
}

static void FreeBitmaps(Bitmap *bm)
{
  if (bm == 0) return;
  
  FreeBitmaps (bm->next);

  free (Bytes(bm->bitmap));
  free (Bytes(bm));
}

static word Size(word *obj, int show)
{
  GCSpace space;
  
  C            = &H->gc_const;
  total_length = 0;
  show_size    = show;
  
  space.i_bottom = A.I.pointer;
  space.i_top    = A.I.top;
  space.m_bottom = A.M.pointer;
  space.m_top    = A.M.top;
  space.parent   = H;
  
  bitmaps = MakeBitmaps (& space);

  VisitAddresses (obj,1);
  
  FreeBitmaps (bitmaps);
  
  return total_length;
}

Handle objsize_c(word *obj)
{
  return Make_arbitrary_precision(Size(obj,0));
}

Handle showsize_c(word *obj)
{
	word result = Size(obj,1);
	proper_fflush(stdout); /* We need this for Windows at least. */
	return Make_arbitrary_precision(result);
}

void init_objsize_system(void){}
